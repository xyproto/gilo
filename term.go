package gilo

import (
	"fmt"
	"syscall"
	"unsafe"
)

// Key constants
const (
	ctrlA        = 1
	ctrlC        = 3
	ctrlD        = 4
	ctrlE        = 5
	ctrlF        = 6
	ctrlH        = 8
	keyTab       = 9
	ctrlL        = 12
	keyEnter     = 13
	ctrlQ        = 17
	ctrlS        = 19
	keyEsc       = 27
	keyBackspace = 127

	arrowLeft  = 1000
	arrowRight = 1001
	arrowUp    = 1002
	arrowDown  = 1003
	delKey     = 1004
	homeKey    = 1005
	endKey     = 1006
	pageUp     = 1007
	pageDown   = 1008
)

type winsize struct {
	Row    uint16
	Col    uint16
	Xpixel uint16
	Ypixel uint16
}

func (e *Editor) enableRawMode() error {
	if e.rawmode {
		return nil
	}
	if !isatty(syscall.Stdin) {
		return fmt.Errorf("not a tty")
	}
	orig, err := tcgetattr(syscall.Stdin)
	if err != nil {
		return err
	}
	e.origTermios = orig

	raw := orig
	// Input modes
	raw.Iflag &^= syscall.BRKINT | syscall.ICRNL | syscall.INPCK | syscall.ISTRIP | syscall.IXON
	// Output modes
	raw.Oflag &^= syscall.OPOST
	// Control modes
	raw.Cflag |= syscall.CS8
	// Local modes
	raw.Lflag &^= syscall.ECHO | syscall.ICANON | syscall.IEXTEN | syscall.ISIG
	// Control chars
	raw.Cc[syscall.VMIN] = 0
	raw.Cc[syscall.VTIME] = 1

	if err := tcsetattr(syscall.Stdin, raw); err != nil {
		return err
	}
	e.rawmode = true
	return nil
}

// DisableRawMode restores the terminal to its original mode.
func (e *Editor) DisableRawMode() {
	if e.rawmode {
		tcsetattr(syscall.Stdin, e.origTermios)
		e.rawmode = false
	}
}

func (e *Editor) readKey() int {
	buf := make([]byte, 1)
	for {
		n, err := syscall.Read(syscall.Stdin, buf)
		if n == 1 {
			break
		}
		if err != nil && err != syscall.EAGAIN {
			return -1
		}
	}
	c := int(buf[0])
	if c == keyEsc {
		seq := make([]byte, 3)
		n, _ := syscall.Read(syscall.Stdin, seq[0:1])
		if n == 0 {
			return keyEsc
		}
		n, _ = syscall.Read(syscall.Stdin, seq[1:2])
		if n == 0 {
			return keyEsc
		}
		if seq[0] == '[' {
			if seq[1] >= '0' && seq[1] <= '9' {
				n, _ = syscall.Read(syscall.Stdin, seq[2:3])
				if n == 0 {
					return keyEsc
				}
				if seq[2] == '~' {
					switch seq[1] {
					case '3':
						return delKey
					case '5':
						return pageUp
					case '6':
						return pageDown
					}
				}
			} else {
				switch seq[1] {
				case 'A':
					return arrowUp
				case 'B':
					return arrowDown
				case 'C':
					return arrowRight
				case 'D':
					return arrowLeft
				case 'H':
					return homeKey
				case 'F':
					return endKey
				}
			}
		} else if seq[0] == 'O' {
			switch seq[1] {
			case 'H':
				return homeKey
			case 'F':
				return endKey
			}
		}
		return keyEsc
	}
	return c
}

func getWindowSize() (int, int, error) {
	ws := winsize{}
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL,
		uintptr(syscall.Stdout),
		uintptr(syscall.TIOCGWINSZ),
		uintptr(unsafe.Pointer(&ws)))
	if errno != 0 || ws.Col == 0 {
		// The cursor-position fallback requires raw mode, which may not be
		// active yet (see antirez/kilo#97, #69). Return a safe default.
		return 24, 80, nil
	}
	return int(ws.Row), int(ws.Col), nil
}

func (e *Editor) updateWindowSize() error {
	rows, cols, err := getWindowSize()
	if err != nil {
		return err
	}
	e.screenrows = rows - 2 // room for status bar
	e.screencols = cols
	return nil
}

func (e *Editor) handleSigWinCh() {
	e.updateWindowSize()
	if e.cy > e.screenrows {
		e.cy = e.screenrows - 1
	}
	if e.cx > e.screencols {
		e.cx = e.screencols - 1
	}
	e.refreshScreen()
}

// ---------- termios helpers (POSIX) ----------

func isatty(fd int) bool {
	_, err := tcgetattr(fd)
	return err == nil
}

func tcgetattr(fd int) (syscall.Termios, error) {
	var t syscall.Termios
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL,
		uintptr(fd),
		uintptr(ioctlReadTermios),
		uintptr(unsafe.Pointer(&t)))
	if errno != 0 {
		return t, errno
	}
	return t, nil
}

func tcsetattr(fd int, t syscall.Termios) error {
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL,
		uintptr(fd),
		uintptr(ioctlWriteTermios),
		uintptr(unsafe.Pointer(&t)))
	if errno != 0 {
		return errno
	}
	return nil
}
