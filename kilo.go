// Package kilo is a port of antirez's kilo text editor to Go.
// It is a minimal terminal text editor that emits VT100 escape sequences
// directly, without depending on ncurses.
package kilo

import (
	"bytes"
	"fmt"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"
	"unicode"
	"unsafe"
)

const Version = "0.0.1"

// Syntax highlight types
const (
	hlNormal    = 0
	hlNonprint  = 1
	hlComment   = 2
	hlMLComment = 3
	hlKeyword1  = 4
	hlKeyword2  = 5
	hlString    = 6
	hlNumber    = 7
	hlMatch     = 8
)

const (
	hlHighlightStrings = 1 << 0
	hlHighlightNumbers = 1 << 1
)

// Key constants
const (
	keyNull      = 0
	ctrlC        = 3
	ctrlD        = 4
	ctrlF        = 6
	ctrlH        = 8
	keyTab       = 9
	ctrlL        = 12
	keyEnter     = 13
	ctrlQ        = 17
	ctrlS        = 19
	ctrlU        = 21
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

const (
	kiloQuitTimes = 3
	kiloQueryLen  = 256
	tabStop       = 8
)

// Syntax defines a syntax highlighting scheme.
type Syntax struct {
	FileMatch              []string
	Keywords               []string
	SingleLineCommentStart string
	MultiLineCommentStart  string
	MultiLineCommentEnd    string
	Flags                  int
}

// erow represents a single line of the file being edited.
type erow struct {
	idx    int
	chars  string
	render string
	hl     []byte
	hlOC   bool // had open comment at end
}

// Editor holds the complete state of the editor.
type Editor struct {
	cx, cy      int
	rowoff      int
	coloff      int
	screenrows  int
	screencols  int
	rows        []*erow
	dirty       int
	filename    string
	statusmsg   string
	statustime  time.Time
	syntax      *Syntax
	rawmode     bool
	origTermios syscall.Termios
	quitTimes   int
}

// The built-in syntax highlight database.
var HLDB = []Syntax{
	{
		FileMatch: []string{".c", ".h", ".cpp", ".hpp", ".cc"},
		Keywords: []string{
			// C keywords
			"auto", "break", "case", "continue", "default", "do", "else", "enum",
			"extern", "for", "goto", "if", "register", "return", "sizeof", "static",
			"struct", "switch", "typedef", "union", "volatile", "while", "NULL",
			// C++ keywords
			"alignas", "alignof", "and", "and_eq", "asm", "bitand", "bitor", "class",
			"compl", "constexpr", "const_cast", "deltype", "delete", "dynamic_cast",
			"explicit", "export", "false", "friend", "inline", "mutable", "namespace",
			"new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq",
			"private", "protected", "public", "reinterpret_cast", "static_assert",
			"static_cast", "template", "this", "thread_local", "throw", "true", "try",
			"typeid", "typename", "virtual", "xor", "xor_eq",
			// C types (trailing | means keyword2)
			"int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
			"void|", "short|", "auto|", "const|", "bool|",
		},
		SingleLineCommentStart: "//",
		MultiLineCommentStart:  "/*",
		MultiLineCommentEnd:    "*/",
		Flags:                  hlHighlightStrings | hlHighlightNumbers,
	},
}

// New creates a new Editor instance, initializes terminal size, and installs
// the SIGWINCH handler.
func New() (*Editor, error) {
	e := &Editor{
		quitTimes: kiloQuitTimes,
	}
	if err := e.updateWindowSize(); err != nil {
		return nil, err
	}
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, syscall.SIGWINCH)
	go func() {
		for range ch {
			e.handleSigWinCh()
		}
	}()
	return e, nil
}

// ---------- Low level terminal handling ----------

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

func getCursorPosition() (int, int, error) {
	if _, err := syscall.Write(syscall.Stdout, []byte("\x1b[6n")); err != nil {
		return 0, 0, err
	}
	var buf [32]byte
	i := 0
	for i < len(buf)-1 {
		n, _ := syscall.Read(syscall.Stdin, buf[i:i+1])
		if n != 1 || buf[i] == 'R' {
			break
		}
		i++
	}
	if buf[0] != keyEsc || buf[1] != '[' {
		return 0, 0, fmt.Errorf("failed to parse cursor position")
	}
	var rows, cols int
	_, err := fmt.Sscanf(string(buf[2:i]), "%d;%d", &rows, &cols)
	if err != nil {
		return 0, 0, err
	}
	return rows, cols, nil
}

type winsize struct {
	Row    uint16
	Col    uint16
	Xpixel uint16
	Ypixel uint16
}

func getWindowSize() (int, int, error) {
	ws := winsize{}
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL,
		uintptr(syscall.Stdout),
		uintptr(syscall.TIOCGWINSZ),
		uintptr(unsafe.Pointer(&ws)))
	if errno != 0 || ws.Col == 0 {
		// Fallback: move cursor to bottom-right and query position
		if _, err := syscall.Write(syscall.Stdout, []byte("\x1b[999C\x1b[999B")); err != nil {
			return 0, 0, err
		}
		return getCursorPosition()
	}
	return int(ws.Row), int(ws.Col), nil
}

// ---------- Syntax highlighting ----------

func isSeparator(c byte) bool {
	return c == 0 || c == ' ' || c == '\t' || c == '\n' || c == '\r' ||
		strings.ContainsRune(",.()+-/*=~%[];", rune(c))
}

func (e *Editor) rowHasOpenComment(row *erow) bool {
	if len(row.hl) > 0 && len(row.render) > 0 &&
		row.hl[len(row.hl)-1] == hlMLComment {
		rs := row.render
		if len(rs) < 2 || rs[len(rs)-2] != '*' || rs[len(rs)-1] != '/' {
			return true
		}
	}
	return false
}

func (e *Editor) updateSyntax(row *erow) {
	row.hl = make([]byte, len(row.render))
	// all initialized to hlNormal (0)

	if e.syntax == nil {
		return
	}

	keywords := e.syntax.Keywords
	scs := e.syntax.SingleLineCommentStart
	mcs := e.syntax.MultiLineCommentStart
	mce := e.syntax.MultiLineCommentEnd

	r := row.render
	i := 0
	// Skip leading whitespace
	for i < len(r) && (r[i] == ' ' || r[i] == '\t') {
		i++
	}

	prevSep := true
	inString := byte(0)
	inComment := false

	if row.idx > 0 && e.rowHasOpenComment(e.rows[row.idx-1]) {
		inComment = true
	}

	for i < len(r) {
		c := r[i]

		// Handle single line comments
		if prevSep && inString == 0 && !inComment && len(scs) == 2 &&
			i+1 < len(r) && c == scs[0] && r[i+1] == scs[1] {
			for j := i; j < len(r); j++ {
				row.hl[j] = hlComment
			}
			return
		}

		// Handle multi-line comments
		if inComment {
			row.hl[i] = hlMLComment
			if len(mce) == 2 && i+1 < len(r) && c == mce[0] && r[i+1] == mce[1] {
				row.hl[i+1] = hlMLComment
				i += 2
				inComment = false
				prevSep = true
				continue
			}
			prevSep = false
			i++
			continue
		} else if len(mcs) == 2 && i+1 < len(r) && c == mcs[0] && r[i+1] == mcs[1] {
			row.hl[i] = hlMLComment
			row.hl[i+1] = hlMLComment
			i += 2
			inComment = true
			prevSep = false
			continue
		}

		// Handle strings
		if inString != 0 {
			row.hl[i] = hlString
			if c == '\\' && i+1 < len(r) {
				row.hl[i+1] = hlString
				i += 2
				prevSep = false
				continue
			}
			if c == inString {
				inString = 0
			}
			i++
			continue
		} else if c == '"' || c == '\'' {
			inString = c
			row.hl[i] = hlString
			i++
			prevSep = false
			continue
		}

		// Handle non-printable chars
		if c < 32 && c != '\t' {
			row.hl[i] = hlNonprint
			i++
			prevSep = false
			continue
		}
		if !unicode.IsPrint(rune(c)) && c >= 127 {
			row.hl[i] = hlNonprint
			i++
			prevSep = false
			continue
		}

		// Handle numbers
		if e.syntax.Flags&hlHighlightNumbers != 0 {
			if (c >= '0' && c <= '9' && (prevSep || (i > 0 && row.hl[i-1] == hlNumber))) ||
				(c == '.' && i > 0 && row.hl[i-1] == hlNumber) {
				row.hl[i] = hlNumber
				i++
				prevSep = false
				continue
			}
		}

		// Handle keywords
		if prevSep {
			matched := false
			for _, kw := range keywords {
				kw2 := false
				kwClean := kw
				if strings.HasSuffix(kw, "|") {
					kw2 = true
					kwClean = kw[:len(kw)-1]
				}
				klen := len(kwClean)
				if i+klen <= len(r) && r[i:i+klen] == kwClean {
					// Check separator after keyword
					if i+klen == len(r) || isSeparator(r[i+klen]) {
						hlType := byte(hlKeyword1)
						if kw2 {
							hlType = hlKeyword2
						}
						for j := 0; j < klen; j++ {
							row.hl[i+j] = hlType
						}
						i += klen
						matched = true
						break
					}
				}
			}
			if matched {
				prevSep = false
				continue
			}
		}

		prevSep = isSeparator(c)
		i++
	}

	oc := e.rowHasOpenComment(row)
	if row.hlOC != oc && row.idx+1 < len(e.rows) {
		e.updateSyntax(e.rows[row.idx+1])
	}
	row.hlOC = oc
}

func syntaxToColor(hl byte) int {
	switch hl {
	case hlComment, hlMLComment:
		return 36 // cyan
	case hlKeyword1:
		return 33 // yellow
	case hlKeyword2:
		return 32 // green
	case hlString:
		return 35 // magenta
	case hlNumber:
		return 31 // red
	case hlMatch:
		return 34 // blue
	default:
		return 37 // white
	}
}

// SelectSyntaxHighlight selects the syntax scheme based on filename.
func (e *Editor) SelectSyntaxHighlight(filename string) {
	for i := range HLDB {
		s := &HLDB[i]
		for _, pattern := range s.FileMatch {
			if strings.HasPrefix(pattern, ".") {
				if strings.HasSuffix(filename, pattern) {
					e.syntax = s
					return
				}
			} else {
				if strings.Contains(filename, pattern) {
					e.syntax = s
					return
				}
			}
		}
	}
}

// ---------- Row operations ----------

func (e *Editor) updateRow(row *erow) {
	var buf bytes.Buffer
	for _, c := range []byte(row.chars) {
		if c == keyTab {
			buf.WriteByte(' ')
			for (buf.Len()+1)%tabStop != 0 {
				buf.WriteByte(' ')
			}
		} else {
			buf.WriteByte(c)
		}
	}
	row.render = buf.String()
	e.updateSyntax(row)
}

func (e *Editor) insertRow(at int, s string) {
	if at > len(e.rows) {
		return
	}
	row := &erow{
		idx:   at,
		chars: s,
	}
	if at == len(e.rows) {
		e.rows = append(e.rows, row)
	} else {
		e.rows = append(e.rows, nil)
		copy(e.rows[at+1:], e.rows[at:])
		e.rows[at] = row
		for j := at + 1; j < len(e.rows); j++ {
			e.rows[j].idx = j
		}
	}
	e.updateRow(row)
	e.dirty++
}

func (e *Editor) delRow(at int) {
	if at >= len(e.rows) {
		return
	}
	e.rows = append(e.rows[:at], e.rows[at+1:]...)
	for j := at; j < len(e.rows); j++ {
		e.rows[j].idx = j
	}
	e.dirty++
}

func (e *Editor) rowsToString() string {
	var buf bytes.Buffer
	for _, row := range e.rows {
		buf.WriteString(row.chars)
		buf.WriteByte('\n')
	}
	return buf.String()
}

func (e *Editor) rowInsertChar(row *erow, at int, c byte) {
	if at > len(row.chars) {
		padding := at - len(row.chars)
		row.chars += strings.Repeat(" ", padding) + string(c)
	} else {
		row.chars = row.chars[:at] + string(c) + row.chars[at:]
	}
	e.updateRow(row)
	e.dirty++
}

func (e *Editor) rowAppendString(row *erow, s string) {
	row.chars += s
	e.updateRow(row)
	e.dirty++
}

func (e *Editor) rowDelChar(row *erow, at int) {
	if at >= len(row.chars) {
		return
	}
	row.chars = row.chars[:at] + row.chars[at+1:]
	e.updateRow(row)
	e.dirty++
}

// ---------- Editor operations ----------

func (e *Editor) insertChar(c int) {
	filerow := e.rowoff + e.cy
	filecol := e.coloff + e.cx

	if filerow >= len(e.rows) {
		for len(e.rows) <= filerow {
			e.insertRow(len(e.rows), "")
		}
	}
	row := e.rows[filerow]
	e.rowInsertChar(row, filecol, byte(c))
	if e.cx == e.screencols-1 {
		e.coloff++
	} else {
		e.cx++
	}
	e.dirty++
}

func (e *Editor) insertNewline() {
	filerow := e.rowoff + e.cy
	filecol := e.coloff + e.cx

	if filerow >= len(e.rows) {
		if filerow == len(e.rows) {
			e.insertRow(filerow, "")
			e.fixCursorNewline()
		}
		return
	}

	row := e.rows[filerow]
	if filecol >= len(row.chars) {
		filecol = len(row.chars)
	}
	if filecol == 0 {
		e.insertRow(filerow, "")
	} else {
		e.insertRow(filerow+1, row.chars[filecol:])
		row = e.rows[filerow]
		row.chars = row.chars[:filecol]
		e.updateRow(row)
	}
	e.fixCursorNewline()
}

func (e *Editor) fixCursorNewline() {
	if e.cy == e.screenrows-1 {
		e.rowoff++
	} else {
		e.cy++
	}
	e.cx = 0
	e.coloff = 0
}

func (e *Editor) delChar() {
	filerow := e.rowoff + e.cy
	filecol := e.coloff + e.cx

	if filerow >= len(e.rows) {
		return
	}
	row := e.rows[filerow]

	if filecol == 0 && filerow == 0 {
		return
	}
	if filecol == 0 {
		filecol = len(e.rows[filerow-1].chars)
		e.rowAppendString(e.rows[filerow-1], row.chars)
		e.delRow(filerow)
		if e.cy == 0 {
			e.rowoff--
		} else {
			e.cy--
		}
		e.cx = filecol
		if e.cx >= e.screencols {
			shift := e.screencols - e.cx + 1
			e.cx -= shift
			e.coloff += shift
		}
	} else {
		e.rowDelChar(row, filecol-1)
		if e.cx == 0 && e.coloff > 0 {
			e.coloff--
		} else {
			e.cx--
		}
	}
	e.dirty++
}

// ---------- File I/O ----------

// Open loads a file into the editor.
func (e *Editor) Open(filename string) error {
	e.dirty = 0
	e.filename = filename

	data, err := os.ReadFile(filename)
	if err != nil {
		if os.IsNotExist(err) {
			return nil // new file
		}
		return err
	}

	lines := strings.Split(string(data), "\n")
	// Remove trailing empty line that results from split of trailing newline
	if len(lines) > 0 && lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}
	for _, line := range lines {
		line = strings.TrimRight(line, "\r")
		e.insertRow(len(e.rows), line)
	}
	e.dirty = 0
	return nil
}

// Save writes the current buffer to disk.
func (e *Editor) Save() error {
	if e.filename == "" {
		return fmt.Errorf("no filename")
	}
	buf := e.rowsToString()
	err := os.WriteFile(e.filename, []byte(buf), 0644)
	if err != nil {
		e.SetStatusMessage("Can't save! I/O error: %s", err)
		return err
	}
	e.dirty = 0
	e.SetStatusMessage("%d bytes written on disk", len(buf))
	return nil
}

// ---------- Terminal update ----------

func (e *Editor) refreshScreen() {
	var ab bytes.Buffer

	ab.WriteString("\x1b[?25l") // Hide cursor
	ab.WriteString("\x1b[H")    // Go home

	for y := 0; y < e.screenrows; y++ {
		filerow := e.rowoff + y

		if filerow >= len(e.rows) {
			if len(e.rows) == 0 && y == e.screenrows/3 {
				welcome := fmt.Sprintf("Kilo editor -- verison %s", Version)
				if len(welcome) > e.screencols {
					welcome = welcome[:e.screencols]
				}
				padding := (e.screencols - len(welcome)) / 2
				if padding > 0 {
					ab.WriteByte('~')
					padding--
				}
				for padding > 0 {
					ab.WriteByte(' ')
					padding--
				}
				ab.WriteString(welcome)
				ab.WriteString("\x1b[0K\r\n")
			} else {
				ab.WriteString("~\x1b[0K\r\n")
			}
			continue
		}

		r := e.rows[filerow]
		renderLen := len(r.render) - e.coloff
		if renderLen < 0 {
			renderLen = 0
		}
		if renderLen > 0 {
			if renderLen > e.screencols {
				renderLen = e.screencols
			}
			rStr := r.render[e.coloff : e.coloff+renderLen]
			hl := r.hl[e.coloff : e.coloff+renderLen]
			currentColor := -1
			for j := 0; j < len(rStr); j++ {
				if hl[j] == hlNonprint {
					ab.WriteString("\x1b[7m")
					if rStr[j] <= 26 {
						ab.WriteByte('@' + rStr[j])
					} else {
						ab.WriteByte('?')
					}
					ab.WriteString("\x1b[0m")
				} else if hl[j] == hlNormal {
					if currentColor != -1 {
						ab.WriteString("\x1b[39m")
						currentColor = -1
					}
					ab.WriteByte(rStr[j])
				} else {
					color := syntaxToColor(hl[j])
					if color != currentColor {
						ab.WriteString(fmt.Sprintf("\x1b[%dm", color))
						currentColor = color
					}
					ab.WriteByte(rStr[j])
				}
			}
		}
		ab.WriteString("\x1b[39m")
		ab.WriteString("\x1b[0K")
		ab.WriteString("\r\n")
	}

	// Status bar (first row)
	ab.WriteString("\x1b[0K")
	ab.WriteString("\x1b[7m")
	modifiedStr := ""
	if e.dirty > 0 {
		modifiedStr = "(modified)"
	}
	fname := e.filename
	if len(fname) > 20 {
		fname = fname[:20]
	}
	status := fmt.Sprintf("%.20s - %d lines %s", fname, len(e.rows), modifiedStr)
	rstatus := fmt.Sprintf("%d/%d", e.rowoff+e.cy+1, len(e.rows))
	if len(status) > e.screencols {
		status = status[:e.screencols]
	}
	ab.WriteString(status)
	slen := len(status)
	for slen < e.screencols {
		if e.screencols-slen == len(rstatus) {
			ab.WriteString(rstatus)
			break
		}
		ab.WriteByte(' ')
		slen++
	}
	ab.WriteString("\x1b[0m\r\n")

	// Status message (second row)
	ab.WriteString("\x1b[0K")
	if e.statusmsg != "" && time.Since(e.statustime).Seconds() < 5 {
		msg := e.statusmsg
		if len(msg) > e.screencols {
			msg = msg[:e.screencols]
		}
		ab.WriteString(msg)
	}

	// Cursor position (account for tabs)
	cx := 1
	filerow := e.rowoff + e.cy
	if filerow < len(e.rows) {
		row := e.rows[filerow]
		for j := e.coloff; j < e.cx+e.coloff; j++ {
			if j < len(row.chars) && row.chars[j] == keyTab {
				cx += 7 - (cx % tabStop)
			}
			cx++
		}
	}
	ab.WriteString(fmt.Sprintf("\x1b[%d;%dH", e.cy+1, cx))
	ab.WriteString("\x1b[?25h") // Show cursor
	syscall.Write(syscall.Stdout, ab.Bytes())
}

// SetStatusMessage sets the editor status message.
func (e *Editor) SetStatusMessage(format string, args ...interface{}) {
	e.statusmsg = fmt.Sprintf(format, args...)
	e.statustime = time.Now()
}

// ---------- Find mode ----------

func (e *Editor) find() {
	query := make([]byte, 0, kiloQueryLen)
	lastMatch := -1
	findNext := 0
	savedHLLine := -1
	var savedHL []byte

	savedCx := e.cx
	savedCy := e.cy
	savedColoff := e.coloff
	savedRowoff := e.rowoff

	restoreHL := func() {
		if savedHL != nil {
			copy(e.rows[savedHLLine].hl, savedHL)
			savedHL = nil
		}
	}

	for {
		e.SetStatusMessage("Search: %s (Use ESC/Arrows/Enter)", string(query))
		e.refreshScreen()

		c := e.readKey()
		switch {
		case c == delKey || c == ctrlH || c == keyBackspace:
			if len(query) > 0 {
				query = query[:len(query)-1]
			}
			lastMatch = -1
		case c == keyEsc || c == keyEnter:
			if c == keyEsc {
				e.cx = savedCx
				e.cy = savedCy
				e.coloff = savedColoff
				e.rowoff = savedRowoff
			}
			restoreHL()
			e.SetStatusMessage("")
			return
		case c == arrowRight || c == arrowDown:
			findNext = 1
		case c == arrowLeft || c == arrowUp:
			findNext = -1
		default:
			if c >= 32 && c < 127 && len(query) < kiloQueryLen {
				query = append(query, byte(c))
				lastMatch = -1
			}
		}

		if lastMatch == -1 {
			findNext = 1
		}
		if findNext != 0 {
			current := lastMatch
			qstr := string(query)
			for i := 0; i < len(e.rows); i++ {
				current += findNext
				if current == -1 {
					current = len(e.rows) - 1
				} else if current == len(e.rows) {
					current = 0
				}
				matchOffset := strings.Index(e.rows[current].render, qstr)
				if matchOffset != -1 {
					restoreHL()
					row := e.rows[current]
					lastMatch = current
					savedHLLine = current
					savedHL = make([]byte, len(row.hl))
					copy(savedHL, row.hl)
					for j := 0; j < len(qstr) && matchOffset+j < len(row.hl); j++ {
						row.hl[matchOffset+j] = hlMatch
					}
					e.cy = 0
					e.cx = matchOffset
					e.rowoff = current
					e.coloff = 0
					if e.cx > e.screencols {
						diff := e.cx - e.screencols
						e.cx -= diff
						e.coloff += diff
					}
					break
				}
			}
			findNext = 0
		}
	}
}

// ---------- Cursor movement ----------

func (e *Editor) moveCursor(key int) {
	filerow := e.rowoff + e.cy
	filecol := e.coloff + e.cx
	var row *erow
	if filerow < len(e.rows) {
		row = e.rows[filerow]
	}

	switch key {
	case arrowLeft:
		if e.cx == 0 {
			if e.coloff > 0 {
				e.coloff--
			} else if filerow > 0 {
				e.cy--
				e.cx = len(e.rows[filerow-1].chars)
				if e.cx > e.screencols-1 {
					e.coloff = e.cx - e.screencols + 1
					e.cx = e.screencols - 1
				}
			}
		} else {
			e.cx--
		}
	case arrowRight:
		if row != nil && filecol < len(row.chars) {
			if e.cx == e.screencols-1 {
				e.coloff++
			} else {
				e.cx++
			}
		} else if row != nil && filecol == len(row.chars) {
			e.cx = 0
			e.coloff = 0
			if e.cy == e.screenrows-1 {
				e.rowoff++
			} else {
				e.cy++
			}
		}
	case arrowUp:
		if e.cy == 0 {
			if e.rowoff > 0 {
				e.rowoff--
			}
		} else {
			e.cy--
		}
	case arrowDown:
		if filerow < len(e.rows) {
			if e.cy == e.screenrows-1 {
				e.rowoff++
			} else {
				e.cy++
			}
		}
	}

	// Fix cx if current line doesn't have enough chars
	filerow = e.rowoff + e.cy
	filecol = e.coloff + e.cx
	rowlen := 0
	if filerow < len(e.rows) {
		rowlen = len(e.rows[filerow].chars)
	}
	if filecol > rowlen {
		e.cx -= filecol - rowlen
		if e.cx < 0 {
			e.coloff += e.cx
			e.cx = 0
		}
	}
}

// ---------- Event processing ----------

func (e *Editor) processKeypress() bool {
	c := e.readKey()
	switch c {
	case keyEnter:
		e.insertNewline()
	case ctrlC:
		// Ignore
	case ctrlQ:
		if e.dirty > 0 && e.quitTimes > 0 {
			e.SetStatusMessage("WARNING!!! File has unsaved changes. Press Ctrl-Q %d more times to quit.", e.quitTimes)
			e.quitTimes--
			return true
		}
		return false
	case ctrlS:
		e.Save()
	case ctrlF:
		e.find()
	case keyBackspace, ctrlH, delKey:
		e.delChar()
	case pageUp, pageDown:
		if c == pageUp && e.cy != 0 {
			e.cy = 0
		} else if c == pageDown && e.cy != e.screenrows-1 {
			e.cy = e.screenrows - 1
		}
		times := e.screenrows
		dir := arrowDown
		if c == pageUp {
			dir = arrowUp
		}
		for times > 0 {
			e.moveCursor(dir)
			times--
		}
	case arrowUp, arrowDown, arrowLeft, arrowRight:
		e.moveCursor(c)
	case ctrlL, keyEsc:
		// Nothing
	default:
		if c >= 0 {
			e.insertChar(c)
		}
	}
	e.quitTimes = kiloQuitTimes
	return true
}

// ---------- Window size ----------

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

// FileWasModified returns true if the file has unsaved changes.
func (e *Editor) FileWasModified() bool {
	return e.dirty > 0
}

// Run is the main editor loop. It enables raw mode, shows the help message,
// and processes keys until the user quits.
func (e *Editor) Run() error {
	if err := e.enableRawMode(); err != nil {
		return err
	}
	defer e.DisableRawMode()

	e.SetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find")
	for {
		e.refreshScreen()
		if !e.processKeypress() {
			// Clear screen on exit
			syscall.Write(syscall.Stdout, []byte("\x1b[2J\x1b[H"))
			return nil
		}
	}
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
