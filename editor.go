// Package kilo is a port of antirez's kilo text editor to Go.
// It is a minimal terminal text editor that emits VT100 escape sequences
// directly, without depending on ncurses.
package gilo

import (
	"bytes"
	"fmt"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"
)

const Version = "0.0.1"

const (
	kiloQuitTimes = 3
	kiloQueryLen  = 256
	tabStop       = 8
)

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
				welcome := fmt.Sprintf("Kilo editor -- version %s", Version)
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
	case ctrlC, ctrlD:
		// Ignore
	case ctrlA, homeKey:
		e.cx = 0
		e.coloff = 0
	case ctrlE, endKey:
		filerow := e.rowoff + e.cy
		if filerow < len(e.rows) {
			rowlen := len(e.rows[filerow].chars)
			if rowlen > e.screencols-1 {
				e.cx = e.screencols - 1
				e.coloff = rowlen - e.screencols + 1
			} else {
				e.cx = rowlen
				e.coloff = 0
			}
		}
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
	case keyBackspace, ctrlH:
		e.delChar()
	case delKey:
		e.moveCursor(arrowRight)
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

// Run is the main editor loop. It enables raw mode, switches to the
// alternate screen buffer, and processes keys until the user quits.
// The terminal is restored on exit, SIGTERM, and SIGINT.
func (e *Editor) Run() error {
	if err := e.enableRawMode(); err != nil {
		return err
	}

	// Switch to alternate screen buffer (#105, #98, #92, #91)
	syscall.Write(syscall.Stdout, []byte("\x1b[?1049h"))

	cleanup := func() {
		// Leave alternate screen buffer and restore terminal
		syscall.Write(syscall.Stdout, []byte("\x1b[?1049l"))
		e.DisableRawMode()
	}
	defer cleanup()

	// Handle SIGTERM/SIGINT to restore terminal (#90)
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGTERM, syscall.SIGINT)
	go func() {
		<-sigCh
		cleanup()
		os.Exit(0)
	}()

	e.SetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find")
	for {
		e.refreshScreen()
		if !e.processKeypress() {
			return nil
		}
	}
}
