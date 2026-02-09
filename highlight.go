package gilo

import (
	"strings"
	"unicode"
)

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

// Syntax defines a syntax highlighting scheme.
type Syntax struct {
	FileMatch              []string
	Keywords               []string
	SingleLineCommentStart string
	MultiLineCommentStart  string
	MultiLineCommentEnd    string
	Flags                  int
}

// HLDB is the built-in syntax highlight database.
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
	{
		FileMatch: []string{".go"},
		Keywords: []string{
			"break", "case", "chan", "const", "continue", "default", "defer",
			"else", "fallthrough", "for", "func", "go", "goto", "if",
			"import", "interface", "map", "package", "range", "return",
			"select", "struct", "switch", "type", "var",
			// Types (keyword2)
			"bool|", "byte|", "complex64|", "complex128|", "error|",
			"float32|", "float64|", "int|", "int8|", "int16|", "int32|",
			"int64|", "rune|", "string|", "uint|", "uint8|", "uint16|",
			"uint32|", "uint64|", "uintptr|", "any|",
			// Constants
			"true|", "false|", "nil|", "iota|",
			// Built-in functions
			"append", "cap", "close", "copy", "delete", "len", "make",
			"new", "panic", "print", "println", "recover",
		},
		SingleLineCommentStart: "//",
		MultiLineCommentStart:  "/*",
		MultiLineCommentEnd:    "*/",
		Flags:                  hlHighlightStrings | hlHighlightNumbers,
	},
	{
		FileMatch: []string{".py"},
		Keywords: []string{
			"and", "as", "assert", "async", "await", "break", "class",
			"continue", "def", "del", "elif", "else", "except", "finally",
			"for", "from", "global", "if", "import", "in", "is", "lambda",
			"nonlocal", "not", "or", "pass", "raise", "return", "try",
			"while", "with", "yield",
			// Types / built-ins (keyword2)
			"True|", "False|", "None|",
			"int|", "float|", "str|", "bool|", "list|", "dict|", "set|",
			"tuple|", "bytes|", "type|", "object|", "range|",
			// Built-in functions
			"print", "len", "input", "open", "super", "self",
			"isinstance", "issubclass", "hasattr", "getattr", "setattr",
		},
		SingleLineCommentStart: "# ",
		Flags:                  hlHighlightStrings | hlHighlightNumbers,
	},
}

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

	if e.syntax == nil {
		return
	}

	keywords := e.syntax.Keywords
	scs := e.syntax.SingleLineCommentStart
	mcs := e.syntax.MultiLineCommentStart
	mce := e.syntax.MultiLineCommentEnd

	r := row.render
	i := 0
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
