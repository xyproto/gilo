Gilo
====

Gilo is a port of Kilo (a small text editor in less than 1K lines of code) from C to Go.

### Usage

Usage: gilo `<filename>`

Keys:

    CTRL-S: Save
    CTRL-Q: Quit
    CTRL-F: Find string in file (ESC to exit search, arrows to navigate)

### Dependencies

Gilo does not depend on any library (not even curses). It uses fairly standard VT100 (and similar terminals) escape sequences.

### Installation

```
go install github.com/xyproto/gilo/cmd/editor@latest
```

### General info

* License: BSD-2
* Version: 1.0.0
