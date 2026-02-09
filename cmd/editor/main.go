package main

import (
	"fmt"
	"os"

	"github.com/xyproto/kilo"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "Usage: kilo <filename>\n")
		os.Exit(1)
	}

	e, err := kilo.New()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing editor: %s\n", err)
		os.Exit(1)
	}

	filename := os.Args[1]
	e.SelectSyntaxHighlight(filename)
	if err := e.Open(filename); err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %s\n", err)
		os.Exit(1)
	}

	if err := e.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %s\n", err)
		os.Exit(1)
	}
}
