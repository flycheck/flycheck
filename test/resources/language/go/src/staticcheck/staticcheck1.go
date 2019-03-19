package main

import "strings"

func main() {
	// S1005 (gosimple)
	// xs is declared in a different file to ensure that test fails if megacheck is called incorrectly
	for _ = range xs {
	}

	// SA1018 (staticcheck)
	_ = strings.Replace("foo", "f", "b", 0)
}

// U1000 (unused)
func unused() {}
