package main

import "strings"

func main() {
	// S1005 (gosimple); xs is declared in a sibling file, so the check
	// fails if staticcheck is handed this file alone instead of the package
	for _ = range xs {
	}

	// SA1018 (staticcheck)
	_ = strings.Replace("foo", "f", "b", 0)
}

// U1000 (unused)
func unused() {}
