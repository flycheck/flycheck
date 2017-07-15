package main

func main() {
	// xs is declared in a different file to ensure that test fails if gosimple is called incorrectly
	for _ = range xs {
	}
}
