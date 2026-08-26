package main

import "os"

func main() {
	f, _ := os.Open("enoent")
	f.Close()

	os.Stat("enoent")
}
