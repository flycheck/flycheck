package main

import (
	"fmt"
	"warn"
)

func main() {
	warn.Warn("%s")
	warn.Warnf("d", 1)
	return
	fmt.Printf("%s", 1)
}
