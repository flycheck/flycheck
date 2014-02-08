package testpackage

import (
	. "fmt"
)

func foo() {
	fmt.Printf("%s", 1)

	return

	if true {
		return
	} else {
		return
	}
}
