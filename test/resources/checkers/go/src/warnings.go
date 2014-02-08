package testpackage

import (
	. "fmt"
)

func Warn(a ...interface{}) {
	fmt.Printf(a...)
}

func Warnf(prefix string, format string, a ...interface{}) {
	fmt.Printf(format, a...)
}


func foo() {
	fmt.Printf("%s", 1)
	Warn("%s")
	Warnf("d", 1)

	return

	if true {
		return
	} else {
		return
	}
}
