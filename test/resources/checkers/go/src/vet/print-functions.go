package printFunctions

import (
	"fmt"
	"io"
	"os"
)

func Warn(a ...interface{}) (n int, err error) {
	return fmt.Fprint(os.Stdout, a...)
}

func Warnf(w io.Writer, format string, a ...interface{}) (n int, err error) {
	n, err = w.Write(nil)
	return
}

func printFunctions() {
	Warn("%s")
	Warnf("d", 1)
}
