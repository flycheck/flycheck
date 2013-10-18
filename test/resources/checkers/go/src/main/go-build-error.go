// A simple syntax error in Go

package main

import "testpackage2"

func tata() {
	testpackage2.TestFunc()
	missing.DoesNotExist()
}
