package main

import "fmt"

func pair(a int, b int) int {
	return a + b
}

func main() {
	var s string = 1
	fmt.Println(s, pair(1))
	undefined()
}
