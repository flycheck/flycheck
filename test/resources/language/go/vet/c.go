package vet

import "fmt"

func Bad() {
	fmt.Printf("%d", "s")
}
