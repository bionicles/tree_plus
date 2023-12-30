// go_test.go
package main

import (
	"context"
	"errors"
	"fmt"
)

type Greeting struct {
	message string
}

func (g Greeting) sayHello() {
	fmt.Println(g.message)
}

func createGreeting(m string) Greeting {
	return Greeting{message: m}
}

type SomethingLong struct {
	// struct fields...
}

// Complex multiline function signature
func (s *SomethingLong) WithAReasonableName(
	ctx context.Context,
	param1 string,
	param2 int,
	param3 map[string]interface{},
	callback func(int) error,
) (resultType, error) {
	// Function implementation...

	// Dummy return
	return resultType{}, errors.New("not implemented")
}

type resultType struct {
	// struct fields...
}

func main() {
	// Main function implementation...
}
