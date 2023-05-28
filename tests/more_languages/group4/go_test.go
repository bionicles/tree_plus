// go_test.go
package main

import "fmt"

type Greeting struct {
    message string
}

func (g Greeting) sayHello() {
    fmt.Println(g.message)
}

func createGreeting(m string) Greeting {
    return Greeting{message: m}
}