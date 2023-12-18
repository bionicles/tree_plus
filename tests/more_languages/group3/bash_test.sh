#!/bin/bash
# bash_test.sh

echo_hello_world() {
  echo "Hello, World!"
}

function fun_echo_hello_world() {
  echo "Hello, World!"
}

export SECRET=sk123
alias md='make debug'