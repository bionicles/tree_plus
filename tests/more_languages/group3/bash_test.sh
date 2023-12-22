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

# Function to idempotently add an alias if it doesn't exist in RC_FILE
add_alias() {
    grep -qF "alias $1=\"$2\"" $RC_FILE || echo "alias $1=\"$2\"" >> $RC_FILE
}

# Function to idempotently create a conda environment if it doesn't exist
create_conda_env() {
    conda env list | grep -qF "$1" || conda create -n $1 python=$2 -y
}