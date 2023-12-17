#!/bin/bash

# set your RC_FILE to .bashrc if you use bash 
# or set it to .zshrc if you use zsh
RC_FILE=~/.bash_profile # where the aliases will be added
TREE_PLUS_PATH=$(pwd) # our current location
CONDA_ENV=py310 # name of the conda environment
PYTHON_VERSION=3.10 # to be used in our conda environment

# Function to idempotently add an alias if it doesn't exist in RC_FILE
add_alias() {
    grep -qF "alias $1=\"$2\"" $RC_FILE || echo "alias $1=\"$2\"" >> $RC_FILE
}

# Function to idempotently create a conda environment if it doesn't exist
create_conda_env() {
    conda env list | grep -qF "$1" || conda create -n $1 python=$2 -y
}

# Add multiple aliases
add_alias tp tree_plus
add_alias cdtp "cd $TREE_PLUS_PATH"
add_alias tpcli "cdtp && make cli"

# Source RC_FILE and change directory
source $RC_FILE
cd $TREE_PLUS_PATH

# Create conda environment if it doesn't exist and activate it
create_conda_env $CONDA_ENV $PYTHON_VERSION
conda activate $CONDA_ENV