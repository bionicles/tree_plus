function prepare_package_manager {
    declare -A commands=$1

    if ! command -v ${commands["name"]} &> /dev/null; then
        echo "Package manager ${commands["name"]} is not installed, installing"
        eval ${commands["prepare"]}
    fi

    echo "Updating package manager ${commands["name"]}"
    eval ${commands["self_update"]}
}

function install_or_update {
    declare -A commands=$1
    package=$2

    if ${commands["list"]} | grep -q "$package"; then
        echo "Package $package is installed, updating"
        ${commands["update"]} "$package"
    else
        echo "Package $package is not installed, installing"
        ${commands["install"]} "$package"
    fi
}

# what's more "basic" than apt-get?

# apt-get
# Let's use apt get in bash, since scoop is a windows package manager
declare -A apt_get_commands=(
    ["name"]="apt-get"
    ["prepare"]="sudo su"
    ["self_update"]="apt-get update"
    ["list"]="?"
    ["install"]="apt-get install"
    ["update"]="apt-get update"
)
apt_get_commands=("git" "python")
# Then to use it:
prepare_package_manager "apt_get_commands[@]"
for package in "${scoop_packages[@]}"; do
    install_or_update "scoop_commands[@]" "$package"
done

# node
declare -A npm_commands=(
    ["name"]="npm"
    ["prepare"]="npm install -g npm"
    ["self_update"]="npm update -g npm"
    ["list"]="npm list -g --depth 0"
    ["install"]="npm install -g"
    ["update"]="npm update -g"
)
npm_packages=("typescript" "nodemon" "ng")

prepare_package_manager "npm_commands[@]"
for package in "${npm_packages[@]}"; do
    install_or_update "npm_commands[@]" "$package"
done

# rust
declare -A rust_commands=(
    ["name"]="cargo"
    ["prepare"]="curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
    ["self_update"]="rustup self update && rustup update"
    ["list"]="?"
    ["install"]="cargo install -g"
    ["update"]="cargo update -g"
)
rust_packages=("ripgrep" "exa" "bat" "serde-json")

prepare_package_manager "npm_commands[@]"
for package in "${npm_packages[@]}"; do
    install_or_update "npm_commands[@]" "$package"
done

# github
# Let's us apt get in bash, since scoop is a windows package manager. 
declare -A github_repos=(
    ["name"]="gh"
    ["prepare"]=prepare_package_manager "scoop_commands[@]"
    ["self_update"]="sudo apt-get update gh -y"
    ["list"]="scoop list"
    ["install"]="scoop install"
    ["update"]="scoop update"
)
scoop_packages=("git" "python")

