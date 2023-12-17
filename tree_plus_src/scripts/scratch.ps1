# Install
$managers_and_packages = @(
    @{
        "name" = "scoop";
        "prepare" = "Invoke-Expression (New-Object System.Net.WebClient).DownloadString('https://get.scoop.sh')";
        "self_update" = "scoop update";
        "list" = {scoop list};
        "install" = {param($p) scoop install $p};
        "update" = {param($p) scoop update $p};
        "packages" = @("git", "conda", "nodejs", "gradle", "maven", "cypress", "rust")
    }
    @{
        "name" = "npm";
        "prepare" = "npm install -g";
        "self_update" = "npm update -g npm";
        "list" = {npm list -g --depth 0};
        "install" = {param($p) npm install -g $p};
        "update" = {param($p) npm update -g $p};
        "packages" = @("typescript", "nodemon", "ng")
    };
    @{
        "name" = "cargo";
        "prepare" = "Invoke-Expression (New-Object System.Net.WebClient).DownloadString('https://win.rustup.rs/')";
        "self_update" = "rustup update stable";
        "list" = {cargo install --list};
        "install" = {param($p) cargo install $p};
        "update" = {param($p) cargo update $p};
        "packages" = @("ripgrep", "bat", "exa",)
    };
)
function With-ErrorHandling {
    param (
        [Parameter(Mandatory=$true)]
        [ScriptBlock]$Operation,
        [string]$ErrorMessage
    )

    try {
        & $Operation
    }
    catch {
        Write-Host "$ErrorMessage: $_"
    }
}

function Prepare-PackageManager {
    param (
        [Parameter(Mandatory=$true)]
        [hashtable]$PackageManager
    )

    # Check if the package manager is in PATH
    if (!(Get-Command $PackageManager["name"] -ErrorAction SilentlyContinue)) {
        Write-Host "Package manager $($PackageManager["name"]) is not installed, installing"
        With-ErrorHandling -Operation {
            Invoke-Expression $PackageManager["prepare"]
        } -ErrorMessage "Error installing package manager $($PackageManager["name"])"
    }

    Write-Host "Updating package manager $($PackageManager["name"])"
    With-ErrorHandling -Operation {
        Invoke-Expression $PackageManager["self_update"]
    } -ErrorMessage "Error updating package manager $($PackageManager["name"])"
}

function InstallAndUpdate-Package {
    param (
        [Parameter(Mandatory=$true)]
        [hashtable]$PackageManager,
        [Parameter(Mandatory=$true)]
        [string]$Package
    )

    With-ErrorHandling -Operation {
        if (& $PackageManager["list"] | Select-String -Quiet -Pattern "\b$Package\b") {
            Write-Host "Package $Package is installed, updating"
            & $PackageManager["update"] $Package
        } else {
            Write-Host "Package $Package is not installed, installing"
            & $PackageManager["install"] $Package
            Write-Host "Updating package $Package"
            & $PackageManager["update"] $Package
        }
        Write-Host "Package path: " -NoNewline
        Get-Command $Package | Select-Object -ExpandProperty Source
    } -ErrorMessage "Error installing/updating package $Package"
}



foreach ($manager in $managers_and_packages) {
    Prepare-PackageManager -PackageManager $manager
    foreach ($package in $manager["packages"]) {
        InstallAndUpdate-Package -PackageManager $manager -Package $package
    }
}

# CRUDL for Processes

function Start-ProcessIfNotRunning {
    param (
        [Parameter(Mandatory=$true)]
        [string]$Name,
        [Parameter(Mandatory=$true)]
        [string]$Command,
        [Parameter(Mandatory=$true)]
        [string]$WorkingDirectory
    )

    $running = Get-Process | Where-Object {$_.ProcessName -eq $Name}

    if (!$running) {
        Write-Host "Starting $Name process..."
        $process = Start-Process -PassThru -NoNewWindow -WorkingDirectory $WorkingDirectory -FilePath "$Command"
        $global:processes += $process
    }
    else {
        Write-Host "$Name is already running."
    }
}

function Get-ProcessInfo {
    param (
        [Parameter(Mandatory=$true)]
        [string]$Name
    )

    Get-Process -Name $Name | Format-Table -AutoSize
}

function Stop-ProcessByName {
    param (
        [Parameter(Mandatory=$true)]
        [string]$Name
    )

    $process = Get-Process -Name $Name
    if ($process) {
        $process | Stop-Process
    }
}

function List-Processes {
    Get-Process | Format-Table -AutoSize
}


# CRUDL for Services

function Create-Service {
    param (
        [Parameter(Mandatory=$true)]
        [string]$Name,
        [Parameter(Mandatory=$true)]
        [string]$BinaryFilePathName
    )

    New-Service -Name $Name -BinaryFilePathName $BinaryFilePathName
}

function Get-ServiceInfo {
    param (
        [Parameter(Mandatory=$true)]
        [string]$Name
    )

    Get-Service -Name $Name | Format-Table -AutoSize
}

function Update-Service {
    param (
        [Parameter(Mandatory=$true)]
        [string]$Name,
        [Parameter(Mandatory=$true)]
        [string]$Status # "Running" or "Stopped"
    )

    if ($Status -eq "Running") {
        Start-Service -Name $Name
    } elseif ($Status -eq "Stopped") {
        Stop-Service -Name $Name
    }
}

function Remove-Service {
    param (
        [Parameter(Mandatory=$true)]
        [string]$Name
    )

    Stop-Service -Name $Name -Force
    Remove-Service -Name $Name
}

function List-Services {
    Get-Service | Format-Table -AutoSize
}
