# C:\Users\[user]\Documents\PowerShell\Microsoft.PowerShell_profile.ps1
# Abstract Idempotent Provision of Requirements with PowerShell, or
# "doing microsoft's job for them"
$SCOOP_REQUIREMENTS = @(
	"make",
	"rustup",
	"rust",
	"gh",
	"nodejs",
	"go",
	"fzf"
)
$CARGO_REQUIREMENTS = @(
	"ripgrep",
	"eza",
	"fd-find"
)
$PIP_REQUIREMENTS = @(
	"tree_plus",
	"pandas"
)
$REQUIREMENTS = @(
	"scoop",
	"make",
	"gh",
	"conda",
	"python",
	"node",
	"cargo",
	"rustup"
	"rg",
	"grep",
	"go",
	"fzf",
	"eza",
	"fd",
	"tree_plus"
)

function Log($message) {
	$timestamp = Get-Date -Format o
	Write-Host "[$timestamp] $message"
}
Log "Initiate Idempotent Requirement Provision"

function Remove-ChocolateyFromPath {
	Log "WARNING: Removing Chocolatey paths from env:Path."
	Log "Rationale: Avoid choco because it doesn't know where it installs things! (NEED PATH!!!)"
	$env:Path = ($env:Path -split ';' | Where-Object {$_ -notlike "*chocolatey*"}) -join ';'
	Log "WARNING: Chocolatey paths removed from env:Path"
}
Remove-ChocolateyFromPath

# Alias "cp" to cat all the profiles
function Show-Profiles {
	$profilePaths = $PROFILE.PSObject.Properties.Value
	$profilePaths | ForEach-Object {
		if (Test-Path $_) {
			Write-Host "`n`n=== Contents of $_ ===`n`n"
			Get-Content $_
		}
	}
}
Set-Alias -Name catp -Value Show-Profiles
Log "Initial Profiles:"
Show-Profiles

# Alias "pp" to print $PATH
function Show-Path {
	Log "env.Path:"
	$env:Path -split ';'
}
Set-Alias -Name pp -Value Show-Path
Show-Path


function Show-Error($err) {
	Log "ERROR!: $($err.Exception.Message)"
	Log "Continue provision..."
}

function Get-ScoopPackagePath {
	param(
		[Parameter(Mandatory = $true)]
		[string]$PackageName
	)
	Log "Get-ScoopPackagePath PackageName $PackageName"
	$result = scoop prefix $PackageName
	Log "Get-ScoopPackagePath PackageName = $PackageName result = $result"
	return $result
}

function Check-Command {
	param(
		[Parameter(Mandatory = $true)]
		[string]$Name
	)
	try {
		$command = Get-Command $Name -ErrorAction SilentlyContinue
		if ($null -eq $command) {
			Log "Get-Command Failed on $Name. Falling back to Get-ScoopPackagePath"
			return Get-ScoopPackagePath  -PackageName $Name
		}
		else {
			Log "Command '$Name' is found at $($command.Source)."
			# Check if the path is from Chocolatey
			if ($command.Source -like "*chocolatey*") {
				Log "Command '$Name' found in Chocolatey path, reinstalling with Scoop."
				Scoop-Install -Name $Name
				return Get-ScoopPackagePath -PackageName $Name
			}
			return $command.Source
		}
	}
	catch {
		Log "Command '$Name' is not found."
	}
}

function Add-ToPath {
	param(
		[Parameter(Mandatory = $true)]
		[string]$PathToAdd
	)
	$paths = $env:Path -split ';'
	if ($paths -notcontains $PathToAdd) {
		Log "Add-ToPath PathToAdd = $PathToAdd"
		$paths += $PathToAdd
		$env:Path = $paths -join ';'
	}
}

function Install-Scoop {
	if (!(Test-Path C:\Users\$env:USERNAME\scoop)) {
		Log "Trying to Install Scoop"
		try {
			Log "Installing Scoop"
			Invoke-Expression "& {$(Invoke-RestMethod get.scoop.sh)} -RunAsAdmin"
			Log "Scoop installed"
		}
		catch {
			Log "Install-Scoop did not install scoop."
		}
	} else {
		Log "Scoop already installed, updating scoop..."
		try {
			Log "Updating Scoop"
			Invoke-Expression "scoop update"
			Log "Scoop updated"
		}
		catch {
			Log "Install-Scoop failed to update scoop"
		}
	}
	Add-ToPath -PathToAdd "C:\Users\$env:USERNAME\scoop\shims"
	scoop bucket add extras
}
Install-Scoop



function Scoop-Install {
	param(
		[Parameter(Mandatory = $true)]
		[string]$Name,
		[string]$PathToAdd
	)
	Log "Scoop-Install $Name"
	$commandPath = Check-Command $Name
	try {
		if ($null -eq $commandPath) {
			Log "Installing $Name with scoop"
			scoop install $Name
			Log "$Name Installed with scoop"
		}
		else {
			Log "$Name is already installed at $commandPath."
		}
	}
	catch {
		Log "caught error in scoop install Name = $Name & PathToAdd = $PathToAdd"
		Show-Error $_
	}
	Log "Update $Name"
	try {
		scoop update $Name
	}
	catch {
		Log "caught error in scoop upgrade Name = $Name & PathToAdd = $PathToAdd"
		Show-Error $_
	}
	Log "Finished Scoop-Install $Name."
}
foreach ($requirement in $SCOOP_REQUIREMENTS) {
	Log "Provision requirement $requirement"
	if ($requirement -eq "rust") {
		Log "Updating Rust with rustup"
		# nightmarish nested try-catch should be a function
		try {
			try {
				Log "Update STABLE Rust toolchain with rustup"
				Invoke-Expression "rustup update stable"
				Log "Rust STABLE updated with rustup"
			}
			catch {
				Log "Issue updating STABLE Rust toolchain with rustup"
			}
			try {
				Log "Update NIGHTLY Rust toolchain with rustup"
				Invoke-Expression "rustup update nightly"
				Log "Rust NIGHTLY updated with rustup"
			}
			catch {
				Log "Issue updating NIGHTLY Rust toolchain with rustup"
			}
		}
		catch {
			Log "Failed to provision rust with rustup..."
			Log "Falling back to scoop."
			try {
				Scoop-Install -Name $requirement
			}
			catch {
				Log "Failed to install Rust. Try rustup-init.exe from https://rustup.rs/"
			}
		}
	}
	else {
		Log "Using scoop..."
		try {
			Scoop-Install -Name $requirement
		}
		catch {
			Log "Caught an issue on $requirement"
		}
	}
}


# FUNCTION to install and activate conda environment
function Start-CondaEnv {
	$minicondaInstallPath = "C:\Miniconda3"
	$condaEnvName = "py310"
	$condaEnvPythonVersion = 3.10
	Log "Check Conda Installed"
	if (!(Test-Path $minicondaInstallPath)) {
		Log "conda missing from $minicondaInstallPath"
		Log "Download Miniconda Installer"
		$minicondaInstaller = "https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-x86_64.exe"
		Invoke-WebRequest -Uri $minicondaInstaller -OutFile "$HOME\Downloads\Miniconda3-latest-Windows-x86_64.exe"
		Log "Miniconda Installer Downloaded"
		Log "Install Miniconda"
		Start-Process -FilePath "$HOME\Downloads\Miniconda3-latest-Windows-x86_64.exe" -ArgumentList "/S /InstallationType=JustMe /AddToPath=1 /D=$minicondaInstallPath" -Wait
	}
	Log "Conda Installed"
	Log "Update Conda"
	conda update -n base -c defaults conda -y
	Log "Conda Updated"
	Log "Add conda to PATH"
	Add-ToPath -PathToAdd "$minicondaInstallPath\Scripts"
	Log "conda added to path"
	Log "check conda env exists"
	if (!(Test-Path -Path "C:\Miniconda3\envs\$condaEnvName")) {
		Log "Create Conda Env"
		conda create -n $condaEnvName python=$condaEnvPythonVersion -y
	}
	Log "initialize conda"
	conda init powershell
	Log "conda initialized"
	Log "activate conda env $condaEnvName"
	conda activate $condaEnvName
	Log "conda env $condaEnvName activated"
	# Alias Python
	Log "Alias Python"
	Set-Alias -Name python -Value "C:\Miniconda3\envs\py310\python.exe"
	Log "Python Aliased"
	# Update pip
	Log "Upgrade Pip"
	python -m pip install --upgrade pip
	Log "Pip Upgraded"
	Log "Done Provisioning Conda & Python Env"
}
Start-CondaEnv
function Install-PipPackage {
	param(
        [Parameter(Mandatory = $true)]
		[string]$PackageName
	)
	Log "pip install -U $PackageName"
	pip install -U $PackageName
	Log "$PackageName installed"
}
Log "Install Pip Packages"
foreach ($PipPackage in $PIP_REQUIREMENTS) {
	Install-PipPackage $PipPackage
}

# When installing build tools, these two components should be selected:
# - MSVC - VS C++ x64/x86 build tools
# - Windows SDK
$MSVCPATH = 'C:\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools\VC\Tools\MSVC'
function Install-VSBuildTools {
	$vs_buildtools_uri = "https://aka.ms/vs/17/release/vs_buildtools.exe"
	$vs_buildtools_exe = "$env:TEMP\vs_buildtools.exe"
	$vs_buildtools_args = "--quiet --wait --norestart --nocache "
	$vs_buildtools_args += "--add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 "
	$vs_buildtools_args += "--add Microsoft.VisualStudio.Component.Windows10SDK.18362 "

	if (!(Test-Path $MSVCPATH)) {
		Log "Downloading Visual Studio 2019 Build Tools..."
		Invoke-WebRequest -Uri $vs_buildtools_uri -OutFile $vs_buildtools_exe
		Log "Installing Visual Studio 2019 Build Tools..."
		Start-Process -FilePath $vs_buildtools_exe -ArgumentList $vs_buildtools_args -Wait
		Log "Visual Studio 2019 Build Tools installed"
	}
	else {
		Log "Visual Studio 2019 Build Tools already installed"
	}
}
Install-VSBuildTools
$versionDirectories = Get-ChildItem -Path $MSVCPATH -Directory
Log "versionDirectories $versionDirectories"
# Convert the directory names to version numbers and sort them in descending order.
$versions = $versionDirectories.Name | ForEach-Object { [version] $_ } | Sort-Object -Descending
Log "versions $versions"
# The highest version is the first one in the sorted list.
$highestVersion = $versions[0]
Log "highestVersion $highestVersion"
# The path to the highest version of the build tools is then:
$highestMsvcVersionPath = Join-Path -Path $MSVCPATH -ChildPath $highestVersion.ToString()
Log "Highest MSVC Version Path is $highestMsvcVersionPath"


function Install-Crate {
	param(
        [Parameter(Mandatory = $true)]
		[string]$CrateName
	)
	Log "install $CrateName"
	cargo install $CrateName
	Log "$CrateName installed"
}
Log "Install Crates"
foreach ($Crate in $CARGO_REQUIREMENTS) {
	Install-Crate $Crate
}
Log "alias grep to rg"
$RIPGREP_PATH = "C:\Users\$env:USERNAME\.cargo\bin\rg.exe"
Set-Alias -Name grep -Value $RIPGREP_PATH
Add-ToPath -PathToAdd "C:\Users\$env:USERNAME\.cargo\bin"


function Get-ScoopVersion {
    Log "Getting Scoop Version"
    # Define the path to Scoop's CHANGELOG.md
    $changelogPath = "C:\Users\$env:USERNAME\scoop\apps\scoop\current\CHANGELOG.md"

    # Read the version from CHANGELOG.md
    if (Test-Path -Path $changelogPath) {
        $pattern = '^## \[(?<version>v[\d.]+)\]'
        $content = Get-Content $changelogPath -Raw
        $match = [Regex]::Match($content, $pattern)

        if ($match.Success) {
            $version = $match.Groups["version"].Value
            Log "Scoop version: $version"
			return $version
        } else {
            Log "Scoop version not found in CHANGELOG"
        }
    } else {
        Log "CHANGELOG.md not found at $changelogPath"
    }
	return "Unknown"
}

# helper function for the final status report
function Get-Version {
    param(
        [Parameter(Mandatory = $true)]
        [string]$ExecutablePath,
        [string]$ExecutableName
    )
    # Special case for go.exe
    if ($ExecutableName -eq "go") {
		Log "Handling Golang"
        return Invoke-Expression "& `"$ExecutablePath`" version" 2>$null | Select-Object -First 1
    }
    # Special handling for scoop
    if ($ExecutableName -eq "scoop") {
		return Get-ScoopVersion
    }
	Log "Handling $ExecutablePath"
    # Common version flags
    $versionFlags = @('--version', '-v', '-V', 'version')
    foreach ($flag in $versionFlags) {
        try {
			Log "Trying version flag: '$flag'"
            $versionCommand = "& `"$ExecutablePath`" $flag"
            $versionOutput = Invoke-Expression $versionCommand 2>$null
            if ($versionOutput) {
                return $versionOutput | Select-Object -First 1
            }
        }
        catch {
            continue
        }
    }
    return "Version not available"
}


Show-Path
Log "Final Status Report:"
function Show-Requirements {
	$StatusReport = @{}
	function Measure-Status {
		param(
			[Parameter(Mandatory = $true)]
			[string]$Name			
		)
		$requirementPath = Check-Command $Name
		if ($null -eq $requirementPath) {
			$StatusReport[$Name] = 'Not Ready'
		}
		else {
			if ($Name -eq "grep") {
				$requirementPath = $RIPGREP_PATH
			}
			# ADDED: Version
			$versionInfo = Get-Version -ExecutablePath $requirementPath -ExecutableName $Name
			$StatusReport[$Name] = @{
				'Status' = 'Ready'
				'Path'   = $requirementPath
				'Version' = $versionInfo
			}
		}
	}
	foreach ($app in $REQUIREMENTS) {
		Measure-Status -Name $app
	}
	$msvcVersion = "Unknown"
	if ($null -eq $highestMsvcVersionPath) {
		$StatusReport["C++ x64/x86 build tools"] = @{'Status'='Not Ready'}
	}
	else {
		# Attempt to extract version from the directory name
		$msvcVersion = Split-Path $highestMsvcVersionPath -Leaf
		$StatusReport["C++ x64/x86 build tools"] = @{
			'Status' = 'Ready'
			'Version' = $msvcVersion
			'Path' = $highestMsvcVersionPath
		}
	}
	Log "Table Format:"
	$StatusReport.GetEnumerator() | Sort-Object Name | Format-Table -Property @{
		Name = 'Name'; Expression = { $_.Name }; Width = 12
	}, @{
		Name = 'Status'; Expression = { $_.Value.Status }; Width = 7
	}, @{
		Name = 'Version'; Expression = { $_.Value.Version }; Width = 20
	}, @{
		Name = 'Path'; Expression = { $_.Value.Path }; Width = 50
	} -Wrap

	# Log "List Format:"
	# $StatusReport.GetEnumerator() | Sort-Object Name | Format-List

	Log "Rust Toolchains:"
	Invoke-Expression "rustup show"
}
Show-Requirements
Log "End Abstract Idempotent Provision of Requirements"


# Alias to locate $PROFILE
function Find-Profile {
	$PROFILE | Format-List *
}
Set-Alias -Name lp -Value Find-Profile
# Alias "eb" to edit $PROFILE
function Edit-Profile {
	code $PROFILE
	code -a "$($env:USERPROFILE)\Documents\PowerShell"
}
Set-Alias -Name eb -Value Edit-Profile
# Alias "sb" to source $PROFILE
function Set-Profile {
	. $PROFILE
}
Set-Alias -Name sb -Value Set-Profile
# Alias "cb" to cat $PROFILE
function Show-Profile {
	Get-Content $PROFILE
}
Set-Alias -Name cb -Value Show-Profile

Invoke-Expression "tree_plus C:\Users\$($env.USERNAME)\Documents\PowerShell\Microsoft.PowerShell_profile.ps1"