# C:\Users\[user]\Documents\PowerShell\Microsoft.PowerShell_profile.ps1
# Abstract Idempotent Provision of Requirements with PowerShell, or
# "doing microsoft's job for them"
$SCOOP_REQUIREMENTS = @(
	"make",
	"rust",
	"rustup",
	"gh",
	"nodejs",
	"go",
	"fzf"
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
	"fzf"
)

function Log($message) {
	$timestamp = Get-Date -Format o
	Write-Host "[$timestamp] $message"
}
Log "Initiate Idempotent Requirement Provision"

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
	$env:Path -split ';'
}
Set-Alias -Name pp -Value Show-Path
Log "Initial $env.Path:"
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
	if (!(Test-Path "C:\ProgramData\scoop")) {
		try {
			Log "Installing Scoop"
			Invoke-Expression "& {$(Invoke-RestMethod get.scoop.sh)} -RunAsAdmin"
			Log "Scoop installed"
		}
		catch {
			Log "Install-Scoop did not install scoop."
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
	Log "provision scoop requirement $requirement"
	try {
		Scoop-Install -Name $requirement
	}
	catch {
		Log "Caught an issue on $requirement"
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


function Install-Crates {
	Log "install ripgrep"
	cargo install ripgrep
	Add-ToPath -PathToAdd "C:\Users\$env:USERNAME\.cargo\bin"
	Log "ripgrep installed"
}
Install-Crates
Log "alias grep to rg"
$RIPGREP_PATH = "C:\Users\$env:USERNAME\.cargo\bin\rg.exe"
Set-Alias -Name grep -Value $RIPGREP_PATH

Log "env.Path is now $env:Path"
Log "End Abstract Idempotent Provision of Requirements"
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
			$StatusReport[$Name] = @{
				'Status' = 'Ready'
				'Path'   = $requirementPath
			}
		}
	}
	foreach ($app in $REQUIREMENTS) {
		Measure-Status -Name $app
	}
	if ($null -eq $highestMsvcVersionPath) {
		$StatusReport["C++ x64/x86 build tools"] = @{'Status'='Not Ready'}
	}
	else {
		$StatusReport["C++ x64/x86 build tools"] = @{
			'Status' = 'Ready'
			'Path' = $highestMsvcVersionPath
		}
	}
	$StatusReport.GetEnumerator() | Sort-Object Name | Format-Table -Property Name, @{
		Name = 'Status'; Expression = { $_.Value.Status }
	}, @{
		Name = 'Path'; Expression = { $_.Value.Path }
	} -AutoSize
}
Show-Requirements

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