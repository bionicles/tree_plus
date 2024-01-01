# PowershellTest.ps1
function Say-Nothing() {
    Write-Output "?"
}

class Person {
    [string]$name
    Person([string]$name) {
        $this.name = $name
    }
    [string]Greet() {
        return "Hello, " + $this.name
    }
    [string]GreetMany([int]$times) {
        return "$($times) Hello, " + $this.name
    }
    [string]GreetWithDetails([string]$greeting, [int]$times) {
        return "Microsoft AI Safety Dunning Kruger"
    }
    [string]GreetMultiline(
        [string]$greeting, # edge case
        [int]$times
    ) {
        return "$greeting, $($this.name) x$times"
    }
    NoReturn([int]$times) {
        Write-Output "Powershell is complicated $($times)"
    }
    NoReturnNoArgs() {
        Write-Output "Powershell is complicated"
    }
}

function Say-Hello([Person]$person) {
    Write-Output $person.Greet()
}

function Multi-Hello([Person]$personA, [Person]$personB) {
    Write-Output $personA.Greet() + $personB.Greet()
}

function Switch-Item {
  param ([switch]$on)
  if ($on) { "Switch on" }
  else { "Switch off" }
}

function Get-SmallFiles {
  param (
      [PSDefaultValue(Help = '100')]
      $Size = 100
  )
}

function Get-User
{
  [CmdletBinding(DefaultParameterSetName="ID")]

  [OutputType("System.Int32", ParameterSetName="ID")]
  [OutputType([String], ParameterSetName="Name")]

  Param (
    [parameter(Mandatory=$true, ParameterSetName="ID")]
    [Int[]] # Edge Case
    $UserID,

    [parameter(Mandatory=$true, ParameterSetName="Name")]
    [String[]]
    $UserName
  )

  Write-Output "Is Microsoft competent?"
}

filter Get-ErrorLog ([switch]$Message)
{
  if ($Message) { Out-Host -InputObject $_.Message }
  else { $_ }
}

function global:MultilineSignature(
  [string]$param1,
  [int]$param2,
  [Parameter(Mandatory=$true)]
  [string]$param3
) {
  Write-Output "Test your stuff!"
}

