# PowershellTest.ps1
function Test-Ordering([string]$foo) {
    Write-Output $foo
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
    NoReturn([int]$times) {
        Write-Output "Powershell is complicated $($times)"
    }
    NoReturnNoArgs() {
        Write-Output "Powershell is really complicated"
    }
}

function Say-Hello([Person]$person) {
    Write-Output $person.Greet()
}
