# PowershellTest.ps1
class Person {
    [string]$name
    Person([string]$name) {
        $this.name = $name
    }
    [string]Greet() {
        return "Hello, " + $this.name
    }
}

function Say-Hello([Person]$person) {
    Write-Output $person.Greet()
}
