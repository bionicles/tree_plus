' vba_test.bas
Class CPerson
    Private pName

    Public Property Get Name
        Name = pName
    End Property

    Public Property Let Name(value)
        pName = value
    End Property

    Public Sub Greet
        MsgBox "Hello, " & pName
    End Sub
End Class
