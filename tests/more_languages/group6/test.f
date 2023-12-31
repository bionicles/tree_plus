! test.f
MODULE basic_mod
    TYPE :: person ! edgecase
        CHARACTER(LEN=50) :: name
        INTEGER :: age
    END TYPE person
    
    CONTAINS

    SUBROUTINE short_hello(happy, path) ! edgecase
        PRINT *, happy, path
    END SUBROUTINE short_hello

    SUBROUTINE long_hello(
        p, ! edgecase
        message
    )
        TYPE(person), INTENT(IN) :: p
        CHARACTER(LEN=*), INTENT(IN) :: message
        PRINT *, message, 'Hello, ', p%name, ', age ', p%age
    END SUBROUTINE long_hello
END MODULE basic_mod

PROGRAM HelloFortran
    USE basic_mod
    TYPE(person) :: person2

    person2%name = "John Backus"
    person2%age = 82
    CALL long_hello(person2, "Greetings! ")
END PROGRAM HelloFortran