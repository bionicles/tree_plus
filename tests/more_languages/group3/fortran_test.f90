! fortran_test.f90
MODULE hello_mod
    TYPE :: person
        CHARACTER(LEN=20) :: name
    END TYPE person
    
    CONTAINS

    SUBROUTINE say_hello(p)
        TYPE(person), INTENT(IN) :: p
        PRINT *, 'Hello, ', p%name
    END SUBROUTINE say_hello
END MODULE hello_mod

PROGRAM HelloWorld
    USE hello_mod
    TYPE(person) :: person1

    person1%name = "World"
    CALL say_hello(person1)
END PROGRAM HelloWorld
