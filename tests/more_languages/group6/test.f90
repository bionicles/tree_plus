! test.f90
MODULE complex_mod
    TYPE :: advanced_person
        CHARACTER(LEN=50) :: name
        INTEGER :: age
    END TYPE advanced_person
    
    CONTAINS

    SUBROUTINE short_hello(happy, path)
        PRINT *, happy, path
    END SUBROUTINE short_hello

    SUBROUTINE long_hello(
        p,
        message
    )
        TYPE(advanced_person), INTENT(IN) :: p
        CHARACTER(LEN=*), INTENT(IN) :: message
        PRINT *, message, 'Hello, ', p%name, ', age ', p%age
    END SUBROUTINE long_hello
END MODULE complex_mod