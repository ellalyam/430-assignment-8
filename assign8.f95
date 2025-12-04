PROGRAM A8
    IMPLICIT NONE

    ! Base (abstract) expression type
    TYPE, ABSTRACT :: ExprC
        ! empty 
    END TYPE ExprC

    ! Number
    TYPE, EXTENDS(ExprC) :: NumC
        REAL :: number
    END TYPE NumC

    ! Identifier
    TYPE, EXTENDS(ExprC) :: IDc
        CHARACTER(LEN=1) :: symbol
    END TYPE IDc

    ! Function application 
    TYPE, EXTENDS(ExprC) :: appC
        CHARACTER(LEN=16) :: function
    END TYPE appC

    ! Boolean 
    TYPE, EXTENDS(ExprC) :: boolC
        LOGICAL :: value
    END TYPE boolC

    ! String 
    TYPE, EXTENDS(ExprC) :: stringC
        CHARACTER(LEN=25) :: value
    END TYPE stringC

    ! IF expression node
    TYPE, EXTENDS(ExprC) :: ifC
        CLASS(ExprC), ALLOCATABLE :: test
        CLASS(ExprC), ALLOCATABLE :: then_branch
        CLASS(ExprC), ALLOCATABLE :: else_branch
    END TYPE ifC

    ! Now we can have executable code
    print *, "I hate my life"

    ! ... rest of program 

END PROGRAM A8
