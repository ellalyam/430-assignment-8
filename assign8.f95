PROGRAM A8
    IMPLICIT NONE

    !!! EXPRC START
    TYPE, ABSTRACT :: ExprC
        ! empty 
    END TYPE ExprC

    ! Number
    TYPE, EXTENDS(ExprC) :: numC
        REAL :: number
    END TYPE numC

    ! Identifier
    TYPE, EXTENDS(ExprC) :: idC
        CHARACTER(LEN=1) :: symbol
    END TYPE idC

    ! Function application 
    TYPE, EXTENDS(ExprC) :: appC
        CHARACTER(LEN=16) :: function
    END TYPE appC

    ! Boolean 
    TYPE, EXTENDS(ExprC) :: boolC
        LOGICAL :: value
    END TYPE boolC

    ! String 
    TYPE, EXTENDS(ExprC) :: strC
        CHARACTER(LEN=25) :: value
    END TYPE strC

    ! IF expression node
    TYPE, EXTENDS(ExprC) :: ifC
        CLASS(ExprC), ALLOCATABLE :: test
        CLASS(ExprC), ALLOCATABLE :: then_branch
        CLASS(ExprC), ALLOCATABLE :: else_branch
    END TYPE ifC

    !!! EXPRC END

    !!! VALUE START
    TYPE, ABSTRACT :: Value
        ! empty 
    END TYPE Value

    ! Number
    TYPE, EXTENDS(Value) :: numV
        REAL :: number
    END TYPE numV

    ! Boolean 
    TYPE, EXTENDS(Value) :: boolV
        LOGICAL :: value
    END TYPE boolV

    ! String 
    TYPE, EXTENDS(Value) :: strV
        CHARACTER(LEN=25) :: value
    END TYPE strV

    !!! VALUE END

    !!! INTERP START
    Value FUNCTION interp(expr, env)
        ! IMPLICIT NONE?
        EXPRC, INTENT(IN) :: expr
        ENV, INTENT(IN) :: env

        SELECT CASE (expr)
            CASE(numC)

            CASE (idC)

            CASE (boolC)

            CASE (strC)

            CASE (ifC)

    !!! INTERP END
    ! Now we can have executable code
    print *, "Life is worth living"

    ! ... rest of program 

END PROGRAM A8
