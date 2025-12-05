PROGRAM A8
    IMPLICIT NONE

    !!! EXPRC START
    TYPE, ABSTRACT :: ExprC
        ! empty 
    END TYPE ExprC

    ! Number
    TYPE, EXTENDS(ExprC) :: numC
        REAL :: num
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
        LOGICAL :: bool
    END TYPE boolC

    ! String 
    TYPE, EXTENDS(ExprC) :: strC
        CHARACTER(LEN=25) :: str
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
        REAL :: num
    END TYPE numV

    ! Boolean 
    TYPE, EXTENDS(Value) :: boolV
        LOGICAL :: bool
    END TYPE boolV

    ! String 
    TYPE, EXTENDS(Value) :: strV
        CHARACTER(LEN=25) :: str
    END TYPE strV

    !!! VALUE END

    !!! INTERP START
    RECURSIVE FUNCTION INTERP(expr, env) RESULT(val)
        CLASS(ExprC), INTENT(IN) :: expr
        ENV, INTENT(IN) :: env
        CLASS(Value) :: val

        SELECT TYPE (expr)
        
        TYPE IS (numC)
            val%num :: expr%num

        TYPE IS (idC)
            ! need lookup function
            
        TYPE IS (boolC)
            val%bool :: expr%bool

        TYPE IS (strC)
            val%str :: val%str

        TYPE IS (ifC)

    END FUNCTION INTERP

    !!! INTERP END


    ! Now we can have executable code
    print *, "Life is worth living"

    ! ... rest of program 

END PROGRAM A8
