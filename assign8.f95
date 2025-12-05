MODULE A8
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
        CLASS(ExprC), ALLOCATABLE :: if_branch
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

     !!! define env (dummy for now)
    TYPE :: Environment
        ! empty
    END TYPE Environment

! need contains for functions
CONTAINS 

    !! start constructors

    ! numV constructor
    FUNCTION make_numV(n) RESULT(val)
        REAL, INTENT(IN) :: n
        CLASS(Value), ALLOCATABLE :: val

        ALLOCATE(numV :: val)
        SELECT TYPE (v => val)
            TYPE IS (numV)
                v%num = n
        END SELECT
    END FUNCTION make_numV


    ! boolV constructor
    FUNCTION make_boolV(b) RESULT(val)
        LOGICAL, INTENT(IN) :: b
        CLASS(Value), ALLOCATABLE :: val

        ALLOCATE(boolV :: val)
        SELECT TYPE (v => val)
            TYPE IS (boolV)
                v%bool = b
        END SELECT
    END FUNCTION make_boolV


    ! strV constructor
    FUNCTION make_strV(s) RESULT(val)
        CHARACTER(LEN=*), INTENT(IN) :: s
        CLASS(Value), ALLOCATABLE :: val

        ALLOCATE(strV :: val)
        SELECT TYPE (v => val)
            TYPE IS (strV)
                v%str = s
        END SELECT
    END FUNCTION make_strV



    !!! INTERP START
    RECURSIVE FUNCTION INTERP(expr, env) RESULT(val)
        IMPLICIT NONE

        CLASS(ExprC), INTENT(IN) :: expr
        TYPE(Environment), INTENT(IN) :: env
        CLASS(Value), ALLOCATABLE :: val

        CLASS(exprC), POINTER :: e

        CLASS(Value), ALLOCATABLE :: if_statement

        SELECT TYPE (e => expr)
        
        TYPE IS (numC)
            val = make_numV(e%num)

        TYPE IS (idC)
            ! need lookup function
            
        TYPE IS (boolC)
            val = make_boolV(e%bool)

        TYPE IS (strC)
            val = make_strV(e%str)

        TYPE IS (ifC)
            ! interp if statement
            if_statement = interp(e%if_branch, env)

            ! make sure if statement is a boolV
            SELECT TYPE (v => if_statement)

            TYPE IS (boolV)
                IF (v%bool) THEN
                    val = interp(e%then_branch, env)
                ELSE
                    val = interp(e%else_branch, env)
                END IF

            END SELECT

        END SELECT
        
    END FUNCTION INTERP

    !!! INTERP END


END MODULE A8


program main 

    ! execute code here

    print *, "Life is worth living"


end program main

