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

    FUNCTION IS_NUMBER(input) RESULT(num)
      CHARACTER(LEN=*), INTENT(IN) :: input
      LOGICAL :: num
      REAL :: dummy
      INTEGER :: ierr
      READ(input, *, IOSTAT=ierr) dummy
      IF(ierr == 0) THEN
        num = .TRUE. !it can be a number
      ELSE 
        num = .FALSE. !it cant be a number
      END IF
    END FUNCTION IS_NUMBER

    ! PARSE
    !Takes a string and returns an AST, only handles a string containing a number right now
    FUNCTION PARSE(input) RESULT(expr)
      CHARACTER(LEN=*), INTENT(IN) :: input
      CLASS(ExprC), ALLOCATABLE :: expr
      IF (IS_NUMBER(input)) THEN
        ALLOCATE(numC :: expr)
        SELECT TYPE(e => expr)
        TYPE IS (numC)
          READ(input, *) e%num
        END SELECT
      END IF
    END FUNCTION PARSE


END MODULE A8

MODULE TEST
    USE A8
    IMPLICIT NONE

CONTAINS
  subroutine tests()
    !data declarations
    TYPE(numC) :: test_numC
    TYPE(numV) :: reference_numV
    TYPE(boolV) :: reference_boolV
    TYPE(strV) :: reference_strV
    CLASS(Value), ALLOCATABLE :: test_numV, test_boolV, test_strV
    CLASS(ExprC), ALLOCATABLE :: then_branch, else_branch, if_branch
    TYPE(Environment) :: env

    !numC basic test
    test_numC%num = 5
    if (test_numC%num /= 5) then
       error stop "SHEQTRAN: numC error"
    endif
    
    !numV test 
    test_numV = make_numV(3.0)
       
    IF (.NOT. SAME_TYPE_AS(test_numV, reference_numV)) THEN
      error stop "SHEQTRAN: make_numV failed"
    ENDIF
  
    !select type of test_numV
    SELECT TYPE (v => test_numV) 
    TYPE IS (numV) !only executes if the type is numV, this is necessary and acts like a cast
        IF(v%num /= 3.0) THEN
          error stop "SHEQTRAN: make_numV value error"
       ! confirm value of numV, spoiler: it works
       ! ELSE
       !   print *, v%num
        END IF
    END SELECT

    !make_boolV test
    test_boolV = make_boolV(.TRUE.)
    
    IF (.NOT. SAME_TYPE_AS(test_boolV, reference_boolV)) THEN
      error stop "SHEQTRAN: make_boolV failed - wrong type"
    ENDIF
  
    SELECT TYPE (v => test_boolV) 
    TYPE IS (boolV)
        IF(.not. v%bool) THEN
          error stop "SHEQTRAN: make_boolV .TRUE. value error"
        END IF
    END SELECT


    !make_strV test
    test_strV = make_strV("Test String")
    
    IF (.NOT. SAME_TYPE_AS(test_strV, reference_strV)) THEN
      error stop "SHEQTRAN: make_strV failed - wrong type"
    ENDIF
  
    SELECT TYPE (v => test_strV) 
    TYPE IS (strV)
        IF(v%str /= "Test String") THEN
          error stop "SHEQTRAN: make_strV value error"
        END IF
    END SELECT

    !test interp using numC
    test_numC%num = 42.0
    test_numV = INTERP(test_numC, env)
    SELECT TYPE (v => test_numV) 
    TYPE IS (numV)
        IF(v%num /= 42.0) THEN
          error stop "SHEQTRAN: INTERP numC error"
        END IF
    END SELECT

    !test parse
    SELECT TYPE (v => PARSE("4"))
    TYPE IS (numC)
      test_numC = v 
    END SELECT

    IF (test_numC%num /= 4) THEN
      error stop "SHEQTRAN: PARSE numC error"
    END IF

    print *, "Tests ran successfully"
   end subroutine tests
 
END MODULE TEST


program main 
    ! tell main to use the module A8
    USE A8
    USE TEST
    ! execute code here
    call tests()
     
    print *, "main ran successfully"



end program main

