!  forConsole.f90 
!
!  FUNCTIONS:
!  forConsole - Entry point of console application.
!
      
! **********************************************************************************
!>  \brief   Do something
!!
!!  \param[p1]  some parameter
!!  \param[p2]  some parameter
!!
!!  \author  sofistik
! **********************************************************************************
      SUBROUTINE doSomething(p1, p2, res)
! **********************************************************************************
      implicit none
      DOUBLE PRECISION, INTENT(IN)  :: p1, p2
      DOUBLE PRECISION :: res
      
      res = p1*p2

      END SUBROUTINE doSomething
    
    
! **********************************************************************************
      DOUBLE PRECISION Function doOtherThing(p1, p2) RESULT(res)
! **********************************************************************************
      implicit none
      DOUBLE PRECISION, INTENT(IN)  :: p1, p2
      
      res = p1*p2

      END Function doOtherThing

! **********************************************************************************
      SUBROUTINE simpleTest()
! **********************************************************************************
      use ModuleA, doModule => A
      implicit none
      
      ! Variables
      DOUBLE PRECISION    :: a, b, x
      DOUBLE PRECISION    :: doOtherThing
      DOUBLE PRECISION, ALLOCATABLE, TARGET :: Mat(:,:), Vec(:)
      DOUBLE PRECISION           :: T(0:3)
      INTEGER              :: i, j, m = 2, status
      DOUBLE PRECISION, POINTER :: p(:)=>null()
      
      ! Body of forConsole
      write (*,*) "enter two values"
      read (*,*) a,b
    
      ! try for loop
      do i=1,10
         print*,i**2
      end do
    
      ! call sub
      call doSomething(a, b, x)
      write (*,*) "res_sub = ",x
    
      ! call function
      x= doOtherThing(a, b)
      write (*,*) "res_func = ", x
      
      ! call module
      x= doModule(a, b)
      write (*,*) "res_mod = ", x
      
      ! test vector
      ALLOCATE(Mat(m, m), stat = status)
      
      if (.not. allocated(Mat)) then
         stop 'Error during allocation of Vec'
      end if
      
      do i=1,m
         do j=1,m
            Mat(i,j)= i+j
         end do
      end do
      !  Matrix Mat is:
      !     Mat = [
      !           [ 2  3 ]
      !           [ 3  4 ]
      !           ]
      
      write (*,*) "Mat_test = ", Mat
      
      ! test vector/elemental
      p => Mat(:, 1)
      write (*,*) "point2(1) = ", p
      nullify(p)
      
      ALLOCATE(Vec(m), stat = status)
      Vec= doModule(Mat(:, 1), Mat(:, 2))
      
      write (*,*) "Vec_test = ", Vec
      
      write (*,*) "Vec_addScalar = ", Vec+2
      write (*,*) "Vec_addVec = ", Vec+Vec
      
      call multiply(Vec, x)
      write (*,*) "Mod_mult = ", Vec
      
      ! Arrays are 0-based!
      write (*,*) "Arrey [0-3] = ", T
      
      ! Deallokation
      deallocate(Vec, stat=status)
      deallocate(Mat, stat=status)


      END SUBROUTINE !simpleTest


   
! **********************************************************************************
      SUBROUTINE simpleTest2()
! **********************************************************************************
      use ModuleA, only: doVector, printVector, printVector2
      implicit none
      
      ! Variables
      integer, ALLOCATABLE, TARGET :: vec(:)
      integer, POINTER :: p(:)=>null()
      integer        :: m, i, status

      ! test vector
      call doVector(vec)

      write (*,*) "vec = ", vec
      p => vec(:)
      
      call printVector(p)
      call printVector2(vec)
      !write (*,*) "point2(1) = ", p
      
      !m= size(vec)
      !! try for loop
      !do i=1,m
      !   print*,vec(i)
      !end do
      
 
      ! Deallokation
      nullify(p)
      deallocate(vec, stat=status)

   END SUBROUTINE !simpleTest

   
   
! **********************************************************************************
      SUBROUTINE moduleTest()
! **********************************************************************************
      use Point3dMod
      use LineMod
      implicit none
      
      ! Variables
      type(Point3d) :: objPt_00, objPt_01
      type(Line)     :: line_00
      
      ! Body of forConsole
      objPt_00 = Point3d(1,2,3)
      objPt_01 = Point3d(-1,-2,-3)
      
      line_00 = Line(objPt_00, objPt_01)
      
      !call line_00%p0%writePoint3d()
      call line_00%writeLine()

   END SUBROUTINE ! moduleTest

   
! **********************************************************************************
      SUBROUTINE otherTest()
! **********************************************************************************
      use ModuleA
      implicit none
      
      !! Variables
      !DOUBLE PRECISION :: c, Vec(3,3)
      !
      !Vec = 1
      !Vec(1,1) = 326D0
      !!Vec(1) = Vec(1)  * 2
      !!Vec(2) = Vec(2)  + 2
      !!Vec(3) = Vec(3)  / 2
      !c = Vec(1,1)
      !
      !! Body of forConsole
      !!write (*,*) "Vec_test = ", sqrt(sum(Vec**2))
      
      !type foo
      !  integer i
      !  integer j
      !end type foo
      !
      !type(foo) :: a,b
      !
      !ENUM, bind(c)
      !      ENUMERATOR  :: Foot_Undefined   = 0   ! Not defined
      !      ENUMERATOR  :: Foot_Isolated    = 1   ! Einzelfundament / Isolated
      !      ENUMERATOR  :: Foot_Sleeve      = 2   ! Köcherfundament / Sleeve foundation
      !      ENUMERATOR  :: Foot_BlockF      = 3   ! Blockfundament / Block foundation
      !      ENUMERATOR  :: Foot_Strip       = 4   ! Streifenfundament / Strip foundation
      !      ENUMERATOR  :: Foot_LShaped     = 5   ! Stiefelfundament / L-Shaped foundation
      !END ENUM
      !
      !INTEGER(KIND(1))  :: my_DataType
      
      
      !real  :: val1
      !double precision :: val2
      !!my_DataType = Foot_Isolated
      !!write (*,*) my_DataType
      !val1 = 8.0
      !val2 = 7.0D0
      !call subNumber(val2)
      
      type(testType), pointer :: pTest
      
      pTest =>funcpointer() 
      !deallocate(pTest)
      
   END SUBROUTINE !simpleTest
   

   
! **********************************************************************************
      SUBROUTINE mapTest()
! **********************************************************************************
      use Dictionary_str
      
      implicit none
      
      TYPE(DictionaryStr)            :: dic
      INTEGER, parameter :: tbl_length = 127
      CHARACTER(len=:), ALLOCATABLE :: output
      logical :: flag
      
      CALL dic%init()
      CALL dic%set('first_name','John')
      PRINT*, 'Hash: ', hashMap('first_name',tbl_length)
      CALL dic%set('last_name', 'Smith')
      PRINT*, 'Hash: ', hashMap('last_name',tbl_length)
      CALL dic%set(key='key01', val='val01')
      PRINT*, 'Hash: ', hashMap('key01',tbl_length)
      

      CALL dic%get(key='first_name',val=output)
      PRINT*, output
      CALL dic%get('last_name',output)
      PRINT*, output
      CALL dic%get('key01',output)
      PRINT*, output
      
      flag = dic%tryGet('key074471',output)
      if(flag) then
         PRINT*, output
      else
         PRINT*, 'keyt not found'
      endif

      CALL dic%free()
      
   END SUBROUTINE !simpleTest
   

! **********************************************************************************
      SUBROUTINE mapTest2()
! **********************************************************************************
      use GenericDictionary_Mod
      use Dictionary_Str2Int
      use ModuleA, only: makeHash
      implicit none
      
      TYPE(DictionaryStr)           :: dic
      TYPE(DictionaryStr2Int)       :: dicInt
      INTEGER, parameter :: tbl_length = 127
      CHARACTER(len=:), ALLOCATABLE :: output
      logical :: flag
      INTEGER :: value
      PROCEDURE(hashFunction_template), POINTER :: local_hash
      PROCEDURE(hashFunction_str2Int), POINTER :: other_hash
      
      local_hash => makeHash
      dic = GenericDictionary(local_hash, tbl_length)
      
      CALL dic%set('key01','val001')
      flag = dic%tryGet('key01',output)
      if(flag) then
         PRINT*, output
      else
         PRINT*, 'keyt not found'
      endif

      CALL dic%free()
      
      
      other_hash => makeHash
      dicInt = createDictionaryStr2Int()
      CALL dicInt%set('key02',33)
      flag = dicInt%tryGet('key02',value)
      if(flag) then
         PRINT*, value
      else
         PRINT*, 'keyt not found (int)'
      endif
      CALL dicInt%free()
      
   END SUBROUTINE !simpleTest
   
   
   
!****************************************************************************
!
!  PROGRAM: forConsole
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
      program forConsole
! *********************************************************************************
      implicit none
      
      !call simpleTest()
      
      !call moduleTest()
      
      !call otherTest()
      
      !call mapTest()
      
      !call mapTest2()
      
      call simpleTest2()
      
      pause
    
      end program forConsole
    
