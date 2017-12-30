
    
! **********************************************************************************
!>    ModuleA test001 
! **********************************************************************************
      MODULE ModuleA

      implicit none
      
      interface subNumber
       module procedure subNumber_double, subNumber_real
      end interface
      
      type testType
         integer :: val
      end type

      contains
   
      elemental function A(x,y) result(res)
         DOUBLE PRECISION :: res
         DOUBLE PRECISION, intent(IN) :: x, y
         res = x * y 
      end function !A
      
! **********************************************************************************
      SUBROUTINE subTypeRoutine(item)
! **********************************************************************************
      implicit none
      type(testType), intent(in) :: item
      integer  :: other
      !type(testType), pointer :: pTest
      !pTest =>funcpointer() 

      other = 1 + item%val

      END SUBROUTINE !multiply
      
! **********************************************************************************
      FUNCTION funcpointer() result(p)
! **********************************************************************************
      implicit none
      type(testType), pointer :: p
      integer :: ie
      
      p=>null()
      allocate(p, STAT=ie)

      END FUNCTION !multiply
   
! **********************************************************************************
      SUBROUTINE multiply(p1, val)
! **********************************************************************************
      implicit none
      DOUBLE PRECISION, ALLOCATABLE,TARGET, intent(INOUT) :: p1(:)
      DOUBLE PRECISION, intent(IN)  :: val

      p1 = p1*val

      END SUBROUTINE !multiply

! **********************************************************************************
      SUBROUTINE subNumber_double(val)
! **********************************************************************************
      implicit none
      double precision, intent(in) :: val
      
      write (*,*) 'double!  ', val

      END SUBROUTINE !simpleTest
   
   ! **********************************************************************************
      SUBROUTINE subNumber_real(val)
! **********************************************************************************
      implicit none
      real, intent(in) :: val
      
      write (*,*) 'real!  ', val

      END SUBROUTINE !simpleTest
      
      
! **********************************************************************************    
      FUNCTION makeHash(str, tbl_len) RESULT(hsh)
! **********************************************************************************    
      use Dictionary_str, only: sum_string
      CHARACTER(len=*), INTENT(in)   :: str
      INTEGER, INTENT(in) ::tbl_len
      INTEGER :: hsh

      hsh = MOD(sum_string(str),tbl_len) + 1
      
      END FUNCTION makeHash
      
      
      
! **********************************************************************************    
      subroutine doVector(vec)
! **********************************************************************************    
      implicit none
      !integer, POINTER  :: p(:)
      integer, ALLOCATABLE  :: vec(:)
      integer           ::m, status, i
      
      m=5
      ALLOCATE(vec(m), stat = status)
      
      do i=1,m
         vec(i) = i+100
      end do
      

      end subroutine doVector
      
! **********************************************************************************    
      subroutine printVector(p)
! **********************************************************************************    
      implicit none
      integer, POINTER  :: p(:)
      integer           ::m, status, i
      
      m=size(p)
      
      do i=1,m
         print*,p(i)
      end do
      
      end subroutine printVector
      
! **********************************************************************************    
      subroutine printVector2(vec)
! **********************************************************************************    
      implicit none
      integer, ALLOCATABLE  :: vec(:)
      integer           ::m, status, i
      
      m=size(vec)
      
      do i=1,m
         print*,vec(i)
      end do
      
      end subroutine printVector2

! **********************************************************************************
      END MODULE ! ModuleA
! **********************************************************************************