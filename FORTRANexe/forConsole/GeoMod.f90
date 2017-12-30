
! **********************************************************************************
!>    Module Point3D 
! **********************************************************************************   
      module Point3dMod
         implicit none
         save  

         type :: Point3d
            real:: x, y, z
         contains 
            procedure, pass :: writePoint3d
         end type Point3d  
         
         interface Point3d
            module procedure createPoint3d
         end interface
         
         !interface
         !   subroutine writePoint3d(pt)
         !      import Point3d
         !      class(Point3d) :: pt
         !   end subroutine
         !end interface
  
         contains
! **********************************************************************************
         type(Point3d) function createPoint3d(a,b,c) result(pt)
! **********************************************************************************
         real,intent(in)  :: a,b,c
         
         pt%x = a
         pt%y = b
         pt%z = c
         
         end function ! createPoint3d
   
! **********************************************************************************
         subroutine writePoint3d(this)
! **********************************************************************************
            class(Point3d), intent(in) :: this
            write(*, *) "********** Point3D **********"
            write(*, *) '(', this%x,',', this%y, ',', this%z,')'
         end subroutine
         
      end module Point3dMod   
   
! **********************************************************************************
!>    Module Line 
! **********************************************************************************   
      module LineMod
         use Point3dMod
         implicit none
         save
         
         type :: Line
            type(Point3d):: p0, p1
         contains 
            procedure, pass :: writeLine
         end type Line
         
         interface Line
            module procedure createLine
         end interface
         
         contains
! **********************************************************************************
         type(Line) function createLine(a,b) result(ln)
! **********************************************************************************
         type(Point3d),intent(in)  :: a,b
         
         ln%p0 = a
         ln%p1 = b
         
         end function ! createLine
      
! **********************************************************************************
         subroutine writeLine(this)
! **********************************************************************************
            class(Line), intent(inout) :: this
            write(*, *) "********** Line **********"
            call this%p0%writePoint3d()
            call this%p1%writePoint3d()
         end subroutine
  
      end module LineMod 
