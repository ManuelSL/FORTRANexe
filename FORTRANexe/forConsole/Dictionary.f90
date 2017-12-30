! **********************************************************************************
!>    Module Dictionary_str 
! **********************************************************************************   
   MODULE Dictionary_str
      IMPLICIT NONE
      INTEGER, PARAMETER :: tbl_size = 127

      TYPE StringMap
         TYPE(StringMap), POINTER :: child => NULL()
         CHARACTER(len=:), ALLOCATABLE :: key, val
      CONTAINS
         PROCEDURE, PASS :: set  => set_str
         PROCEDURE, PASS :: get  => get_str
         PROCEDURE, PASS :: free => free_str
      END TYPE StringMap

      TYPE DictionaryStr
         TYPE(StringMap), DIMENSION(:), ALLOCATABLE :: vec
         INTEGER                                 :: vec_len = 0
         LOGICAL                                 :: is_init = .FALSE.
      CONTAINS
         PROCEDURE, PASS :: init => init_dictionary_str
         PROCEDURE, PASS :: set  => set_dictionary_str
         PROCEDURE, PASS :: get  => get_dictionary_str
         PROCEDURE, PASS :: tryGet  => try_get_dictionary_str
         PROCEDURE, PASS :: free => free_dictionary_str
      END TYPE DictionaryStr

      PUBLIC :: DictionaryStr
      
      CONTAINS
   
! **********************************************************************************
!>   internal subroutines
! **********************************************************************************     
   
! **********************************************************************************
      RECURSIVE SUBROUTINE set_str(this,key,val)
! **********************************************************************************
         CLASS(StringMap), INTENT(inout) :: this
         CHARACTER(len=*), INTENT(in)    :: key, val
         INTEGER                         :: keylen, vallen

         keylen = LEN(key)
         vallen = LEN(val)
         IF (ALLOCATED(this%key)) THEN
            IF (this%key /= key) THEN
               IF ( .NOT. ASSOCIATED(this%child) ) ALLOCATE(this%child)
               CALL set_str(this%child,key,val)
            ELSE !in case the key already exist update the value
               IF (ALLOCATED(this%val)) DEALLOCATE(this%val)
               ALLOCATE(CHARACTER(len=vallen) :: this%val)
               this%val = val
            END IF
         ELSE
            IF (.NOT. ALLOCATED(this%key)) ALLOCATE(CHARACTER(len=keylen) :: this%key)
            this%key = key
            IF (ALLOCATED(this%val)) DEALLOCATE(this%val)
            ALLOCATE(CHARACTER(len=vallen) :: this%val)
            this%val = val
         END IF
      END SUBROUTINE set_str

! **********************************************************************************
      RECURSIVE SUBROUTINE get_str(this,key,val)
! **********************************************************************************
         CLASS(StringMap),INTENT(in)                  :: this
         CHARACTER(len=*),INTENT(in)                  :: key
         CHARACTER(len=:), ALLOCATABLE, INTENT(out)   :: val
         INTEGER                                      :: vallen

         vallen = 0
         IF (ALLOCATED(this%key).AND.(this%key == key)) THEN
            vallen = LEN(this%val)
            IF (ALLOCATED(val)) DEALLOCATE(val)
            ALLOCATE(CHARACTER(len=vallen) :: val)
            val = this%val
         ELSE IF(ASSOCIATED(this%child)) THEN ! next item
            CALL get_str(this%child,key,val)
         ELSE 
            ! key not found
            IF (ALLOCATED(val)) DEALLOCATE(val)
            RETURN
         END IF
         
      END SUBROUTINE get_str

! **********************************************************************************
      RECURSIVE SUBROUTINE free_str(this)
! **********************************************************************************
         CLASS(StringMap), INTENT(inout) :: this
         
         IF (ASSOCIATED(this%child)) THEN
            CALL free_str(this%child)
            DEALLOCATE(this%child)
         END IF
         this%child => NULL()
         IF (ALLOCATED(this%key)) DEALLOCATE(this%key)
         IF (ALLOCATED(this%val)) DEALLOCATE(this%val)
      END SUBROUTINE free_str
  
! **********************************************************************************
!>   Access subroutines
! **********************************************************************************      
      
! **********************************************************************************          
      SUBROUTINE init_dictionary_str(this,tbl_len)
! **********************************************************************************          
         CLASS(DictionaryStr),   INTENT(inout) :: this
         INTEGER,     OPTIONAL, INTENT(in)    :: tbl_len

         IF (ALLOCATED(this%vec)) DEALLOCATE(this%vec)
         IF (PRESENT(tbl_len)) THEN
            ALLOCATE(this%vec(tbl_len))
            this%vec_len = tbl_len
         ELSE
            ALLOCATE(this%vec(tbl_size))
            this%vec_len = tbl_size
         END IF
         this%is_init = .TRUE.
      END SUBROUTINE init_dictionary_str

! **********************************************************************************          
      SUBROUTINE set_dictionary_str(this,key,val)
! **********************************************************************************    
         CLASS(DictionaryStr), INTENT(inout) :: this
         CHARACTER(len=*),    INTENT(in)     :: key, val
         INTEGER                             :: hash

         hash = hashMap(key,this%vec_len)
         CALL this%vec(hash)%set(key,val)
      END SUBROUTINE set_dictionary_str

! **********************************************************************************    
      SUBROUTINE get_dictionary_str(this,key,val)
! **********************************************************************************    
         CLASS(DictionaryStr), INTENT(in)             :: this
         CHARACTER(len=*), INTENT(in)                 :: key
         CHARACTER(len=:), ALLOCATABLE, INTENT(out)   :: val
         INTEGER                                      :: hash

         hash = hashMap(key, this%vec_len)
         CALL this%vec(hash)%get(key,val)
      END SUBROUTINE get_dictionary_str
      
      
! **********************************************************************************    
      FUNCTION try_get_dictionary_str(this,key,val) RESULT(res)
! **********************************************************************************    
         CLASS(DictionaryStr),           INTENT(in)   :: this
         CHARACTER(len=*),              INTENT(in)    :: key
         CHARACTER(len=:), ALLOCATABLE, INTENT(out)   :: val
         INTEGER                                      :: hash
         LOGICAL                                      :: res

         CALL get_dictionary_str(this,key,val)
         res = ALLOCATED(val)
         
      END FUNCTION try_get_dictionary_str

! **********************************************************************************    
      SUBROUTINE free_dictionary_str(this)
! **********************************************************************************    
         CLASS(DictionaryStr), INTENT(inout) :: this    
         INTEGER     :: i, low, high

         low  = LBOUND(this%vec,dim=1)
         high = UBOUND(this%vec,dim=1) 
         IF (ALLOCATED(this%vec)) THEN
            DO i=low,high
               CALL this%vec(i)%free()
            END DO
            DEALLOCATE(this%vec)
         END IF
         this%is_init = .FALSE.
      END SUBROUTINE free_dictionary_str
      
! **********************************************************************************
!>   Hash Functions
! **********************************************************************************
      
! **********************************************************************************    
      ELEMENTAL FUNCTION sum_string(str) RESULT(sig)
! **********************************************************************************    
         CHARACTER(len=*), INTENT(in)   :: str
         INTEGER                        :: sig
         CHARACTER, DIMENSION(LEN(str)) :: tmp
         INTEGER :: i

         FORALL (i=1:LEN(str))
            tmp(i) = str(i:i)
         END FORALL
         sig = SUM(ICHAR(tmp))
      END FUNCTION sum_string
      
! **********************************************************************************    
      FUNCTION hashMap(str, tbl_len) RESULT(hsh)
! **********************************************************************************    
      CHARACTER(len=*), INTENT(in)   :: str
      INTEGER, INTENT(in), optional ::tbl_len
      INTEGER :: hsh, tbLen
      
      tbLen = merge(tbl_len, tbl_size, present(tbl_len))
      hsh = MOD(sum_string(str),tbLen) + 1
      
      END FUNCTION hashMap
      
      
! **********************************************************************************    
      END MODULE Dictionary_str
! **********************************************************************************     
  