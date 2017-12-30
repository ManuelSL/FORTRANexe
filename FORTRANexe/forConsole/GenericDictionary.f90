! **********************************************************************************
!>    Module Dictionary_str 
! **********************************************************************************   
   MODULE GenericDictionary_Mod
      IMPLICIT NONE
      INTEGER, PARAMETER :: tbl_size = 127
      
      TYPE StringMap
         PRIVATE
         TYPE(StringMap), POINTER :: child => NULL()
         CHARACTER(len=:), ALLOCATABLE :: key, val
      CONTAINS
         PRIVATE
         PROCEDURE, PASS :: set  => set_str
         PROCEDURE, PASS :: get  => get_str
         PROCEDURE, PASS :: free => free_str
      END TYPE StringMap

      TYPE DictionaryStr
         PRIVATE
         TYPE(StringMap), DIMENSION(:), ALLOCATABLE         :: vec
         INTEGER                                            :: vec_len = 0
         LOGICAL                                            :: is_init = .FALSE.
         PROCEDURE(hashFunction_template), POINTER, NOPASS  :: hmap
      CONTAINS 
         PROCEDURE, PASS :: init    => init_dictionary_str
         PROCEDURE, PASS :: set     => set_dictionary_str
         PROCEDURE, PASS :: get     => get_dictionary_str
         PROCEDURE, PASS :: tryGet  => try_get_dictionary_str
         PROCEDURE, PASS :: free    => free_dictionary_str
      END TYPE DictionaryStr
      
      ABSTRACT INTERFACE
         FUNCTION hashFunction_template(a,b) RESULT(res)
            CHARACTER(len=*),  INTENT(in) :: a
            INTEGER, INTENT(in)           :: b
            INTEGER :: res
         END FUNCTION hashFunction_template
      END INTERFACE
      
      INTERFACE GenericDictionary
            MODULE PROCEDURE createGenericDictionary
      END INTERFACE
      
      PUBLIC :: DictionaryStr, hashFunction_template, GenericDictionary
      
      PRIVATE
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
!>   public subroutines
! **********************************************************************************      
      
! **********************************************************************************
         FUNCTION createGenericDictionary(hfunc, tbl_len) RESULT(dic)
! **********************************************************************************
         PROCEDURE(hashFunction_template), POINTER, OPTIONAL, INTENT(inout) :: hfunc
         INTEGER, OPTIONAL,INTENT(in)  :: tbl_len
         TYPE(DictionaryStr)           :: dic
         
         dic%is_init = .FALSE.
         
         if(present(hfunc)) then
            dic%hmap => hfunc
         else
            dic%hmap => hashMap
         endif
         
         call init_dictionary_str(dic, tbl_len)
         
         END FUNCTION  createGenericDictionary
 
      
! **********************************************************************************          
      SUBROUTINE init_dictionary_str(this,tbl_len)
! **********************************************************************************          
         CLASS(DictionaryStr),   INTENT(inout) :: this
         INTEGER,     OPTIONAL, INTENT(in)    :: tbl_len

         !IF (ALLOCATED(this%vec)) DEALLOCATE(this%vec)
         IF (ALLOCATED(this%vec)) call free_dictionary_str(this)
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

         hash = this%hmap(key,this%vec_len)
         CALL this%vec(hash)%set(key,val)
      END SUBROUTINE set_dictionary_str

! **********************************************************************************    
      SUBROUTINE get_dictionary_str(this,key,val)
! **********************************************************************************    
         CLASS(DictionaryStr), INTENT(in)             :: this
         CHARACTER(len=*), INTENT(in)                 :: key
         CHARACTER(len=:), ALLOCATABLE, INTENT(out)   :: val
         INTEGER                                      :: hash

         hash = this%hmap(key, this%vec_len)
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

         IF (ALLOCATED(this%vec)) THEN
            low  = LBOUND(this%vec,dim=1)
            high = UBOUND(this%vec,dim=1) 
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
      CHARACTER(len=*), INTENT(in)  :: str
      INTEGER, INTENT(in)           :: tbl_len
      INTEGER :: hsh

      hsh = MOD(sum_string(str),tbl_len) + 1
      
      END FUNCTION hashMap
      
      
! **********************************************************************************    
   END MODULE GenericDictionary_Mod
! **********************************************************************************     
  
   
! **********************************************************************************
!>    Module Dictionary_Str2Int 
! **********************************************************************************   
   MODULE Dictionary_Str2Int
      IMPLICIT NONE
      INTEGER, PARAMETER :: tbl_size = 127

      TYPE StringIntMap
         PRIVATE
         TYPE(StringIntMap), POINTER   :: child => NULL()
         CHARACTER(len=:), ALLOCATABLE :: key
         INTEGER                       :: val
      CONTAINS
         PRIVATE
         PROCEDURE, PASS :: set  => set_item
         PROCEDURE, PASS :: get  => get_item
         PROCEDURE, PASS :: free => free_item
      END TYPE StringIntMap

      TYPE DictionaryStr2Int
         PRIVATE
         TYPE(StringIntMap), DIMENSION(:), ALLOCATABLE      :: vec
         INTEGER                                            :: vec_len = 0
         LOGICAL                                            :: is_valid = .FALSE.
         PROCEDURE(hashFunction_str2Int), POINTER, NOPASS   :: hmap
      CONTAINS 
         PROCEDURE, PASS :: init    => init_dictionary
         PROCEDURE, PASS :: set     => set_dictionary
         PROCEDURE, PASS :: tryGet  => try_get_dictionary
         PROCEDURE, PASS :: free    => free_dictionary
      END TYPE DictionaryStr2Int
      
      ABSTRACT INTERFACE
         FUNCTION hashFunction_str2Int(a,b) RESULT(res)
            CHARACTER(len=*),  INTENT(in) :: a
            INTEGER, INTENT(in)           :: b
            INTEGER                       :: res
         END FUNCTION hashFunction_str2Int
      END INTERFACE
      
      !INTERFACE DictionaryStr2Int
      !      MODULE PROCEDURE createDictionaryStr2Int
      !END INTERFACE
      
      PUBLIC :: DictionaryStr2Int, hashFunction_str2Int, createDictionaryStr2Int
      
      PRIVATE
      CONTAINS
! **********************************************************************************
!>   internal subroutines
! **********************************************************************************     
      
! **********************************************************************************
      RECURSIVE SUBROUTINE set_item(this,key,val)
! **********************************************************************************
         CLASS(StringIntMap), INTENT(inout)  :: this
         CHARACTER(len=*), INTENT(in)        :: key
         INTEGER, INTENT(in)                 :: val
         INTEGER                             :: keylen

         keylen = LEN(key)
         IF (ALLOCATED(this%key)) THEN
            IF (this%key /= key) THEN
               IF ( .NOT. ASSOCIATED(this%child) ) ALLOCATE(this%child)
               CALL set_item(this%child,key,val)
            ELSE !in case the key already exist update the value
               this%val = val
            END IF
         ELSE
            IF (.NOT. ALLOCATED(this%key)) ALLOCATE(CHARACTER(len=keylen) :: this%key)
            this%key = key
            this%val = val
         END IF
      END SUBROUTINE set_item

! **********************************************************************************
      RECURSIVE SUBROUTINE get_item(this,key,val,found)
! **********************************************************************************
         CLASS(StringIntMap),INTENT(in)   :: this
         CHARACTER(len=*),INTENT(in)      :: key
         INTEGER, INTENT(out)             :: val
         LOGICAL, INTENT(out)             :: found

         IF (ALLOCATED(this%key).AND.(this%key == key)) THEN
            val = this%val
            found = .TRUE.
         ELSE IF(ASSOCIATED(this%child)) THEN ! next item
            CALL get_item(this%child,key,val,found)
         ELSE 
            ! key not found
            !val = 0
            found = .FALSE.
            RETURN
         END IF
         
      END SUBROUTINE get_item

! **********************************************************************************
      RECURSIVE SUBROUTINE free_item(this)
! **********************************************************************************
         CLASS(StringIntMap), INTENT(inout) :: this
         
         IF (ASSOCIATED(this%child)) THEN
            CALL free_item(this%child)
            DEALLOCATE(this%child)
         END IF
         this%child => NULL()
         IF (ALLOCATED(this%key)) DEALLOCATE(this%key)
         this%val = 0
      END SUBROUTINE free_item
  
! **********************************************************************************
!>   public subroutines
! **********************************************************************************      
      
! **********************************************************************************
         FUNCTION createDictionaryStr2Int(tbl_len, hfunc) RESULT(dic)
! **********************************************************************************
         PROCEDURE(hashFunction_str2Int), POINTER, OPTIONAL, INTENT(inout) :: hfunc
         INTEGER, OPTIONAL,INTENT(in)  :: tbl_len
         TYPE(DictionaryStr2Int)       :: dic
         
         dic%is_valid = .FALSE.
         
         if(present(hfunc)) then
            dic%hmap => hfunc
         else
            dic%hmap => hashMap
         endif
         
         call init_dictionary(dic, tbl_len)
         
         END FUNCTION  createDictionaryStr2Int
 
      
! **********************************************************************************          
      SUBROUTINE init_dictionary(this,tbl_len)
! **********************************************************************************          
         CLASS(DictionaryStr2Int), INTENT(inout)   :: this
         INTEGER, OPTIONAL, INTENT(in)             :: tbl_len

         IF (ALLOCATED(this%vec)) call free_dictionary(this)
         IF (PRESENT(tbl_len)) THEN
            ALLOCATE(this%vec(tbl_len))
            this%vec_len = tbl_len
         ELSE
            ALLOCATE(this%vec(tbl_size))
            this%vec_len = tbl_size
         END IF
         this%is_valid = .TRUE.
      END SUBROUTINE init_dictionary

! **********************************************************************************          
      SUBROUTINE set_dictionary(this,key,val)
! **********************************************************************************    
         CLASS(DictionaryStr2Int), INTENT(inout)   :: this
         CHARACTER(len=*),    INTENT(in)           :: key
         INTEGER,INTENT(in)                        :: val
         INTEGER                                   :: hash

         if(.NOT.this%is_valid) RETURN

         hash = this%hmap(key,this%vec_len)
         CALL this%vec(hash)%set(key,val)
      END SUBROUTINE set_dictionary
      
      
! **********************************************************************************    
      FUNCTION try_get_dictionary(this,key,val) RESULT(res)
! **********************************************************************************    
         CLASS(DictionaryStr2Int), INTENT(in):: this
         CHARACTER(len=*), INTENT(in)        :: key
         INTEGER, INTENT(out)                :: val
         LOGICAL                             :: res
         INTEGER                             :: hash

         if(this%is_valid) THEN
            hash = this%hmap(key, this%vec_len)
            CALL this%vec(hash)%get(key,val, res)
         ELSE
            res = .FALSE.
         END IF

      END FUNCTION try_get_dictionary

! **********************************************************************************    
      SUBROUTINE free_dictionary(this)
! **********************************************************************************    
         CLASS(DictionaryStr2Int), INTENT(inout) :: this    
         INTEGER     :: i, low, high

         IF (ALLOCATED(this%vec)) THEN
            low  = LBOUND(this%vec,dim=1)
            high = UBOUND(this%vec,dim=1) 
            DO i=low,high
               CALL this%vec(i)%free()
            END DO
            DEALLOCATE(this%vec)
         END IF
         this%is_valid = .FALSE.
      END SUBROUTINE free_dictionary
      
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
      CHARACTER(len=*), INTENT(in)  :: str
      INTEGER, INTENT(in)           :: tbl_len
      INTEGER :: hsh

      hsh = MOD(sum_string(str),tbl_len) + 1
      
      END FUNCTION hashMap
      
      
! **********************************************************************************    
      END MODULE Dictionary_Str2Int
! **********************************************************************************     