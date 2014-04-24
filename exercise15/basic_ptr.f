
c     This program demonstrates pointers in Fortran 90.  Note that
c     when a pointer is associated with a second pointer, the first
c     pointer is given the same association status as the second
c     at the time of the association.

      PROGRAM basic_ptr

      REAL, TARGET :: atarg=1., btarg=2.   !   Default values are set.
      REAL, POINTER :: rptr1,rptr2

      rptr1 => atarg   !   Pointer 1 now associated with memory for atarg.
      rptr2 => rptr1   !   Pointer 2 now associated with memory for rptr1.

      WRITE(*,*) "First evaluation of rptr1 is", rptr1
      WRITE(*,*) "First evaluation of rptr2 is", rptr2

      rptr1 => btarg   !   Pointer 1 now associated with memory for btarg.

      WRITE(*,*) "Second evaluation of rptr1 is", rptr1
      WRITE(*,*) "Second evaluation of rptr2 is", rptr2

c     The following creates un-named storage and associates Pointer 1
c     with that location.

      ALLOCATE(rptr1) !   Pointer 1 is no longer associated with btarg.
      rptr1 = 3.
      rptr2 => rptr1  !   Pointer 2 also associated with un-named storage.

      WRITE(*,*) "Third evaluation of rptr1 is", rptr1
      WRITE(*,*) "Third evaluation of rptr2 is", rptr2

c     The following changes the association of Pointer 1 only.
c     The un-named storage would become inaccessible without the
c     Pointer 2 association

      rptr1 => btarg  !   This moves Pointer 1 only.

      WRITE(*,*) "Fourth evaluation of rptr1 is", rptr1
      WRITE(*,*) "Fourth evaluation of rptr2 is", rptr2

      END PROGRAM basic_ptr
