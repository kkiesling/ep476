!=======================================================================
! File chp_data_mod.f90 contains module variables that are used to
! specify the properties of individual particles.
!
! The contained subroutine is used to initialize the arrays.
!=======================================================================

  MODULE chp_data_mod
  USE chp_inp_mod
  USE chp_kind_mod
  IMPLICIT NONE

  REAL(rknd), DIMENSION(:), ALLOCATABLE :: pchrg  ! charge of each
  REAL(rknd), DIMENSION(:), ALLOCATABLE :: pmass  ! mass of each
  REAL(rknd), DIMENSION(:), ALLOCATABLE :: qom    ! charge to mass ratio

  CONTAINS

!=======================================================================
! The module subroutine chp_data_init initializes the data arrays for
! the collection of particles.
!=======================================================================

  SUBROUTINE chp_data_init

  INTEGER(iknd) :: ipart

!-----------------------------------------------------------------------
! Allocate the arrays for the number of particles in the computation.
!-----------------------------------------------------------------------

  ALLOCATE(pchrg(npart))
  ALLOCATE(pmass(npart))
  ALLOCATE(qom(npart))

!-----------------------------------------------------------------------
! Loop over the particles and use the input specifications or default
! values to assign the data arrays.
!-----------------------------------------------------------------------

  DO ipart=1,npart

    pchrg(ipart)=elem_chrg*(num_prots(ipart)-num_elecs(ipart))
    pmass(ipart)=proton_mass*num_prots(ipart) &
                +neutron_mass*num_neuts(ipart) &
                +electron_mass*num_elecs(ipart)
    qom(ipart)=pchrg(ipart)/pmass(ipart)

  ENDDO

  RETURN
  END SUBROUTINE chp_data_init


  END MODULE chp_data_mod
