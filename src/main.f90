!
!     G M S H 2 C B
!     Version: Mark 1
!
!     This code reads the file from GMSH, which has msh extension, and
!     creates another two raw files, *_gen.dat and *gri_.dat, that are
!     compatible  with CODE_BRIGHT (UFPE version). Note  that the data
!     of  the problem and initial conditions still need to be properly
!     calibrated.
!
!
!     Created by Michael A. Maedo on December-01-2017
!     Copyrigth (C) 2017-2018
!
!     email: mmaedo@tamu.edu
!
!
program main

    use precision_mod, only: LONG
    use string_mod, only: LEN_NAME_FILE
    
    use file_mod, only: &
        & data_file, &
        & open_file, &
        & close_file, &
        & get_file_name, &
        & get_file_ID

    use error_mod, only: &
        & error, &
        & error_, &
        & NO_ERROR, &
        & there_is_no_error, &
        & get_error_status, &
        & print_error

    use gmsh2cb_mod, only: gmsh2cb
    
    implicit none
    
    type(data_file) :: msh
    type(data_file) :: gri
    type(data_file) :: gen
    type(data_file) :: log
    type(error) :: err

    integer(LONG) :: argc
    character(len = LEN_NAME_FILE) :: argv
    character(len = LEN_NAME_FILE) :: loc_str

    character(len = *), parameter :: red    = achar(27)//'[31m'
    character(len = *), parameter :: yellow = achar(27)//'[33m'
    character(len = *), parameter :: green  = achar(27)//'[32m'
    character(len = *), parameter :: ndisp  = achar(27)//'[0m' 

!----------------------------------------------------------------
500 format(                                                  / &
    &'      * * * G M S H 2 C B * * *'                       / &
    &'      Version: Mark 1'                                 / &
    &''                                                      / & 
    &'      Created by Michael A. Maedo on December-01-2017' / &
    &'      Copyrigth (C) 2017-2018'                         / &
    &''                                                      / &
    &'      Consulting: mmaedo@tamu.edu'/)
!----------------------------------------------------------------
    write(*,500)

    call error_( err )
    
    !Get arguments from the command line
    argc = 0; argv = ''
    do
        call get_command_argument(argc, loc_str)
        if (len_trim(loc_str) == 0) exit
        argc = argc + 1
        argv = loc_str
    end do
    
    call open_gmsh2cb_files(argv, msh, gen, gri, log, err)

    if ( there_is_no_error( get_error_status( err ) ) ) &
        call gmsh2cb(msh, gen, gri, log, err)

    call print_error( get_file_ID( log ), err)
    
    call close_gmsh2cb_files(msh, gen, gri, log)

    
contains

    subroutine open_gmsh2cb_files(filename, msh, gen, gri, log, err)
        character(len = *), intent(in) :: filename
        type(error), intent(inout) :: err
        type(data_file), intent(out) :: msh 
        type(data_file), intent(out) :: gen
        type(data_file), intent(out) :: gri
        type(data_file), intent(out) :: log

        call open_file(trim(filename)//'.msh', msh, 'INPUT', err)        
        call open_file(trim(filename)//'_gen.dat', gen, 'OUTPUT', err)
        call open_file(trim(filename)//'_gri.dat', gri, 'OUTPUT', err)
        call open_file(trim(filename)//'.log', log, 'OUTPUT', err)
        
    end subroutine open_gmsh2cb_files


    subroutine close_gmsh2cb_files(msh, gen, gri, log)
        type(data_file), intent(inout) :: msh 
        type(data_file), intent(inout) :: gen
        type(data_file), intent(inout) :: gri
        type(data_file), intent(inout) :: log        

        call close_file( msh )
        call close_file( gen )
        call close_file( gri )
        call close_file( log )
    end subroutine close_gmsh2cb_files
    
end program main
