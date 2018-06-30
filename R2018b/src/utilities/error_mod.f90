!
! Copyright (C) 2016 - 2017 Michael A. Maedo
!
module error_mod
    
    use precision_mod, only: LONG    
  
    implicit none
    
    type error
        integer(LONG) :: status
        character(len = 1000) :: msg
    end type error

    !Errors associated with the string module
    integer(LONG), parameter :: ERROR_INVALID_CARD = -1
    integer(LONG), parameter :: ERROR_WORD_NOT_FOUND = -2
    integer(LONG), parameter :: ERROR_SIZE_STRING_OF_NUM = -3
    integer(LONG), parameter :: ERROR_SIZE_STRING_OF_TXT = -4   
    integer(LONG), parameter :: INFO_SIZE_STRING = -5

    !Exceptions related to file operations
    integer(LONG), parameter :: ERROR_END_FILE = -6
    integer(LONG), parameter :: ERROR_OPEN_FILE = -7
    integer(LONG), parameter :: ERROR_3RD_PARAM = -8

    integer(LONG) :: NO_ERROR = 0

    interface there_is_any_error
        module procedure &
            & int_there_is_any_error, &
            & deriv_there_is_any_error
    end interface there_is_any_error
    
contains


    subroutine error_( err )
        type(error), intent(out) :: err

        err % status = NO_ERROR
        err % msg = ''
    end subroutine error_


    subroutine set_error_status(in_status, err)
        type(error), intent(inout) :: err
        integer(LONG), intent(in) :: in_status
        
        err % status = in_status
        return
    end subroutine set_error_status

    
    subroutine set_error_msg(in_msg, err)
        type(error), intent(inout) :: err
        character(len = *), intent(in) :: in_msg

        err % msg = in_msg
        return
    end subroutine set_error_msg
    

    function get_error_status(err) result (out_status)
        type(error), intent(in) :: err
        integer(LONG) :: out_status

        out_status = err % status
        return
    end function get_error_status


    function get_error_msg(err) result (out_msg)
        type(error), intent(inout) :: err
        character(len = :), allocatable :: out_msg
        
        out_msg = err % msg
        return
    end function get_error_msg


    function int_there_is_any_error(status) result (ans)
        integer(LONG), intent(in) :: status 
        logical :: ans

        ans = .false.
        if (status /= NO_ERROR) ans = .true.
    end function int_there_is_any_error
    

    function deriv_there_is_any_error(err) result (ans)
        type(error), intent(inout) :: err
        logical :: ans

        ans = .false.
        if (err % status /= NO_ERROR) ans = .true.
    end function deriv_there_is_any_error

    
    function there_is_no_error(status) result (ans)
        integer(LONG), intent(in) :: status 
        logical :: ans

        ans = .false.
        if (status == NO_ERROR) ans = .true.
    end function there_is_no_error

    
    !Print an error message on a file
    subroutine print_error(file_ID, err)
        integer(LONG), intent(in) :: file_ID
        type(error), intent(in) :: err

        character(len = *), parameter :: red   = achar(27)//'[31m'
        character(len = *), parameter :: ndisp = achar(27)//'[0m'

500     format(                                                               &
'==========================================================================',/)
510     format(/,                                                             &
'    * * * ERROR in the input file * * *'                                   ,/&  
/                                                                             &
'    ERROR DESCRIPTION', /                                                    &
'    -----------------')                                                       

520     format(A,/)
530     format(/,                                                             &
'==========================================================================',/)            

        character(len = 15) :: line = ''

        write(file_ID,500)
        if ( there_is_any_error( get_error_status (err) ) ) then
            write(file_ID,510)
            write(file_ID,520) err % msg
        else
            write(file_ID,*) ' * * * P R O C E S S    C O M P L E T E D * * * '
        end if 
        write(file_ID,530)
       
    end subroutine print_error

end module error_mod
