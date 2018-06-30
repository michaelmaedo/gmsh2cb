subroutine gmsh2cb(msh, gen, gri, log, err)

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

    implicit none
    
    type(data_file) :: msh
    type(data_file) :: gri
    type(data_file) :: gen
    type(data_file) :: log
    type(error) :: err


    
end subroutine gmsh2cb
