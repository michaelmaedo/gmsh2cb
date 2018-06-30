!Allocate and deallocate memory
module memory_mod
    use precision    , only: BYTE, DOUBLE, SINGLE, LONG, SHORT                 !
    use real_cons    , only: ZERO                                              !
    use logical_units, only: OUT                                               !
    use string       , only: LEN_STR                                           !
    use error_list   , only: mem_error_info , ERROR_ALREADY_ALLOCATED, &       !
                             ERROR_MEM_ALLOC, ERROR_INVALID_DIME     , &       !
                             ERROR_DEALLOC  , NO_ERROR                         !

    implicit none                                                              !

contains

    !Allocate memory for double precision vectors
    subroutine double_vec_alloc(vec_name, vec, nsize, info)
        integer(LONG)     , intent(out) :: info                          !
        integer(LONG)     , intent(in)  :: nsize                         !
        character(len = *), intent(in)  :: vec_name                      !

        real(double), allocatable, intent(inout) :: vec(:)               !

        integer(LONG) :: linfo

        character(len = 16) :: subname = 'double_vec_alloc'

        info = 0
        if (.not.allocated(vec)) then
            if (nsize > 0) then
                allocate(vec(nsize), stat = linfo)
                if (linfo == 0) then
                    vec = ZERO
                else
                    info = 1
                end if
            else
                info = 2
            end if
        else
            info = 10
        end if
        call mem_error_info(subname, vec_name, info)
    end subroutine double_vec_alloc


    subroutine single_vec_alloc(vec_name, vec, nsize, info)
!========================================================================!
!             Allocate memory for single precision vectors               !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        integer(LONG)     , intent(in)  :: nsize                         !
        character(len = *), intent(in)  :: vec_name                      !
!                                                                        !
!       ** Array Arguments **                                            !
        real(SINGLE), allocatable, intent(inout) :: vec(:)               !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       vec_name : name of the variable                                  !
!       vec      : 1-dimensional array to be allocated                   !
!       nsize    : number of elements to be allocated.                   !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       vec      : 1-dimensional array to be allocated                   !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 16) :: subname = 'single_vec_alloc'
!
!       Executable statements
        info = 0
        if (.not.allocated(vec)) then
            if (nsize > 0) then
                allocate(vec(nsize), stat = linfo)
                if (linfo == 0) then
                    vec = ZERO
                else
                    info = 1
                end if
            else
                info = 2
            end if
        else
            info = 10
        end if
        call mem_error_info(subname, vec_name, info)

    end subroutine single_vec_alloc


    subroutine long_vec_alloc(vec_name, vec, nsize, info)
!========================================================================!
!               Allocate memory for integer(LONG) vectors                !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        integer(LONG)     , intent(in)  :: nsize                         !
        character(len = *), intent(in)  :: vec_name                      !
!                                                                        !
!       ** Array Arguments **                                            !
        integer(LONG), allocatable, intent(inout) :: vec(:)              !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       vec_name : name of the variable                                  !
!       vec      : 1-dimensional array to be allocated                   !
!       nsize    : number of elements to be allocated.                   !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       vec      : 1-dimensional array to be allocated                   !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 14) :: subname = 'long_vec_alloc'
!
!       Executable statements
        info = 0
        if (.not.allocated(vec)) then
            if (nsize > 0) then
                allocate(vec(nsize), stat = linfo)
                if (linfo == 0) then
                    vec = 0_LONG
                else
                    info = 1
                end if
            else
                info = 2
            end if
        else
            info = 10
        end if
        call mem_error_info(subname, vec_name, info)

    end subroutine long_vec_alloc


    subroutine short_vec_alloc(vec_name, vec, nsize, info)
!========================================================================!
!               Allocate memory for integer(LONG) vectors                !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        integer(LONG)     , intent(in)  :: nsize                         !
        character(len = *), intent(in)  :: vec_name                      !
!                                                                        !
!       ** Array Arguments **                                            !
        integer(SHORT), allocatable, intent(inout) :: vec(:)             !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       vec_name : name of the variable                                  !
!       vec      : 1-dimensional array to be allocated                   !
!       nsize    : number of elements to be allocated.                   !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       vec      : 1-dimensional array to be allocated                   !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 15) :: subname = 'short_vec_alloc'
!
!       Executable statements
        info = 0
        if (.not.allocated(vec)) then
            if (nsize > 0) then
                allocate(vec(nsize), stat = linfo)
                if (linfo == 0) then
                    vec = 0_LONG
                else
                    info = 1
                end if
            else
                info = 2
            end if
        else
            info = 10
        end if
        call mem_error_info(subname, vec_name, info)

    end subroutine short_vec_alloc


    subroutine double_matx_alloc(matx_name, matx, nrow, ncol, info)
!========================================================================!
!             Allocate memory for double precision matrices              !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        integer(LONG)     , intent(in)  :: nrow, ncol                    !
        character(len = *), intent(in)  :: matx_name                     !
!                                                                        !
!       ** Array Arguments **                                            !
        real(DOUBLE), allocatable, intent(inout) :: matx(:,:)            !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       matx_name : name of the variable                                 !
!       matx      : 2-dimensional array to be allocated                  !
!       nrow      : number of rows to be allocated                       !
!       ncol      : number of columns to be allocated                    !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       matx      : 2-dimensional array to be allocated                  !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 17) :: subname = 'double_matx_alloc'
!
!       Executable statements
        info = 0
        if (.not.allocated(matx)) then
            if (nrow <= 0) info = 3
            if (ncol <= 0) info = info + 4
            if (info == 0) then
                allocate(matx(nrow,ncol), stat = linfo)
                if (linfo == 0) then
                    matx = ZERO
                else
                    info = 1
                end if
            end if
        else
            info = 10
        end if
        call mem_error_info(subname, matx_name, info)

    end subroutine double_matx_alloc


    subroutine single_matx_alloc(matx_name, matx, nrow, ncol, info)
!========================================================================!
!             Allocate memory for single precision matrices              !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        integer(LONG)     , intent(in)  :: nrow, ncol                    !
        character(len = *), intent(in)  :: matx_name                     !
!                                                                        !
!       ** Array Arguments **                                            !
        real(SINGLE), allocatable, intent(inout) :: matx(:,:)            !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       matx_name : name of the variable                                 !
!       matx      : 2-dimensional array to be allocated                  !
!       nrow      : number of rows to be allocated                       !
!       ncol      : number of columns to be allocated                    !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       matx      : 2-dimensional array to be allocated                  !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 17) :: subname = 'single_matx_alloc'
!
!       Executable statements
        info = 0
        if (.not.allocated(matx)) then
            if (nrow <= 0) info = 3
            if (ncol <= 0) info = info + 4
            if (info == 0) then
                allocate(matx(nrow,ncol), stat = linfo)
                if (linfo == 0) then
                    matx = ZERO
                else
                    info = 1
                end if
            end if
        else
            info = 10
        end if
        call mem_error_info(subname, matx_name, info)

    end subroutine single_matx_alloc


    subroutine long_matx_alloc(matx_name, matx, nrow, ncol, info)
!========================================================================!
!                Allocate memory for integer(LONG) matrices              !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        integer(LONG)     , intent(in)  :: nrow, ncol                    !
        character(len = *), intent(in)  :: matx_name                     !
!                                                                        !
!       ** Array Arguments **                                            !
        integer(LONG), allocatable, intent(inout) :: matx(:,:)           !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       matx_name : name of the variable                                 !
!       matx      : 2-dimensional array to be allocated                  !
!       nrow      : number of rows to be allocated                       !
!       ncol      : number of columns to be allocated                    !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       matx      : 2-dimensional array to be allocated                  !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 15) :: subname = 'long_matx_alloc'
!
!       Executable statements
        info = 0
        if (.not.allocated(matx)) then
            if (nrow <= 0) info = 3
            if (ncol <= 0) info = info + 4
            if (info == 0) then
                allocate(matx(nrow,ncol), stat = linfo)
                if (linfo == 0) then
                    matx = 0_LONG
                else
                    info = 1
                end if
            end if
        else
            info = 10
        end if
        call mem_error_info(subname, matx_name, info)

    end subroutine long_matx_alloc


    subroutine short_matx_alloc(matx_name, matx, nrow, ncol, info)
!========================================================================!
!               Allocate memory for integer(SHORT) matrices              !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        integer(LONG)     , intent(in)  :: nrow, ncol                    !
        character(len = *), intent(in)  :: matx_name                     !
!                                                                        !
!       ** Array Arguments **                                            !
        integer(SHORT), allocatable, intent(inout) :: matx(:,:)          !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       matx_name : name of the variable                                 !
!       matx      : 2-dimensional array to be allocated                  !
!       nrow      : number of rows to be allocated                       !
!       ncol      : number of columns to be allocated                    !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       matx      : 2-dimensional array to be allocated                  !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 16) :: subname = 'short_matx_alloc'
!
!       Executable statements
        info = 0
        if (.not.allocated(matx)) then
            if (nrow <= 0) info = 3
            if (ncol <= 0) info = info + 4
            if (info == 0) then
                allocate(matx(nrow,ncol), stat = linfo)
                if (linfo == 0) then
                    matx = 0_LONG
                else
                    info = 1
                end if
            end if
        else
            info = 10
        end if
        call mem_error_info(subname, matx_name, info)

    end subroutine short_matx_alloc


    subroutine set_vec_free(vec_name, vec, error)
!==============================================================================!
!                       Deallocate memory for set vectors                      !
!==============================================================================!
!                                                                              !
!       ** Scalar Arguments **                                                 !
        integer(LONG)     , intent(out) :: error                               !
        character(len = *), intent(in)  :: vec_name                            !
!                                                                              !
!       ** Array Arguments **                                                  !
        type(set), allocatable, intent(inout) :: vec(:)                        !
!                                                                              !
!       Input Arguments                                                        !
!       ===============                                                        !
!       vec_name : name of the variable                                        !
!       vec      : 1-dimensional array to be deallocated                       !
!                                                                              !
!       Output Arguments                                                       !
!       ================                                                       !
!       vec      : 1-dimensional array to be deallocated                       !
!       error    : if (error == 0) then the array was not allocated            !
!                                                                              !
!       Further Details                                                        !
!       ===============                                                        !
!       created by Michael A. Maedo on August-21-2016                          !
!       updated by Michael A. Maedo on August-28-2016                          !
!                                                                              !
!==============================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: ii, loc_error
!
!       Executable statements
        error = NO_ERROR
        if (allocated(vec)) then
            deallocate(vec, stat = loc_error)
            if (loc_error /= NO_ERROR) error = ERROR_DEALLOC
        end if

        if (error == NO_ERROR) return

        call mem_error_info('SET_VEC_FREE', vec_name, error)
    end subroutine set_vec_free


    subroutine double_vec_free(vec_name, vec, info)
!========================================================================!
!             Deallocate memory of double precision vectors              !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        character(len = *), intent(in)  :: vec_name                      !
!                                                                        !
!       ** Array Arguments **                                            !
        real(DOUBLE), allocatable, intent(inout) :: vec(:)               !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       vec_name : name of the variable                                  !
!       vec      : 1-dimensional array to be deallocated                 !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       vec      : 1-dimensional array to be deallocated                 !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 15) :: subname = 'double_vec_free'
!
!       Executable statements
        info = 0
        if (allocated(vec)) then
            deallocate(vec, stat = linfo)
            if (linfo /= 0) info = -1
        end if
        call mem_error_info(subname, vec_name, info)

    end subroutine double_vec_free


    subroutine single_vec_free(vec_name, vec, info)
!========================================================================!
!             Deallocate memory of single precision vectors              !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        character(len = *), intent(in)  :: vec_name                      !
!                                                                        !
!       ** Array Arguments **                                            !
        real(SINGLE), allocatable, intent(inout) :: vec(:)               !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       vec_name : name of the variable                                  !
!       vec      : 1-dimensional array to be deallocated                 !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       vec      : 1-dimensional array to be deallocated                 !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 15) :: subname = 'single_vec_free'
!
!       Executable statements
        info = 0
        if (allocated(vec)) then
            deallocate(vec, stat = linfo)
            if (linfo /= 0) info = -1
        end if
        call mem_error_info(subname, vec_name, info)

    end subroutine single_vec_free


    subroutine long_vec_free(vec_name, vec, info)
!========================================================================!
!               Deallocate memory of integer(LONG) vectors               !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        character(len = *), intent(in)  :: vec_name                      !
!                                                                        !
!       ** Array Arguments **                                            !
        integer(LONG), allocatable, intent(inout) :: vec(:)              !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       vec_name : name of the variable                                  !
!       vec      : 1-dimensional array to be deallocated                 !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       vec      : 1-dimensional array to be deallocated                 !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 13) :: subname = 'long_vec_free'
!
!       Executable statements
        info = 0
        if (allocated(vec)) then
            deallocate(vec, stat = linfo)
            if (linfo /= 0) info = -1
        end if
        call mem_error_info(subname, vec_name, info)

    end subroutine long_vec_free


    subroutine short_vec_free(vec_name, vec, info)
!========================================================================!
!              Deallocate memory of integer(SHORT) vectors               !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        character(len = *), intent(in)  :: vec_name                      !
!                                                                        !
!       ** Array Arguments **                                            !
        integer(SHORT), allocatable, intent(inout) :: vec(:)             !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       vec_name : name of the variable                                  !
!       vec      : 1-dimensional array to be deallocated                 !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       vec      : 1-dimensional array to be deallocated                 !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 14) :: subname = 'short_vec_free'
!
!       Executable statements
        info = 0
        if (allocated(vec)) then
            deallocate(vec, stat = linfo)
            if (linfo /= 0) info = -1
        end if
        call mem_error_info(subname, vec_name, info)

    end subroutine short_vec_free


    subroutine double_matx_free(matx_name, matx, info)
!========================================================================!
!             Deallocate memory of double precision matrices             !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        character(len = *), intent(in)  :: matx_name                     !
!                                                                        !
!       ** Array Arguments **                                            !
        real(DOUBLE), allocatable, intent(inout) :: matx(:,:)            !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       matx_name : name of the variable                                 !
!       matx      : 2-dimensional array to be deallocated                !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       matx      : 2-dimensional array to be deallocated                !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 16) :: subname = 'double_matx_free'
!
!       Executable statements
        info = 0
        if (allocated(matx)) then
            deallocate(matx, stat = linfo)
            if (linfo /= 0) info = -1
        end if
        call mem_error_info(subname, matx_name, info)

    end subroutine double_matx_free


    subroutine single_matx_free(matx_name, matx, info)
!========================================================================!
!             Deallocate memory of single precision matrices             !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        character(len = *), intent(in)  :: matx_name                     !
!                                                                        !
!       ** Array Arguments **                                            !
        real(SINGLE), allocatable, intent(inout) :: matx(:,:)            !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       matx_name : name of the variable                                 !
!       matx      : 2-dimensional array to be deallocated                !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       matx      : 2-dimensional array to be deallocated                !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 16) :: subname = 'single_matx_free'
!
!       Executable statements
        info = 0
        if (allocated(matx)) then
            deallocate(matx, stat = linfo)
            if (linfo /= 0) info = -1
        end if
        call mem_error_info(subname, matx_name, info)

    end subroutine single_matx_free


    subroutine long_matx_free(matx_name, matx, info)
!========================================================================!
!               Deallocate memory of integer(LONG) matrices              !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        character(len = *), intent(in)  :: matx_name                     !
!                                                                        !
!       ** Array Arguments **                                            !
        integer(LONG), allocatable, intent(inout) :: matx(:,:)           !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       matx_name : name of the variable                                 !
!       matx      : 2-dimensional array to be deallocated                !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       matx      : 2-dimensional array to be deallocated                !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 14) :: subname = 'long_matx_free'
!
!       Executable statements
        info = 0
        if (allocated(matx)) then
            deallocate(matx, stat = linfo)
            if (linfo /= 0) info = -1
        end if
        call mem_error_info(subname, matx_name, info)

    end subroutine long_matx_free


    subroutine short_matx_free(matx_name, matx, info)
!========================================================================!
!              Deallocate memory of integer(SHORT) matrices              !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        integer(LONG)     , intent(out) :: info                          !
        character(len = *), intent(in)  :: matx_name                     !
!                                                                        !
!       ** Array Arguments **                                            !
        integer(SHORT), allocatable, intent(inout) :: matx(:,:)          !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       matx_name : name of the variable                                 !
!       matx      : 2-dimensional array to be deallocated                !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       matx      : 2-dimensional array to be deallocated                !
!       info     : if (info == 0) then the array was not allocated       !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-22-2016                      !
!       updated by Michael A. Maedo on July-22-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: linfo
!
!       ** Parameters **
        character(len = 15) :: subname = 'short_matx_free'
!
!       Executable statements
        info = 0
        if (allocated(matx)) then
            deallocate(matx, stat = linfo)
            if (linfo /= 0) info = -1
        end if
        call mem_error_info(subname, matx_name, info)

    end subroutine short_matx_free


end module memory_mod
