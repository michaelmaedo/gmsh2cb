module matx_mult
!========================================================================!
!         Overload Matrix-matrix and matrix-vector multiplication        !
!========================================================================!                                                                        !
!                                                                        !                                                                        !
!   ** Modules **                                                        !
    use real_cons, only: ZERO, ONE                                       !
    use precision, only: LONG, DOUBLE                                    !
    use error_list                                                       !
!                                                                        !
!   ** All variables must be declared **                                 !
    implicit none                                                        !
!                                                                        !
!   ** External Subroutines **                                           !
    interface                                                            !
        subroutine dgemm(transa, transb, m, n, k, alpha, &               !
                        & A, lda, B, ldb, beta, C, ldc)                  !
            double precision  alpha, beta                                !
            integer k, lda, ldb, ldc, m, n                               !
            character transa, transb                                     !
            double precision A(lda,*), B(ldb,*), C(ldc,*)                !
        end subroutine dgemm                                             !
    end interface                                                        !
                                                                         !
    interface                                                            !
        subroutine dgemv(trans, m, n, alpha, A, lda, &                   !
                       & x, incx, beta, y, incy)                         !
            double precision alpha, beta                                 !
            integer incx, incy, lda, m, n                                !
            character trans                                              !
            double precision A(lda,*), x(*), y(*)                        !
        end subroutine                                                   !
    end interface                                                        !
!                                                                        !
!   ** Intrinsic Functions **                                            !
    intrinsic size                                                       !
!                                                                        !
!   Notation                                                             !
!   ========                                                             !
!   uppercase letters are used for materices. Ex: A, B and C             !
!   lowercase letters are used for vectors. Ex: a, b and c               !
!                                                                        !
!   Functions and Subroutines                                            !
!   =========================                                            !
!   .x.   : matrix-matrix or matrix-vector multiplication                !
!   .Tx.  : tranpose the first matrix operand and then perform .x.       !
!   .Tx.  : tranpose the second matrix operand and then perform .x.      !
!   .TxT. : compute the transpose of both matrices and then perform .x.  !
!                                                                        !
!   Further Details                                                      !
!   ===============                                                      !
!   created by Michael A. Maedo on May-29-2016                           !
!   updated by Michael A. Maedo on July-05-2016                          !
!                                                                        !
!========================================================================!

!------------------------------------------------------------------------!
! Overload .x. to perform array multiplication                           !
!------------------------------------------------------------------------!
    interface operator (.x.)
        module procedure dmatx_matx_mult, dmatx_vec_mult
     end interface operator (.x.)

!------------------------------------------------------------------------!
! Overload .Tx. to find the transpose of the first matrix operand, and   !
! then multiply it by another matrix (vector)                            !
!------------------------------------------------------------------------!
    interface operator (.Tx.)
        module procedure dmatxT_matx_mult, dmatxT_vec_mult
    end interface operator (.Tx.)

!------------------------------------------------------------------------!
! Overload .xT. to multiply te first matrix operand by the transpose of  !
! the second matrix operand                                              !
!------------------------------------------------------------------------!
    interface operator (.xT.)
        module procedure dmatx_matxT_mult
    end interface operator (.xT.)

!------------------------------------------------------------------------!
! Overload .TxT. to multiply the transpose of the first matrix operand   !
! by the transpose of the second matrix operand                          !
!------------------------------------------------------------------------!
    interface operator (.TxT.)
        module procedure dmatxT_matxT_mult
   end interface operator (.TxT.)

contains

    function dmatx_matx_mult(A, B) result (C)
!========================================================================!
!         Multiply two matrices with double precision components         !
!========================================================================!
!                                                                        !
!       ** Array arguments **                                            !
        real(DOUBLE), intent(in)  :: A(:,:), B(:,:)                      !
        real(DOUBLE), allocatable :: C(:,:)                              !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       A : fist operand of the matrix-matrix multiplication             !
!       B : second operand of the matrix-matrix multiplication           !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       C : resultant matrix from A*B operation                          !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on June-02-2016                      !
!       updated by Michael A. Maedo on June-02-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Intrinsic Functions **
        intrinsic size
!
!       ** Local Scalars **
        integer(LONG) :: nrowA, ncolA
        integer(LONG) :: nrowB, ncolB
        integer(LONG) :: astat, error
!
!       ** Parameters **
        character(len = 1),  parameter :: TRANS = 'N'
        character(len = 15), parameter :: FNAME = 'DMATX_MATX_MULT'
!
!       Executable statements
        nrowA = size(A, 1); ncolA = size(A, 2)
        nrowB = size(B, 1); ncolB = size(B, 2)
        error = NO_ERROR
!
!       Check dimensions of matrices
        if (nrowA <= 0 .or. ncolA <= 0) error = ERROR_DIM_1OP_ZERO
        if (nrowB <= 0 .or. ncolB <= 0) error = error + ERROR_DIM_2OP_ZERO
        if (ncolA /= nrowB)             error = error + ERROR_INCOMP_DIM

        if (error /= NO_ERROR) go to 100

        allocate(C(nrowA,ncolB), stat = astat)
        if (astat /= NO_ERROR) then
            error = ERROR_MEM_ALLOC
            go to 100
        end if

        call dgemm(TRANS, TRANS, nrowA, ncolB, nrowA, ONE, &
                   A, nrowA, B, nrowB, ZERO, C, nrowA)
        return

100     call math_error_info(FNAME, error)
    end function dmatx_matx_mult


    function dmatx_vec_mult(A, x) result (y)
!========================================================================!
!   Perform matrix-vector operations, where components of both are       !
!   double precision                                                     !
!========================================================================!
!                                                                        !
!       ** Array arguments **                                            !
        real(DOUBLE), intent(in)  :: A(:,:), x(:)                        !
        real(DOUBLE), allocatable :: y(:)                                !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       A : matrix of the matrix-vector multiplication                   !
!       x : vector of the matrix-vector multiplication                   !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       y : resultant vector from A*x operation                          !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on June-04-2016                      !
!       updated by Michael A. Maedo on June-04-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Intrinsic Functions **
        intrinsic size
!
!       ** Local Scalars **
        integer(LONG) :: nrowA, ncolA, sizex
        integer(LONG) :: inc_x, inc_y
        integer(LONG) :: error, astat
!
!       ** Parameters **
        character(len = 1),  parameter :: TRANS = 'N'
        character(len = 14), parameter :: FNAME = 'DMATX_VEC_MULT'
!
!       Executable statements
        nrowA = size(A, 1)
        ncolA = size(A, 2)
        sizex = size(x)
        inc_x = 1; inc_y = 1
        error = NO_ERROR
!
!       Check dimensions of both, matrix and vector
        if (nrowA <= 0 .or. ncolA <= 0) error = ERROR_DIM_1OP_ZERO
        if (sizex <= 0)                 error = error + ERROR_DIM_2OP_ZERO
        if (ncolA /= sizex)             error = error + ERROR_INCOMP_DIM

        if (error /= 0) go to 200

        allocate(y(nrowA), stat = astat)
        if (astat /= 0) then
            error = ERROR_MEM_ALLOC
            go to 200
        end if

        call dgemv(TRANS, nrowA, ncolA, ONE, A, nrowA, x, inc_x, ZERO, y, inc_y)
        return

200    call math_error_info(FNAME, error)
    end function dmatx_vec_mult


    function dmatxT_matx_mult(A, B) result (C)
!========================================================================!
!   Find the tranpose of the first matris operand, and then multiply     !
!   it by the second matrix operand. Note that both matrices contain     !
!   double precision components                                          !
!========================================================================!
!                                                                        !
!       ** Array arguments **                                            !
        real(DOUBLE), intent(in)  :: A(:,:), B(:,:)                      !
        real(DOUBLE), allocatable :: C(:,:)                              !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       A : fist operand of the matrix-matrix multiplication             !
!       B : second operand of the matrix-matrix multiplication           !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       C : resultant matrix from traspose(A)*B operation                !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on June-05-2016                      !
!       updated by Michael A. Maedo on June-05-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Intrinsic Functions **
        intrinsic size
!
!       ** Local Scalars **
        integer(LONG) :: nrowA, ncolA
        integer(LONG) :: nrowB, ncolB
        integer(LONG) :: astat, error
!
!       ** Parameters **
        character(len = 1),  parameter :: TRANSA = 'T'
        character(len = 1),  parameter :: TRANSB = 'N'
        character(len = 16), parameter :: FNAME  = 'DMATXT_MATX_MULT'
!
!       Executable statements
        nrowA = size(A, 1); ncolA = size(A, 2)
        nrowB = size(B, 1); ncolB = size(B, 2)
        error = NO_ERROR
!
!       Check dimensions of the matrices
        if (nrowA <= 0 .or. ncolA <= 0) error = ERROR_DIM_1OP_ZERO
        if (nrowB <= 0 .or. ncolB <= 0) error = error + ERROR_DIM_2OP_ZERO
        if (nrowA /= nrowB)             error = error + ERROR_INCOMP_DIM

        if (error /= 0) go to 300

        allocate(C(ncolA,ncolB), stat = astat)
        if (astat == 0) then
            error = ERROR_MEM_ALLOC
            go to 300
        end if

        call dgemm(TRANSA, TRANSB, nrowA, ncolB, nrowA, ONE, &
                    A, nrowA, B, nrowB, ZERO, C, nrowA)
        return

300     call math_error_info(FNAME, error)
!
!       end of dmatxT_matx_mult
!
    end function dmatxT_matx_mult


    function dmatxT_vec_mult(A, x) result (y)
!========================================================================!
!   Find the  transpose of the first matrix operand, and then multiply   !
!   it by a vector. Note that both contain double precision components   !
!========================================================================!
!                                                                        !
!       ** Array arguments **                                            !
        real(DOUBLE), intent(in)  :: A(:,:), x(:)                        !
        real(DOUBLE), allocatable :: y(:)                                !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       A : matrix of the matrix-vector multiplication                   !
!       x : vector of the matrix-vector multiplication                   !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       y : resultant vector from A*x operation                          !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on June-05-2016                      !
!       updated by Michael A. Maedo on June-05-2016                      !
!                                                                        !
!========================================================================!
!
!       ** External Subroutine **
        external dgemv
!
!       ** Intrinsic Functions **
        intrinsic size
!
!       ** Local Scalars **
        integer(LONG) :: nrowA, ncolA, sizex
        integer(LONG) :: inc_x, inc_y
        integer(LONG) :: error, astat
!
!       ** Parameters **
        character(len = 1),  parameter :: TRANS = 'T'
        character(len = 15), parameter :: FNAME = 'DMATXT_VEC_MULT'
!
!       Executable statements
        nrowA = size(A, 1)
        ncolA = size(A, 2)
        sizex = size(x)
        inc_x = 1; inc_y = 1
        error = NO_ERROR
!
!       Check dimensions of both, matrix and vector
        if (nrowA <= 0 .or. ncolA <= 0) error = ERROR_DIM_1OP_ZERO
        if (sizex <= 0)                 error = ERROR_DIM_2OP_ZERO
        if (nrowA /= sizex)             error = error + ERROR_INCOMP_DIM

        if (error /= 0) go to 400

        allocate(y(ncolA), stat = astat)
        if (astat /= 0) then
            error = ERROR_MEM_ALLOC
            go to 400
        end if

        call dgemv(TRANS, nrowA, ncolA, ONE, A, nrowA, x, inc_x, ZERO, y, inc_y)
        return

400     call math_error_info(FNAME, error)
    end function dmatxT_vec_mult


    function dmatx_matxT_mult(A, B) result (C)
!========================================================================!
!   Multiply  the first matrix operand by the transpose  of the second   !
!   matrix operand. Note that both contain double precision components   !
!========================================================================!
!                                                                        !
!       ** Array arguments **                                            !
        real(DOUBLE), intent(in)  :: A(:,:), B(:,:)                      !
        real(DOUBLE), allocatable :: C(:,:)                              !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       A : fist operand of the matrix-matrix multiplication             !
!       B : second operand of the matrix-matrix multiplication           !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       C : resultant matrix from A*traspose(B) operation                !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on June-05-2016                      !
!       updated by Michael A. Maedo on June-05-2016                      !
!                                                                        !
!========================================================================!
!
!       ** External Subroutine **
        external dgemm
!
!       ** Intrinsic Functions **
        intrinsic size
!
!       ** Local Scalars **
        integer(LONG) :: nrowA, ncolA
        integer(LONG) :: nrowB, ncolB
        integer(LONG) :: astat, error
!
!       ** Parameters **
        character(len = 1),  parameter :: TRANSA = 'N'
        character(len = 1),  parameter :: TRANSB = 'T'
        character(len = 16), parameter :: FNAME  = 'DMATXT_MATX_MULT'
!
!       Executable statements
        nrowA = size(A, 1); ncolA = size(A, 2)
        nrowB = size(B, 1); ncolB = size(B, 2)
        error = NO_ERROR
!
!       Check dimensions of the matrices
        if (nrowA <= 0 .or. ncolA <= 0) error = ERROR_DIM_1OP_ZERO
        if (nrowB <= 0 .or. ncolB <= 0) error = error + ERROR_DIM_2OP_ZERO
        if (ncolA /= ncolB)             error = error + ERROR_INCOMP_DIM

        if (error /= 0) go to 500

        allocate(C(nrowA,nrowB), stat = astat)
        if (astat /= 0) then
            error = ERROR_MEM_ALLOC
            go to 500
        end if

        call dgemm(TRANSA, TRANSB, nrowA, ncolB, nrowA, ONE, &
                    A, nrowA, B, nrowB, ZERO, C, nrowA)
        return

500     call math_error_info(FNAME, error)
    end function dmatx_matxT_mult


    function dmatxT_matxT_mult(A, B) result (C)
!========================================================================!
!   Multiply the transpose of the first matrix operand by the tranpose   !
!   of the  second  matrix  operand.  Note  that both  contains double   !
!   precision components                                                 !
!========================================================================!
!                                                                        !
!       ** Array arguments **                                            !
        real(DOUBLE), intent(in)  :: A(:,:), B(:,:)                      !
        real(DOUBLE), allocatable :: C(:,:)                              !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       A : fist operand of the matrix-matrix multiplication             !
!       B : second operand of the matrix-matrix multiplication           !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       C : resultant matrix from transpose(A)*traspose(B) operation     !
!                                                                        !
!       Furthe Details                                                   !
!       ==============                                                   !
!       created by Michael A. Maedo on June-05-2016                      !
!       updated by Michael A. Maedo on June-05-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: nrowA, ncolA
        integer(LONG) :: nrowB, ncolB
        integer(LONG) :: astat, error
!
!       ** Parameters **
        character(len = 1),  parameter :: TRANSA = 'T'
        character(len = 1),  parameter :: TRANSB = 'T'
        character(len = 17), parameter :: FNAME  = 'DMATXT_MATXT_MULT'
!
!       Executable statements
        nrowA = size(A, 1); ncolA = size(A, 2)
        nrowB = size(B, 1); ncolB = size(B, 2)
        error = NO_ERROR
!
!       Check dimensions of the matrices
        if (nrowA <= 0 .or. ncolA <= 0) error = ERROR_DIM_1OP_ZERO
        if (nrowB <= 0 .or. ncolB <= 0) error = error + ERROR_DIM_2OP_ZERO
        if (nrowA /= ncolB)             error = error + ERROR_INCOMP_DIM

        if (error /= 0) go to 600

        allocate(C(ncolA,nrowB), stat = astat)
        if (astat == 0) then
            error = ERROR_MEM_ALLOC
            go to 600
        end if

        call dgemm(TRANSA, TRANSB, nrowA, ncolB, nrowA, ONE, &
                   A, nrowA, B, nrowB, ZERO, C, nrowA)
        return

600     call math_error_info(FNAME, error)
    end function dmatxT_matxT_mult


end module matx_mult
