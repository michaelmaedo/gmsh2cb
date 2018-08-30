module operat_dmatx_3x3
!========================================================================!
!                    Contain operations of 3x3 matrices                  !
!========================================================================!
!                                                                        !
!   ** Modules **                                                        !
    use precision, only: DOUBLE, LONG                                    !
    use real_cons, only: ZERO, ONE, ONE_HALF                             !
!                                                                        !
!   ** All variables must be declared **                                 !
    implicit none                                                        !
!                                                                        !
!   ** Intrinsic functions **                                            !
    intrinsic reshape, shape, transpose                                  !
!                                                                        !
!   ** Parameters **                                                     !
!                                                                        !
!           [ 1  0  0 ]                                                  !
!   Imatx = [ 0  1  0 ]                                                  !
!           [ 0  0  1 ]                                                  !
!                                                                        !
    real(DOUBLE), parameter :: Imatx(3,3) = reshape((/ONE , ZERO, ZERO, &!
                                        &             ZERO, ONE , ZERO, &!
                                        &             ZERO, ZERO, ONE/),&!
                                        &   shape(Imatx))                !
!                                                                        !
!   Overload .xx. to compute the Frobenius inner product                 !
!   ====================================================                 !
    interface operator (.xx.)                                            !
        module procedure frobenius                                       !
    end interface operator (.xx.)                                        !
!                                                                        !
!   Functions and Subroutines                                            !
!   =========================                                            !
!   det  : compute the determinant of a 3x3 matrix                       !
!   inv  : return inverse of a 3x3 matrix                                !
!   tr   : compute the trace of a 3x3 matrix                             !
!   skew : return the skew part of a 3x3 matrix                          !
!   sym  : return the symmetric part of a 3x3 matrix                     !
!   .xx. : compute the Frobenius inner product of two 3x3 matrices       !
!                                                                        !
!   Further Details                                                      !
!   ===============                                                      !
!   created by Michael A. Maedo on July-23-2016                          !
!   updated by Michael A. Maedo on July-23-2016                          !
!                                                                        !
!========================================================================!

contains

    function det(A) result(d)
!========================================================================!
!                 Compute the determinant of a 3x3 matrix                !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        real(DOUBLE) :: d                                                !
!                                                                        !
!       ** Array Arguments **                                            !
        real(DOUBLE), intent(in) :: A(3,3)                               !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       A : 3x3 matrix containing double precision components            !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       d : determinant of A                                             !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-23-2016                      !
!       updated by Michael A. Maedo on July-23-2016                      !
!                                                                        !
!========================================================================!
!
!       Executable statements
        d = ZERO
        d = (A(1,1)*A(2,2)*A(3,3)  +  A(3,2)*A(2,1)*A(1,3) &
        &  + A(3,1)*A(1,2)*A(2,3)) - (A(1,3)*A(2,2)*A(3,1) &
        &  + A(3,2)*A(2,3)*A(1,1)  +  A(3,3)*A(1,2)*A(2,1))

    end function det


    function inv(A) result(inv_A)
!========================================================================!
!                       Return inverse of a 3x3 matrix                   !
!========================================================================!
!                                                                        !
!       ** Array Arguments **                                            !
        real(DOUBLE), intent(in) :: A(3,3)!                              !
        real(DOUBLE) :: inv_A(3,3)                                       !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       A     : 3x3 matrix containing double precision components        !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       inv_A : inverse of matrix A                                      !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-23-2016                      !
!       updated by Michael A. Maedo on July-23-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Arrays **
        real(DOUBLE) :: C(3,3)
!
!       Executable statements
        inv_A = ZERO; C = ZERO
        C(1,1) = A(2,2)*A(3,3) - A(3,2)*A(2,3)
        C(1,2) = A(1,3)*A(3,2) - A(3,3)*A(1,2)
        C(1,3) = A(1,2)*A(2,3) - A(2,2)*A(1,3)
        C(2,1) = A(2,3)*A(3,1) - A(3,3)*A(2,1)
        C(2,2) = A(1,1)*A(3,3) - A(3,1)*A(1,3)
        C(2,3) = A(1,3)*A(2,1) - A(2,3)*A(1,1)
        C(3,1) = A(2,1)*A(3,2) - A(3,1)*A(2,2)
        C(3,2) = A(1,2)*A(3,1) - A(3,2)*A(1,1)
        C(3,3) = A(1,1)*A(2,2) - A(2,1)*A(1,2)
        inv_A = C/det(A)

    end function inv


    function tr(A) result(trace)
!========================================================================!
!                     Compute the trace of a 3x3 matrix                  !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        real(DOUBLE) :: trace                                            !
!                                                                        !
!       ** Array Arguments **                                            !
        real(DOUBLE), intent(in) :: A(3,3)                               !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       A     : 3x3 matrix containing double precision components        !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       trace : trace of A                                               !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-23-2016                      !
!       updated by Michael A. Maedo on July-23-2016                      !
!                                                                        !
!========================================================================!
!
!       Executable statements
        trace = ZERO
        trace = A(1,1) + A(2,2) + A(3,3)

    end function tr


    function skew(A) result(skew_A)
!========================================================================!
!              Return the skews-symmetric part of a 3x3 matrix           !
!========================================================================!
!                                                                        !
!       ** Array Arguments **                                            !
        real(DOUBLE), intent(in) :: A(3,3)                               !
        real(DOUBLE) :: skew_A(3,3)                                      !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       A      : 3x3 matrix containing double precision components       !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       skew_A : skew-symmetric part of A                                !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-23-2016                      !
!       updated by Michael A. Maedo on July-23-2016                      !
!                                                                        !
!========================================================================!
!
!       Executable statements
        skew_A = ZERO
        skew_A = ONE_HALF*(A - transpose(A))

    end function skew


    function sym(A) result(sym_A)
!========================================================================!
!                Return the symmetric part of a 3x3 matrix               !
!========================================================================!
!                                                                        !
!       ** Array Arguments **                                            !
        real(DOUBLE), intent(in) :: A(3,3)                               !
        real(DOUBLE) :: sym_A(3,3)                                       !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       A     : 3x3 matrix containing double precision components        !
!                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       sym_A : symmetric part of A                                      !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-23-2016                      !
!       updated by Michael A. Maedo on July-23-2016                      !
!                                                                        !
!========================================================================!
!
!       Executable statements
        sym_A = ZERO
        sym_A = ONE_HALF*(A + transpose(A))

    end function sym


    function frobenius(A, B) result(x)
!========================================================================!
!        Compute the Frobenius inner product of two 3x3 matrices         !
!========================================================================!
!                                                                        !
!       ** Scalar Arguments **                                           !
        real(DOUBLE) :: x!                                               !
!                                                                        !
!       ** Array Arguments **                                            !
        real(DOUBLE), intent(in) :: A(3,3), B(3,3)                       !
!                                                                        !
!       Input Arguments                                                  !
!       ===============                                                  !
!       A : first matrix operand of the frobenius inner product          !
!       B : second  matrix operand of the frobenius inner product        !
!                                                                        !                                                                        !
!       Output Arguments                                                 !
!       ================                                                 !
!       x : it is the result of the double dot product (i.e. A:B)        !
!                                                                        !
!       Further Details                                                  !
!       ===============                                                  !
!       created by Michael A. Maedo on July-23-2016                      !
!       updated by Michael A. Maedo on July-23-2016                      !
!                                                                        !
!========================================================================!
!
!       ** Local Scalars **
        integer(LONG) :: i, j
!
!       Executable statements
        x = ZERO
        do i = 1, 3
            do j = 1 , 3
                x = A(i,j)*B(i,j)
            end do
        end do

    end function frobenius

end module operat_dmatx_3x3
