recursive subroutine quicksort(A, left, right)

   use part_mod
!
!  .. Array Arguments ..
   integer, intent(inout) :: A(:)
!  ..
!  .. Scalar Arguments ..
   integer, intent(in) :: left, right
!  ..
!  .. Local Scalars ..
   integer :: j

   if (left < right) then
      call partition(A, left, right, j)
      call quicksort(A, left, j)
      call quicksort(A, j + 1, right)
   end if

end subroutine quicksort
