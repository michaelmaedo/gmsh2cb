subroutine partition(A, l, r, j)

!  ..
!  .. Scalar Arguments ..
   integer, intent(in) :: l, r
!  ..
!  .. Result ..
   integer, intent(out) :: j
!  ..
!  .. Array Arguments ..
   integer, intent(inout) :: A(:)
!  ..
!  .. Local Scalar ..
   integer :: pivot, i, t

   pivot = A(l)
   i = l;  j = r

   do
      do while (A(i) < pivot)
         i = i + 1
      end do

      do while (A(j) > pivot)
         j = j - 1
      end do

      if (i >= j) exit
      t = A(i)
      A(i) = A(j)
      A(j) = t
   end do

end subroutine partition
