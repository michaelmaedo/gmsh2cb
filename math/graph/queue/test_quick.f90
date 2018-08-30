program test_quick

   use qsort_mod

   integer :: A(9) = (/7, 12, 1, -2, 0, 15, 4, 11, 9/)
   integer :: i

   print *
   write(*,fmt='(A)',advance='no'), 'Initial array is: '
   do i = 1, 9
      write(*,fmt='(I5)',advance='no') A(i)
   end do
   print *
   print *

   call quicksort(A, 1, 9)
   write(*,fmt='(A)',advance='no'), 'Sorted array is: '
   do i = 1, 9
      write(*,fmt='(I5)',advance='no') A(i)
   end do
   print *
   print *

end program test_quick
