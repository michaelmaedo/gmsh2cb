module qsort_mod

   interface
      recursive subroutine quicksort(A, left, right)
         use part_mod
         integer, intent(inout) :: A(:)
         integer, intent(in) :: left, right
      end subroutine quicksort
   end interface

end module qsort_mod
