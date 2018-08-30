module part_mod

      interface
         subroutine partition(A, l, r, j)
            integer, intent(in) :: l, r
            integer, intent(out) :: j
            integer, intent(inout) :: A(:)
         end subroutine partition
      end interface

end module part_mod
