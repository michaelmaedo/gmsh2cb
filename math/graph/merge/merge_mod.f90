module merge_mod

    type vertex
        integer :: i_th
        type(vertex), pointer :: next
    end type vertex

contains

    recursive function merge_sort(head) result (new_head)

        type(vertex), pointer, intent(inout) :: head
        type(vertex), pointer :: new_head

        type(vertex), pointer :: left, right

        if (.not.associated(head) .or. &
            .not.associated(head % next)) then
            new_head = head
            return
        end if

        left => head
        right => head % next

        do while (associated(right) .and. associated(right % next))
            head => head % next
            right => head % next % next
        end do
        right => head % next
        nullify(head % next)

        left => merge_sort(left)
        right => merge_sort(right)

        new_head => merge(left, right)

    end function merge_sort

    recursive function merge(left, right) result (new_head)

        type(vertex), pointer, intent(in) :: left, right
        type(vertex), pointer :: new_head

        if (.not.associated(left)) then
            new_head => right
            return
        end if

        if (.not.associated(right)) then
            new_head => left
            return
        end if

        if (left%i_th < right%i_th) then
            new_head => left
            new_head % next => merge(left % next, right)
        else
            new_head => right
            new_head % next => merge(left, right % next)
        end if

        return

    end function

end module merge_mod
