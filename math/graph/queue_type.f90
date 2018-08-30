module queue_type

    use graph_stru

    implicit none

    type queue
        private
        integer :: size
        type(vertex), pointer :: head, tail
    end type queue

contains

    subroutine new_queue(q)
        type(queue), pointer, intent(inout) :: q
        allocate(q)
        allocate(q % head)
        q % size = 0
        q % tail => q % head
        nullify(q % tail % next)
    end subroutine new_queue

    function is_queue_empty(q) result (ans)
        type(queue), pointer :: q
        logical :: ans

        ans = .false.
        if (q % size == 0) then
            ans = .true.
        end if
    end function is_queue_empty

!        function is_queue_full

    subroutine enqueue(x, q)
        type(queue) , pointer :: q
        type(vertex),  pointer :: x

        if (is_queue_empty(q)) then
            q % head = x
            q % tail = x
            q % size = 1
        else
            allocate(q % tail % next)
            q % tail => q % tail % next
            q % tail = x
            nullify(q % tail % next)
            q % size = q % size + 1
        end if
    end subroutine enqueue

    function dequeue(q) result (x)
        type(queue),  pointer :: q
        type(vertex), pointer :: x
        type(vertex), pointer :: w

        allocate(x)
        nullify(x % next)
        if (is_queue_empty(q)) then
            print *, "Error: queue is empty"
            x % i_th = 0
            return
        end if

        w => q % head
        x % i_th = w % i_th
        q % head => q % head % next
        q % size = q % size - 1
        deallocate(w)
        if (is_queue_empty(q)) then
            allocate(q % head)
            q % tail => q % head
            nullify(q % tail % next)
        end if
    end function dequeue

    subroutine print_queue(q)
        type(queue), pointer :: q

        type(vertex), pointer :: pCrawl

        pCrawl => q % head
        write(*,fmt='(I5)',advance='no') pCrawl % i_th
        do while (associated(pCrawl % next))
            pCrawl => pCrawl % next
            write(*,fmt='(A,I5)',advance='no') " -> ", pCrawl % i_th
        end do
        print *

    end subroutine print_queue

end module queue_type
