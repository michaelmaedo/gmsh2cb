module queue_type

    type node
        integer :: data
        type(node), pointer :: next
    end type node

    type queue
        private
        integer :: size
        type(node), pointer :: head, tail
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
            type(queue), pointer :: q
            type(node),  pointer :: x

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
            type(queue), pointer :: q
            type(node), pointer :: x
            type(node), pointer :: pCrawl

            allocate(x)
            nullify(x % next)
            if (is_queue_empty(q)) then
                print *, "Error: queue is empty"
                x % data = 0
                return
            end if

            pCrawl => q % head
            x % data = pCrawl % data
            q % head => q % head % next
            q % size = q % size - 1
            deallocate(pCrawl)
            if (is_queue_empty(q)) then
                allocate(q % head)
                q % tail => q % head
                nullify(q % tail % next)
            end if
        end function dequeue

        subroutine print_queue(q)
            type(queue), pointer :: q

            type(node), pointer :: pCrawl

            pCrawl => q % head
            write(*,fmt='(I5)',advance='no') pCrawl % data
            do while (associated(pCrawl))
                pCrawl => pCrawl % next
                write(*,fmt='(A,I5)',advance='no') " -> ", pCrawl % data
            end do
            print *

        end subroutine print_queue

end module queue_type
