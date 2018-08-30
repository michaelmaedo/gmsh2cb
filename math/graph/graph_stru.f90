module graph_stru
!
! .. All variables must be declared ..
    implicit none
! ..
! .. Private Procedures ..
    private :: merge_sort, merge
! ..
! .. Derived Data Types ..
    type vertex
        integer :: i_th
        type(vertex), pointer :: next
    end type vertex
! ..
! ..
    type adjacency
        type(vertex), pointer :: head
    end type adjacency
! ..

    type graph
        integer :: nvertx
        integer, pointer :: deg(:)
        type(adjacency), pointer :: adj(:)
    end type graph

contains

    function new_vertx(i) result (v)

        integer :: i
        type(vertex), pointer :: v

        allocate(v)
        v % i_th = i
        nullify(v % next)

    end function new_vertx

    function new_empty_graph(nvertx) result (G)

        integer :: nvertx
        type(graph), pointer :: G
        integer :: i

        allocate(G)

        G % nvertx = nvertx
        allocate(G % adj(G % nvertx))
        allocate(G % deg(G % nvertx))

        G % deg = 0
        do i = 1, nvertx
            nullify(G % adj(i) % head)
        end do

    end function new_empty_graph

!    function get_vertx(G, i) result (v)

!    end function get_vertx

    subroutine is_vertx_present(ans, pos, G, u, v)
        logical, intent(out) :: ans
        integer, intent(out) :: pos
        type(graph), pointer, intent(in) :: G
        integer, intent(in)  :: u, v

        type(vertex), pointer :: pCrawl

        ans = .false.
        pCrawl => G % adj(u) % head
        do while (associated(pCrawl))
            if (pCrawl % i_th == v) then
                ans = .true.
                pos = pCrawl % i_th
                exit
            end if
            pCrawl => pCrawl % next
        end do
    end subroutine is_vertx_present

    subroutine add_vertx(G, u, w)

        type(graph), intent(inout), pointer :: G
        integer,     intent(in)    :: u, w

        type(vertex), pointer :: v
        integer :: pos
        logical :: ans

        call is_vertx_present(ans, pos, G, u, w)
        if (.not.ans) then
            v => new_vertx(w)
            v % next  => G % adj(u) % head
            G % adj(u) % head => v
            G % deg(u) = G % deg(u) + 1

            v => new_vertx(u)
            v % next  => G % adj(w) % head
            G % adj(w) % head => v
            G % deg(w) = G % deg(w) + 1
        end if

    end subroutine add_vertx

    subroutine sort(G, u)
        type (graph), pointer, intent(inout) :: G
        integer, intent(in) :: u

        G % adj(u) % head => merge_sort(G % deg, G % adj(u) % head)
    end subroutine sort

    recursive function merge_sort(deg, head) result (new_head)

        integer,  pointer, intent(in) :: deg(:)
        type(vertex), pointer, intent(inout) :: head
        type(vertex), pointer :: new_head

        type(vertex), pointer :: left, right

        if (.not.associated(head) .or. &
            .not.associated(head % next)) then
            new_head => head
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

        left => merge_sort(deg, left)
        right => merge_sort(deg, right)

        new_head => merge(deg, left, right)

    end function merge_sort

    recursive function merge(deg, left, right) result (new_head)

        integer,  pointer, intent(in) :: deg(:)
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

        if (deg(left % i_th) < deg(right % i_th)) then
            new_head => left
            new_head % next => merge(deg, left % next, right)
        else
            new_head => right
            new_head % next => merge(deg, left, right % next)
        end if

        return

    end function

    function min_deg(G) result (v_min)
        type(graph), intent(in), pointer :: G
        type(vertex), pointer :: v_min

        type(vertex), pointer :: v
        integer :: min, ivertx, ndeg

        min = 10000
        v_min => new_vertx(min)
        do ivertx = 1, G % nvertx
            v => G % adj(ivertx) % head
            ndeg = 0
            do while (associated(v))
                ndeg = ndeg + 1
                v => v % next
            end do
            if (ndeg < min) then
                min   = ndeg
                v_min % i_th = ivertx
            end if
        end do

    end function

    subroutine print_graph(out, G)

        type(graph), intent(in), pointer :: G
        integer, intent(in) :: out

        type(vertex), pointer :: u
        integer :: v

        do v = 1, G % nvertx
           u => G % adj(v) % head
           write(out,'(A,I10)'), 'Adjacency list of vertex ', v
           write(out,'(A)',advance='no') '    head'
           do while (associated(u))
              write(out,'(A,I10,A)',advance='no') ' -> ', u % i_th, ' '
              u => u % next
           end do
           write(out,*)
           call flush(out)
        end do

    end subroutine print_graph

    subroutine print_deg(G)

        type(graph), intent(in), pointer :: G
        type(vertex), pointer :: pCrawl
        integer :: v

        print *, ''
        do v = 1, G % nvertx
            pCrawl => G % adj(v) % head
            write(*,'(A,I2)'), 'Adjacency list of vertex ', v
            write(*,'(A)',advance='no') '    Degree'
            do while (associated(pCrawl))
                write(*,'(A,I1,A)',advance='no') ' -> ', G % deg(pCrawl % i_th), ' '
                pCrawl => pCrawl % next
            end do
            print *,
        end do

    end subroutine print_deg

end module graph_stru
