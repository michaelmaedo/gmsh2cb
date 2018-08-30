module graph_stru

    type vertex
        integer :: i_th
        type(vertex), pointer :: next
    end type vertex

    type adjacency
        type(vertex), pointer :: head
    end type adjacency

    type graph
        integer :: nvertx
        integer, pointer :: deg(:)
        type(adjacency), pointer :: adj(:)
    end type graph

contains

    function new_vertx(i, G) result (v)

        integer :: i
        type(vertex), pointer :: v
        type(graph), pointer :: G

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
            v => new_vertx(w, G)
            v % next  => G % adj(u) % head
            G % adj(u) % head => v
            G % deg(u) = G % deg(u) + 1

            v => new_vertx(u, G)
            v % next  => G % adj(w) % head
            G % adj(w) % head => v
            G % deg(w) = G % deg(w) + 1
        end if

    end subroutine add_vertx

    subroutine print_graph(G)

        type(graph), intent(in), pointer :: G
        type(vertex), pointer :: pCrawl
        integer :: v

        do v = 1, G % nvertx
            pCrawl => G % adj(v) % head
            write(*,'(A,I2)'), 'Adjacency list of vertex ', v
            write(*,'(A)',advance='no') '    head'
            do while (associated(pCrawl))
                write(*,'(A,I1,A)',advance='no') ' -> ', pCrawl % i_th, ' '
                pCrawl => pCrawl % next
            end do
            print *,
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
