module graph_stru

    type vertex
        integer :: i_th
        integer :: deg
        type(vertex), pointer :: next
    end type vertex

    type adjacency
        type(vertex), pointer :: head
    end type adjacency

    type graph
        integer :: nvertx
        type(adjacency), pointer :: adj(:)
    end type graph

contains

    function new_vertx(i) result (v)

        integer :: i
        type(vertex), pointer :: v

        allocate(v)
        v % deg  = 0
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
        if (ans) then
            G%adj(u)%deg = G%adj(u)%deg + 1
        else
            v => new_vertx(w)
            v % next  => G % adj(u) % head
            G % adj(u) % head => v
        end if
!        new_node => new_adj_node(v)
!        new_node % next  => G % array(e) % head
!        G % array(e) % head => new_node

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

end module graph_stru
