module cuthill_mckee
!
! .. Required Modules ..
    use graph_stru
    use queue_type
!
! .. All variables must be declared ..
    implicit none
!
! .. Private Procedures ..

contains

    subroutine sym_rev_cuthl_mcke(G, root, v)
        type(graph),  pointer, intent(inout) :: G
        type(vertex), pointer, intent(in) :: root
        integer, intent(out),  allocatable :: v(:)

        type(vertex), pointer :: v_i, w, u
        type(queue), pointer :: I
        integer :: ii, jj, n
        logical, pointer :: visited(:)

        n = G % nvertx
        allocate(v(n)); v = 0
        allocate(visited(n))
        allocate(v_i)
        nullify(v_i % next)
        allocate(u)
        nullify(u % next)

        do ii = 1, n
            visited(ii) = .false.
        end do

        !create empty queue I
        call new_queue(I)

        !mark root vertex as visited and enqueue it
        visited(root % i_th) = .true.
        call enqueue(root, I)

        v(root % i_th) = n

        do while (.not.is_queue_empty(I))

            !get first vertex of I
            v_i = dequeue(I)

            !Put vertices of Adj(v_i) in order of increasing degree
            call sort(G, v_i % i_th)

            w => G % adj(v_i % i_th) % head
            do while (associated(w))
                if (.not.visited(w % i_th)) then

                    visited(w % i_th) = .true.

!                    u % i_th = w % i_th

                    !add w to the end of I
                    call enqueue(w, I)

                    n = n - 1
                    v(w % i_th) = n
                end if
                w => w % next
            end do
        end do
    end subroutine sym_rev_cuthl_mcke

end module cuthill_mckee
