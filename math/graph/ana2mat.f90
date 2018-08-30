program ana2mat

    use graph_stru
    use queue_type
    use cuthill_mckee

    implicit none

    type(graph) , pointer :: G
    type(queue) , pointer :: q
    type(vertex), pointer :: root
    integer     , pointer :: connec(:,:)
    integer     , allocatable :: V(:)
    integer :: nnode, nelem, nnope
    integer :: ielem, inope, iset
    integer :: inode, idelm
    integer :: ii, jj

    integer, parameter :: un_inp = 10
    integer, parameter :: un_out = 20
    integer, parameter :: checkf = 30

    open(un_inp, file = 'inp.dat', status = 'unknown')
    open(un_out, file = 'p_vec.m', status = 'unknown')
    open(checkf, file = 'out.dat', status = 'unknown')

    read(un_inp,*) nnode, nelem, nnope
    allocate(connec(nnope + 2,nelem))
    allocate(V(nnode))
    G => new_empty_graph(nnode)

    do ielem = 1, nelem
        read(un_inp,*) idelm, iset, (connec(inope,ielem), inope = 1, nnope)

        do ii = 1, nnope - 1
            do jj = ii + 1, nnope
                call add_vertx(G, connec(ii,ielem), connec(jj,ielem))
            end do
        end do
    end do

    call print_graph(checkf,G)
    root => min_deg(G)

    call sym_rev_cuthl_mcke(G, root, V)

    write(un_out,*) 'p = ['
    do inode = 1, nnode
        write(un_out,'(I15,A)') V(inode),';'
    end do
    write(un_out,*) '];'
end program ana2mat
