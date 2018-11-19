module frag_mod

contains

    subroutine frag()

        !.. Local Scalars ..
        integer :: nnode0, nelem0, nnel
        integer :: iel, ino
        integer :: i, j, k


        !.. local Arrays ..
        integer, allocatable :: bar_nodes(:)

        allocate( bar(nnode) )
        
        do iel = 1, nelem
            if (iel == mat_frag) then
                nnode = nnode + nnel
                do ino = 1, nnel
                    bar_nodes(ino) = 
                end do
            else
                do ino = 1, nnel
                    bar_nodes(ino) = -1
                end do
            end if
        end do


        do iel = 1, nelem
            
            
        end do
        
    end subroutine frag
    
end module frag_mod
