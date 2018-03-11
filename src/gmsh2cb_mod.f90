module gmsh2cb_mod

    use precision_mod, only: LONG, DOUBLE
    use string_mod, only: LEN_STR, strcmp
    
    use file_mod, only: &
        & data_file, &
        & get_record

    use error_mod, only: &
        & error, &
        & error_, &
        & there_is_any_error, &
        & get_error_status, &
        & print_error

    use error_gmsh2cb_mod
    
#include "utilities/error.fpp"

#define TEST( err ) if ( there_is_any_error( err ) ) return
    
    implicit none

    integer(LONG), parameter :: MAX_NODES = 8
    
    type mesh
        integer(LONG) :: nnode !number of nodes
        integer(LONG) :: nelem !number of elements
        integer(LONG) :: ndime !number of spatial dimensions
        integer(LONG), allocatable :: elset(:) !element sets
        integer(LONG), allocatable :: eltype(:) !element type
        integer(LONG), allocatable :: connec(:,:) !connectivity list
        integer(LONG), allocatable :: bcond(:,:) !boundary conditions
        real(double), allocatable :: coord(:,:) !nodal coordinates
    end type mesh

    type physical
        integer(LONG) :: n
        integer(LONG) :: nsets
        integer(LONG), allocatable :: ndime(:)
        integer(LONG), allocatable :: set(:)
        character(len = LEN_STR), allocatable :: name(:)
    end type physical
    
contains
    
    subroutine gmsh2cb(msh, gen, gri, log, err)
    
        type(data_file), intent(inout) :: msh
        type(data_file), intent(inout) :: gri
        type(data_file), intent(inout) :: gen
        type(data_file), intent(inout) :: log
        type(error), intent(inout) :: err

        type(mesh) :: gmsh
        type(mesh) :: cbmesh
        type(physical) :: matprop
        type(physical) :: mech_bc
        type(physical) :: flux_bc

        integer(LONG) :: new_nelem
        integer(LONG), allocatable :: bcond(:,:)
        
        call read_gmsh(msh, gmsh, matprop, mech_bc, flux_bc, new_nelem, bcond, err)

        call create_cbmesh(gmsh, new_nelem, bcond, cbmesh, err)

        call write_grid( cbmesh, gri, err )

        call write_gen( cbmesh, matprop, mech_bc, flux_bc ,gen, err )
        
    end subroutine gmsh2cb

    
    subroutine read_gmsh(msh, gmsh, matprop, mech_bc, flux_bc, new_nelem, bcond, err)
        type(data_file), intent(inout) :: msh
        type(mesh), intent(inout) :: gmsh
        type(physical), intent(inout) :: matprop
        type(physical), intent(inout) :: mech_bc
        type(physical), intent(inout) :: flux_bc
        type(error), intent(inout) :: err
        integer(LONG), intent(out) :: new_nelem
        integer(LONG), allocatable, intent(out) :: bcond(:,:)


        character(len = LEN_STR) :: errorMessage

        call read_version(msh, err)
        EXCEPT('GET_RECORD', ERROR_GET_RECORD, err )

        errorMessage = 'Keyword $MeshFormat was not found' 
        EXCEPT( errorMessage, ERROR_MESH_FORMAT, err )

        errorMessage = 'Keyword $EndMeshFormat was not found' 
        EXCEPT( errorMessage, ERROR_END_MESH_FORMAT, err )

        call read_physical(msh, gmsh, matprop, mech_bc, flux_bc, err)
        
        call read_nodes(msh, gmsh, bcond, err)

        call read_elements(matprop, flux_bc, mech_bc, msh, gmsh, bcond, new_nelem, err)

       
    end subroutine read_gmsh


    subroutine read_version(msh, err)

        type(data_file), intent(inout) :: msh
        type(error), intent(inout) :: err

        integer(LONG) :: info
        integer(LONG) :: ntxt, nnum
        real(DOUBLE) :: num(3)
        character(len = LEN_STR) :: txt(1)

        call get_record(msh, txt, ntxt, num, nnum, err)
        TEST( err )
        
        info = strcmp('$MeshFormat', txt(1) )
        if ( there_is_any_error( info) ) then
            RAISE( ERROR_MESH_FORMAT, err )
        end if

        call get_record(msh, txt, ntxt, num, nnum, err)
        TEST( err )
        
        call get_record(msh, txt, ntxt, num, nnum, err)
        TEST( err )
        
        info = strcmp('$EndMeshFormat', txt(1) )
        if ( there_is_any_error( info) ) then
            RAISE( ERROR_END_MESH_FORMAT, err )
        end if
        
    end subroutine read_version

   
    subroutine read_physical(msh, gmsh, matprop, mech_bc, flux_bc, err)
        type(data_file), intent(inout) :: msh
        type(mesh), intent(inout) :: gmsh
        type(error), intent(inout) :: err
        type(physical), intent(out) :: matprop
        type(physical), intent(out) :: mech_bc
        type(physical), intent(out) :: flux_bc

        integer(LONG) :: info
        integer(LONG) :: ntxt, nnum
        integer(LONG) :: int_num(2)
        real(DOUBLE) :: num(2)
        character(len = LEN_STR) :: txt(1)

        character(len = LEN_STR) :: loc_name
        character(len = 4) :: label
        integer(LONG) :: i, n
        integer(LONG) :: loc_ndime
        integer(LONG) :: loc_set
        integer(LONG) :: beginning_label
        integer(LONG) :: end_label
        
        call get_record(msh, txt, ntxt, num, nnum, err)
        TEST( err )
        
        info = strcmp('$PhysicalNames', txt(1) )
        if ( there_is_any_error( info ) ) then
            RAISE( ERROR_PHYSICAL_NAMES, err )
        end if

        call get_record(msh, txt, ntxt, num, nnum, err)
        TEST( err )
        n = int( num(1) )

        call malloc_matprop_and_bc( n, matprop, mech_bc, flux_bc, err )
        
        gmsh % ndime = 0
        do i = 1, n
            call get_record(msh, txt, ntxt, num, nnum, err)
            TEST( err )
            loc_ndime = int( num(1) )
            loc_set = int( num(2) )
            loc_name = trim( txt(1) )
            
            end_label = len_trim( loc_name )
            beginning_label = end_label - 2
            label = loc_name( beginning_label : end_label )
            
            if ( it_is_bc( label, 'MBC') ) then
                call set_physical( loc_ndime, loc_set, loc_name, mech_bc, err )
            else if ( it_is_bc( label, 'FBC')  ) then
                call set_physical( loc_ndime, loc_set, loc_name, flux_bc, err )
            else
                call set_physical( loc_ndime, loc_set, loc_name, matprop, err )
            end if

            if ( loc_ndime > gmsh % ndime ) gmsh % ndime = loc_ndime
            
        end do
            
        call get_record(msh, txt, ntxt, num, nnum, err)
        TEST( err )
        
        info = strcmp('$EndPhysicalNames', txt(1) )
        if ( there_is_any_error( info) ) then
            RAISE( ERROR_END_PHYSICAL_NAMES, err )
        end if

    end subroutine read_physical
    

    subroutine read_nodes(msh, gmsh, bcond, err)
        type(data_file), intent(inout) :: msh
        type(mesh), intent(inout) :: gmsh
        type(error), intent(inout) :: err
        integer(LONG), allocatable, intent(inout) :: bcond(:,:)

        integer(LONG) :: info
        integer(LONG) :: ntxt, nnum
        integer(LONG) :: int_num(1)
        real(DOUBLE) :: num(4)
        character(len = LEN_STR) :: txt(1)

        integer(LONG) :: inode, nnode, ndime
        integer(LONG) :: first_dime, last_dime
        
        call get_record(msh, txt, ntxt, num, nnum, err)
        TEST( err )
        
        info = strcmp('$Nodes', txt(1) )
        if ( there_is_any_error( info ) ) then
            RAISE( ERROR_PHYSICAL_NAMES, err )
        end if

        call get_record(msh, txt, ntxt, num, nnum, err)
        TEST( err )
        nnode = int( num(1) )
        ndime = gmsh % ndime
        
        gmsh % nnode = nnode
        allocate( gmsh % coord( ndime, nnode ), stat = info )
        allocate( bcond( 2, nnode ), stat = info )
        bcond = 0
        
        first_dime = 2
        last_dime = ndime + 1
        do inode = 1, nnode
            call get_record(msh, txt, ntxt, num, nnum, err)

            gmsh % coord( :, inode ) = num( first_dime : last_dime )
        end do

        call get_record(msh, txt, ntxt, num, nnum, err)
        TEST( err )

        info = strcmp('$EndNodes', txt(1) )
        if ( there_is_any_error( info) ) then
            RAISE( ERROR_END_PHYSICAL_NAMES, err )
        end if
        
    end subroutine read_nodes


    subroutine read_elements(matprop, flux_bc, mech_bc, msh, gmsh, bcond, new_nelem, err)
        type(physical), intent(in) :: matprop
        type(physical), intent(in) :: mech_bc
        type(physical), intent(in) :: flux_bc
        type(data_file), intent(inout) :: msh
        type(mesh), intent(inout) :: gmsh
        type(error), intent(inout) :: err
        integer(LONG), intent(inout), allocatable :: bcond(:,:)
        integer(LONG), intent(out) :: new_nelem
        
        integer(LONG) :: info
        integer(LONG) :: ntxt, nnum
        integer(LONG) :: int_num(20)
        real(DOUBLE) :: num(20)
        character(len = LEN_STR) :: txt(1)
        
        integer(LONG) :: ielem, nelem
        integer(LONG) :: inode, elnod
        integer(LONG) :: first_node, last_node
        integer(LONG) :: ntags
        integer(LONG) :: loc_eltype, loc_elset


        call get_record(msh, txt, ntxt, num, nnum, err)
        TEST( err )

        info = strcmp('$Elements', txt(1) )
        if ( there_is_any_error( info ) ) then
            RAISE( ERROR_PHYSICAL_NAMES, err )
        end if
        
        call get_record(msh, txt, ntxt, num, nnum, err)
        TEST( err )
        nelem = int( num(1) )
        
        gmsh % nelem = nelem
        allocate( gmsh % connec( MAX_NODES, nelem ), stat = info )
        allocate( gmsh % elset( nelem ), stat = info )
        allocate( gmsh % eltype( nelem ), stat = info )

        new_nelem = 0
        gmsh % connec = 0
        do ielem = 1, nelem
            call get_record(msh, txt, ntxt, num, nnum, err)

            loc_eltype = int( num(2) )
            ntags = int( num(3) )
            loc_elset = int( num(4) )
            elnod = nnum - (ntags + 3)

            first_node = ntags + 4
            last_node = nnum
            
            gmsh % connec( 1 : elnod, ielem ) = int( num( first_node : last_node ) )          

            if ( mech_bc % set( loc_elset )  /= 0 ) then
                do inode = 1, elnod
                    bcond( 1,  gmsh % connec( inode, ielem ) ) = mech_bc % set( loc_elset )
                end do
            else if ( flux_bc % set( loc_elset )  /= 0 ) then
                do inode = 1, elnod
                    bcond( 2,  gmsh % connec( inode, ielem ) ) = flux_bc % set( loc_elset )
                end do
            else
                new_nelem = new_nelem + 1
                gmsh % elset( new_nelem ) = matprop % set( loc_elset )
                gmsh % connec( 1 : elnod, new_nelem ) = gmsh % connec( 1 : elnod, ielem )
                call set_element_type(loc_eltype, gmsh % eltype( new_nelem ), err )
            end if
            
        end do

        call get_record(msh, txt, ntxt, num, nnum, err)
        TEST( err )
        
        info = strcmp('$EndElements', txt(1) )
        if ( there_is_any_error( info) ) then
            RAISE( ERROR_END_PHYSICAL_NAMES, err )
        end if
        
    end subroutine read_elements

    
    subroutine set_physical( ndime, set, name, physic, err )
        integer(LONG), intent(in) :: ndime
        integer(LONG), intent(in) :: set
        character(len = *), intent(in) :: name
        type(physical), intent(inout) :: physic
        type(error), intent(inout) :: err

        integer(LONG) :: i
        
        i = physic % nsets + 1
        physic % nsets = i
        physic % ndime( i ) = ndime
        physic % set( set ) = i
        physic % name( i ) = name                
    end subroutine set_physical

    
    function it_is_bc( label, bc ) result ( ans )
        character(len = * ), intent( in ) :: label
        character(len = * ), intent( in ) :: bc

        logical :: ans

        ans = .false.
        if (label == bc) ans = .true.
    end function it_is_bc
    
    
    subroutine malloc_matprop_and_bc( n, matprop, mech_bc, flux_bc , err )
        integer(LONG), intent(in) :: n
        type(physical), intent(out) :: matprop
        type(physical), intent(out) :: mech_bc
        type(physical), intent(out) :: flux_bc
        type(error), intent(inout) :: err

        call malloc_physical( n, matprop, err )
        call malloc_physical( n, mech_bc, err )
        call malloc_physical( n, flux_bc, err )

    end subroutine malloc_matprop_and_bc


    subroutine malloc_physical( n, physic, err )
        integer(LONG), intent(in) :: n
        type(physical), intent(inout) :: physic
        type(error), intent(inout) :: err

        integer(LONG) :: info

        physic % n = n
        physic % nsets = 0
        
        allocate( physic % ndime( n ), stat = info )
        physic % ndime = 0
        
        allocate( physic % set( n ), stat = info )
        physic % set = 0

        allocate( physic % name( n ), stat = info )
        physic % name = ''
        
    end subroutine malloc_physical


    subroutine set_element_type( gmsh_eltype, cb_eltype, err )
        integer(LONG), intent(in) :: gmsh_eltype
        type(error), intent(inout) :: err
        integer(LONG), intent(out) :: cb_eltype

        select case(gmsh_eltype)
        case (1)
            cb_eltype = 8
        case (2)
            cb_eltype = 1
        case (3)
            cb_eltype = 5
        case (4)
            cb_eltype = 1
        case (5)
            cb_eltype = 3
        case (6)
            cb_eltype = 26
        case (9)
            cb_eltype = 12
        case default
            RAISE( ERROR_INVALID_ELEMENT, err )
        end select
    end subroutine set_element_type
    
    
    subroutine create_cbmesh(gmsh, new_nelem, bcond, cbmesh, err)
    
        type(mesh), intent(in) :: gmsh
        integer(LONG), intent(in) :: new_nelem
        integer(LONG), allocatable, intent(in) :: bcond(:,:)
        type(error), intent(inout) :: err
        type(mesh), intent(out) :: cbmesh

        integer(LONG) :: inode, info
        integer(LONG) :: ielem
        
        cbmesh % nnode = gmsh % nnode
        cbmesh % nelem = new_nelem
        cbmesh % ndime = gmsh % ndime

        allocate( cbmesh % coord( gmsh % ndime, gmsh % nnode ) , stat = info )
        allocate( cbmesh % bcond( 2, gmsh % nnode ) , stat = info )
        allocate( cbmesh % connec( MAX_NODES, new_nelem ) , stat = info )
        allocate( cbmesh % elset( new_nelem ) , stat = info )
        allocate( cbmesh % eltype( new_nelem ) , stat = info )

        cbmesh % coord = gmsh % coord
        cbmesh % bcond = bcond
 
        cbmesh % connec = gmsh % connec( :, 1 : new_nelem )
        cbmesh % eltype = gmsh % eltype( 1 : new_nelem )
        cbmesh % elset = gmsh % elset( 1 : new_nelem )
        
    end subroutine create_cbmesh

    
    subroutine write_grid( cbmesh, gri, err )
        type(mesh), intent(in) :: cbmesh
        type(data_file), intent(in) :: gri
        type(error), intent(inout) :: err

        integer(LONG) :: icond, ncond
        integer(LONG) :: idime, ndime
        integer(LONG) :: inode, nnode
        integer(LONG) :: ielem, nelem

        character( len = 20 ) :: ifmt
        
        write( gri % ID, fmt = * ) 0, 0
        
        ncond = 2
        ndime = cbmesh % ndime
        nnode = cbmesh % nnode

        ifmt = ''
        if ( ndime == 2 ) then
            ifmt = '(i10,2e20.13,2i10)'
        else if ( ndime == 3 ) then
            ifmt = '(i10,3e20.13,2i10)'
        end if
        
        do inode = 1, nnode
            write( gri % ID, ifmt) inode, &
                & ( cbmesh % coord( idime, inode ), idime = 1, ndime ), &
                & ( cbmesh % bcond( icond, inode ), icond = 1, 2 )
        end do

        nelem = cbmesh % nelem
        do ielem = 1, nelem
            write( gri % ID, 200) ielem, &
                & cbmesh % elset( ielem ), &
                & cbmesh % eltype( ielem ), &
                & ( cbmesh % connec( inode, ielem ), inode = 1, MAX_NODES )
        end do

200     format(11i10)
        
    end subroutine write_grid


    subroutine write_gen( cbmesh, matprop, mech_bc, flux_bc ,gen, err )
        type(data_file), intent(out) :: gen
        type(error), intent(inout) :: err

        type(mesh), intent(in) :: cbmesh
        type(physical), intent(in) :: matprop
        type(physical), intent(in) :: mech_bc
        type(physical), intent(in) :: flux_bc

        integer :: i
        
!-----------------------------------------------------------------------------------------
500     format(&
            &'X-dir_Force-Stress  0.0000E+00 Dfx (ramp_loading) 0.0000E+00',/&
            &'Y-dir_Force-Stress  0.0000E+00 Dfy (ramp_loading) 0.0000E+00',/&
            &'X-Displ_rate        0.0000E+00                    0.0000E+00',/&
            &'Y-Displ_rate        0.0000E+00                    0.0000E+00',/&
            &'X_dir_prescribed    1.0000E+00                    0.0000E+00',/&
            &'void                0.0000E+00                    0.0000E+00',/&
            &'Y_dir_prescribed    1.0000E+00                    0.0000E+00',/&
            &'Multiplier          0.1000E+15 Index              1.0000E+00'&
        &)
!-----------------------------------------------------------------------------------------
510     format(&
            &'X-dir_Force-Stress  0.0000E+00 Dfx (ramp_loading) 0.0000E+00',/&
            &'Y-dir_Force-Stress  0.0000E+00 Dfy (ramp_loading) 0.0000E+00',/&
            &'Z-dir_Force-Stress  0.0000E+00 Dfz (ramp_loading) 0.0000E+00',/&
            &'X-Displ_rate        0.0000E+00                    0.0000E+00',/&
            &'Y-Displ_rate        0.0000E+00                    0.0000E+00',/&
            &'Z-Displ_rate        0.0000E+00                    0.0000E+00',/&
            &'X_dir_prescribed    1.0000E+00                    0.0000E+00',/&
            &'void                0.0000E+00                    0.0000E+00',/&
            &'void                0.0000E+00                    0.0000E+00',/&
            &'void                0.0000E+00                    0.0000E+00',/&
            &'Y_dir_prescribed    1.0000E+00                    0.0000E+00',/&
            &'void                0.0000E+00                    0.0000E+00',/&
            &'void                0.0000E+00                    0.0000E+00',/&
            &'void                0.0000E+00                    0.0000E+00',/&
            &'Z_dir_prescribed    1.0000E+00                    0.0000E+00',/&
            &'Multiplier          0.1000E+15 Index              1.0000E+00'  &
        &)
!-----------------------------------------------------------------------------------------
600     format(&
            &'-humidity(Kg-Kg)     0.000E+00',/& 
            &'-gas flow(Kg-s)      0.000E+00',/& 
            &'-gas pressure(MPa)   0.000E+00',/&
            &'-gas gamma           0.000E+00',/&
            &'-gas beta            0.000E+00',/& 
            &'-gas density(Kg-m3)  0.000E+00',/& 
            &'-salt conc.(Kg-Kg)   0.000E+00',/& 
            &'-air conc.(Kg-Kg)    0.000E+00',/& 
            &'-liquid flow(Kg-s)   0.000E+00',/&
            &'-liquid pres(MPa)        -59.0',/& 
            &'-liquid gamma         5.75e-11',/&
            &'-liquid beta         0.000E+00',/& 
            &'-liquid dens.(Kg-m3) 0.000E+00',/& 
            &'-heat flow(J-s)      0.000E+00',/& 
            &'-temperature(C)           25.0',/& 
            &'-heat gamma(J-s-C)       1.e12',/& 
            &'-decay heat(1-s)     0.000E+00',/& 
            &'-tot.vol.flow(m3-s)  0.000E+00',/& 
            &'-smoothing param.    0.000E+00',/& 
            &'-index                1.00E+00'&
            &)
!-----------------------------------------------------------------------------------------
        
        write( gen % ID, * ) cbmesh % nnode, cbmesh % nelem
        do i = 1, matprop % nsets
            write( gen % ID, fmt = '(A)', advance ='no' ) '*'
            write( gen % ID, * ) i
            write( gen % ID, * ) trim( matprop % name( i ) )
        end do
        write( gen % ID, * ) -1

        !Print mechanical boundary conditions
        do i = 1, mech_bc % nsets
            write( gen % ID, * ) i, '       !.. ', trim( mech_bc % name( i ) )
            if ( cbmesh % ndime == 2 ) then
                write( gen % ID, 500 )
            else if ( cbmesh % ndime == 3 ) then
                write( gen % ID, 510 )
            end if
        end do
        write( gen % ID, * ) -1

        !Print flux boundary conditions
        do i = 1, flux_bc % nsets
            write( gen % ID, * ) i, '        !.. ', trim( flux_bc % name( i ) )
            write( gen % ID, 600 )
        end do
        write( gen % ID, * ) -1

        
    end subroutine write_gen

end module gmsh2cb_mod
