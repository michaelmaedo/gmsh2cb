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

    use jacob_mod, only: check_jacob
    
#include "utilities/error.fpp"

#define TEST( err ) if ( there_is_any_error( err ) ) return
    
    implicit none

    integer(LONG), parameter :: MAX_NODES = 8

    type control
        integer(LONG) :: unsat
        integer(LONG) :: iopttemp
        integer(LONG) :: ioptpl
        integer(LONG) :: ioptpg
        integer(LONG) :: ioptdispl
        integer(LONG) :: reord
        integer(LONG) :: version
    end type control
    
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
    
    subroutine gmsh2cb(msh, gen, gri, log, param, err)
    
        type(data_file), intent(inout) :: msh
        type(data_file), intent(inout) :: gri
        type(data_file), intent(inout) :: gen
        type(data_file), intent(inout) :: log
        type(error), intent(inout) :: err
        type(control), intent(in) :: param

        type(mesh) :: gmsh
        type(mesh) :: cbmesh
        type(physical) :: matprop
        type(physical) :: mech_bc
        type(physical) :: flux_bc

        integer(LONG) :: new_nelem
        integer(LONG), allocatable :: bcond(:,:)
        
        call read_gmsh(msh, gmsh, matprop, mech_bc, flux_bc, new_nelem, bcond, err)

        call create_cbmesh(gmsh, new_nelem, bcond, cbmesh, err)

        call write_grid( cbmesh, param, gri, err )

        call write_gen( cbmesh, param, matprop, mech_bc, flux_bc ,gen, err )
        
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
    

    subroutine set_control_param(param)

        type(control), intent(inout) :: param

        param % iopttemp = 0
        param % ioptpl = 0
        param % ioptpg = 0
        param % ioptdispl = 0
        param % reord = 0
        param % unsat = 0
        param % version = -999
        
    end subroutine set_control_param


    subroutine set_iopt(loc_str, param)

        character(len = *), intent(in) :: loc_str
        type(control), intent(inout) :: param

        if (strcmp(loc_str,'THM') == 0) then
            param % ioptdispl = 1
            param % ioptpl = 1
            param % iopttemp = 1
        else if (strcmp(loc_str,'TH') == 0) then
            param % ioptdispl = 0
            param % ioptpl = 1
            param % iopttemp = 1
        else if (strcmp(loc_str,'HM') == 0) then
            param % ioptdispl = 1
            param % ioptpl = 1
            param % iopttemp = 0
        else if (strcmp(loc_str,'TM') == 0) then
            param % ioptdispl = 1
            param % ioptpl = 0
            param % iopttemp = 1
        end if
        
        if (strcmp(loc_str,'THERMO') == 0 .or. &
            & strcmp(loc_str,'T') == 0) then
            param % iopttemp = 1
        end if

        if (strcmp(loc_str,'HYDRO') == 0 .or. &
            & strcmp(loc_str,'H') == 0) then
            param % ioptpl = 1
        end if
        
        if (strcmp(loc_str,'MECHANICAL') == 0 .or. &
            & strcmp(loc_str,'M') == 0) then
            param % ioptdispl = 1
        end if

        if (strcmp(loc_str,'PG') == 0) param % ioptpg = 1

        return
    end subroutine set_iopt


    subroutine set_unsat(loc_str, param)

        character(len = *), intent(in) :: loc_str
        type(control), intent(inout) :: param

        if (strcmp(loc_str, 'UNSAT') == 0) then
            param % unsat = 1
        end if
        
    end subroutine set_unsat

    
    subroutine set_version(loc_str, param)

        character(len = *), intent(in) :: loc_str
        type(control), intent(inout) :: param
        
        if (strcmp(loc_str, 'old') == 0) then
            param % version = 0
        end if
        
    end subroutine set_version
    
    
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

        call check_jacob( &
            & cbmesh % ndime, &
            & cbmesh % nelem, &
            & cbmesh % coord, &
            & cbmesh % eltype, &
            & cbmesh % connec )
        
    end subroutine create_cbmesh

    
    subroutine write_grid( cbmesh, param, gri, err )
        type(mesh), intent(in) :: cbmesh
        type(control), intent(in) :: param
        type(data_file), intent(in) :: gri
        type(error), intent(inout) :: err

        integer(LONG) :: icond, ncond
        integer(LONG) :: idime, ndime
        integer(LONG) :: inode, nnode
        integer(LONG) :: ielem, nelem
        integer(LONG) :: ndf, nvar

        character( len = 50 ) :: ifmt
        character( len = 150 ) :: str
        
!-----------------------------------------------------------------------------------------
10      format(&
            &'$=======================================================================',/&
            &'$     Node             Coords_X             Coords_Y   Mech_BC   Flux_BC',/&
            &'$=======================================================================')
!-----------------------------------------------------------------------------------------
20      format(&
            &'$=============================================================================================================',/&
            &'$  Element    El.Type    Mat.Prop                                   Connectivities                            ',/&
            &'$=============================================================================================================')
!-----------------------------------------------------------------------------------------
30      format('$     node')
!-----------------------------------------------------------------------------------------
40      format('        Displ_X')
!-----------------------------------------------------------------------------------------
50      format('        Displ_Y')
!-----------------------------------------------------------------------------------------
60      format('        Displ_Z')
!-----------------------------------------------------------------------------------------
70      format('     Liq._Press')
!-----------------------------------------------------------------------------------------
80      format('     Gas._Press')
!-----------------------------------------------------------------------------------------
90      format('    Temperature')
!-----------------------------------------------------------------------------------------
100     format(&
            &'$===================================================================================================',/&
            &'$  element      stress_XX      stress_YY      stress_ZZ      stress_XY      stress_YZ      stress_XZ',/&
            &'$===================================================================================================')
!-----------------------------------------------------------------------------------------
110     format('$  element       porosity')
!-----------------------------------------------------------------------------------------
120     format('         perm_X')
!-----------------------------------------------------------------------------------------
130     format('         perm_Y')
!-----------------------------------------------------------------------------------------
140     format('         perm_Z')
!-----------------------------------------------------------------------------------------
150     format('       anisot_X')
!-----------------------------------------------------------------------------------------
160     format('       anisot_Y')
!-----------------------------------------------------------------------------------------
170     format('       anisot_Z')
!-----------------------------------------------------------------------------------------
180     format('       Vol_elem')
!-----------------------------------------------------------------------------------------
200     format(&
            &'$=====================',/&
            &'$ Plot nodal variables',/&
            &'$=====================',/&
            &'         0',/&
            &'         0',//&
            &'$===============================',/&
            &'$ Flux boundary conditions plots',/&
            &'$===============================',/&
            &'         0',/&
            &'         0',//&
            &'$=======================',/&
            &'$ Plot element variables',/&
            &'$=======================',/&
            &'         0',/&
            &'         0',/&
            &'         0')
            
!-----------------------------------------------------------------------------------------
210     format(11i10)
!-----------------------------------------------------------------------------------------
        
        write( gri % ID, fmt = * ) 0, 0, 0
        
        ncond = 2
        ndime = cbmesh % ndime
        nnode = cbmesh % nnode

!-----------------------------------------------------------------------------------------
!       Nodal Coordinates
!-----------------------------------------------------------------------------------------
        ifmt = ''
        if ( ndime == 2 ) then
            ifmt = '(i10,2(" ",e20.13),2i10)'
        else if ( ndime == 3 ) then
            ifmt = '(i10,3(" ", e20.13),2i10)'
        end if
        
        write( gri % ID, 10 )
        do inode = 1, nnode
            write( gri % ID, ifmt) inode, &
                & ( cbmesh % coord( idime, inode ), idime = 1, ndime ), &
                & ( cbmesh % bcond( icond, inode ), icond = 1, 2 )
        end do
        write( gri % ID, * )
        
!-----------------------------------------------------------------------------------------
!       Connectivity list
!-----------------------------------------------------------------------------------------
        nelem = cbmesh % nelem
        write( gri % ID, 20 )
        do ielem = 1, nelem
            write( gri % ID, 210 ) ielem, &
                & cbmesh % elset( ielem ), &
                & cbmesh % eltype( ielem ), &
                & ( cbmesh % connec( inode, ielem ), inode = 1, MAX_NODES )
        end do
        write( gri % ID, * )
        
!-----------------------------------------------------------------------------------------
!       Initial Unknown
!-----------------------------------------------------------------------------------------

        str = ''
        ndf = param % ioptdispl * ndime + &
            & param % ioptpl + param % ioptpg + param % iopttemp
        str = repeat('===============',ndf)
        str = '$========='//trim(str)
        
        write( gri % ID, '(A)' ) str
        write( gri % ID, 30, advance = 'no' )
        if ( param % ioptdispl == 1 ) then

            write( gri % ID, 40, advance = 'no' )
            if ( ndime > 1 )  write( gri % ID, 50, advance = 'no' )
            if ( ndime == 3 ) write( gri % ID, 60, advance = 'no' )

        end if

        if ( param % ioptpl == 1 )   write( gri % ID, 70, advance = 'no')
        if ( param % ioptpg == 1 )   write( gri % ID, 80, advance = 'no')
        if ( param % iopttemp == 1 ) write( gri % ID, 90, advance = 'no')
        write( gri % ID, * )
        write( gri % ID, '(A)' ) str

        ifmt = ''; str = ''
        str = repeat(' 0.000000000000',ndf)
        write ( gri % ID, '(i10,A)')     1, str 
        write ( gri % ID, '(i10,A)') nnode, str 
        write( gri % ID, * )

!-----------------------------------------------------------------------------------------
!       Initial Stresses and Historical Variables
!-----------------------------------------------------------------------------------------
        if ( param % ioptdispl == 1 ) then
            str = repeat(' 0.000000000000',6)
            write( gri % ID, 100 )
            write( gri % ID, '(i10,A)' )     1, str
            write( gri % ID, '(i10,A)' ) nelem, str
            write( gri % ID, * )
        end if
        
!-----------------------------------------------------------------------------------------
!       Porosity and Permeability
!-----------------------------------------------------------------------------------------
        nvar = 2
        if ( param % ioptpl == 1 .or. param % ioptpg == 1 ) then
            nvar = nvar + 1
            if ( ndime == 2) nvar = nvar + 2
            if ( ndime == 3 ) nvar = nvar + 5
        end if

        str = ''
        str = repeat('===============',nvar)
        str = '$========='//trim(str)
        write( gri % ID, '(A)') str
        
        write( gri % ID, 110, advance = 'no')
        if ( param % ioptpl == 1 .or. param % ioptpg == 1 ) then
            write( gri % ID, 120, advance = 'no')
            if ( ndime == 2) then
                write( gri % ID, 130, advance = 'no')
                write( gri % ID, 150, advance = 'no')
            else if ( ndime == 3 ) then
                write( gri % ID, 130, advance = 'no')
                write( gri % ID, 140, advance = 'no')
                write( gri % ID, 150, advance = 'no')
                write( gri % ID, 160, advance = 'no')
                write( gri % ID, 170, advance = 'no')
            end if
        end if
        write( gri % ID, 180)
        write( gri % ID, '(A)') str

        str = ''
        str = repeat(' 0.000000000000', nvar)
        write( gri % ID, '(i10,A)' )     1, str
        write( gri % ID, '(i10,A)' ) nelem, str
        write( gri % ID, * )

!-----------------------------------------------------------------------------------------
!       Plots       
!-----------------------------------------------------------------------------------------
        write( gri % ID, 200 )
        
    end subroutine write_grid


    subroutine write_gen( cbmesh, param, matprop, mech_bc, flux_bc ,gen, err )
        type(data_file), intent(out) :: gen
        type(error), intent(inout) :: err

        type(control), intent(in) :: param
        type(mesh), intent(in) :: cbmesh
        type(physical), intent(in) :: matprop
        type(physical), intent(in) :: mech_bc
        type(physical), intent(in) :: flux_bc

        integer :: i
        
!-----------------------------------------------------------------------------------------
300     format(&
            &'$=======================================================================',/&
            &'$  numnp   numel    ndim   axisym   nmat  nhistvar  igauss  exterf  frac',/&
            &'$=======================================================================',/&
            & 9i8,/)       
!-----------------------------------------------------------------------------------------
310     format(&
            &'$=================================================================',/&
            &'$ mxdifn  mbandt   mfronth   ndf   mnval  isolve  ioptconv  ioptth',/&
            &'$=================================================================',/&
            & 8i8,/)
!-----------------------------------------------------------------------------------------
320     format(&
            &'$===================',/&
            &'$ nfdtype  nfluxtype',/&
            &'$===================',/&
            & 2i8,/)
!-----------------------------------------------------------------------------------------
330     format(&
            &'$===============================================',/&
            &'$  displ      Pl      Pg    temp    salt    chem',/&
            &'$===============================================',/&
            & 6i8,/)
!-----------------------------------------------------------------------------------------
340     format(&
            &'$==================================================================',/&
            &'$ ioptxhl  updpor  ioptxwg ioptxal  ioptpc  iopthys  iupdc  iopthom',/&
            &'$==================================================================',/&
            & 8i8,/)
!-----------------------------------------------------------------------------------------
350     format(&
            &'$===============================================',/&
            &'$  flag1   flag2   flag3   flag4   flag5   flag6',/&
            &'$===============================================',/&
            &'       0       0       0       0       0       0',/&
            &'',/&      
            &'$===========================================================',/&
            &'$    epsilon       theta     pg_cons   temp_cons     pl_cons',/&
            &'$===========================================================',/&
            &' 0.10000E+01 0.10000E+01 0.00000E+00 0.20000E+02 0.00000E+00',/&
            &'',//&
            &'$========================================================================',/&
            &'$      time0       dtime       time1      dtimec       timef     facttime',/&
            &'$========================================================================',/&
            &' 0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00 0.000000E+00',/&
            &'',/&
            &'$==========================================================',/&
            &'$  iowit stepspos itermax  typost  itermax  itime   iteropt',/&
            &'$==========================================================',/&
            &'       1      50      25       1       0       0       0',/&
            &)
!-----------------------------------------------------------------------------------------
360     format(&
            &'$===========================================================',/&
            &'$     delmxu        facu      delfmx        dumx     fdtimeu',/&
            &'$===========================================================')
!-----------------------------------------------------------------------------------------
370     format(&
            &'$===========================================================',/&
            &'$    delmxpl       facpl     delqwmx       dplmx    fdtimepl',/&
            &'$===========================================================')
!-----------------------------------------------------------------------------------------
380     format(&
            &'$===========================================================',/&
            &'$    delmxpg       facpg     delqgmx       dpgmx    fdtimepg',/&
            &'$===========================================================')
!-----------------------------------------------------------------------------------------
390     format(&
            &'$===========================================================',/&
            &'$     delmxt        fact      delemx        dtmx     fdtimet',/&
            &'$===========================================================')
!-----------------------------------------------------------------------------------------
400     format(' 1.00000e-04 1.00000e+00 1.00000e-04 1.00000e+06 0.00000e+00',/)
!-----------------------------------------------------------------------------------------
410     format('   -1',/&
            &'$==========================================================',/&
            &'$    GX(m/s2)   GY(MN/kg)  Grav_time0  Grav_timef  ioptmech',/&
            &'$==========================================================',/&
            &'  0.00000E+00 0.00000E+00 0.00000E+00 0.10000E+02       0',/)
!-----------------------------------------------------------------------------------------
420     format('   -1',/&
            &'$==================================================================================',/&
            &'$    GX(m/s2)    GY(m/s2)   GX(MN/kg)   GY(MN/kg)  Grav_time0  Grav_timef  ioptmech',/&
            &'$==================================================================================',/&
            &'  0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00 0.10000E+02       0',/)
!-----------------------------------------------------------------------------------------
430     format('   -1',/&
            &'$==========================================================================================================',/&
            &'$    GX(m/s2)    GY(m/s2)    GZ(m/s2)   GX(MN/kg)   GY(MN/kg)   GZ(MN/kg)  Grav_time0  Grav_timef  ioptmech',/&
            &'$==========================================================================================================',/&
            &'  0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00 0.10000E+02       0',/)
!-----------------------------------------------------------------------------------------
440     format(&
            &'$========================================================================',/&
            &'$      time0       dtime       time1      dtimec       timef     facttime',/&
            &'$========================================================================',/&
            &' 0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00 0.000000E+00',/)
!-----------------------------------------------------------------------------------------
500     format(&
            &'X-dir_Force-Stress  0.0000E+00 Dfx_(ramp_loading) 0.0000E+00',/&
            &'Y-dir_Force-Stress  0.0000E+00 Dfy_(ramp_loading) 0.0000E+00',/&
            &'X-Displ_rate        0.0000E+00 void               0.0000E+00',/&
            &'Y-Displ_rate        0.0000E+00 void               0.0000E+00',/&
            &'X_dir_prescribed    1.0000E+00 void               0.0000E+00',/&
            &'void                0.0000E+00 void               0.0000E+00',/&
            &'void                0.0000E+00 void               0.0000E+00',/&
            &'Y_dir_prescribed    1.0000E+00 void               0.0000E+00',/&
            &'Multiplier          0.1000E+21 Index              1.0000E+00'&
        &)
!-----------------------------------------------------------------------------------------
510     format(&
            &'X-dir_Force-Stress  0.0000E+00 Dfx_(ramp_loading) 0.0000E+00',/&
            &'Y-dir_Force-Stress  0.0000E+00 Dfy_(ramp_loading) 0.0000E+00',/&
            &'Z-dir_Force-Stress  0.0000E+00 Dfz_(ramp_loading) 0.0000E+00',/&
            &'X-Displ_rate        0.0000E+00 void               0.0000E+00',/&
            &'Y-Displ_rate        0.0000E+00 void               0.0000E+00',/&
            &'Z-Displ_rate        0.0000E+00 void               0.0000E+00',/&
            &'X_dir_prescribed    1.0000E+00 void               0.0000E+00',/&
            &'void                0.0000E+00 void               0.0000E+00',/&
            &'void                0.0000E+00 void               0.0000E+00',/&
            &'void                0.0000E+00 void               0.0000E+00',/&
            &'Y_dir_prescribed    1.0000E+00 void               0.0000E+00',/&
            &'void                0.0000E+00 void               0.0000E+00',/&
            &'void                0.0000E+00 void               0.0000E+00',/&
            &'void                0.0000E+00 void               0.0000E+00',/&
            &'Z_dir_prescribed    1.0000E+00 void               0.0000E+00',/&
            &'Multiplier          0.1000E+21 Index              1.0000E+00'  &
        &)
!-----------------------------------------------------------------------------------------
600     format(&
            &'-humidity(Kg-Kg)     0.000E+00 void               0.0000E+00',/& 
            &'-gas_flow(Kg-s)      0.000E+00 void               0.0000E+00',/& 
            &'-gas_pressure(MPa)   0.000E+00 DPress.gas(MPa)    0.0000E+00',/&
            &'-gas_gamma           0.000E+00 void               0.0000E+00',/&
            &'-gas_beta            0.000E+00 void               0.0000E+00',/& 
            &'-gas_density(Kg-m3)  0.000E+00 void               0.0000E+00',/& 
            &'-salt_conc.(Kg-Kg)   0.000E+00 void               0.0000E+00',/& 
            &'-air_conc.(Kg-Kg)    0.000E+00 void               0.0000E+00',/& 
            &'-liquid_flow(Kg-s)   0.000E+00 void               0.0000E+00',/&
            &'-liquid_pres(MPa)        -59.0 DPress.liq.(MPa)   0.0000E+00',/& 
            &'-liquid_gamma         5.75e+12 void               0.0000E+00',/&
            &'-liquid_beta         0.000E+00 void               0.0000E+00',/& 
            &'-liquid_dens.(Kg-m3) 0.000E+00 void               0.0000E+00',/& 
            &'-heat_flow(J-s)      0.000E+00 void               0.0000E+00',/& 
            &'-temperature(C)           25.0 DTemp.(C)          0.0000E+00',/& 
            &'-heat_gamma(J-s-C)       1.e12 void               0.0000E+00',/& 
            &'-decay_heat(1-s)     0.000E+00 void               0.0000E+00',/& 
            &'-tot.vol.flow(m3-s)  0.000E+00 void               0.0000E+00',/& 
            &'-smoothing_param.    0.000E+00 void               0.0000E+00',/& 
            &'-index                1.00E+00 void               0.0000E+00'&
            &)
!-----------------------------------------------------------------------------------------
!       Control Parameters        
!-----------------------------------------------------------------------------------------

        write( gen % ID, * ) ' Gen file created by means of gmsh and gmsh2cb'
        write( gen % ID, * ) param % version
        write( gen % ID, * )
        
        write( gen % ID, 300 ) cbmesh % nnode, cbmesh % nelem, cbmesh % ndime, &
            & 0, matprop % nsets, 185, 0, 0, 0

        write( gen % ID, 310 ) 99999, 1, 0, &
            & param % ioptdispl * cbmesh % ndime + param % ioptpl + param % ioptpg + param % iopttemp, &
            & 1, 6, 1, 0

        write( gen % ID, 320 ) mech_bc % nsets, flux_bc % nsets

        write( gen % ID, 330 ) param % ioptdispl, &
            & param % ioptpl, &
            & param % ioptpg, &
            & param % iopttemp, &
            & 0, 0

        if (param % unsat == 1) then
            write( gen % ID, 340 ) 0, 0, 0, 0, -1, 0, 0, 0 
        else
            write( gen % ID, 340 ) 0, 0, 1, 1, -1, 0, 0, 0
        end if

        write( gen % ID, 350 )
        
!-----------------------------------------------------------------------------------------
!       Convergence parameters
!-----------------------------------------------------------------------------------------
!
!       Displacement        
        if (param % ioptdispl == 1) then
            write( gen % ID, 360 )
            write( gen % ID, 400 )
        end if
!
!       Liquid Pressure        
        if (param % ioptpl == 1) then
            write( gen % ID, 370 )
            write( gen % ID, 400 )
        end if
!
!       Gas Pressure        
        if (param % ioptpg == 1) then
            write( gen % ID, 380 )
            write( gen % ID, 400 )
        end if
!
!       Temperature        
        if (param % iopttemp == 1) then
            write( gen % ID, 390 )
            write( gen % ID, 400 )
        end if
!
!-----------------------------------------------------------------------------------------     
!       Gravity
!-----------------------------------------------------------------------------------------
        if (cbmesh % ndime == 1) then
            write( gen % ID, 410 )
        else if (cbmesh % ndime == 2) then
            write( gen % ID, 420 )
        else if (cbmesh % ndime == 3) then
            write( gen % ID, 430 )
        else
        end if

!-----------------------------------------------------------------------------------------     
!       Time Interval
!-----------------------------------------------------------------------------------------
        write( gen % ID, 440 )
        
!-----------------------------------------------------------------------------------------
!       Material Properties
!-----------------------------------------------------------------------------------------
        do i = 1, matprop % nsets
            write( gen % ID, fmt = '(A)', advance ='no' ) '*'
            write( gen % ID, * ) i
            write( gen % ID, * ) trim( matprop % name( i ) )
        end do
        write( gen % ID, * ) -1

!-----------------------------------------------------------------------------------------
!       Boundary Conditions
!-----------------------------------------------------------------------------------------

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
