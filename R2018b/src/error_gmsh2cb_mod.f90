module error_gmsh2cb_mod

    use precision_mod, only: LONG

    implicit none
    
    integer(LONG) :: ERROR_GET_RECORD = 1
    integer(LONG) :: ERROR_MESH_FORMAT = 2
    integer(LONG) :: ERROR_END_MESH_FORMAT = 3

    integer(LONG) :: ERROR_PHYSICAL_NAMES = 4
    integer(LONG) :: ERROR_END_PHYSICAL_NAMES = 5

    integer(LONG) :: ERROR_INVALID_ELEMENT = 6
    
end module error_gmsh2cb_mod
