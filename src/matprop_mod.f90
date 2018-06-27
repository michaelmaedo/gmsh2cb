module matprop_mod
    
contains

    subroutine write_mech( fileID, filename )
        integer, intent(in) :: fileID
        character(len=*), intent(in) :: filename

        integer :: endfname
        character(len=3) :: file_extens
        
        endfname = len_trim(filename)
        file_extens(1:3) = filename(endfname-2:endfname)

        select case( file_extens )
        case ('tep')
            call write_tep( fileID )
    !     case ('dam')
    !     case ('dp')
        case default
            !call write_tep( fileID )            
        end select
    end subroutine write_mech


    subroutine write_tep( fileID )
        integer :: fileID
1       format(&
            & '   21 TEP_Elasticity         1'/&
            & ' kappai0                  0.05 alphass            0.0000E+00'/&
            & ' kappas0                  0.30 void               0.0000E+00'/&
            & ' Kmin_(MPa)                0.1 alphai                 -0.003'/&
            & ' void               0.0000E+00 alphasp                -0.147'/&
            & ' nu                       0.40 pref_(MPa)               0.01'/&
            & '   22 TEP_Thermal            1'/&
            & ' alpha0                1.5E-04 void               0.0000E+00'/&
            & ' alpha1             0.0000E+00 void               0.0000E+00'/&
            & ' alpha2             0.0000E+00 void               0.0000E+00'/&
            & ' alpha3             0.0000E+00 void               0.0000E+00'/&
            & ' Tref_(oC)                  20 void               0.0000E+00'/&
            & '   23 TEP_Plastic_1          1'/&
            & ' lambda0                  0.15 ps0_(MPa)                 0.1'/&
            & ' r                        0.75 void               0.0000E+00'/&
            & ' beta_(MPa^-1)            0.05 void               0.0000E+00'/&
            & ' rho_(oC^-1)               0.2 void               0.0000E+00'/&
            & ' k                         0.1 void               0.0000E+00'/&
            & '   24 TEP_Plastic_2          1'/&
            & ' pc_(MPa)                  0.1 void               0.0000E+00'/&
            & ' M                         1.0 void               0.0000E+00'/&
            & ' alpha                     0.3 void               0.0000E+00'/&
            & ' e0                        0.6 void               0.0000E+00'/&
            & ' p0*_(MPa)                  14 void               0.0000E+00'/&
            & '   25 TEP_ShapeYield_Surf    3'/&
            & ' phi_Mohr_Coulomb   0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & '   26 TEP_ShapePlast.Pot.    3'/&
            & ' phi_Mohr-Coulomb   0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & '   27 TEP_Integ._Control     1'/&
            & ' tole1              1.0000E-08 itermaxc           0.0000E+00'/&
            & ' tole2              1.0000E-03 itermaxs           0.0000E+00'/&
            & ' tole3              1.0000E-03 void               0.0000E+00'/&
            & ' mu                          1 void               0.0000E+00'/&
            & ' Index                      -1 void               0.0000E+00')
        write(fileID,1)
    end subroutine write_tep
        
end module matprop_mod
