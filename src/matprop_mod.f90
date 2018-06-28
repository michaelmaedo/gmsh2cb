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
        case ('dam')
            call write_damage( fileID )
        case ('.dp')
            call write_drucker_prager( fileID )
        case default
            call write_damage( fileID )            
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

    
!     subroutine write_mohr_coulomb( fileID )
!         integer :: fileID
! 20      format(&
!             & '' &
!             &)
!         write(fileID,20)
!     end subroutine write_mohr_coulomb

    
!     subroutine write_corrosion( fileID )
!         integer :: fileID
! 50      format(&
!             & '' &
!             &)
!         write(fileID,50)
!     end subroutine write_corrosion


    subroutine write_damage( fileID )
        integer :: fileID
60      format(&
            & '   21 Damage_model_param.   65'/&
            & ' E_(MPa)                17E+03 alphabiot                 0.0'/&
            & ' nu                        0.2 visco                     0.0'/&
            & ' ft_(MPa)                 1.25 r-r0_ratio                1.0'/&
            & ' H_(m^-1)                  0.0 iflag_tension             1.0'/&
            & ' q                      17E+03 Gf_(MN-m)             120E-06'/&
            & '   22 Crack_properties       2'/&
            & ' t_mech             0.0000E+00 c_perm             0.0000E+00'/&
            & ' normal_x           0.0000E+00 n_perm             0.0000E+00'/&
            & ' normal_y           0.0000E+00 t_hydro_max        0.0000E+00'/&
            & ' normal_z           0.0000E+00 ks0_(m2)              2.0e-16'/&
            & ' fcrack_0           0.0000E+00 ks_max_(m2)           1.0e-05'/&
            & '   33 Time_Step_Control      1'/&
            & ' f_tol                 1.0E-04 void               0.0000E+00'/&
            & ' dtime_max_(s)            0.01 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & '   35 Shear_Strengths        1'/&
            & ' tau_s_(MPa)           4.0E-04 void               0.0000E+00'/&
            & ' tau_l_(MPa)           4.0E-04 void               0.0000E+00'/&
            & ' frict_s            0.0000E+00 void               0.0000E+00'/&
            & ' frict_l            0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & '   36 Stress_directions      1'/&
            & ' n(1)                      0.0 s(3)                      0.0'/&
            & ' n(2)                      0.0 l(1)                      0.0'/&
            & ' n(3)                      1.0 l(2)                      1.0'/&
            & ' s(1)                      1.0 l(3)                      0.0'/&
            & ' s(2)                      0.0 void               0.0000E+00'/&             
            &)
        write(fileID,60)
    end subroutine write_damage


!     subroutine write_von_Mises( fileID )
!         integer :: fileID
! 70      format(&
!             & '' &
!             &)
!         write(fileID,70)
!     end subroutine write_von_Mises


    subroutine write_drucker_prager( fileID )
        integer :: fileID
80      format(&
            & '   21 Damage_model_param.   80'/&
            & ' E_(MPa)                  10.0 alphabiot                 1.0'/&
            & ' nu                        0.3 void               0.0000E+00'/&
            & ' cohesion_(MPa)            0.0 void               0.0000E+00'/&
            & ' friction_(o)             30.0 void               0.0000E+00'/&
            & ' dilantace                20.0 iflag_tension             0.0'/&
            & '   33 Time_Step_Control      1'/&
            & ' f_tol                    0.30 void               0.0000E+00'/&
            & ' dtime_max_(s)          1.0E-6 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            &)
        write(fileID,80)
    end subroutine write_drucker_prager


!     subroutine write_homog( fileID )
!         integer :: fileID
! 100    format(&
!             & '' &
!             &)
!         write(fileID,100)
!     end subroutine write_homog


    subroutine write_hydro( fileID )
        integer :: fileID
110     format(&
            & '    7 Intrinsic_Permeab.     1'/&
            & ' k11O_(m2)             2.0E-16 beta(arg._koz)     0.0000E+00'/&
            & ' k22O_(m2)             2.0E-16 osm_eff            0.0000E+00'/&
            & ' k33O_(m2)             2.0E-16 void               0.0000E+00'/&
            & ' phi0               0.0000E+00 void               0.0000E+00'/&
            & ' phimin             0.0000E+00 void               0.0000E+00'/&
            & '   14 Liquid_rel.permeab.    6'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' A                         1.0 void               0.0000E+00'/&
            & ' lambda                    3.0 void               0.0000E+00'/&
            & ' Srl                 0.000E+00 void               0.0000E+00'/&
            & ' Sls                 0.000E+00 void               0.0000E+00'/&
            & '   19 Gas_rel.permeab.       1'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' A                  0.0000E+00 void               0.0000E+00'/&
            & ' lambda             0.0000E+00 void               0.0000E+00'/&
            & ' Srg                0.0000E+00 void               0.0000E+00'/&
            & ' Sgs                0.0000E+00 void               0.0000E+00'/&            
            &)
        write(fileID,110)
    end subroutine write_hydro

    
    subroutine write_unsat( fileID )
        integer :: fileID
120     format(&
            & '    6 Retention_curve        1'/&
            & ' P0_(MPa)                 1.05 a                  0.0000E+00'/&
            & ' sigma0_(N-m)            0.072 b                  0.0000E+00'/&
            & ' lambda                    0.2 void               0.0000E+00'/&
            & ' Srl                     0.001 phi0               0.0000E+00'/&
            & ' Sls                       1.0 void               0.0000E+00'/&
            & '   11 Diffus._vapour_flux    1'/&
            & ' D_(m2.Pa-s-K^n)       5.9E-06 void               0.0000E+00'/&
            & ' n                         2.3 void               0.0000E+00'/&
            & ' tau                       0.8 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & '   12 Diffus._disso._flux    1'/&
            & ' D_(m2-s)              1.1E-04 void               0.0000E+00'/&
            & ' Q_(J-mol)               24530 void               0.0000E+00'/&
            & ' tau                       0.8 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & '    8 Dispers_M+E_flux       1'/&
            & ' dl_liquid_(m)      0.0000E+00 dl_vapor_(m)       0.0000E+00'/&
            & ' dt_liquid_(m)      0.0000E+00 dt_vapor_(m)       0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' dl_heat_(m)        0.0000E+00 void               0.0000E+00'/&
            & ' dt_heat_(m)        0.0000E+00 void               0.0000E+00'/&
            &)
        write(fileID,120) 
    end subroutine write_unsat


    subroutine write_solid_prop( fileID )
        integer :: fileID
130    format(&
            & '   10 Solid_Phase_Prop.      3'/&
            & ' Cs_(J-kg-K)              1000 dCs-dt                   1.38'/&
            & ' rhos0_(kg-m3)            2770 betas_(MPa^-1)            0.0'/&
            & ' alphas_(oC^-1)        7.8E-06 p0_(MPa)                  0.1'/&
            & ' T0_(oC)                    40 BodyForce_0               0.0'/&
            & ' alphabiot                0.75 BodyForce_f               0.0'/&
            &)
        write(fileID,130)
    end subroutine write_solid_prop


    subroutine write_liquid_prop( fileID )
        integer :: fileID
140     format(&
            & '   15 Liquid_density         1'/&
            & ' rhol0_(kg-m3)          1002.6 void               0.0000E+00'/&
            & ' betal_(MPa^-1)        4.5E-04 void               0.0000E+00'/&
            & ' alphal_(oC^-1)        3.4E-04 void               0.0000E+00'/&
            & ' gamma                  0.6923 void               0.0000E+00'/&
            & ' Pl0_(MPa)                 0.1 void               0.0000E+00'/&
            & '   16 Liquid_viscosity       1'/&
            & ' A_(MPa.s)             2.1E-12 void               0.0000E+00'/&
            & ' B_(K)                  1808.5 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            &)
            write(fileID,140)
    end subroutine write_liquid_prop

    
    subroutine write_gas_prop( fileID )
        integer :: fileID
150      format(&
             & '   17 Dry_air_density       1'/&
             & ' rhog0_(kg-m3)            0.1 P6                 0.0000E+00'/&
             & ' betag_(MPa^-1)            10 P7                 0.0000E+00'/&
             & ' alphag_(oC^-1)    0.0000E+00 P8                 0.0000E+00'/&
             & ' void              0.0000E+00 P9                 0.0000E+00'/&
             & ' Pg0_(MPa)         0.0000E+00 P10                0.0000E+00'/&
             & '   18 Gas_viscosity         1'/&
             & ' A_(MPa.s)           1.48E-12 void               0.0000E+00'/&
             & ' B_(oC)                 119.4 void               0.0000E+00'/&
             & ' C                       0.14 void               0.0000E+00'/&
             & ' D                    1.2E+15 void               0.0000E+00'/&
             & ' void              0.0000E+00 void               0.0000E+00'/&
             &)
        write(fileID,150)
    end subroutine write_gas_prop

    
    subroutine write_thermal_prop( fileID )
        integer :: fileID
160     format(&
            & '    9 Conduct.Heat_flux_1    1'/&
            & ' lam_dry_(W.m-K)          0.47 a1                 0.0000E+00'/&
            & ' lam_sat_(W.m-K)          1.15 a2                 0.0000E+00'/&
            & ' lam_solid0_(W.m-K) 0.0000E+00 a3                 0.0000E+00'/&
            & ' lam_gas_(W.m-K)    0.0000E+00 void               0.0000E+00'/&
            & ' lam_liq_(W.m-K)    0.0000E+00 void               0.0000E+00'/&
            & '   20 Conduct.Heat_flux_2    1'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            & ' void               0.0000E+00 void               0.0000E+00'/&
            &)
        write(fileID,160)
    end subroutine write_thermal_prop
        
end module matprop_mod
