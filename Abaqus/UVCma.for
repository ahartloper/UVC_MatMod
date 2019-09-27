      SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     1RPL,DDSDDT,DRPLDE,DRPLDT,
     2STRAN,DSTRAN,TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,CMNAME,
     3NDI,NSHR,NTENS,NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,
     4CELENT,DFGRD0,DFGRD1,NOEL,NPT,LAYER,KSPT,JSTEP,KINC)
      ! Note that IMPLICIT definition is active
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME
C
      DIMENSION STRESS(NTENS),STATEV(NSTATV),
     1DDSDDE(NTENS,NTENS),DDSDDT(NTENS),DRPLDE(NTENS),
     2STRAN(NTENS),DSTRAN(NTENS),TIME(2),PREDEF(1),DPRED(1),
     3PROPS(NPROPS),COORDS(3),DROT(3,3),DFGRD0(3,3),DFGRD1(3,3),
     4JSTEP(4)
C
C
      ! Subroutine control
      INTEGER :: i, j, n_backstresses, it_num, converged, ind_alpha
      ! Material properties
      REAL(8) :: elastic_modulus, Q_inf, b, D_inf, a,
     1shear_modulus, bulk_modulus, poission_ratio, mu2, lame_first
      ! Used for intermediate calculations
      REAL(8) :: yield_stress, ep_eq, ep_eq_init, a_temp,
     1hard_iso_Q, hard_iso_D, hard_iso_total, a_dot_n,
     2plastic_mult, p_mult_numer, p_mult_denom, yield_function,
     3isotropic_modulus, kin_modulus,
     4stress_relative_norm, strain_trace, alpha_trace, e_k,
     5ID2_out_ID2, n_out_n, stress_hydro, sigma_vm,
     6Lam, n33_check, alpha_out_n, beta, theta_1, theta_2, theta_3,srn2
      ! Backstress arrays
      REAL(8), DIMENSION(:, :), ALLOCATABLE :: alpha_k
      REAL(8), DIMENSION(:), ALLOCATABLE :: C_k, gamma_k
      ! Tensors
      REAL(8), DIMENSION(6, 6) :: ID4, c_mat
      REAL(8), DIMENSION(6) :: strain_tens, strain_plastic,
     1yield_normal, alpha, strain_trial, stress_relative,
     2stress_dev, ID2, stress_tens, check, dstran_tens, alpha_diff,
     3alpha_upd
      ! Parameters
      INTEGER :: N_BASIC_PROPS, TERM_PER_BACK, MAX_ITERATIONS,
     1I_ALPHA
      REAL(8) :: TOL, ONE, TWO, THREE, ZERO, SQRT23
      PARAMETER(TOL=1.0D-10,
     1N_BASIC_PROPS=7, TERM_PER_BACK=2, MAX_ITERATIONS=1000,
     2ONE=1.0D0, TWO=2.0D0, THREE=3.0D0, ZERO=0.D0,
     3SQRT23=SQRT(2.0D0/3.0D0), I_ALPHA=7)
C ----------------------------------------------------------------------C
C
      ! Subroutine start
C
C ----------------------------------------------------------------------C
      ! Get the number of backstresses
      n_backstresses = (nprops - N_BASIC_PROPS) / TERM_PER_BACK
      IF (n_backstresses .EQ. 0) THEN
        PRINT *, "No backstresses defined, exiting!"
        CALL XIT  ! Exit from analysis command in Abaqus
      END IF
C
      ! Allocate the backstress related arrays
      ALLOCATE(C_k(n_backstresses))
      ALLOCATE(gamma_k(n_backstresses))
      ALLOCATE(alpha_k(n_backstresses, ntens))
C
      ! Initialize
      ddsdde(:, :) = ZERO
      ID4(:, :) = ZERO  ! 4th order symmetric identity tensor
      DO i = 1, ndi
        ID4(i, i) = ONE  
      END DO
      DO i = ndi+1, ntens
        ID4(i, i) = ONE / TWO
      END DO
      ! 2nd order symmetric identity tensor
      ID2(:) = (/ ONE, ONE, ONE, ZERO, ZERO, ZERO /)
C
      ! Read in state variables
      ! 1               = Equivalent plastic strain
      ! 2 - 7           = Plastic strain
      ! 8 - 8 + 6 * N   = Backstress components
      ep_eq = statev(1)
      ep_eq_init = statev(1)
      CALL ROTSIG(statev(2), drot, strain_plastic, 2, ndi, nshr)
      alpha(:) = ZERO
      DO i = 1, n_backstresses
        ind_alpha = I_ALPHA + 1 + (i - 1) * ntens
        CALL ROTSIG(statev(ind_alpha), drot, alpha_k(i, :), 1, ndi,nshr)
        alpha = alpha + alpha_k(i, :)
      END DO
C
      ! Read in the material properties
      elastic_modulus = props(1)
      poission_ratio = props(2)
      yield_stress = props(3)
      q_inf = props(4)
      b = props(5)
      d_inf = props(6)
      a = props(7)
      DO i = 1, n_backstresses  ! First backstress starts at index = 8
        c_k(i) = props((N_BASIC_PROPS - 1) + 2 * i)
        gamma_k(i) = props(N_BASIC_PROPS + 2 * i)
      END DO
C
      ! Calculate elastic parameters
      shear_modulus = elastic_modulus / (TWO * (ONE + poission_ratio))
      bulk_modulus = elastic_modulus /
     1(THREE * (ONE - TWO * poission_ratio))
      mu2 = TWO * shear_modulus
C
      ! Set-up strain tensor
      ! Tensors are stored: 11, 22, 33, 12, 13, 23
      strain_tens = stran + dstran
C ----------------------------------------------------------------------C
C
      ! Elastic trial step
C
C ----------------------------------------------------------------------C
      ! Tensor of elastic moduli
      DO j = 1, ntens
        DO i = 1, ntens
          ID2_out_ID2 = ID2(i) * ID2(j)
          c_mat(i, j) = ID2_out_ID2 * bulk_modulus +
     1    mu2 * (ID4(i, j) - ONE/THREE * ID2_out_ID2)
        END DO
      END DO
      ! Stress tensor
      stress_tens = MATMUL(c_mat, (strain_tens - strain_plastic))
C
      stress_hydro = SUM(stress_tens(1:3)) / THREE
      strain_trace = SUM(strain_tens(1:3))
      DO i = 1, ndi
        stress_dev(i) = stress_tens(i) - stress_hydro
        stress_relative(i) = stress_dev(i) - alpha(i)
      END DO
      DO i = ndi+1, ntens
        stress_dev(i) = stress_tens(i)
        stress_relative(i) = stress_dev(i) - alpha(i)
      END DO
      stress_relative_norm = 
     1sqrt(dotprod6(stress_relative, stress_relative))
C
      ! Yield condition
      hard_iso_Q = q_inf * (ONE - EXP(-b * ep_eq))
      hard_iso_D = d_inf * (ONE - EXP(-a * ep_eq))
      hard_iso_total = yield_stress + hard_iso_Q - hard_iso_D
      yield_function = stress_relative_norm - SQRT23 * hard_iso_total
      IF (yield_function .GT. TOL) THEN
        converged = 0
      ELSE
        converged = 1
      END IF
C
      ! Calculate the normal to the yield surface
      yield_normal = stress_relative / (TOL + stress_relative_norm)
C ----------------------------------------------------------------------C
C
      ! Radial return mapping if plastic loading
C
C ----------------------------------------------------------------------C
      ! Calculate the consitency parameter (plastic multiplier)
      plastic_mult = ZERO
      it_num = 0
      DO WHILE ((converged .EQ. 0) .AND. (it_num .LT. MAX_ITERATIONS))
        it_num = it_num + 1
C
        ! Calculate the isotropic hardening parameters
        hard_iso_Q = q_inf * (ONE - EXP(-b * ep_eq))
        hard_iso_D = d_inf * (ONE - EXP(-a * ep_eq))
        hard_iso_total = yield_stress + hard_iso_Q - hard_iso_D
        isotropic_modulus = b * (q_inf - hard_iso_Q) -
     1  a * (d_inf - hard_iso_D)
        ! Calculate the kinematic hardening parameters
        kin_modulus = ZERO
        DO i = 1, n_backstresses
          e_k = EXP(-gamma_k(i) * (ep_eq - ep_eq_init))
          kin_modulus = kin_modulus + C_k(i) * e_k 
     1    - SQRT(THREE/TWO)*gamma_k(i)*e_k
     2    * dotprod6(yield_normal, alpha_k(i, :))
        END DO
        a_dot_n = ZERO
        alpha_upd(:) = ZERO
        DO i = 1, n_backstresses
          e_k = EXP(-gamma_k(i) * (ep_eq - ep_eq_init))
          alpha_upd = alpha_upd + e_k * alpha_k(i, :)
     1    + SQRT23 * C_k(i) / gamma_k(i) * (ONE - e_k) * yield_normal
        END DO
        a_dot_n = dotprod6(alpha_upd - alpha, yield_normal)  ! n : \Delta \alpha
        
        p_mult_numer = stress_relative_norm -
     1  (a_dot_n + SQRT23 * hard_iso_total + mu2 * plastic_mult)
C
        p_mult_denom = -mu2 *
     1  (ONE + (kin_modulus + isotropic_modulus) /
     2  (THREE * shear_modulus))
C
        ! Update variables
        plastic_mult = plastic_mult - p_mult_numer / p_mult_denom
        ep_eq = ep_eq_init + SQRT23 * plastic_mult
C
        IF (ABS(p_mult_numer) .LT. TOL) THEN
          converged = 1
        END IF
      END DO
C ----------------------------------------------------------------------C
C
      ! Update variables
C
C ----------------------------------------------------------------------C
      IF (it_num .EQ. 0) THEN  ! Elastic loading
        stress = stress_tens
      ELSE  ! Plastic loading
        strain_plastic = strain_plastic + plastic_mult * yield_normal
        strain_plastic(4:6) = strain_plastic(4:6) 
     1  + plastic_mult * yield_normal(4:6)
        stress = MATMUL(c_mat, (strain_tens - strain_plastic))
C
        alpha_diff = alpha
        alpha(:) = ZERO
        DO i = 1, n_backstresses  ! Update backstress components
          e_k = EXP(-gamma_k(i) * (ep_eq - ep_eq_init))
          alpha_k(i, :) = e_k * alpha_k(i, :) +
     1    SQRT23 * yield_normal * C_k(i) / gamma_k(i) * (ONE - e_k)
          alpha = alpha + alpha_k(i, :)
        END DO
        alpha_diff = alpha - alpha_diff
      END IF
C
C     Tangent modulus
      IF (it_num .EQ. 0) THEN  ! Elastic loading
        DO j = 1, ntens
          DO i = 1, ntens
            ddsdde(i, j) = c_mat(i, j)
          END DO
        END DO
        DO j = ndi+1, ntens
          ddsdde(j, j) = shear_modulus
        END DO
      ELSE  ! Plastic loading
        beta = ONE +
     1  (kin_modulus + isotropic_modulus) / (THREE * shear_modulus)
        theta_1 = ONE - mu2 * plastic_mult / stress_relative_norm
        theta_3 = ONE / (beta * stress_relative_norm)
        theta_2 = ONE / beta 
     1  + dotprod6(yield_normal, alpha_diff) * theta_3 
     2  - (ONE - theta_1)
        DO j = 1, ntens
          DO i = 1, ntens
            ID2_out_ID2 = ID2(i) * ID2(j)
            n_out_n = yield_normal(i) * yield_normal(j)
            alpha_out_n = alpha_diff(i) * yield_normal(j)
            ddsdde(i, j) = bulk_modulus * ID2_out_ID2
     1      +mu2 * theta_1*(ID4(i, j) - ONE/THREE*ID2_out_ID2)
     2      -mu2 * theta_2 * n_out_n +
     3      +mu2 * theta_3 * alpha_out_n
          END DO
        END DO
        ddsdde = ONE/TWO * (TRANSPOSE(ddsdde) + ddsdde)
      END IF
C ----------------------------------------------------------------------C
C
      ! Update the state variables
C
C ----------------------------------------------------------------------C
      statev(1) = ep_eq
      DO i = 1, ntens
        statev(i + 1) = strain_plastic(i)
      END DO
      DO i = 1, n_backstresses
        DO j = 1, ntens
          statev(I_ALPHA + j + (i-1) * ntens) = alpha_k(i, j)
        END DO
      END DO
C ----------------------------------------------------------------------C
C
      ! Reduce time increment if did not converge
C
C ----------------------------------------------------------------------C
      IF (it_num .EQ. MAX_ITERATIONS) THEN
        PRINT *, "WARNING: Return mapping in integration point ", npt,
     1  " of element ", noel, " did not converge."
        PRINT *, "Reducing time increment to 1/4 of current value."
        PNEWDT = 0.25
      END IF
      RETURN
        
      CONTAINS
C ----------------------------------------------------------------------C
C
      ! Define dot product for vectors
C
C ----------------------------------------------------------------------C
      pure function dotprod6(A, B) result(C)
      !! Returns the dot product of two symmetric length 6 vectors
      !! that are reduced from 9 components, with the last 3 symmetric
      REAL(8), intent(in) :: A(6), B(6)
      REAL(8)             :: C
      INTEGER             :: i      
      ! Calculate the dot product
      C = 0.0D0
      DO i = 1, 3
        C = C + A(i) * B(i)
      END DO
      DO i = 4, 6
        C = C + TWO * (A(i) * B(i))
      END DO
      end function
      END
