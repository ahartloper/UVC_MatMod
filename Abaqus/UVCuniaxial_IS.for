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
      ! Variables defined in the subroutine
      INTEGER :: i, j, n_backstresses, it_num, is_converged
      ! Material properties
      REAL(8) :: elastic_modulus, sy_0, Q, b, c_k, gamma_k,
     1ep_eq_init, alpha_init, D, a
      ! Used for intermediate calculations
      REAL(8) :: alpha, sy, sigma, ep_eq, e_p, phi, aux, dit, dep,
     1A_term,  yield_radius, iso_Q, iso_D, e_p_total
      ! Vectors
      REAL, DIMENSION(:, :), ALLOCATABLE :: chab_coef
      REAL, DIMENSION(:), ALLOCATABLE :: alpha_k, alpha_k_init
      ! Parameters
      INTEGER :: N_BASIC_PROPS, TERM_PER_BACK, MAX_ITERATIONS,
     1DEBUG_ON, n_mat_props
      REAL(8) :: TOL, ONE, TWO, ZERO
      PARAMETER(TOL=1.0D-10,
     1N_BASIC_PROPS=6, TERM_PER_BACK=2, MAX_ITERATIONS=1000,
     2ONE=1.0D0, TWO=2.0D0, ZERO=0.D0)
C-----------------------------------------------------------------------C
C     
      ! Start of subroutine
C
C-----------------------------------------------------------------------C
      ! Set the properties of the material model
      n_is_props = 6
      n_backstresses = (nprops-N_BASIC_PROPS-n_is_props)/TERM_PER_BACK
      IF (n_backstresses .EQ. 0) THEN
        PRINT *, "No backstresses defined, exiting!"
        CALL XIT  ! Exit from analysis command in Abaqus
      END IF
      ALLOCATE(chab_coef(n_backstresses, 2))
      ALLOCATE(alpha_k(n_backstresses))
      ALLOCATE(alpha_k_init(n_backstresses))
C      
      elastic_modulus = props(1)
      sy_0 = props(2)
      Q = props(3)
      b = props(4)
      D = props(5)
      a = props(6)
C-----------------------------------------------------------------------C
C     
      ! Add initial stress
C
C-----------------------------------------------------------------------C
      ! Add initial stress when total time = 0
      IF (time(2) == 0.) THEN
        n_mat_props = N_BASIC_PROPS + 2 * n_backstresses + 1
        ! kspt (section point) = integration point on cross-section
        stress(1) = stress(1)+resid_npt(kspt, props(n_mat_props:nprops))
      END IF
C-----------------------------------------------------------------------C
C     
      ! Elastic trial step
C
C-----------------------------------------------------------------------C
      sigma = stress(1) + elastic_modulus * dstran(1)
C
      ! Determine isotropic component of hardening
      ep_eq = statev(1)  ! 1st state variable assumed to be equivalent plastic strain
      ep_eq_init = ep_eq
      sy = sy_0 + Q * (ONE - EXP(-b * ep_eq)) -
     1D * (ONE - EXP(-a * ep_eq))
C      
      ! Determine kinematic component of hardening
      alpha = ZERO
      DO i = 1, n_backstresses  ! c and gamma assumed to start at 7th entry
        chab_coef(i, 1) = props(N_BASIC_PROPS - 1 + 2 * i)
        chab_coef(i, 2) = props(N_BASIC_PROPS + 2 * i)
        alpha_k(i) = statev(1 + i)  ! alpha_k assumed to be 2nd, ..., state variables (as many as backstresses)
        alpha_k_init(i) = statev(i + 1)
        alpha = alpha + alpha_k(i)
      END DO
      yield_radius = sigma - alpha
      phi = yield_radius ** 2 - sy ** 2
C-----------------------------------------------------------------------C
C     
      ! Return mapping
C
C-----------------------------------------------------------------------C
      is_converged = 1
      IF (phi .GT. TOL) THEN
        is_converged = 0
      END IF
      it_num = 0
      e_p_total = 0.d0
      DO WHILE (is_converged .EQ. 0 .AND. it_num .LT. MAX_ITERATIONS)
        it_num = it_num + 1
C        
        ! Determine the plastic strain increment
        aux = elastic_modulus
        DO i = 1, n_backstresses
          aux = aux + chab_coef(i, 1) -
     1    SIGN(ONE, yield_radius) * chab_coef(i, 2) * alpha_k(i)
        END DO
C            
      dit = TWO * yield_radius * aux +
     1 SIGN(ONE, yield_radius) * TWO * sy * Q * b * EXP(-b * ep_eq) -
     2 SIGN(ONE, yield_radius) * TWO * sy * D * a * EXP(-a * ep_eq)
      dep = phi / dit
C            
      ! Prevents newton step from overshooting
      IF (ABS(dep) > ABS(sigma / elastic_modulus)) THEN
        dep = SIGN(ONE, dep) * 0.95D0 *
     1  ABS(sigma / elastic_modulus)
      END IF
C-----------------------------------------------------------------------C
C     
      ! Update variables
C
C-----------------------------------------------------------------------C
      e_p_total = e_p_total + dep
      ep_eq = ep_eq_init + ABS(e_p_total)
      iso_Q = Q * (ONE - EXP(-b * ep_eq))
      iso_D = D * (ONE - EXP(-a * ep_eq))
      sy = sy_0 + iso_Q - iso_D
C            
      DO i = 1, n_backstresses
        c_k = chab_coef(i, 1)
        gamma_k = chab_coef(i, 2)
        alpha_k(i) = SIGN(ONE, yield_radius) * c_k / gamma_k -
     1  (SIGN(ONE, yield_radius) * c_k / gamma_k - alpha_k_init(i)) *
     2  EXP(-gamma_k * (ep_eq - ep_eq_init))
      END DO
      alpha = SUM(alpha_k(:))  ! don't put in the loop since will change the SIGN
C            
      yield_radius = sigma - alpha
      phi = yield_radius ** 2 - sy ** 2
C
      ! Check convergence
      IF (ABS(phi) .LT. TOL) THEN
        is_converged = 1
      END IF
      END DO
C      
      ! Set the stress and tangent stiffness (Jacobian)
      DO j = 1, ntens
        DO i = 1, ntens
          ddsdde(i, j) = 0.
        END DO
      END DO
      ! Condition of plastic loading is determined by whether or not iterations were required
      IF (it_num .EQ. 0) THEN
        ddsdde(1, 1) = elastic_modulus
      ELSE
        A_term =  b * (Q - iso_Q) - a * (D - iso_D)
        DO i = 1, n_backstresses
          c_k = chab_coef(i, 1)
          gamma_k = chab_coef(i, 2)
          A_term = A_term +
     1    gamma_k * (c_k/gamma_k-SIGN(ONE, yield_radius)*alpha_k(i))
        END DO
        ddsdde(1, 1) = (elastic_modulus * A_term) /
     1  (elastic_modulus + A_term)
      END IF
      stress(1) = sigma
C
      ! Update the state variables
      statev(1) = ep_eq
      DO i = 1, n_backstresses
        statev(1 + i) = alpha_k(i)
      END DO
C      
      IF (it_num .EQ. MAX_ITERATIONS) THEN
        PRINT *, "WARNING: Return mapping in integration point ", npt,
     1  " of element ", noel, " did not converge."
        PRINT *, "Reducing time increment to 1/4 of current value."
        PNEWDT = 0.25
      END IF

      RETURN
      
C-----------------------------------------------------------------------C
C     
      ! Initial stress functions
C
C-----------------------------------------------------------------------C
      contains
      
      FUNCTION resid_parab(loc, s_tip, s_cent, l) result(rsi)
        ! Returns the value on the residual stress parabola.
        real(8), intent(in) :: loc, s_tip, s_cent, l
        real(8) :: rsi, a_rs
        a_rs = (s_tip - s_cent) * (2. / l) ** 2
        rsi = a_rs * loc ** 2 + s_cent
      END function
      
      FUNCTION resid_npt(npt_i, geom_params) result(rs)
        ! Returns the residual stress value at the integration point.
        integer, intent(in) :: npt_i
        real(8), intent(in) :: geom_params(:)
        real(8) :: rs
        ! Internal
        real(8) :: dcl, bf, tf, tw, s_f, s_w, s_fw, st, sc, lp
        real(8) :: div_l, xa, xb, xm, f_xa, f_xb, f_xm, aW_aF
        integer :: npt_flange, npt_web, web_npt_min, web_npt_max
        
        ! Extract geom
        dcl = geom_params(1)
        bf = geom_params(2)
        tf = geom_params(3)
        tw = geom_params(4)
        ! Extract int pt info
        npt_flange = geom_params(5)
        npt_web = geom_params(6)
        web_npt_min = npt_flange + 1
        web_npt_max = npt_flange + npt_web - 2
        
        ! Determine the location of the integration points and stress properties
        aW_aF = dcl * tw / (2. * bf * tf)
        s_f = -165. * (1. - aW_aF / 1.2)
        s_fw = 100. * (0.7 + aW_aF)
        s_w = -100. * (1.5 + aW_aF / 1.2)
        ! s_w = -203.75 + 12.5 / aW_aF - 50. * aW_aF
        IF (npt_i < web_npt_min) THEN
          ! Bottom flange
          st = s_f
          sc = s_fw
          lp = bf
          div_l = lp / (npt_flange - 1)
          xa = -bf/2. + div_l * (npt_i - 1)
        ELSE IF (npt_i > web_npt_max) THEN
          ! Top flange
          st = s_f
          sc = s_fw
          lp = bf
          div_l = lp / (npt_flange - 1)
          xa = -bf/2. + div_l * (npt_i - web_npt_max - 1)
        ELSE
          ! Web
          st = s_fw
          sc = s_w
          lp = dcl
          div_l = lp / (npt_web - 1)
          xa = -dcl/2. + div_l * (npt_i - web_npt_min + 1)
        END IF
        ! Compute the stress
        rs = resid_parab(xa, st, sc, lp)
        
        ! print *, 'npt = ', npt_i, 'rs = ', rs
        
      END function
      
      END
