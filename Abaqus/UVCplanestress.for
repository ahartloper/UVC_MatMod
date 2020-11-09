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
      INTEGER :: i, j, n_backstresses, it_num, converged
      ! Material properties
      REAL(8) :: elastic_modulus, Q_inf, b, D_inf, a,
     1shear_modulus, bulk_modulus, poisson_ratio, mu2
      ! Used for intermediate calculations
      REAL(8) :: yield_stress, ep_eq, ep_eq_init,
     1hard_iso_Q, hard_iso_D, hard_iso_total, isotropic_modulus,
     2yield_function, e_k, beta, gamma_denom, denom,
     3yield_condition, f_bar, f_bar_2, beta_prime, consist_param
      ! Tangent modulus
      REAL(8), DIMENSION(3) :: n_tilde, n_hat, H_prime, H_tilde
      REAL(8), DIMENSION(3,3) :: H_out_n, n_out_n, A_mat,
     1Xi_tilde, Xi_tilde_A, C_ep
      REAL(8) :: theta_1, theta_2
      ! Backstress arrays
      REAL(8), DIMENSION(:, :), ALLOCATABLE :: alpha_k, alpha_k_i
      REAL(8), DIMENSION(:), ALLOCATABLE :: C_k, gamma_k
      ! Tensors
      REAL(8), DIMENSION(3) :: strain_plastic, dstran_p,
     1alpha, gamma_diag, alpha_tilde,alpha_tilde_prime,
     2strain, eta, eta_tilde, eta_trial, stress_trial,
     3gamma_prime_diag, stress_rel
      REAL(8), DIMENSION(3, 3) :: Q_mat, Q_mat_t, Lambda_P, Lambda_C,
     1elastic_matrix, P_mat, compliance_matrix, ID3
      ! Parameters
      INTEGER :: N_BASIC_PROPS, TERM_PER_BACK, MAX_ITERATIONS,
     1I_ALPHA
      REAL(8) :: TOL, ONE, TWO, THREE, ZERO, SQRT23, SIX
      PARAMETER(TOL=1.0D-8,
     1N_BASIC_PROPS=7, TERM_PER_BACK=2, MAX_ITERATIONS=1000,
     2ONE=1.0D0, TWO=2.0D0, THREE=3.0D0, ZERO=0.D0, SIX=6.0D0,
     3SQRT23=SQRT(2.0D0/3.0D0), I_ALPHA=4)
C ----------------------------------------------------------------------C
C
      ! Initialize
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
      ALLOCATE(alpha_k_i(n_backstresses, ntens))
C
      ! Material Properties
      elastic_modulus = props(1)
      poisson_ratio = props(2)
      yield_stress = props(3)
      q_inf = props(4)
      b = props(5)
      d_inf = props(6)
      a = props(7)
      DO i = 1, n_backstresses  ! First backstress starts at index = 8
        c_k(i) = props((N_BASIC_PROPS - 1) + 2 * i)
        gamma_k(i) = props(N_BASIC_PROPS + 2 * i)
      END DO
      shear_modulus = elastic_modulus / (TWO * (ONE + poisson_ratio))
      mu2 = TWO * shear_modulus
C
      ! Elastic matrix (defined in column major)
      elastic_matrix = RESHAPE((/ ONE, poisson_ratio, ZERO,
     1poisson_ratio, ONE, ZERO,
     2ZERO, ZERO, (ONE-poisson_ratio) / TWO /),
     3SHAPE(elastic_matrix))
      elastic_matrix = elastic_modulus / (ONE - poisson_ratio ** 2) * 
     1elastic_matrix
      ! Compliance matrix
      compliance_matrix = RESHAPE((/ ONE, -poisson_ratio, ZERO,
     1-poisson_ratio, ONE, ZERO,
     2ZERO, ZERO, TWO*(ONE + poisson_ratio)/), SHAPE(compliance_matrix))
      compliance_matrix = ONE/elastic_modulus * compliance_matrix
C
      gamma_diag(:) = ZERO
      Q_mat = RESHAPE((/ ONE, ONE, ZERO, -ONE, ONE, ZERO,
     1ZERO, ZERO, SQRT(TWO) /), SHAPE(Q_mat))
      Q_mat = ONE / SQRT(TWO) * Q_mat
      Q_mat_t = TRANSPOSE(Q_mat)
C
      P_mat = RESHAPE((/ TWO, -ONE, ZERO, -ONE, TWO, ZERO,
     1ZERO, ZERO, SIX /), SHAPE(P_mat))
      P_mat = ONE / THREE * P_mat
C
      Lambda_P(:, :) = ZERO
      Lambda_P(1, 1) = ONE/THREE
      Lambda_P(2, 2) = ONE
      Lambda_P(3, 3) = TWO
C
      Lambda_C(:, :) = ZERO
      Lambda_C(1, 1) = elastic_modulus / (ONE - poisson_ratio)
      Lambda_C(2, 2) = mu2
      Lambda_C(3, 3) = shear_modulus
C
      ! Read state variables
      ep_eq = statev(1)
      ep_eq_init = statev(1)
      CALL ROTSIG(statev(2), drot, strain_plastic, 2, ndi, nshr)
      alpha(:) = ZERO
      DO i = 1, n_backstresses
        DO j = 1, ntens
        CALL ROTSIG(statev(I_ALPHA+1 + (i-1) * ntens), drot,
     1  alpha_k(i, :), 1, ndi, nshr)
        END DO
        alpha = alpha + alpha_k(i, :)
      END DO
      alpha_k_i = alpha_k
C
      ID3(:, :) = ZERO
      DO i = 1, NTENS
        ID3(i, i) = ONE
      END DO
C ----------------------------------------------------------------------C
C
      ! Elastic trial step
C
C ----------------------------------------------------------------------C
      ! Don't include e33, and calculate it later
      strain = stran + dstran
      stress_trial = stress + MATMUL(elastic_matrix, dstran)
      !stress_trial = MATMUL(elastic_matrix, strain - strain_plastic)
C
      hard_iso_Q = q_inf * (ONE - EXP(-b * ep_eq))
      hard_iso_D = d_inf * (ONE - EXP(-a * ep_eq))
      hard_iso_total = yield_stress + hard_iso_Q - hard_iso_D
C
      eta_trial = MATMUL(Q_mat_t, stress_trial - alpha)
      eta = eta_trial
C
      f_bar_2 = ZERO
      DO i = 1, ntens
        f_bar_2 = f_bar_2 + eta_trial(i)**2 * Lambda_P(i, i)
      END DO
      yield_condition = ONE/TWO*f_bar_2 - ONE/THREE*hard_iso_total**2
C
      IF (yield_condition .LT. TOL) THEN
        converged = 1
      ELSE
        converged = 0
      END IF
C ----------------------------------------------------------------------C
C
      ! Return mapping in plane-stress subspace if plastic loading
C
C ----------------------------------------------------------------------C
      ! Calculate the consitency parameter
      it_num = 0
      consist_param = ZERO
      DO WHILE ((converged .EQ. 0) .AND. (it_num .LT. MAX_ITERATIONS))
        it_num = it_num + 1
C
        ! Calculate the isotropic hardening parameters
        hard_iso_Q = q_inf * (ONE - EXP(-b * ep_eq))
        hard_iso_D = d_inf * (ONE - EXP(-a * ep_eq))
        hard_iso_total = yield_stress + hard_iso_Q - hard_iso_D
        isotropic_modulus = b*(q_inf-hard_iso_Q) - a*(d_inf-hard_iso_D)
C
        ! Calculate the kinematic hardening related parameters
        beta = ZERO
        alpha_tilde(:) = ZERO
        DO i = 1, n_backstresses
          e_k = EXP(-gamma_k(i) * (ep_eq - ep_eq_init))
          beta = beta + C_k(i) / gamma_k(i) * (ONE - e_k)
          DO j = 1, ntens
            alpha_tilde(j) = alpha_tilde(j) + alpha_k(i, j) * e_k
          END DO
        END DO
        alpha_tilde = alpha - alpha_tilde
        beta = ONE + beta / hard_iso_total
C        
        ! Update the relative stress and eta
        gamma_denom = beta + mu2 * consist_param
        gamma_diag(1) = ONE / (beta
     1  + consist_param*elastic_modulus / (THREE*(ONE - poisson_ratio)))
        gamma_diag(2) = ONE / gamma_denom
        gamma_diag(3) = ONE / gamma_denom
        eta_tilde = eta_trial + MATMUL(Q_mat_t, alpha_tilde)
        eta = gamma_diag * eta_tilde
C
        ! Calculate the norm of the relative stress
        f_bar_2 = ONE/THREE * eta(1)**2 + eta(2)**2 + TWO * eta(3)**2
        f_bar = SQRT(f_bar_2)
C
        beta_prime = ZERO
        alpha_tilde_prime(:) = ZERO
        DO i = 1, n_backstresses
          e_k = EXP(-gamma_k(i) * (ep_eq - ep_eq_init))
          beta_prime = beta_prime
     1    - C_k(i) * isotropic_modulus
     2    / (gamma_k(i) * hard_iso_total**2) * (ONE - e_k)
     3    + C_k(i) * e_k / hard_iso_total
          DO j = 1, ntens
            alpha_tilde_prime(j) = alpha_tilde_prime(j)
     1      + gamma_k(i) * e_k * alpha_k(i, j)
          END DO
        END DO
        beta_prime = SQRT23 * f_bar * beta_prime
        alpha_tilde_prime = SQRT23 * f_bar * alpha_tilde_prime
        DO i = 1, ntens
          gamma_prime_diag(i) = -gamma_diag(i) ** 2
     1    * (beta_prime + Lambda_P(i, i) * Lambda_C(i, i))
        END DO

        yield_condition = ONE/TWO*f_bar_2 - ONE/THREE*hard_iso_total**2
        denom = DOT_PRODUCT( MATMUL(Lambda_P, eta),
     1  gamma_prime_diag * eta_tilde
     2  + gamma_diag * MATMUL(Q_mat_t, alpha_tilde_prime) )
     3  - SQRT23*TWO/THREE * hard_iso_total * isotropic_modulus * f_bar
C
        consist_param = consist_param - yield_condition / denom
        ep_eq = ep_eq_init + SQRT23 * consist_param * f_bar
C
        IF (ABS(yield_condition) .LT. TOL) THEN
          converged = 1
        END IF
      END DO
C ----------------------------------------------------------------------C
C
      ! Update variables
C
C ----------------------------------------------------------------------C
      IF (it_num .EQ. 0) THEN  ! Elastic loading
        stress = stress_trial
        ddsdde = elastic_matrix
      ELSE  ! Plastic loading
        eta_tilde = eta_trial + MATMUL(Q_mat_t, alpha_tilde)
        eta = gamma_diag * eta_tilde
        stress_rel = MATMUL(Q_mat, eta)
C
        hard_iso_Q = q_inf * (ONE - EXP(-b * ep_eq))
        hard_iso_D = d_inf * (ONE - EXP(-a * ep_eq))
        hard_iso_total = yield_stress + hard_iso_Q - hard_iso_D
        DO i = 1, n_backstresses
          e_k = EXP(-gamma_k(i) * (ep_eq - ep_eq_init))
          DO j = 1, ntens
            alpha_k(i, j) = alpha_k(i, j) * e_k +
     1      stress_rel(j)/hard_iso_total * c_k(i)/gamma_k(i)*(ONE-e_k)
          END DO
        END DO
C        strain_plastic = strain_plastic
C     1  + consist_param * MATMUL(P_mat, stress_rel)
C        stress = MATMUL(elastic_matrix, strain - strain_plastic)
        dstran_p = consist_param * MATMUL(P_mat, stress_rel)
        strain_plastic = strain_plastic + dstran_p
        stress = stress_trial - MATMUL(elastic_matrix, dstran_p)
      END IF
C ----------------------------------------------------------------------C
C
      ! Compute consistent tangent moduli
C
C ----------------------------------------------------------------------C
      IF (it_num .GT. 0) THEN
        n_hat = stress_rel / f_bar
        H_prime =-(beta-ONE)*isotropic_modulus*stress_rel/hard_iso_total
        DO i = 1, n_backstresses
          e_k = EXP(-gamma_k(i) * (ep_eq - ep_eq_init))
          H_prime = H_prime + C_k(i) * e_k * stress_rel / hard_iso_total
     1    - gamma_k(i) * e_k * alpha_k_i(i, :)
        END DO
        H_prime = SQRT23 * H_prime
        DO i = 1, NTENS
          DO j = 1, NTENS
            H_out_n(i, j) = H_prime(i) * n_hat(j)
          END DO
        END DO
        A_mat = beta * ID3 + consist_param * MATMUL(H_out_n, P_mat)
        A_mat = matinv3(A_mat)
C
        n_tilde = n_hat - consist_param * MATMUL(A_mat, H_prime)
        Xi_tilde = compliance_matrix+consist_param*MATMUL(P_mat, A_mat)
        Xi_tilde = matinv3(Xi_tilde)
        Xi_tilde_A = MATMUL(A_mat, Xi_tilde)
C
        theta_2 = ONE - TWO/THREE * isotropic_modulus * consist_param
        H_tilde = H_prime + MATMUL(Xi_tilde, MATMUL(P_mat, n_tilde))
        theta_1 = TWO/THREE * isotropic_modulus
     1  + theta_2*DOT_PRODUCT(n_hat,MATMUL(P_mat,MATMUL(A_mat,H_tilde)))
        DO i = 1, NTENS
          DO j = 1, NTENS
            n_out_n(i, j) = n_tilde(i) * n_hat(j)
          END DO
        END DO

        C_ep = Xi_tilde - theta_2 / theta_1 *
     1  MATMUL(Xi_tilde,
     2  MATMUL(P_mat,
     3  MATMUL(n_out_n,
     4  MATMUL(P_mat, Xi_tilde_A))))
C
        ddsdde = ONE/TWO * (C_ep + TRANSPOSE(C_ep))
        END IF
C ----------------------------------------------------------------------C
C
      ! Update the state variables
C
C ----------------------------------------------------------------------C
      statev(1) = ep_eq
      statev(2:(1+ntens)) = strain_plastic
      DO i = 1, n_backstresses
        DO j = 1, ntens
          statev(I_ALPHA + (i-1) * ntens + j) = alpha_k(i, j)
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
      ! Define matrix inverse
C
C ----------------------------------------------------------------------C
      ! From: http://fortranwiki.org/fortran/show/Matrix+inversion
      pure function matinv3(A) result(B)
      !! Performs a direct calculation of the inverse of a 3×3 matrix.
      REAL(8), intent(in) :: A(3,3)   !! Matrix
      REAL(8)             :: B(3,3)   !! Inverse matrix
      REAL(8)             :: detinv

      ! Calculate the inverse determinant of the matrix
      detinv = 1.D0 / (A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)
     1- A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)
     2+ A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

      ! Calculate the inverse of the matrix
      B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
      B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
      B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
      B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
      B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
      B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
      B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
      B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
      B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
      end function
      END