# Tests for RESSForLab

Three tests are provided for the RESSForLab UMATs, one for each UMAT (stress state).
Each test is divided into two files, one for the UMAT and one for the built-in nonlinear isotropic/kinematic material model.
The tests all contains a single element subjected to cyclic displacement boundary conditions.
Evaluation of the tests is done by comparing the stress-strain response in a single integration point between the equivalent models with the UMAT and with the built-in material model.

The material properties used in all cases are:
E = 170800.0, nu = 0.3, sy0 = 318.5, QInfty = 100.7, b = 8.0, DInfy = 0., a = 200.0, C1 = 11608.2, gamma1 = 145.2, C2 = 1026.3, gamma2 = 4.7.

## Running the tests

Each of the UMAT tests are associated with a particular UMAT, the correspondance is:
- Truss-UMAT-Disp-Cyclic.inp = RESSForLab.for
- Shell-Biaxial-UMAT.inp = RESSForLabPS.for
- Cube_Cyclic_Disp_UMAT.inp = RESSForLabMA.for
The tests can be ran either from Abaqus CAE or from the commandline using
```
abaqus job=[input] user=[umat]
```
where input is the input file (e.g., Truss-No-UMAT-Disp-Cyclic) and user is the corresponding Fortran file (e.g., ../RESSForLab.for) if you are running from the RESSForLab/testing/ directory.
All the UMATs are validated using Abaqus v6.14, with the Intel Fortran Compiler (ifort) 2018.2.185, and Visual Studio Community 2013.
