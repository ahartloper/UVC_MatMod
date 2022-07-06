# Tests for the OpenSees Implementations

Five tests are provided for the UVC OpenSees material models.
The tests all contains a single element subjected to cyclic force boundary conditions.
Evaluation of the tests can be done by comparing the stress-strain response in a single element between the equivalent models with the Abaqus response in `Abaqus/testing`.

The material properties used in all cases are:
E = 170800.0, nu = 0.3, sy0 = 318.5, QInfty = 100.7, b = 8.0, DInfy = 0., a = 200.0, C1 = 11608.2, gamma1 = 145.2, C2 = 1026.3, gamma2 = 4.7.

## Running the tests

First the user must create the directory `Output_Data/` in the `OpenSees/testing` directory.
This directory will contain all the output from the test files.

Each of the tests are associated with a particular material implementation, the correspondance is:
- 1d_Truss_test.tcl = UVCuniaxial.for
- 2d_Shell_test.tcl = UVCplanestress.for
- 2d_Shell_test_biaxial.tcl = UVCplanestress.for
- 2d_Shell_test_shear.tcl = UVCplanestress.for
- 3d_Brick_test.tcl = UVCmultiaxial.for
After the `Data_Output` directory has been created, the tests can be run from the command line using
```
opensees [file]
```
where `file` corresponds to one of the OpenSees models above.
