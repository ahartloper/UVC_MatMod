# Fortran User Materials for the UVC Material Model

This readme file outlines the usage of the UVC material model in Abaqus (previously RESSForLab).

## Installation

### Prerequisites

- Compatible version of Abaqus linked with the Fortran compiler
- Local copy of the user subroutine file of interest (i.e., the files included in this repository)

Note: all testing for the UMATs have been done with Abaqus v6.14, the Intel Fortran compiler (ifort), and Visual Studio Community 2013.
The user is responsible for correctly linking Abaqus with a compatible Fortran compiler.

### Obtaining the UMAT files 

The user material subroutine (UMAT) files simply need to be downloaded and made available to the Abaqus model of interest.
All the relevant steps are described in the Usage section.
The UMAT files can be downloaded individually from github, or the entire repository can be cloned using git using the command line.

The git command to clone the repository is:
```
git clone https://github.com/ahartloper/UVC_MatMod.git
```
Alternatively ssh can be used instead of https when cloning the repository.
Use of the UMAT files are described in the next section.

## Usage

This section outlines the usage with Abaqus CAE.
Usage directly with an input file is similar and the reader is instructed to \*USER MATERIAL of the Abaqus Keyword Reference Guide for more information.
Usage is divided into parts depending on the UMAT file used, generally the steps are: 1) collect the model parameters, 2) set-up the user material, 3) optionally specify the transverse shear stiffness and the hourglass control, 4) make the UMAT file available to Abaqus.
The number of parameters to be specified depends on whether you are using the uniaxial version, or the plane-stress/multiaxial version.
Enhanced hourglass control is recommended for reduced integration elements, and the transverse shear stiffness needs to be calculated and specified if shell elements are used.
Details of how to calculate and specify the transverse shear stiffness for shell elements are summarized under the UVCplanestress section.


### UVCuniaxial

This UMAT (UVCuniaxial.for) should be used in elements with uniaxial stress states (e.g., beam elements).

#### Parameters
- E: Young's modulus of the material
- sy0: Initial yield stress
- QInf: Maximum increase in yield stress due to cyclic hardening at model saturation 
- b: Saturation rate of QInf
- DInf: Maximum initial reduction in yield surface for materials with discontinuous yielding (set to 0.0 to neglect this effect)
- a: Saturation rate of DInf, this parameter should be non-zero
- C1: Increase in stress due to kinematic hardening at saturation for backstress 1
- gamma1: Rate term for backstress 1
- [C2 gamma2 C3 gamma3 ... CN gammaN] Additional backstress parameters, if CK is specified then the corresponding gammaK must also be specified. 
N is the total number of backstress in the model.

#### Set-up the user defined material

- Create a user defined material, specify that the type is "Mechanical":
```
General > User Material
```
- Set the number of parameters to be 8 + 2(N - 1).
For example, there should be 10 parameters for 2 backstresses.
- Enter the parameters following the order listed in this Parameters section
- Assign space for the internal variables:
```
General > Depvar
```
Set the "Number of solution-dependent state variables" to 1 + N.
For example, for two backstresses the value should be 3.

Now the user material can be assigned to sections as any material included with Abaqus.

#### Make the UMAT available to Abaqus

Now that the user material and model is properly set-up, we just need to make the UMAT file available to Abaqus.
- Specify the UMAT file
```
Double-click on the associated Job > General tab > Locate the User subroutine file
```

### UVCplanestress

This UMAT (UVCplanestress.for) should be used in elements with plane-stress stress states (e.g., shell elements).

#### Parameters
- E: Young's modulus of the material
- nu: Poisson's ratio
- sy0: Initial yield stress
- QInf: Maximum increase in yield stress due to cyclic hardening at model saturation 
- b: Saturation rate of QInf
- DInf: Maximum initial reduction in yield surface for materials with discontinuous yielding (set to 0.0 to neglect this effect)
- a: Saturation rate of DInf, this parameter should be non-zero
- C1: Increase in stress due to kinematic hardening at saturation for backstress 1
- gamma1: Rate term for backstress 1
- [C2 gamma2 C3 gamma3 ... CN gammaN] Optional, additional backstress parameters, if CK is specified then the corresponding gammaK must also be specified.
N is the total number of backstress in the model.

#### Set-up the user defined material

- Create a user defined material, specify that the type is "Mechanical":
```
General > User Material
```
- Set the number of parameters to be 9 + 2(N - 1).
For example, there should be 11 parameters for 2 backstresses.
- Enter the parameters following the order listed in this Parameters section
- Assign space for the internal variables:
```
General > Depvar
```
Set the "Number of solution-dependent state variables" to 4 + 3N.
For example, for two backstresses the value should be 10.

Now the user material can be assigned to sections as any material included with Abaqus.
For shell elements, the transverse shear stiffness needs to be assigned to the section as described in the next section.

#### Transverse shear stiffness

The transverse shear stiffness needs to be assigned to shell elements when UMATs are used for these sections.
- Calculate the transverse shear stiffness value according to
```
K11 = K22 = 5/6 G * t
```
where G = E / (2(1 + nu)) is the shear modulus and t is the shell section thickness, and
``` 
K12 = 0
```
- Set the transverse shear stiffness as follows:
```
Sections > Edit Section > Advanced tab > check Specify Values under Transverse Shear Stiffnesses
```
- Set the values in the boxes according to the calculated values

#### Specify enhanced hourglass control

We recommend that enhanced hourglass control is specified for reduced integration elements (e.g., S4R, C3D8R).
If you are not using reduced integration elements this section can be ignored.

- Specify enhanced hourglass control for all elements that will be assigned the UMAT
```
Go to Element Type > choose Hourglass Control > check Enhanced
```

#### Make the UMAT available to Abaqus

Follow the directions under Make the UMAT available to Abaqus section under UVCuniaxial.

### UVCmultiaxial

This UMAT (UVCmultiaxial.for) should be used with solid elements.

#### Parameters

Follow the instructions under the Parameters section under UVCplanestress.

#### Set-up the user defined material

- Create a user defined material, specify that the type is "Mechanical":
```
General > User Material
```
- Set the number of parameters to be 9 + 2(N - 1).
For example, there should be 11 parameters for 2 backstresses.
- Enter the parameters following the order listed in this Parameters section
- Assign space for the internal variables:
```
General > Depvar
```
Set the "Number of solution-dependent state variables" to 7 + 6N.
For example, for two backstresses the value should be 19.

Now the user material can be assigned to sections as any material included with Abaqus.


#### Specify enhanced hourglass control
 
Follow the instructions under the Specify enhanced hourglass control section under UVCplanestress.


#### Make the UMAT available to Abaqus

Follow the directions under Make the UMAT available to Abaqus section under UVCuniaxial.
