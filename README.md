# RESSLab Updated Voce-Chaboche Material Model

Updated Voce-Chaboche (UVC) material model user materials for Abaqus to model materials with discontinuous yielding.

The updated Voce-Chaobche material model combines the nonlinear kinematic hardening rule of Chaboche with an updated formulation of the nonlinear isotropic hardening law of Voce.
This constitutive model is intended to be used for mild steels subjected to cyclic loading.
Notably, accounting for the discontinuous yielding phenomenon is shown to influence the buckling modes of steel wide-flange columns subjected to multi-axis cyclic loading.
Three UMAT user subroutine files are provided for use with various element types in Abaqus.
The implemented UMATs are shown to have similar, if not better, efficiency in terms of model convergence than the built-in nonlinear isotropic/kinematic model in Abaqus.
Both the theory and implementation are described in detail in [link report].

## Installation

Instructions for installation are specific to the choice of simulation platform.
Detailed instructions are provided in each of the Abaqus and OpenSees directories. 

## Usage

Specific instructions for use are provided in each of the Abaqus and OpenSees directories.

## Contributing

Bug fixes can be raised by opening a new issue in the RESSForLab repository.

## Authors

Code written and maintained by Alex Hartloper (alexander.hartloper@epfl.ch).

## License

This project is licensed under the MIT License - see the LICENSE.md file for details.

## Acknowledgments

- Dimitrios Lignos and Albano de Castro e Sousa for their guidance and assistance in the formulation of the model theory and return mapping algorithms.
