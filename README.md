# Updated Voce-Chaboche Material Model

Updated Voce-Chaboche (UVC) material model to model mild structural steel materials.

The UVC material model combines the nonlinear kinematic hardening rule of Chaboche with an refined formulation of the nonlinear isotropic hardening law of Voce.
This constitutive model is intended to be used for mild structural steels subjected to cyclic loading.
Notably, accounting for the discontinuous yielding phenomenon is shown to influence the buckling modes of steel wide-flange columns subjected to multi-axis cyclic loading.
Material models are provided for a variety of stress states, the implementations are shown to have similar, if not better, efficiency in terms of model convergence than the built-in nonlinear isotropic/kinematic model in Abaqus.

The theory and implementations of the material model are described in detail in the references below.

## Installation

Instructions for installation are specific to the choice of simulation platform.
Detailed instructions are provided in each of the Abaqus and OpenSees directories. 

## Usage

Specific instructions for use are provided in each of the Abaqus and OpenSees directories.
The examples used to validate the implementations are provided in both the `Abaqus/testing` and `OpenSees/testing` directores.

## Contributing

Bug fixes can be raised by opening a new issue in the UVC_MatMod repository.

## Authors

Code written and maintained by Alex Hartloper (alexander.hartloper@epfl.ch).

## License

This project is licensed under the MIT License - see the `LICENSE` file for details.

## Acknowledgments

- Dimitrios Lignos and Albano de Castro e Sousa for their guidance and assistance in the formulation of the model theory and return mapping algorithms.

## References
[1] Hartloper, de Castro e Sousa, and Lignos (2019). "A Nonlinear Isotropic/Kinematic Hardening Model for Materials with Discontinuous Yielding". Technical Report.

[2] Hartloper, de Castro e Sousa, and Lignos (2019). "Sensitivity of Simulated Steel Column Instabilities to Plasticity Model Assumptions", Proceedings of the 12th Canadian Conference on Earthquake Engineering, Quebec City, QC, Canada
