// #include "stdafx.h"
#include "CppUnitTest.h"
#include <vector>
#include <iostream>
#include <sstream>
#include <fstream>

#include "../../SRC/material/uniaxial/UVCua.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace updated_model_tests
{

  // Create the material to test
  // Similar to the material in the other model, however sy0 = 368.5 and 
  // D_inf = 50.
  int tag = 2;
  double E = 179800.0;
  double sy0 = 368.5;
  double Q_inf = 100.7;
  double b = 8.;
  double D_inf = 50.;
  double a = 200.;
  std::vector<double> c_k = { 11608.2, 1026.0 };
  std::vector<double> gamma_k = { 145.2, 4.7 };
  UVCua ress_mat(tag, E, sy0, Q_inf, b,
    D_inf, a,
    c_k, gamma_k);



	TEST_CLASS(RESSCppLab_Updated_UnitTest)
	{
	public:

    /* ---------------------------------------------------------------------- */
    TEST_METHOD(TestGetters)
    {
      Assert::AreEqual(ress_mat.getStrain(), 0.);
      Assert::AreEqual(ress_mat.getStress(), 0.);
      Assert::AreEqual(ress_mat.getTangent(), E);
      Assert::AreEqual(ress_mat.getInitialTangent(), E);
    }
		
    /* ---------------------------------------------------------------------- */
    /*
    Load path: (0.002, 0.01, 0.05, -0.05, 0.10, -0.10).
    Validation values obtained from RESSPyLab.
    */
    /* ---------------------------------------------------------------------- */
    TEST_METHOD(TestUpdatedReturnMappingCyclic)
    {
      ress_mat.revertToStart();
      double test_delta = 10.0e-8;
      std::ostringstream os;
      double strain_inc;

      // First point (elastic)
      ress_mat.setTrialStrain(0.002);
      double val1 = 0.002 * E;
      Assert::AreEqual(ress_mat.getStress(), val1, test_delta);
      Assert::AreEqual(ress_mat.getStrain(), 0.002);
      os << "Increment 1" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;

      // Second point
      ress_mat.commitState();
      ress_mat.setTrialStrain(0.01);
      Assert::AreEqual(ress_mat.getStress(), 397.1140732828134, test_delta);
      Assert::AreEqual(ress_mat.getStrain(), 0.01);
      os << "Increment 1" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;
      
      // Third point
      ress_mat.commitState();
      ress_mat.setTrialStrain(0.05);
      Assert::AreEqual(ress_mat.getStress(), 473.6963793399446, test_delta);
      Assert::AreEqual(ress_mat.getStrain(), 0.05);
      os << "Increment 1" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;

      // Fourth point
      ress_mat.commitState();
      ress_mat.setTrialStrain(-0.05);
      Assert::AreEqual(ress_mat.getStress(), -517.1107057407197);
      Assert::AreEqual(ress_mat.getStrain(), -0.05);
      os << "Increment 2" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;

      // Fifth point
      ress_mat.commitState();
      ress_mat.setTrialStrain(0.1);
      Assert::AreEqual(ress_mat.getStress(), 570.6470766656312, test_delta);
      Assert::AreEqual(ress_mat.getStrain(), 0.1);
      os << "Increment 3" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;

      // Sixth point
      ress_mat.commitState();
      ress_mat.setTrialStrain(-0.1);
      Assert::AreEqual(ress_mat.getStress(), -594.4426271387101, test_delta);
      Assert::AreEqual(ress_mat.getStrain(), -0.1);
      os << "Increment 4" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;

      // Use this for writing output
      std::string out_string;
      out_string = os.str();
      const char* message = out_string.c_str();
      Logger::WriteMessage(message);
    }

    /* ---------------------------------------------------------------------- */
    /*
    Load path: (0.002, 0.01, 0.05, -0.05, 0.10, -0.10).
    Validation values obtained from RESSPyLab.
    */
    /* ---------------------------------------------------------------------- */
    TEST_METHOD(TestUpdatedStiffness)
    {
      ress_mat.revertToStart();
      double test_delta = 10.0e-8;
      std::ostringstream os;
      double strain_inc;

      // Check initial stiffness
      Assert::AreEqual(ress_mat.getInitialTangent(), E);

      // First (elastic)
      ress_mat.setTrialStrain(0.002);
      Assert::AreEqual(ress_mat.getTangent(), E);

      // Second
      ress_mat.commitState();
      ress_mat.setTrialStrain(0.01);
      Assert::AreEqual(ress_mat.getTangent(), 3323.3895171673266, test_delta);
      os << "Increment 1" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stiffness = " << ress_mat.getTangent() << std::endl;

      // Third
      ress_mat.commitState();
      ress_mat.setTrialStrain(0.05);
      Assert::AreEqual(ress_mat.getTangent(), 1373.3694215338755, test_delta);
      os << "Increment 1" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stiffness = " << ress_mat.getTangent() << std::endl;

      // Fourth
      ress_mat.commitState();
      ress_mat.setTrialStrain(-0.05);
      Assert::AreEqual(ress_mat.getTangent(), 1042.3429672720051, test_delta);
      os << "Increment 2" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stiffness = " << ress_mat.getTangent() << std::endl;

      // Fifth
      ress_mat.commitState();
      ress_mat.setTrialStrain(0.1);
      Assert::AreEqual(ress_mat.getTangent(), 720.8167615153994, test_delta);
      os << "Increment 3" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stiffness = " << ress_mat.getTangent() << std::endl;


      // Sixth point
      ress_mat.commitState();
      ress_mat.setTrialStrain(-0.1);
      Assert::AreEqual(ress_mat.getTangent(), 583.3893181358072, test_delta);
      os << "Increment 4" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stiffness = " << ress_mat.getTangent() << std::endl;

      // Use this for writing output
      std::string out_string;
      out_string = os.str();
      const char* message = out_string.c_str();
      Logger::WriteMessage(message);
    }

	};
}