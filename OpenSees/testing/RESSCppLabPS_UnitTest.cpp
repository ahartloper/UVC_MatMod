#include "CppUnitTest.h"
#include <vector>
#include <iostream>
#include <sstream>
#include <fstream>
#include "Vector.h"
#include "Matrix.h"

#include "../RESSCppLab/UVCps.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace plane_stress_unit_tests
{
  const unsigned int NDIM = 3;  // matrix and vector size
  const double TEST_TOLERANCE_PCT = 0.1;

  // Create the material to test
  // DInf = 0, "Chaboche" model w/ two backstresses
  int tag = 2;
  double E = 179800.0;
  double nu = 0.3;
  double sy0 = 318.5;
  double Q_inf = 100.7;
  double b = 8.;
  double D_inf = 0.;
  double a = 200.;
  std::vector<double> c_k = { 11608.2, 1026.0 };
  std::vector<double> gamma_k = { 145.2, 4.7 };
  UVCps mat_orig(tag, E, nu, sy0, Q_inf, b,
    D_inf, a,
    c_k, gamma_k);

  TEST_CLASS(RESSCppLabPS_UnitTest)
  {
  public:
    /* ------------------------------------------------------------------------------------------------------------- */
    TEST_METHOD(TestPSSetGetStrain)
    {
      std::vector<double> materialStrain = { 0., 0., 0. };
      Vector strain = Vector(NDIM);
      std::vector<double> strainInput = { 0.005, -0.0015, 0.0 };
      for (unsigned int i = 0; i < NDIM; ++i) {
        strain(i) = strainInput[i];
      }

      mat_orig.setTrialStrain(strain);
      for (unsigned int i = 0; i < NDIM; ++i) {
        materialStrain[i] = mat_orig.getStrain()(i);
        Assert::AreEqual(materialStrain[i], strainInput[i]);
      }
    }  // end TEST_METHOD
    /* ------------------------------------------------------------------------------------------------------------- */
    TEST_METHOD(TestPSElasticStep)
    {
      mat_orig.revertToStart();
      std::ostringstream os;

      Vector strain = Vector(NDIM);
      std::vector<double> materialStress = { 0., 0., 0. };

      std::vector<double> stressCompare = { 89.9, 0., 0. };
      std::vector<double> strainInput = { 0.0005, -0.00015, 0.0 };
      for (unsigned int i = 0; i < NDIM; ++i) {
        strain(i) = strainInput[i];
      }
      mat_orig.setTrialStrain(strain);
      for (unsigned int i = 0; i < NDIM; ++i) {
        materialStress[i] = mat_orig.getStress()(i);
        // os << materialStress[i] << " " << stressCompare[i] << std::endl;
      }

      // Calculate the norm error
      double norm_diff = 0.;
      double norm_denom = 0.;
      for (int i = 0; i < materialStress.size(); ++i) {
        norm_diff += pow(materialStress[i] - stressCompare[i], 2);
        norm_denom += pow(stressCompare[i], 2);
      }
      norm_diff = sqrt(norm_diff);
      norm_denom = sqrt(norm_denom);
      double norm_error = norm_diff / norm_denom * 100.0;
      os << "Norm error [%]: " << norm_error << std::endl;
      Assert::AreEqual(norm_error, 0., TEST_TOLERANCE_PCT);

      // ---------------- Increment 2 ----------------
      mat_orig.commitState();
      mat_orig.commitState();
      stressCompare = { 89.9, 0., 34.57692 };
      strainInput = { 0.0005, -0.00015, 0.0005 };
      for (unsigned int i = 0; i < NDIM; ++i) {
        strain(i) = strainInput[i];
      }

      mat_orig.setTrialStrain(strain);
      for (unsigned int i = 0; i < NDIM; ++i) {
        materialStress[i] = mat_orig.getStress()(i);
        // os << materialStress[i] << " " << stressCompare[i] << std::endl;
      }

      // Calculate the norm error
      norm_diff = 0.;
      norm_denom = 0.;
      for (int i = 0; i < materialStress.size(); ++i) {
        norm_diff += pow(materialStress[i] - stressCompare[i], 2);
        norm_denom += pow(stressCompare[i], 2);
      }
      norm_diff = sqrt(norm_diff);
      norm_denom = sqrt(norm_denom);
      norm_error = norm_diff / norm_denom * 100.0;
      os << "Norm error [%]: " << norm_error << std::endl;
      //Assert::AreEqual(norm_error, 0., TEST_TOLERANCE_PCT);


      // Use this for writing output
      std::string out_string;
      out_string = os.str();
      const char* message = out_string.c_str();
      Logger::WriteMessage(message);
    }  // end TEST_METHOD
    /* ------------------------------------------------------------------------------------------------------------- */
    TEST_METHOD(TestPSReturnMap)
    {
      mat_orig.revertToStart();


      std::ostringstream os;
      Vector strain = Vector(NDIM);
      std::vector<double> materialStress = { 0., 0., 0.};
      // Test a few increments, validation data from Abaqus biaxial loading, combined kin+iso

      // ---------------- Increment 1 ----------------
      std::vector<double> stressCompare = { 304.23, 344.99, 79.857 };
      std::vector<double> strainInput = { 0.0023295, 0.0031873, 0.0033609 };
      for (unsigned int i = 0; i < NDIM; ++i) {
        strain(i) = strainInput[i];
      }
      mat_orig.setTrialStrain(strain);
      for (unsigned int i = 0; i < NDIM; ++i) {
        materialStress[i] = mat_orig.getStress()(i);
        //os << materialStress[i] << " " << stressCompare[i] << std::endl;
      }

      // Calculate the norm error
      double norm_diff = 0.;
      double norm_denom = 0.;
      for (int i = 0; i < materialStress.size(); ++i) {
        norm_diff += pow(materialStress[i] - stressCompare[i], 2);
        norm_denom += pow(stressCompare[i], 2);
      }
      norm_diff = sqrt(norm_diff);
      norm_denom = sqrt(norm_denom);
      double norm_error = norm_diff / norm_denom * 100.0;
      os << "Norm error [%]: " << norm_error << std::endl;
      Assert::AreEqual(norm_error, 0., TEST_TOLERANCE_PCT);

      
      // ---------------- Increment 2 ----------------
      mat_orig.commitState();
      stressCompare = { 339.83, 382.96, 84.503 };
      strainInput = { 0.0045493, 0.0063311, 0.0069814 };
      for (unsigned int i = 0; i < NDIM; ++i) {
        strain(i) = strainInput[i];
      }

      mat_orig.setTrialStrain(strain);
      for (unsigned int i = 0; i < NDIM; ++i) {
        materialStress[i] = mat_orig.getStress()(i);
        // os << materialStress[i] << " " << stressCompare[i] << std::endl;
      }

      // Calculate the norm error
      norm_diff = 0.;
      norm_denom = 0.;
      for (int i = 0; i < materialStress.size(); ++i) {
        norm_diff += pow(materialStress[i] - stressCompare[i], 2);
        norm_denom += pow(stressCompare[i], 2);
      }
      norm_diff = sqrt(norm_diff);
      norm_denom = sqrt(norm_denom);
      norm_error = norm_diff / norm_denom * 100.0;
      os << "Norm error [%]: " << norm_error << std::endl;
      Assert::AreEqual(norm_error, 0., TEST_TOLERANCE_PCT);
      
      // ---------------- Increment 3 ----------------
      mat_orig.commitState();
      stressCompare = { 360.08, 405.07, 88.139 };
      strainInput = { 0.0067642, 0.009473, 0.010614 };
      for (unsigned int i = 0; i < NDIM; ++i) {
        strain(i) = strainInput[i];
      }

      mat_orig.setTrialStrain(strain);
      for (unsigned int i = 0; i < NDIM; ++i) {
        materialStress[i] = mat_orig.getStress()(i);
        // os << materialStress[i] << " " << stressCompare[i] << std::endl;
      }

      // Calculate the norm error
      norm_diff = 0.;
      norm_denom = 0.;
      for (int i = 0; i < materialStress.size(); ++i) {
        norm_diff += pow(materialStress[i] - stressCompare[i], 2);
        norm_denom += pow(stressCompare[i], 2);
      }
      norm_diff = sqrt(norm_diff);
      norm_denom = sqrt(norm_denom);
      norm_error = norm_diff / norm_denom * 100.0;
      os << "Norm error [%]: " << norm_error << std::endl;
      Assert::AreEqual(norm_error, 0., TEST_TOLERANCE_PCT);

      
      // Use this for writing output
      std::string out_string;
      out_string = os.str();
      const char* message = out_string.c_str();
      Logger::WriteMessage(message);
    }  // end TEST_METHOD
    /* ------------------------------------------------------------------------------------------------------------- */


  };  // end test class
}  // end namespace