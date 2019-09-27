#include "CppUnitTest.h"
#include <vector>
#include <iostream>
#include <sstream>
#include <fstream>
#include "Vector.h"
#include "Matrix.h"

#include "../RESSCppLab/UVCma.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace multi_axial_unit_tests
{
  const unsigned int NDIM = 6;  // matrix and vector size
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
  UVCma mat_orig(tag, E, nu, sy0, Q_inf, b,
    D_inf, a,
    c_k, gamma_k);

  TEST_CLASS(RESSCppLabMA_UnitTest)
  {
  public:
    /* ------------------------------------------------------------------------------------------------------------- */
    TEST_METHOD(TestMAGetters)
    {
      mat_orig.revertToStart();
      Vector zeroVec = Vector(NDIM);
      zeroVec.Zero();

      for (unsigned int i = 0; i < NDIM; ++i) {
        Assert::AreEqual(mat_orig.getStrain()(i), zeroVec(i));
        Assert::AreEqual(mat_orig.getStress()(i), zeroVec(i));
      }
    }  // end TEST_METHOD
    /* ------------------------------------------------------------------------------------------------------------- */
    TEST_METHOD(TestMASetGetStrain)
    {
      std::vector<double> materialStrain = { 0., 0., 0., 0., 0., 0. };
      Vector strain = Vector(NDIM);
      std::vector<double> strainInput = { 0.01, 0.02, 0.01, 0.001, 0.05, 0.001 };
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
    TEST_METHOD(TestMAReturnMap)
    {
      mat_orig.revertToStart();


      std::ostringstream os;
      Vector strain = Vector(NDIM);
      std::vector<double> materialStress = { 0., 0., 0., 0., 0., 0. };
      // Test a few increments, validation data from Abaqus biaxial loading, combined kin+iso

      // ---------------- Increment 1 ----------------
      // elastic increment
      std::vector<double> stressCompare = { 152.25, 315.38, 2.8422e-14, 81.563, -1.8744e-15, 3.1865e-14 };
      std::vector<double> strainInput = { 0.00032056, 0.0015, -0.00078024, 0.0011794, -2.7105e-20, 4.6079e-19 };
      for (unsigned int i = 0; i < NDIM; ++i) {
        strain(i) = strainInput[i];
      }
      mat_orig.setTrialStrain(strain);
      for (unsigned int i = 0; i < NDIM; ++i) {
        materialStress[i] = mat_orig.getStress()(i);
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
      stressCompare = { 211.03, 361.7, 0.8199, 75.339, -4.4879e-15, 8.9759e-16 };
      strainInput = { 0.00072494, 0.003, -0.002449, 0.0022751, -1.3553e-19, 2.7105e-20 };
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
      stressCompare = { 220.64, 380.16, 0.31531, 79.758, 1.2987e-15, 3.9517e-15 };
      strainInput = { 0.00090749, 0.0045, -0.0040702, 0.0035925, -2.7105e-20, 1.3553e-19 };
      for (unsigned int i = 0; i < NDIM; ++i) {
        strain(i) = strainInput[i];
      }

      mat_orig.setTrialStrain(strain);
      for (unsigned int i = 0; i < NDIM; ++i) {
        materialStress[i] = mat_orig.getStress()(i);
        //os << materialStress[i] << " " << stressCompare[i] << std::endl;
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