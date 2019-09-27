//#include "stdafx.h"
#include "CppUnitTest.h"
#include <vector>
#include <iostream>
#include <sstream>
#include <fstream>

#include "../../SRC/material/uniaxial/UVCua.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace original_model_tests
{

  // Create the material to test
  int tag = 1;
  double E = 179800.0;
  double sy0 = 318.5;
  double qInf = 100.7;
  double b = 8;
  double dInf = 0.;
  double a = 200.;
  std::vector<double> cK = { 11608.2, 1026.0 };
  std::vector<double> gammaK = { 145.2, 4.7 };
  UVCua ress_mat(tag, E, sy0, qInf, b,
    dInf, a,
    cK, gammaK);


  /* ------------------------------------------------------------------------ */
  /* Define the tests                                                         */
  /* ------------------------------------------------------------------------ */
  TEST_CLASS(RESSCppLab_UnitTest)
  {
  public:

    /* ---------------------------------------------------------------------- */
    TEST_METHOD(TestRevertStart)
    {
      E = 179800.0;
      ress_mat.revertToStart();

      std::ostringstream os;

      os << ress_mat.getTangent() << " " << E << std::endl;

      std::string out_string;
      out_string = os.str();
      const char* message = out_string.c_str();
      Logger::WriteMessage(message);

      Assert::AreEqual(ress_mat.getStrain(), 0.);
      Assert::AreEqual(ress_mat.getStress(), 0.);
      Assert::AreEqual(ress_mat.getTangent(), 0.);
      Assert::AreEqual(ress_mat.getInitialTangent(), E);


    }

    /* ---------------------------------------------------------------------- */
    /*
    Tests some elastic loading, and plastic loading to 0.05 strain.
    Plastic loading validation values obtained from RESSPyLab.
    Note: the RESSPyLab values are based on a strain increment of 0.0001,
    however the error is still within tolerance (10^-8).
    /* ---------------------------------------------------------------------- */
    TEST_METHOD(TestReturnMappingMonotonic)
    {
      ress_mat.revertToStart();
      double test_delta = 10.0e-8;
      std::ostringstream os;
      double strain_inc;
      strain_inc = 0.0005;  // Yield strain is 0.00177

      // First strain increment (elastic)
      ress_mat.setTrialStrain(strain_inc);
      Assert::AreEqual(ress_mat.getStress(), strain_inc * E);
      Assert::AreEqual(ress_mat.getStrain(), strain_inc);
      os << "Increment 1" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;

      // Second strain increment (elastic)
      ress_mat.commitState();
      ress_mat.setTrialStrain(2. * strain_inc);
      Assert::AreEqual(ress_mat.getStress(), 2. * strain_inc * E);
      Assert::AreEqual(ress_mat.getStrain(), 2. * strain_inc);
      os << "Increment 2" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;

      // Third strain increment (plastic)
      ress_mat.commitState();
      ress_mat.setTrialStrain(4. * strain_inc);
      Assert::AreEqual(ress_mat.getStress(), 321.32308726007312, test_delta);
      Assert::AreEqual(ress_mat.getStrain(), 4. * strain_inc);
      os << "Increment 3" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;


      // Fourth strain increment (plastic)
      ress_mat.commitState();
      ress_mat.setTrialStrain(100. * strain_inc);
      Assert::AreEqual(ress_mat.getStress(), 473.69256403279007, test_delta);
      Assert::AreEqual(ress_mat.getStrain(), 100. * strain_inc);
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
    Tests an excursion to 0.05 strain, then to -0.05 strain, then to
    0.10 strain, then to -0.10 strain.
    Validation values obtained from RESSPyLab.
    */
    /* ---------------------------------------------------------------------- */
    TEST_METHOD(TestReturnMappingCyclic)
    {
      ress_mat.revertToStart();
      double test_delta = 10.0e-8;
      std::ostringstream os;
      double strain_inc;

      // First positive excursion
      ress_mat.setTrialStrain(0.05);
      Assert::AreEqual(ress_mat.getStress(), 473.6925640327903, test_delta);
      Assert::AreEqual(ress_mat.getStrain(), 0.05);
      os << "Increment 1" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;

      // First negative excursion
      ress_mat.commitState();
      ress_mat.setTrialStrain(-0.05);
      Assert::AreEqual(ress_mat.getStress(), -517.1107222100989);
      Assert::AreEqual(ress_mat.getStrain(), -0.05);
      os << "Increment 2" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;

      // Second positive excursion
      ress_mat.commitState();
      ress_mat.setTrialStrain(0.1);
      Assert::AreEqual(ress_mat.getStress(), 570.6470772671765, test_delta);
      Assert::AreEqual(ress_mat.getStrain(), 0.1);
      os << "Increment 3" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;


      // Second negative excursion
      ress_mat.commitState();
      ress_mat.setTrialStrain(-0.1);
      Assert::AreEqual(ress_mat.getStress(), -594.442629017056, test_delta);
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
    Tests an excursion to 0.05 strain, then to -0.05 strain, then to
    0.10 strain, then to -0.10 strain.
    Validation values obtained from RESSPyLab.
    */
    /* ---------------------------------------------------------------------- */
    TEST_METHOD(TestStiffness)
    {
      ress_mat.revertToStart();
      double test_delta = 10.0e-8;
      std::ostringstream os;
      double strain_inc;

      // Check initial stiffness
      Assert::AreEqual(ress_mat.getInitialTangent(), E);

      // Check elastic stiffness
      ress_mat.setTrialStrain(0.0005);
      Assert::AreEqual(ress_mat.getTangent(), E);

      // First positive excursion to 5%
      ress_mat.setTrialStrain(0.05);
      Assert::AreEqual(ress_mat.getTangent(), 1374.126445299805, test_delta);
      os << "Increment 1" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stiffness = " << ress_mat.getTangent() << std::endl;

      // First negative excursion to -5%
      ress_mat.commitState();
      ress_mat.setTrialStrain(-0.05);
      Assert::AreEqual(ress_mat.getTangent(), 1042.3428549170953, test_delta);
      os << "Increment 2" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stiffness = " << ress_mat.getTangent() << std::endl;

      // Second positive excursion to 10%
      ress_mat.commitState();
      ress_mat.setTrialStrain(0.1);
      Assert::AreEqual(ress_mat.getTangent(), 720.8167473864271, test_delta);
      os << "Increment 3" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stiffness = " << ress_mat.getTangent() << std::endl;


      // Second negative excursion to -10%
      ress_mat.commitState();
      ress_mat.setTrialStrain(-0.1);
      Assert::AreEqual(ress_mat.getTangent(), 583.3893069538766, test_delta);
      os << "Increment 4" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stiffness = " << ress_mat.getTangent() << std::endl;

      // Use this for writing output
      std::string out_string;
      out_string = os.str();
      const char* message = out_string.c_str();
      Logger::WriteMessage(message);
    }

    /* ---------------------------------------------------------------------- */
    /*
    Tests that the copy is an exact copy.
    */
    /* ---------------------------------------------------------------------- */
    TEST_METHOD(TestGetCopy)
    {
      ress_mat.revertToStart();
      ress_mat.setTrialStrain(0.0005);
      ress_mat.commitState();

      UniaxialMaterial* ress_mat_copy = ress_mat.getCopy();

      // Check equality for all public implemented methods just after copied
      Assert::AreEqual(ress_mat.getClassTag(), ress_mat_copy->getClassTag());
      Assert::AreEqual(ress_mat.getInitialTangent(),
        ress_mat_copy->getInitialTangent());
      Assert::AreEqual(ress_mat.getStrain(),
        ress_mat_copy->getStrain());
      Assert::AreEqual(ress_mat.getStress(),
        ress_mat_copy->getStress());
      Assert::AreEqual(ress_mat.getTag(),
        ress_mat_copy->getTag());
      Assert::AreEqual(ress_mat.getTangent(),
        ress_mat_copy->getTangent());


      // Check that the plastic response is the same
      ress_mat.setTrialStrain(0.05);
      ress_mat.commitState();
      ress_mat_copy->setTrialStrain(0.05);
      ress_mat_copy->commitState();

      Assert::AreEqual(ress_mat.getStrain(),
        ress_mat_copy->getStrain());
      Assert::AreEqual(ress_mat.getStress(),
        ress_mat_copy->getStress());
      Assert::AreEqual(ress_mat.getTangent(),
        ress_mat_copy->getTangent());

      delete ress_mat_copy;

    }

    /* ---------------------------------------------------------------------- */
    /*
    Tests cyclic input data.
    */
    /* ---------------------------------------------------------------------- */
    TEST_METHOD(TestReturnMappingCyclicData)
    {

      double test_delta = 10.0e-8;
      ress_mat.revertToStart();
      std::ostringstream os;

      //todo: make this not an absolute path
      std::string file_path = "C:\\Users\\hartlope\\Documents\\EPFL\\"
        "RESSPyLab_update\\examples\\fortran_verify\\RESSPyLab_Cyclic_1_stress-strain.txt";
      std::ifstream input_stream(file_path.c_str());
      std::string line;
      double strain_val;
      double stress_val;
      std::vector<double> strain_history;
      std::vector<double> stress_history;

      while (input_stream.good()) {
        getline(input_stream, line);
        os << line << std::endl;
        std::stringstream ss(line);
        ss >> strain_val;
        ss >> stress_val;
        strain_history.push_back(strain_val);
        stress_history.push_back(stress_val);
      }
      input_stream.close();

      // Calculate the response
      std::vector<double> stress_response;
      for (auto it = strain_history.begin(); it != strain_history.end(); ++it) {
        ress_mat.setTrialStrain(*it);
        stress_response.push_back(ress_mat.getStress());
        os << ress_mat.getStrain() << " " << ress_mat.getStress() << std::endl;
        ress_mat.commitState();
      }

      // Calculate the norm error
      double norm_diff = 0.;
      double norm_denom = 0.;
      for (int i = 0; i < strain_history.size(); ++i) {
        norm_diff += pow(stress_response[i] - stress_history[i], 2);
        norm_denom += pow(stress_history[i], 2);
      }
      norm_diff = sqrt(norm_diff);
      norm_denom = sqrt(norm_denom);
      double norm_error = norm_diff / norm_denom * 100.0;
      os << "Norm error [%]: " << norm_error << std::endl;
      Assert::AreEqual(norm_error, 0., test_delta);

      // Write Log
      std::string out_string;
      out_string = os.str();
      const char* message = out_string.c_str();
      Logger::WriteMessage(message);
    }

    /* ---------------------------------------------------------------------- */
    /*
    Tests cyclic input data 2.
    */
    /* ---------------------------------------------------------------------- */
    TEST_METHOD(TestReturnMappingCyclicData2)
    {

      double test_delta = 10.0e-8;
      ress_mat.revertToStart();
      std::ostringstream os;

      //todo: make this not an absolute path
      std::string file_path = "C:\\Users\\hartlope\\Documents\\EPFL\\"
        "RESSPyLab_update\\examples\\fortran_verify\\RESSPyLab_Cyclic_2_stress-strain.txt";
      std::ifstream input_stream(file_path.c_str());
      std::string line;
      double strain_val;
      double stress_val;
      std::vector<double> strain_history;
      std::vector<double> stress_history;

      while (input_stream.good()) {
        getline(input_stream, line);
        os << line << std::endl;
        std::stringstream ss(line);
        ss >> strain_val;
        ss >> stress_val;
        strain_history.push_back(strain_val);
        stress_history.push_back(stress_val);
      }
      input_stream.close();

      // Calculate the response
      std::vector<double> stress_response;
      for (auto it = strain_history.begin(); it != strain_history.end(); ++it) {
        ress_mat.setTrialStrain(*it);
        stress_response.push_back(ress_mat.getStress());
        os << ress_mat.getStrain() << " " << ress_mat.getStress() << std::endl;
        ress_mat.commitState();
      }

      // Calculate the norm error
      double norm_diff = 0.;
      double norm_denom = 0.;
      for (int i = 0; i < strain_history.size(); ++i) {
        norm_diff += pow(stress_response[i] - stress_history[i], 2);
        norm_denom += pow(stress_history[i], 2);
      }
      norm_diff = sqrt(norm_diff);
      norm_denom = sqrt(norm_denom);
      double norm_error = norm_diff / norm_denom * 100.0;
      os << "Norm error [%]: " << norm_error << std::endl;
      Assert::AreEqual(norm_error, 0., test_delta);

      // Write Log
      std::string out_string;
      out_string = os.str();
      const char* message = out_string.c_str();
      Logger::WriteMessage(message);
    }

    /* ---------------------------------------------------------------------- */
    /*
    Tests monotonic input data.
    */
    /* ---------------------------------------------------------------------- */
    TEST_METHOD(TestReturnMappingMonotonicData)
    {

      double test_delta = 10.0e-8;
      ress_mat.revertToStart();
      std::ostringstream os;

      //todo: make this not an absolute path
      std::string file_path = "C:\\Users\\hartlope\\Documents\\EPFL\\"
        "RESSPyLab_update\\examples\\fortran_verify\\RESSPyLab_Tensile_stress-strain.txt";

      std::ifstream input_stream(file_path.c_str());
      std::string line;
      double strain_val;
      double stress_val;
      std::vector<double> strain_history;
      std::vector<double> stress_history;

      while (input_stream.good()) {
        getline(input_stream, line);
        os << line << std::endl;
        std::stringstream ss(line);
        ss >> strain_val;
        ss >> stress_val;
        strain_history.push_back(strain_val);
        stress_history.push_back(stress_val);
      }
      input_stream.close();

      // Calculate the response
      std::vector<double> stress_response;
      for (auto it = strain_history.begin(); it != strain_history.end(); ++it) {
        ress_mat.setTrialStrain(*it);
        stress_response.push_back(ress_mat.getStress());
        os << ress_mat.getStrain() << " " << ress_mat.getStress() << std::endl;
        ress_mat.commitState();
      }

      // Calculate the norm error
      double norm_diff = 0.;
      double norm_denom = 0.;
      for (int i = 0; i < strain_history.size(); ++i) {
        norm_diff += pow(stress_response[i] - stress_history[i], 2);
        norm_denom += pow(stress_history[i], 2);
      }
      norm_diff = sqrt(norm_diff);
      norm_denom = sqrt(norm_denom);
      double norm_error = norm_diff / norm_denom * 100.0;
      os << "Norm error [%]: " << norm_error << std::endl;
      Assert::AreEqual(norm_error, 0., test_delta);

      // Write Log
      std::string out_string;
      out_string = os.str();
      const char* message = out_string.c_str();
      Logger::WriteMessage(message);
    }

    /* ---------------------------------------------------------------------- */
    /*
    Tests simulated failed iterations.
    Validation values obtained from RESSPyLab.
    */
    /* ---------------------------------------------------------------------- */
    TEST_METHOD(TestReturnMappingFailedIterations)
    {
      ress_mat.revertToStart();
      double test_delta = 10.0e-8;
      std::ostringstream os;
      double strain_inc;

      // First positive excursion
      ress_mat.setTrialStrain(0.05);
      Assert::AreEqual(ress_mat.getStress(), 473.6925640327903, test_delta);
      os << "Increment 1" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;

      ress_mat.commitState();
      // Simulated failed iterations
      ress_mat.setTrialStrain(-0.03);

      // Simulated failed iterations
      ress_mat.setTrialStrain(-0.1);

      // Simulated failed iterations
      ress_mat.setTrialStrain(-0.6);

      // Desired negative excursion
      ress_mat.setTrialStrain(-0.05);
      Assert::AreEqual(ress_mat.getStress(), -517.1107222100989);
      Assert::AreEqual(ress_mat.getStrain(), -0.05);
      os << "Increment 2" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;

      // Second positive excursion
      ress_mat.commitState();
      ress_mat.setTrialStrain(0.1);
      Assert::AreEqual(ress_mat.getStress(), 570.6470772671765, test_delta);
      Assert::AreEqual(ress_mat.getStrain(), 0.1);
      os << "Increment 3" << std::endl;
      os << "Strain = " << ress_mat.getStrain() <<
        " Stress: " << ress_mat.getStress() << std::endl;

      // Use this for writing output
      std::string out_string;
      out_string = os.str();
      const char* message = out_string.c_str();
      Logger::WriteMessage(message);
    }


    /* ---------------------------------------------------------------------- */
  };
}