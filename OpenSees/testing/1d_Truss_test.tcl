# This model is created to validate the UVCuniaxial material in OpenSees

# units: N, mm

# Remove existing model
wipe

# Create ModelBuilder (with two-dimensions and 2 DOF/node)
model BasicBuilder -ndm 2 -ndf 2

# Create nodes & add to Domain - command: node nodeId xCrd yCrd
node 1   0.0  0.0
node 2 1000.0  0.0
    
# Set the boundary conditions - command: fix nodeID xResrnt? yRestrnt?
fix 1 1 1 
fix 2 0 1

# uniaxialMaterial Elastic 1 3000
uniaxialMaterial UVCuniaxial 1   179800.0    318.5   100.7   8.0 0.0 1.0   2   11608.2 145.2   1026.0  4.7

# 
# Define elements
#

# Create truss element
element Truss 1 1 2 1.0 1

timeSeries Path 1 -time {0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9} -values {0.0 0.25 -0.25 0.5 -0.5 0.75 -0.75 1.0 -1.0 0.0}

pattern Plain 1 1 {
   # Create the nodal load - command: load nodeID xForce yForce
   load 2 500 0
}


# OUTPUT
# create a Recorder object for the nodal displacements at node 4
recorder Node -file ./Output_Data/1d_Truss_test_disp.out -time -node 2 -dof 1 disp

# create a Recorder for element forces, one for global system and the other for local system
recorder Node -file ./Output_Data/1d_Truss_test_force.out -time -node 1 -dof 1 reaction


# ANALYSIS
system BandSPD
numberer RCM
constraints Plain
test NormUnbalance 1.0e-6 400
algorithm KrylovNewton
integrator LoadControl 0.005

analysis Static 

analyze 180
