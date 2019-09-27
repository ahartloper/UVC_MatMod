# Model for a single-bar truss of 1000 mm length and 1 mm^2 area
# This model is created to validate the RESSCppLab material in OpenSees

# units: N, mm

# Remove existing model
wipe

# Create ModelBuilder (with two-dimensions and 2 DOF/node)
model BasicBuilder -ndm 2 -ndf 2

# Create nodes
# ------------
# Create nodes & add to Domain - command: node nodeId xCrd yCrd
node 1   0.0  0.0
node 2 1000.0  0.0
    
# Set the boundary conditions - command: fix nodeID xResrnt? yRestrnt?
fix 1 1 1 
fix 2 0 1
    
# Define materials for truss elements
# -----------------------------------
# Create Elastic material prototype - command: uniaxialMaterial Elastic matID E
# uniaxialMaterial Elastic 1 3000
uniaxialMaterial RESSCppLab 1 179800.0 368.5 100.7 8.0 50. 200. 2 11608.2 145.2 1026.0 4.7

# 
# Define elements
#

# Create truss elements - command: element truss trussID node1 node2 A matID
element Truss 1 1 2 1.0 1
        
# Define loads
# ------------
#

# create a Triangular TimeSeries with a tag of 1
timeSeries Path 1 -time {0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9} -values {0.0 0.25 -0.25 0.5 -0.5 0.75 -0.75 1.0 -1.0 0.0}

pattern Plain 1 1 {
	
   # Create the nodal load - command: load nodeID xForce yForce
   load 2 500 0
}


# OUTPUT
# create a Recorder object for the nodal displacements at node 4
recorder Node -file Upd_Cyclic_Force_Disp_x_Node_2.out -node 2 -dof 1 disp

# create a Recorder for element forces, one for global system and the other for local system
recorder Node -file Upd_Cyclic_Force_Force_x_Node_1.out -node 1 -dof 1 reaction


# ANALYSIS
# Create the system of equation
system BandSPD
    
# Create the DOF numberer, the reverse Cuthill-McKee algorithm
numberer RCM
    
# Create the constraint handler, a Plain handler is used as homo constraints
constraints Plain

# Create the solution algorithm, a Newton algorithm is created
test NormUnbalance 1.0e-6 400
#algorithm NewtonLineSearch
algorithm KrylovNewton

# Create the integration scheme, the LoadControl scheme using steps of 1.0
#integrator DisplacementControl 2 1 0.5
integrator LoadControl 0.005

# create the analysis object 
analysis Static 

analyze 180
