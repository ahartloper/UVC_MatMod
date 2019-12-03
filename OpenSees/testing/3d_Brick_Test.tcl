wipe
 
model BasicBuilder -ndm 3 -ndf 3
 
# nodes
node  1  1.0 0.0 1.0
node  2  1.0 0.0 0.0
node  3  0.0 0.0 0.0
node  4  0.0 0.0 1.0
node  5  1.0 1.0 1.0
node  6  1.0 1.0 0.0
node  7  0.0 1.0 0.0
node  8  0.0 1.0 1.0
 
# boundary conditions
fix  1  0 1 0
fix  2  0 1 1
fix  3  1 1 1
fix  4  1 1 0
 
# material
#nDMaterial ElasticIsotropic 1 179800.0 0.0
nDMaterial UVCmultiaxial 1 179800.0 0.3 318.5 100.7 8.0 0.0 1.0 2 11608.2 145.2 1026.0 4.7
 
# brick element
element SSPbrick 1  1 2 3 4 5 6 7 8 1
 
# surface load elements
set appliedStress 318.5
element SurfaceLoad 2  5 6 7 8  $appliedStress 
 
# recorders
# Check the "22" components of stress and strain in the output
recorder Element -file ./Output_Data/3d_Brick_test_stress.out    -ele  1  stress
recorder Element -file ./Output_Data/3d_Brick_test_strain.out    -ele  1  strain
 
# load pattern
timeSeries Path 1 -time {0.0 0.2 0.4 0.6 0.8 1.0 1.2} -values {0.0 1.0 1.5 -1.5 2.0 -2.0 0.0}
pattern Plain 1 1 {
	eleLoad -ele 2 -type -surfaceLoad
}
 
# analysis
set dt 0.005
constraints Transformation
test        NormDispIncr 1e-8 50 1
algorithm   Newton
numberer    Plain
system      BandGeneral
integrator  LoadControl $dt
analysis    Static
 
analyze    [expr int(1.2 / $dt)]
 
wipe