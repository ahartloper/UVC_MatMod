wipe
 
model BasicBuilder -ndm 2 -ndf 2
 
# nodes
node  1  0.0 0.0
node  2  1.0 0.0
node  3  1.0 1.0
node  4  0.0 1.0
 
# boundary conditions
fix  1  1 1
fix  2  0 1
fix  3  0 0
fix  4  1 0 
 
# material
#nDMaterial ElasticIsotropic 1 179800.0 0.0
nDMaterial UVCplanestress 1 179800.0 0.3 318.5 100.7 8.0 0.0 1.0 2 11608.2 145.2 1026.0 4.7
 
# shell element
element SSPquad 1  1 2 3 4  1 "PlaneStress" 1.0
 
# surface load elements
set appliedStress 318.5
 
# recorders
# Check the "11" and "22" components of stress and strain in the output
recorder Element -file ./Output_Data/2d_Shell_test_biaxial_stress.out    -ele  1  stress
recorder Element -file ./Output_Data/2d_Shell_test_biaxial_strain.out    -ele  1  strain
 
# load pattern
#timeSeries Linear 1
timeSeries Path 1 -time {0.0 0.2 0.4 0.6 0.8 1.0 1.2} -values {0.0 1.0 1.5 -1.5 2.0 -2.0 0.0}
pattern Plain 1 1 {
	load 2 [expr 0.5 * $appliedStress] 0.                          
	load 3 [expr 0.5 * $appliedStress] [expr 0.5 * $appliedStress]
	load 4 0.                          [expr 0.5 * $appliedStress]
}
 
# analysis
set dt 0.005
constraints Transformation
test        NormDispIncr 1e-5 50 1
algorithm   Newton
numberer    Plain
system      BandGeneral
integrator  LoadControl $dt
analysis    Static
 
analyze    [expr int(1.2 / $dt)]
 
wipe