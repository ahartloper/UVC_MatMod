# To use a plate element, you need to provide the plane stress material model
# then use the PlateFromPlaneStress material that takes the plane stress
# condition and modifies it for use with the shell elements.
# This material is essentially an adapter between a material of type "PlaneStress"
# and "PlateFiber".
#
# The PlateFromPlaneStress material considers the out-of-plane shear stiffness
# and adds these components to the stress and tangent modulus from the plane-
# stress formulation.
#
# There are 5 components to the stress and strain vectors in the "PlateFiber".
# The first 3 are the plane-stress components, and the last two are the out
# of plane shear stresses.
#
# The type of material that this element requires is "PlateFiber".
# This element has 5 fibers (integration points?) through the thickness.
#
wipe
 
model BasicBuilder -ndm 3 -ndf 6
 
# nodes
node  1  0.0 0.0 0.0
node  2  1.0 0.0 0.0
node  3  1.0 1.0 0.0
node  4  0.0 1.0 0.0
 
# boundary conditions
fix  1  0 1 0
fix  2  0 1 1
fix  3  1 1 1
fix  4  1 1 0
 
# material
set thick 1.0
set shearModulus [expr 179800.0 / 2.6]
nDMaterial RESSCppLabPS 600 179800.0 0.3 318.5 100.7 8.0 0.0 1.0 2 11608.2 145.2 1026.0 4.7

# nDMaterial PlateFromPlaneStress $newmatTag $matTag $OutofPlaneModulus
nDMaterial PlateFromPlaneStress 601 600 $shearModulus

# section PlateFiber $secTag $matTag $h
section PlateFiber 701 601 $thick
 
# shell elements -> NLDKGQ element
# element ShellNLDKGQ $eleTag $iNode $jNode $kNode $lNode $secTag
element ShellNLDKGQ 1  1 2 3 4 701
 
# surface load elements
set appliedStress 318.5
 
# recorders
#recorder Element -file ./Data/stress_Elastic.out    -ele  1  stress
#recorder Element -file ./Data/strain_Elastic.out    -ele  1  strain
recorder Element -file ./Data2/stress_shell_RESSCppLabPS.out    -ele  1 material 1 stresses
recorder Element -file ./Data2/strain_shell_RESSCppLabPS.out    -ele  1 material 1 strains
 
# load pattern
#timeSeries Linear 1
timeSeries Path 1 -time {0.0 0.2 0.4 0.6 0.8 1.0 1.2} -values {0.0 1.0 1.5 -1.5 2.0 -2.0 0.0}
pattern Plain 1 1 {
	load 3 0. [expr 0.5 * $appliedStress]
	load 4 0. [expr 0.5 * $appliedStress]
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
