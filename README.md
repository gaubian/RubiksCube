# Rubik's Cube

rubik_manip.ml provides a lot of useful tool for manipulating rubik's cube's position.

Running it actually counts the number of equivalence class of last layer positions, considerating that :
* doing a rotation of the upper face before applying an algorithm doesn't change its class
* same for doing the rotation after applying the algorithm
* an algorithm and its reverse are in the same class
* an algorithm and its symmetry are in the same class