#Test case 277
#Data of Tone2009 (free case):
#Tone_2009


Data_1 <- matrix(c(0.838, 0.277, 0.879, 0.962, 0.337, 0.894, 0.362,
                   1.233, 0.132, 0.538, 0.443, 0.180, 0.678, 0.188,
                   0.321, 0.045, 0.911, 0.482, 0.198, 0.836, 0.207,
                   1.483, 0.111, 0.570, 0.467, 0.491, 0.869, 0.516,
                   1.592, 0.208, 1.086, 1.073, 0.372, 0.693, 0.407,
                   0.790, 0.139, 0.722, 0.545, 0.253, 0.966, 0.269,
                   0.451, 0.075, 0.509, 0.366, 0.241, 0.647, 0.257,
                   0.408, 0.074, 0.619, 0.229, 0.097, 0.756, 0.103,
                   1.864, 0.061, 1.023, 0.691, 0.380, 1.191, 0.402,
                   1.222, 0.149, 0.769, 0.337, 0.178, 0.792, 0.187),byrow=TRUE, nrow=10);



Input = matrix(c(0.838,0.277,0.962,
                 1.233,0.132,0.443,
                 0.321,0.045,0.482,
                 1.483,0.111,0.467,
                 1.592,0.208,1.073,
                 0.790,0.139,0.545,
                 0.451,0.075,0.366,
                 0.408,0.074,0.229,
                 1.864,0.061,0.691,
                 1.222,0.149,0.337), byrow = TRUE, nrow = 10);

Output = matrix(c(0.879,0.337,
                  0.538,0.180,
                  0.911,0.198,
                  0.570,0.491,
                  1.086,0.372,
                  0.722,0.253,
                  0.509,0.241,
                  0.619,0.097,
                  1.023,0.380,
                  0.769,0.178), byrow = TRUE, nrow = 10);

Link = matrix(c(0.894,0.362,
                0.678,0.188,
                0.836,0.207,
                0.869,0.516,
                0.693,0.407,
                0.966,0.269,
                0.647,0.257,
                0.756,0.103,
                1.191,0.402,
                0.792,0.187), byrow = TRUE, nrow = 10);


weights = matrix(c(0.4, 0.2, 0.4), byrow = TRUE, nrow = 1);

w1 <- weights[1];
w2 <- weights[2];
w3 <- weights[3];


K = 3; # Amount of divisions
N = 10; # Amount of DMUs
sum_m = 3; # Amount of Inputs
sum_r = 2; # Amount of Outputs
sum_l = 2; # Amount of Links


Amount <- c(1,0,1,1,1,1,1,1);
Amount_Input = c(1,1,1);
Amount_Output = c(0,1,1);
Amount_Link = c(1,1);




direction = "input" ;
link_con = 2; #free
return_to_scale = "VRS"
NIRS = 0;
Link_obj = 0; # No Link variable in the objective function


#Loading all the functioN:
setwd(getwd())
setwd("..")
setwd("00_pkg_src")
setwd("Nsbm.function")
setwd("R")
source("load_all_func.R");
load_all_func();
setwd("..")
setwd("..")
setwd("..")
setwd("tests")


test_that("Test case 277",{

  # Division efficiency
  DivEffNSBM <- matrix(c(0.383,0.383,0.389,
                         0.260,0.341,0.652,
                         1.000,1.000,0.919,
                         0.297,1.000,1.000,
                         0.263,1.000,0.377,
                         0.406,0.420,0.593,
                         0.712,0.740,0.863,
                         0.922,1.000,1.000,
                         1.000,1.000,0.581,
                         0.271,0.338,0.825), byrow = TRUE, nrow = 10);


  # Efficiency results
  EfficiencyNSBM <- matrix(c(0.385,0.433,0.968,0.719,0.456,0.484,0.778,0.969,0.832,0.506), byrow = TRUE, nrow = 10);



  #########################################
  #########################################
  #########################################

  # Efficiency results
 expect_equal(round(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$Eff,3), EfficiencyNSBM, check.attributes = FALSE)

 expect_equal(round(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$divEff,3), DivEffNSBM, check.attributes = FALSE)

})
