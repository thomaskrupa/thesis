# Test case 278
# Initialization of Tone Data 2 (Tone2009)
# Tone_2009


Data_1 <- matrix(c(3, 10, 2, 5, 2, 8, 2,
                   14, 1, 1, 5, 5, 9, 5,
                   16,2, 2, 11, 4, 7, 4,
                   19, 0.5, 2, 7, 4, 11, 4), byrow=TRUE, nrow=4);


Input = matrix(c(3,10,5,
                 14,1,5,
                 16,2,11,
                 19,0.5,7), byrow = TRUE, nrow = 4);

Output = matrix(c(2,2,
                  1,5,
                  2,4,
                  2,4), byrow = TRUE, nrow = 4);

Link = matrix(c(8,2,
                9,5,
                7,4,
                11,4), byrow = TRUE, nrow = 4);

K = 3; # 3 divisions
N = 4; # Amount of DMU's
sum_m = 3; # Amount of inputs
sum_r = 2; # Amount of outputs
sum_l = 2; # Amount of Link variables


# Distinguish the Amount vector:
Amount = matrix(c(1,0,1,1,1,1,1,1), byrow=TRUE, nrow=1);
Amount_Input =  c(1,1,1);
Amount_Output = c(0,1,1);
Amount_Link = c(1,1);

weights = matrix(c(0.4, 0.2, 0.4), byrow=TRUE, nrow=1);


direction = "input";
link_con = 2; # free links
return_to_scale = "CRS" ;
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



test_that("Test case 278",{

  # Division efficiency
  DivEffNSBM <- matrix(c(0.8750,0.2000,0.8000,
                         0.3683,0.6250,1.0000,
                         0.2578,0.2500,0.3636,
                         0.2171,1.0000,0.5714), byrow = TRUE, nrow = 4);


  # Projection of Inputs, Outputs and LInk variable
  Input_proj <- matrix(c(2.625,2,4,
                         5.1562,0.625,5,
                         4.125,0.5,4,
                         4.125,0.5,4), byrow = TRUE, nrow = 4);

  Output_proj <- matrix(c(2,4,
                          2.5,5,
                          2,4,
                          2,4), byrow = TRUE, nrow = 4);

  Link_proj <- matrix(c(7,4,
                        13.75,5,
                        11,4,
                        11,4), byrow = TRUE, nrow = 4);


  #Efficiency:
  EfficiencyNSBM <- matrix(c(0.71, 0.6723, 0.2986, 0.5154), byrow = TRUE, nrow = 4);



  #########################################
  #########################################
  #########################################


  # Results of the nsbm function:
  expect_equal(round(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$Eff,4), EfficiencyNSBM, check.attributes = FALSE)

  expect_equal(round(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$divEff,4), DivEffNSBM, check.attributes = FALSE)

  expect_equal(round(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$Input_proj,4), Input_proj, check.attributes = FALSE)

  expect_equal(round(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$Output_proj,4), Output_proj, check.attributes = FALSE)

  expect_equal(round(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$Link_proj,4), Link_proj, check.attributes = FALSE)

})
