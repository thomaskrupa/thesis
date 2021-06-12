#Test case 279
# Data Tone_2001


Data_1 <- matrix(c(4,3,2,3,
                   6,3,2,3,
                   8,1,6,2,
                   8,1,6,1,
                   2,4,1,4), byrow=TRUE, nrow=5);


Input = matrix(c(4,3,
                 6,3,
                 8,1,
                 8,1,
                 2,4), byrow = TRUE, nrow = 5);

Output = matrix(c(2,3,
                  2,3,
                  6,2,
                  6,1,
                  1,4), byrow = TRUE, nrow = 5);

Link <- NaN;


K = 1; # 3 divisions
N = 5; # Amount of DMU's
sum_m = 2; # Amount of inputs
sum_r = 2; # Amount of outputs
sum_l = NaN; # Amount of Link variables


# Distinguish the Amount vector:
Amount = matrix(c(2,2), byrow=TRUE, nrow=1);
Amount_Input =  c(2);
Amount_Output = c(2);
Amount_Link = NaN;

weights = matrix(c(1), byrow=TRUE, nrow=1);


direction = "non";
link_con = NaN;
return_to_scale = "CRS" ;
NIRS <- 0;
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




test_that("Test case 279",{


  # slacks of the SBM solution
  slack_plus <- matrix(c(0.714,0,
                         2.286,0,
                         0,0,
                         0,1,
                         0,0), byrow = TRUE, nrow = 5);


  slack_minus <- matrix(c(0,0.357,
                          0,0.643,
                          0,0,
                          0,0,
                          0,0), byrow = TRUE, nrow = 5);


  #Results of the NSBM function
  EfficiencyNSBM <- matrix(c(0.798,0.568,1.000,0.667,1.000), byrow = TRUE, nrow = 5);



  #########################################
  #########################################
  #########################################


  # Results of the nsbm function:
  expect_equal(round(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$Eff,3), EfficiencyNSBM, check.attributes = FALSE)

  expect_equal(round(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$slacks_plus,3), slack_plus, check.attributes = FALSE)

  expect_equal(round(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$slack_minu,3), slack_minus, check.attributes = FALSE)


})

