#Test case 284
# Data Tone_2004

Data_1 <- matrix(c(1,3,
                   1,2,
                   1,1,
                   1,0,
                   1,-1,
                   1,-2,
                   1,-3), byrow=TRUE, nrow=7);


Input = matrix(c(1,
                 1,
                 1,
                 1,
                 1,
                 1,
                 1), byrow=TRUE, nrow=7);

Output = matrix(c(3,
                  2,
                  1,
                  0,
                  -1,
                  -2,
                  -3), byrow=TRUE, nrow=7);
Link <- NaN;


K = 1; # 3 divisions
N = 7; # Amount of DMU's
sum_m = 1; # Amount of inputs
sum_r = 1; # Amount of outputs
sum_l = NaN; # Amount of Link variables


# Distinguish the Amount vector:
Amount = matrix(c(1,1,0), byrow=TRUE, nrow=1);
Amount_Input =  c(1);
Amount_Output = c(1);
Amount_Link = NaN;

weights = matrix(c(1), byrow=TRUE, nrow=1);


direction = "non";
link_con = NaN;
return_to_scale = "VRS" ;
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




test_that("Test case 284",{

  #Results of the NSBM function
  EfficiencyNSBM <- matrix(c(1,0.667,0.333,0.182,0.111,0.074,0.053), byrow = TRUE, nrow = 7);



  #########################################
  #########################################
  #########################################

  # Results of the nsbm function:
  expect_equal(round(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$Eff,3), EfficiencyNSBM, check.attributes = FALSE)


})
