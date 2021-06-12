#Test case 281
#Data from Cooper2006 page 102
#Solution page 102

Data_1 <- matrix(c(4,3,1,
                   7,3,1,
                   8,1,1,
                   4,2,1,
                   2,4,1,
                   10,1,1,
                   12,1,1,
                   10,1.5,1), byrow=TRUE, nrow=8);

Input = matrix(c(4,3,
                 7,3,
                 8,1,
                 4,2,
                 2,4,
                 10,1,
                 12,1,
                 10,1.5), byrow=TRUE, nrow=8);

Output = matrix(c(1,
                  1,
                  1,
                  1,
                  1,
                  1,
                  1,
                  1), byrow=TRUE, nrow=8);

Link <- NaN;


K = 1; # 3 divisions
N = 8; # Amount of DMU's
sum_m = 2; # Amount of inputs
sum_r = 1; # Amount of outputs
sum_l = NaN; # Amount of Link variables


# Distinguish the Amount vector:
Amount = matrix(c(2,1,0), byrow=TRUE, nrow=1);
Amount_Input =  c(2);
Amount_Output = c(1);
Amount_Link = NaN;

weights = matrix(c(1), byrow=TRUE, nrow=1);


direction = "input";
link_con = NaN;
return_to_scale = "CRS";
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


test_that("Test case 281",{


  # slacks of the SBM solution
  slack_plus <- matrix(c(0,
                         0,
                         0,
                         0,
                         0,
                         0,
                         0,
                         0), byrow=TRUE, nrow=8);


  slack_minus <- matrix(c(0,1,
                          3,1,
                          0,0,
                          0,0,
                          0,0,
                          2,0,
                          4,0,
                          2,0.5), byrow=TRUE, nrow=8);



  # Efficiency results
  EfficiencyNSBM <- matrix(c(0.833,0.619,1,1,1,0.9,0.833,0.733), byrow=TRUE, nrow=8);


  #########################################
  #########################################
  #########################################

  # Efficiency results
  expect_equal(round(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$Eff,3), EfficiencyNSBM, check.attributes = FALSE)

  expect_equal(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$slacks_plus, slack_plus, check.attributes = FALSE)

  expect_equal(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$slack_minus, slack_minus, check.attributes = FALSE)


})

