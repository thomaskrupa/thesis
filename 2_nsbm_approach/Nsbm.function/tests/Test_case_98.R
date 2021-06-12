# Test case 98


Input <- matrix(c(2,
                  2,
                  2), byrow = TRUE, nrow = 3);

Output <- matrix(c(3,4,
                   3,4,
                   3,4), byrow = TRUE, nrow = 3);

Link <- matrix(c(2,
                 2,
                 2), byrow = TRUE, nrow = 3);


K = 2; # 3 divisions
N = 3; # Amount of DMUs
sum_m = 1; # Amount of inputs
sum_r = 2; # Amount of outputs
sum_l = 1; # Amount of Link variables


# Distinguish the Amount vector:
Amount = matrix(c(0,1,1,1,1), byrow=TRUE, nrow=1);
Amount_Input =  c(0,1);
Amount_Output = c(1,1);
Amount_Link = c(1);

weights = matrix(c(0.5,0.5), byrow=TRUE, nrow=1);


direction = "input";
link_con = 1; # fix
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




test_that("Test case 98",{


  # Slack_transformation:
  weightsNSBM <- matrix(c(  2,2,2,2,2,2,3,3,4,
                            2,2,2,2,2,2,3,3,4,
                            2,2,2,2,2,2,3,3,4), byrow = TRUE, nrow = 3);


  t <- NaN;


  lambda <- matrix(c(  2,2,2,2,2,2,
                       2,2,2,2,2,2,
                       2,2,2,2,2,2), byrow = TRUE, nrow = 3);


  slack_plus <- matrix(c(  3,3,
                           3,3,
                           3,3), byrow = TRUE, nrow = 3);


  slack_minus <- matrix(c(  4,
                            4,
                            4), byrow = TRUE, nrow = 3);



  # nsbm_division

  DivEffNSBM <- matrix(c( 0,-1,
                          0,-1,
                          0,-1), byrow = TRUE, nrow = 3);


  # projection_frontier

  Input_proj <- matrix(c(  -2,
                           -2,
                           -2), byrow = TRUE, nrow = 3);

  Output_proj  <- matrix(c(  6,7,
                             6,7,
                             6,7), byrow = TRUE, nrow = 3);

  Link_proj <- Link;


  #########################################
  #########################################
  #########################################


  # slacks_transformation:
  expect_equal(slacks.transformation(direction, weightsNSBM, K, N, sum_m, sum_r, sum_l, Link_obj)$t, t, check.attributes = FALSE)

  expect_equal(slacks.transformation(direction, weightsNSBM, K, N, sum_m, sum_r, sum_l, Link_obj)$slack_plus, slack_plus, check.attributes = FALSE)

  expect_equal(slacks.transformation(direction, weightsNSBM, K, N, sum_m, sum_r, sum_l, Link_obj)$slack_minus, slack_minus, check.attributes = FALSE)


  # nsbm.division
  expect_equal(nsbm.division(direction, slack_plus, slack_minus, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, Link_obj), DivEffNSBM, check.attributes = FALSE)

  # projection.frontier:
  expect_equal(round(projection.frontier(link_con, slack_plus, slack_minus, lambda, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, N, K, sum_m, sum_r, sum_l)$Input_Proj,5), Input_proj, check.attributes = FALSE)

  expect_equal(round(projection.frontier(link_con, slack_plus, slack_minus, lambda, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, N, K, sum_m, sum_r, sum_l)$Output_Proj,3), Output_proj, check.attributes = FALSE)

  expect_equal(round(projection.frontier(link_con, slack_plus, slack_minus, lambda, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, N, K, sum_m, sum_r, sum_l)$Link_Proj,4), Link_proj, check.attributes = FALSE)

})
