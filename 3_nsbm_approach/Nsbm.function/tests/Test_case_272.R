# Test case 272:

Data_1 <- matrix(c(1, 1, 1, 1, 2, 1,
                   2, 1, 1, 1, 1, 2,
                   2, 1, 2, 2, 1, 1),byrow=TRUE, nrow=3);


Input = matrix(c(1,
                 2,
                 2), byrow = TRUE, nrow = 3);

Output = matrix(c(1,1,
                  1,1,
                  1,2), byrow = TRUE, nrow = 3);

Link = matrix(c(1,2,1,
                1,1,2,
                2,1,1), byrow = TRUE, nrow = 3);

weights = c(0.5, 0.25, 0.25);

K = 3;
N = 3;
sum_m = 1; # inputs
sum_r = 2; # outputs
sum_l = 3;

Amount = c(1,1,0,0,0,1,1,2);
Amount_Input = c(1,0,0);
Amount_Output = c(1,0,1);
Amount_Link = c(1,2);

# Inputs
m1 <- 1;
m2 <- 0;
m3 <- 0;

# Outputs
r1 <- 1;
r2 <- 0;
r3 <- 1;

# Link
l1 <- 1;
l2 <- 2;

# weights
w1 = 0.5;
w2 = 0.25;
w3 = 0.25;


direction = "non" ;
link_con = 1; # fix
return_to_scale = "CRS";
NIRS = 0;
Link_obj = 1; # Link variable in the objective function


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


test_that("Test case 272",{


  # weightsNSBM
  weightsNSBM <- matrix(c(1,rep(2,(K*N)),1,2,3,6,7,8,9,10,11,
                          1,rep(2,(K*N)),2,3,4,7,8,9,10,11,12,
                          1,rep(2,(K*N)),3,4,5,8,9,10,11,12,13), byrow = TRUE, nrow = 3);

  # slack_plus
  slack_plus <- matrix(c(1,2,6,7,8,
                         2,3,7,8,9,
                         3,4,8,9,10), byrow = TRUE, nrow = 3);

  # slack_minus
  slack_minus <- matrix(c(3,9,10,11,
                          4,10,11,12,
                          5,11,12,13), byrow = TRUE, nrow = 3);

  t_ = matrix(c(1,
                1,
                1), byrow = TRUE, nrow = 3);

  lambda_ <- matrix(c(rep(2,(K*N)),
                      rep(2,(K*N)),
                      rep(2,(K*N))), byrow = TRUE, nrow = 3);

  Slack_transformation_res <- list("t" = t_, "lambda" = lambda_, "slack_plus" = slack_plus, "slack_minus" = slack_minus);



  nsbm.div_res <- matrix(c(  -2/4.5, -8/6.75, -7/3,
                             -1/5.5, -9/7.25, -7.5/4,
                             -1.5/4.5, -4.5/10.5, -11.5/3), byrow = TRUE, nrow = 3);



  #########################################
  #########################################
  #########################################


  # slack_transformation
  expect_equal(slacks.transformation(direction, weightsNSBM, K, N, sum_m, sum_r, sum_l, Link_obj), Slack_transformation_res, check.attributes = FALSE)

  # nsbm division
  expect_equal(nsbm.division(direction, slack_plus, slack_minus, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, Link_obj), nsbm.div_res, check.attributes = FALSE)

})
