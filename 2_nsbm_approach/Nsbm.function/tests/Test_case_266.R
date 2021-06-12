# Test case 266:
# life insurance model

Data_1 <- matrix(c(1, 1, 2, 2, 1,2,
                   2, 1, 1, 2, 1,1,
                   2, 1, 2, 2, 2,1),byrow=TRUE, nrow=3);


Input = matrix(c(1, 1,2,
                 2, 1,1,
                 2, 1,2), byrow = TRUE, nrow = 3);

Output = matrix(c(2,
                  2,
                  2), byrow = TRUE, nrow = 3);

Link = matrix(c(1,2,
                1,1,
                2,1), byrow = TRUE, nrow = 3);

weights = c(0.5, 0.5);

K = 2;
N = 3;
sum_m = 3; # inputs
sum_r = 1; # outputs
sum_l = 2;

Amount = c(3,0,0,1,2);
Amount_Input = c(3,0);
Amount_Output = c(0,1);
Amount_Link = c(2);

# Inputs
m1 <- 3;
m2 <- 0;

# Outputs
r1 <- 0;
r2 <- 1;

# Link
l1 <- 2;

# weights
w1 = 0.5;
w2 = 0.5;


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


test_that("Test case 266",{


  # weightsNSBM
  weightsNSBM <- matrix(c(1,rep(2,(K*N)),1,2,3,4,5,6,7,8,
                          1,rep(2,(K*N)),2,3,4,5,6,7,8,9,
                          1,rep(2,(K*N)),3,4,5,6,7,8,9,10), byrow = TRUE, nrow = 3);

  # slack_plus
  slack_plus <- matrix(c(1,5,6,
                         2,6,7,
                         3,7,8), byrow = TRUE, nrow = 3);

  # slack_minus
  slack_minus <- matrix(c(2,3,4,7,8,
                          3,4,5,8,9,
                          4,5,6,9,10), byrow = TRUE, nrow = 3);

  t_ = matrix(c(1,
                1,
                1), byrow = TRUE, nrow = 3);

  lambda_ <- matrix(c(rep(2,(K*N)),
                      rep(2,(K*N)),
                      rep(2,(K*N))), byrow = TRUE, nrow = 3);

  Slack_transformation_res <- list("t" = t_, "lambda" = lambda_, "slack_plus" = slack_plus, "slack_minus" = slack_minus);



  nsbm.div_res <- matrix(c(  -(4/3)/5, -4.5/1.5,
                             -(7.5/3)/7.5, -7.5/2,
                             -(7/3)/(13.5/2),-(12.5/2)/(5/2)), byrow = TRUE, nrow = 3);



  #########################################
  #########################################
  #########################################


  # slack_transformation
  expect_equal(slacks.transformation(direction, weightsNSBM, K, N, sum_m, sum_r, sum_l, Link_obj), Slack_transformation_res, check.attributes = FALSE)

  # nsbm division
  expect_equal(nsbm.division(direction, slack_plus, slack_minus, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, Link_obj), nsbm.div_res, check.attributes = FALSE)

})
