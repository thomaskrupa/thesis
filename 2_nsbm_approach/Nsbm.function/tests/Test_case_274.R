# Test case 274:

Data_1 <- matrix(c(3, 5, 8, 1, 2,
                   7, 1, 4, 5, 8,
                   2, 5, 3, 2, 1),byrow=TRUE, nrow=3);


Input = matrix(c(3, 8,
                 7, 4,
                 2, 3), byrow = TRUE, nrow = 3);

Output = matrix(c(5, 1,
                  1, 5,
                  5, 2), byrow = TRUE, nrow = 3);

Link = matrix(c(2,
                8,
                1), byrow = TRUE, nrow = 3);

weights = c(0.5, 0.5);

K = 2;
N = 3;
sum_m = 2;
sum_r = 2;
sum_l = 1;

Amount = c(1,1,1,1,1);
Amount_Input = c(1,1);
Amount_Output = c(1,1);
Amount_Link = c(1);

# Inputs
m1 <- 1;
m2 <- 1;

# Outputs
r1 <- 1;
r2 <- 1;

# Link
l1 <- 1;

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



test_that("Test case 274",{


  # weightsNSBM
  weightsNSBM <- matrix(c(2,rep(2,(K*N)),rep(3,(sum_r)), rep(4,(sum_m)), rep(5,(sum_l)), rep(6,(sum_l)),
                          4,rep(2,(K*N)),rep(7,(sum_r)), rep(8,(sum_m)), rep(9,(sum_l)), rep(10,(sum_l)),
                          5,rep(2,(K*N)),rep(11,(sum_r)), rep(12,(sum_m)), rep(13,(sum_l)), rep(14,(sum_l))), byrow = TRUE, nrow = 3);

  # slack_plus
  slack_plus <- matrix(c(rep(3/2,(sum_r)),rep(5/2,(sum_l)),
                         rep(7/4,(sum_r)),rep(9/4,(sum_l)),
                         rep(11/5,(sum_r)),rep(13/5,(sum_l))), byrow = TRUE, nrow = 3);

  # slack_minus
  slack_minus <- matrix(c(rep(4/2,(sum_r)),rep(6/2,(sum_l)),
                          rep(8/4,(sum_r)),rep(10/4,(sum_l)),
                          rep(12/5,(sum_r)),rep(14/5,(sum_l))), byrow = TRUE, nrow = 3);

  t_ = matrix(c(2,
                4,
                5), byrow = TRUE, nrow = 3);

  lambda_ <- matrix(c(rep(2/2,(K*N)),
                      rep(2/4,(K*N)),
                      rep(2/5,(K*N))), byrow = TRUE, nrow = 3);

  Slack_transformation_res <- list("t" = t_, "lambda" = lambda_, "slack_plus" = slack_plus, "slack_minus" = slack_minus);



  #########################################
  #########################################
  #########################################


  # slack_transformation
  expect_equal(slacks.transformation(direction, weightsNSBM, K, N, sum_m, sum_r, sum_l, Link_obj), Slack_transformation_res, check.attributes = FALSE)

})
