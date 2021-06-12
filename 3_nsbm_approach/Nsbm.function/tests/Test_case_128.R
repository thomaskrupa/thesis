# Test case 128


Output <- matrix(c(0,1,1,
                   0,5,-1,
                   0,8,-1,
                   1,2,-4,
                   0,2,8), byrow = TRUE, nrow = 5);



N = 5; # Amount of DMU's



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




test_that("Test case 128",{


  # negative_zero_value

  #1
  Output_vec_1 <- matrix(c(0,
                           0,
                           0,
                           1,
                           -10), byrow = TRUE, nrow = 5);

  y_head <- 1;
  y_bottom <- 1;
  B <- 100*N; # 500


  Output_vec_res_1 <- matrix(c(1/500,
                               1/500,
                               1/500,
                               1,
                               1/5500), byrow = TRUE, nrow = 5);




  # 3
  Output_vec_3 <- matrix(c(1,
                           -1,
                           -1,
                           -4,
                           8), byrow = TRUE, nrow = 5);

  y_head <- 8;
  y_bottom <- 1;


  Output_vec_res_3 <- Output <- matrix(c(1,
                                         7/9,
                                         7/9,
                                         7/12,
                                         8), byrow = TRUE, nrow = 5);


  #########################################
  #########################################
  #########################################

  # slacks_transformation:
  expect_equal(negative_zero_value(Output_vec_1, N), Output_vec_res_1, check.attributes = FALSE)

  expect_equal(negative_zero_value(Output_vec_3, N), Output_vec_res_3, check.attributes = FALSE)

})

