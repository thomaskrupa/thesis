# Test case 127


Output <- matrix(c(-1,0,0,
                   0,5,0,
                   2,8,0), byrow = TRUE, nrow = 3);



N = 3; # Amount of DMU's



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


test_that("Test case 127",{


  # negative_zero_value

  #1
  Output_vec_1 <- matrix(c(-1,
                           0,
                           2), byrow = TRUE, nrow = 3);

  y_head <- 2;
  y_bottom <- 2;
  B <- 100*N; # 300


  Output_vec_res_1 <- Output <- matrix(c(4/900,
                                         4/600,
                                         2), byrow = TRUE, nrow = 3);



  # 2
  Output_vec_2 <- matrix(c(0,
                           5,
                           8), byrow = TRUE, nrow = 3);

  y_head <- 8;
  y_bottom <- 5;


  Output_vec_res_2 <- Output <- matrix(c(15/8,
                                         5,
                                         8), byrow = TRUE, nrow = 3);

  # 3
  Output_vec_3 <- matrix(c(0,
                           0,
                           0), byrow = TRUE, nrow = 3);

  y_head <- 1;
  y_bottom <- 1;
  B <- 100*N; # 300


  Output_vec_res_3 <- Output <- matrix(c(1/300,
                                         1/300,
                                         1/300), byrow = TRUE, nrow = 3);


  #########################################
  #########################################
  #########################################

  # slacks_transformation:
  expect_equal(negative_zero_value(Output_vec_1, N), Output_vec_res_1, check.attributes = FALSE)

  expect_equal(negative_zero_value(Output_vec_2, N), Output_vec_res_2, check.attributes = FALSE)

  expect_equal(negative_zero_value(Output_vec_3, N), Output_vec_res_3, check.attributes = FALSE)

})

