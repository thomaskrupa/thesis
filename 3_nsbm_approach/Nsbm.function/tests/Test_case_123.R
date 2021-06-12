# Test case 123


Output <- matrix(c(-1,2,3,
                   4,5,6,
                   7,8,9), byrow = TRUE, nrow = 3);



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



test_that("Test case 123",{


  #negative_zero_value

  Output_vec_1 <- matrix(c(-1,
                           4,
                           7), byrow = TRUE, nrow = 3);

  y_head <- 7;
  y_bottom <- 4;


  Output_vec_res <- Output <- matrix(c(1.5,
                                       4,
                                       7), byrow = TRUE, nrow = 3);


  #########################################
  #########################################
  #########################################

  #negative_zero_value:
  expect_equal(negative_zero_value(Output_vec_1, N), Output_vec_res, check.attributes = FALSE)

})
