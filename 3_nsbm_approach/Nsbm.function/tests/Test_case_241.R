# Testcase 241:

Data_1 <- matrix(c(5, 1, 2,
                   1, 5, 8,
                   5, 2, 1),byrow=TRUE, nrow=3);


Input = NaN;

Output = matrix(c(5, 1, 2,
                  1, 5, 8,
                  5, 2, 1), byrow = TRUE, nrow = 3);

Link = NaN;

weights = matrix(c(1), byrow = TRUE, nrow = 1);

K = 1;
N = 3;
sum_m = 0;
sum_r = 2;
sum_l = NaN;

Amount = c(0,3);
Amount_Input = c(0);
Amount_Output = c(3);
Amount_Link = NaN;




direction = "non" ;
link_con = 1; #fix
return_to_scale = "VRS";
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



test_that("Test case 241",{


  error_ <- "The SBM model needs at least one input and one output variable";


  #########################################
  #########################################
  #########################################


  # Check dim of Data_1 and Amount
  expect_error(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K,  weights, NIRS, Link_obj), error_)

})
