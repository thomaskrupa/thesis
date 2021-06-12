# Testcase 234:

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

Amount = c(2,1,1,1,1);
Amount_Input = c(1,1);
Amount_Output = c(1,1);
Amount_Link = c(1);




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



test_that("Test case 234",{


  error_ <- "The dimension of Data and the Amount vector do not fit!";


  #########################################
  #########################################
  #########################################


  # Check dim of Data_1 and Amount
  expect_error(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K,  weights, NIRS, Link_obj), error_)

})
