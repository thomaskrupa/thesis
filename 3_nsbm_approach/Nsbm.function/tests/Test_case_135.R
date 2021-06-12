# Test case 135

Data_1 <- matrix(c(3, 5, 8, 1,
                   7, 1, 4, 5,
                   2, 5, 3, 2),byrow=TRUE, nrow=3);


Input = matrix(c(3, 8,
                 7, 4,
                 2, 3), byrow = TRUE, nrow = 3);

Output = matrix(c(5, 1,
                  1, 5,
                  5, 2), byrow = TRUE, nrow = 3);

Link = NaN;

weights = c(1);
K = 1;
N = 3;
sum_m = 2;
sum_r = 2;
sum_l = NaN;
Amount = c(2,2);
Amount_Input = c(2);
Amount_Output = c(2);
Amount_Link = NaN;




direction = "non" ;
link_con = NaN;
return_to_scale = "VRS";
NIRS = 1; # NIRS = 1
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



test_that("Test case 135",{


  #Righthandside_and_ Direction:
  Righthandside <- c(1, rep(0,1,sum_m), rep(0,1,sum_r), rep(0,1,K), 0);

  # NIRS = 1 adjusted
  Direction_ <- c("=", rep("=",1,sum_m), rep("=",1,sum_r), rep("<=",1,K), ">");

  Righthside_Direction_ <- list( "FDIR" = Direction_, "FRHS" = Righthandside);


  #########################################
  #########################################
  #########################################


  #Righthandside_and_Direction:
  expect_equal(Righthandside_and_Direction(1, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_, check.attributes = FALSE)


})

