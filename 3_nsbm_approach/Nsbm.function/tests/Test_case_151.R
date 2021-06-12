# Testcase 151


Data_1 <- matrix(c(7,3,5,6,7,8,1,
                   4,8,2,1,3,2,5,
                   1,7,5,5,6,7,2,
                   2,1,4,8,3,5,1),byrow=TRUE, nrow=4);


Input = matrix(c(7,5,7,
                 4,2,3,
                 1,5,6,
                 2,4,3), byrow = TRUE, nrow = 4);

Output = matrix(c(3,6,8,
                  8,1,2,
                  7,5,7,
                  1,8,5), byrow = TRUE, nrow = 4);

Link = matrix(c(1,
                5,
                2,
                1), byrow = TRUE, nrow = 4);

weights = c(0.2, 0.3, 0.5);

w1 <- weights[1];
w2 <- weights[2];
w3 <- weights[3];


K = 3;
N = 4;
sum_m = 3;
sum_r = 3;
sum_l = 1;

Amount = c(1,1,1,1,1,1,1,1);
Amount_Input = c(1,1,1);
Amount_Output = c(1,1,1);
Amount_Link = c(0,1);




direction = "non";
link_con = 1; #fix
return_to_scale = "CRS"
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




test_that("Test case 151",{



  #Righthand side and Direction:
  Righthandside <- c(1,rep(0,sum_m),rep(0,sum_r),rep(0,(sum_l*2)),rep(0,1));

  Direction_ <- c(rep("=",1),rep("=",sum_m),rep("=",sum_r),rep("=",(sum_l*2)), ">");

  Righthside_Direction_ <- list( "FDIR" = Direction_, "FRHS" = Righthandside);



  #########################################
  #########################################
  #########################################


  #Righthandside and Direction:
  expect_equal(Righthandside_and_Direction(1, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_, check.attributes = FALSE)

})
