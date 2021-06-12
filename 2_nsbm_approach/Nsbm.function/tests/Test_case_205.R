# Testcase 205

Data_1 <- matrix(c(3,5,8,1,
                   7,1,4,5,
                   2,5,3,2,
                   4,8,1,2),byrow=TRUE, nrow=4);

Input = matrix(c(3,8,
                 7,4,
                 2,3,
                 4,1), byrow = TRUE, nrow = 4);

Output = matrix(c(5,1,
                  1,5,
                  5,2,
                  8,2), byrow = TRUE, nrow = 4);

Link = NULL;

weights = c(0.5, 0.5);
K = 2;
N = 4;
sum_m = 2;
sum_r = 2;
sum_l = 0;
Amount = c(1,1,1,1,0);
Amount_Input = c(1,1);
Amount_Output = c(1,1);
Amount_Link = c(0);




direction = "output" ;
link_con = 2; #free
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




test_that("Test case 205",{

  #Righthand side and Direction
  Righthandside_i1 <- c(Input[1,], Output[1,], rep(1,1,K));
  Righthandside_i2 <- c(Input[2,], Output[2,], rep(1,1,K));
  Righthandside_i3 <- c(Input[3,], Output[3,], rep(1,1,K));
  Righthandside_i4 <- c(Input[4,], Output[4,], rep(1,1,K));

  Direction_ <- c(rep("=",sum_m), rep("=",sum_r),rep("=",1,K));

  Righthside_Direction_1 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i1);
  Righthside_Direction_2 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i2);
  Righthside_Direction_3 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i3);
  Righthside_Direction_4 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i4);



  #########################################
  #########################################
  #########################################


  #Righthandside and Direction:
  #i=1
  expect_equal(Righthandside_and_Direction(1, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_1, check.attributes = FALSE)

  #i=2
  expect_equal(Righthandside_and_Direction(2, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_2, check.attributes = FALSE)

  #i=3
  expect_equal(Righthandside_and_Direction(3, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_3, check.attributes = FALSE)

  #i=4
  expect_equal(Righthandside_and_Direction(4, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_4, check.attributes = FALSE)

})
