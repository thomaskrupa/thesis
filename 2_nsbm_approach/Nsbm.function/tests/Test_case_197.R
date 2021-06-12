# Testcase 197


Data_1 <- matrix(c(7,3,5,6,7,8,10,1,1,
                   4,8,2,1,3,2,4,5,1,
                   1,7,5,5,6,7,8,2,1,
                   2,1,4,8,3,5,4,1,1),byrow=TRUE, nrow=4);


Input = matrix(c(7,5,7,
                 4,2,3,
                 1,5,6,
                 2,4,3), byrow = TRUE, nrow = 4);

Output = matrix(c(3,6,8,
                  8,1,2,
                  7,5,7,
                  1,8,5), byrow = TRUE, nrow = 4);

Link = matrix(c(10,1,1,
                4,5,1,
                8,2,1,
                4,1,1), byrow = TRUE, nrow = 4);

weights = c(0.2, 0.3, 0.5);

w1 <- weights[1];
w2 <- weights[2];
w3 <- weights[3];


K = 3;
N = 4;
sum_m = 3;
sum_r = 3;
sum_l = 3;

Amount = c(1,1,1,1,1,1,1,1);
Amount_Input = c(1,1,1);
Amount_Output = c(1,1,1);
Amount_Link = c(1,2);




direction = "input";
link_con = 2; #free
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



test_that("Test case 197",{


  #Righthand side and Direction
  Righthandside_i1 <- c(Input[1,], Output[1,], rep(0,1,sum_l));
  Righthandside_i2 <- c(Input[2,], Output[2,], rep(0,1,sum_l));
  Righthandside_i3 <- c(Input[3,], Output[3,], rep(0,1,sum_l));
  Righthandside_i4 <- c(Input[4,], Output[4,], rep(0,1,sum_l));

  Direction_ <- c(rep("=",sum_m), rep("=",sum_r),rep("=",(sum_l)));

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
