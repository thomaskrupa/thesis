# Testcase 226:

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




direction = "input" ;
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



test_that("Test case 226",{


  total_wide <- sum(K*N, sum_r,sum_m);
  total_height <- sum(sum_m,sum_r, K, sum_l);




  B <- Constraint2(1, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj);
  C <- Constraint3(1, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj);

  D <- vrs.constraint(direction, K, N, sum_m, sum_r, sum_l, Link_obj);
  E <- Constraint5(1, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj);

  f.con_ <- rbind(B, C, D, E);

  #########################################
  #########################################
  #########################################


  #Object.Function
  expect_equal(dim(Obj.func_and_con(1, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj)$FOBJ)[2], total_wide, check.attributes = FALSE);


  #########################################

  #Constraint 2 and i=1:
  expect_equal(dim(Constraint2(1, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj))[2], total_wide, check.attributes = FALSE)


  ########################################

  #Constraint 3 and i=1:
  expect_equal(dim(Constraint3(1, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj))[2], total_wide, check.attributes = FALSE)


  ########################################

  #vrs constraint:
  expect_equal(dim(vrs.constraint(direction, K, N, sum_m, sum_r, sum_l, Link_obj))[2], total_wide, check.attributes = FALSE)


  ########################################

  #Constraint 5 and i=1:
  expect_equal(dim(Constraint5(1, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj))[2], total_wide, check.attributes = FALSE)


  ########################################

  #height
  expect_equal(dim(f.con_)[1], total_height, check.attributes = FALSE)

})

