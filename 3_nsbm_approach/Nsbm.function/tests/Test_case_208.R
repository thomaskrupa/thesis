# Integration test
# Testcase 208:

Data_1 <- matrix(c(3, -5, 8, 1, 2,
                   7, 1, 4, 5, 8,
                   2, 0, 3, 2, 1),byrow=TRUE, nrow=3);


Input = matrix(c(3, 8,
                 7, 4,
                 2, 3), byrow = TRUE, nrow = 3);

Output = matrix(c(-5, 1,
                  1, 5,
                  2, 2), byrow = TRUE, nrow = 3);

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




direction = "non";
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



test_that("Test case 208",{


  #projection_frontier
  Output_vec_1 <- matrix(c(-5,
                           1,
                           2), byrow = TRUE, nrow = 3);

  y_head <- 2;
  y_bottom <- 1;


  Output_vec_res_1 <- matrix(c(1/7,
                               1,
                               2), byrow = TRUE, nrow = 3);



  # Adjustment Output for the obj function and constraint1:
  Output_N <- matrix(c(1/7,1,
                       1,5,
                       2,2), byrow = TRUE, nrow = 3);




  #objective function and Constraint1:
  Const_1_i1 <- matrix(c(1,rep(0,N),rep(0,N),(0.5/(1/7)),(0.5/1),rep(0,2)), byrow = TRUE, nrow = 1);
  Const_1_i2 <- matrix(c(1,rep(0,N),rep(0,N),(0.5/(1)), (0.5/(5)),rep(0,2)), byrow = TRUE, nrow = 1);
  Const_1_i3 <- matrix(c(1,rep(0,N),rep(0,N),(0.5/(2)), (0.5/2),rep(0,2)), byrow = TRUE, nrow = 1);

  obj_func_i1 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,2),-(0.5/3),-(0.5/8)), byrow = TRUE, nrow = 1);
  obj_func_i2 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,2),-(0.5/7),-(0.5/4)), byrow = TRUE, nrow = 1);
  obj_func_i3 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,2),-(0.5/2),-(0.5/3)), byrow = TRUE, nrow = 1);

  Obj_func_and_Const_i1 <- list( "FOBJ" = obj_func_i1, "FCON" = Const_1_i1);
  Obj_func_and_Const_i2 <- list( "FOBJ" = obj_func_i2, "FCON" = Const_1_i2);
  Obj_func_and_Const_i3 <- list( "FOBJ" = obj_func_i3, "FCON" = Const_1_i3);


  #########################################
  #########################################
  #########################################


  #Object.Function and Constraint 1 and direction="non" and i=1:
  expect_equal(Obj.func_and_con(1, direction, Input, Output_N, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i1, check.attributes = FALSE);

  #Object.Function and Constraint 1 and direction="non" and i=2:
  expect_equal(Obj.func_and_con(2, direction, Input, Output_N, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i2, check.attributes = FALSE) ;

  #Object.Function and Constraint 1 and direction="non" and i=3:
  expect_equal(Obj.func_and_con(3, direction, Input, Output_N, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i3, check.attributes = FALSE) ;


  #########################################

  #negative_zero_value:
  expect_equal(negative_zero_value(Output_vec_1, N), Output_vec_res_1, check.attributes = FALSE)

})
