# Test case 245:

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

# Inputs
m1 <- 1;
m2 <- 1;

# Outputs
r1 <- 1;
r2 <- 1;

# Link
l1 <- 1;

# weights
w1 = 0.5;
w2 = 0.5;


direction = "non" ;
link_con = 1; # fix
return_to_scale = "VRS";
NIRS = 0;
Link_obj = 1; # Link variable in the objective function


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


test_that("Test case 245",{


  #objective function and Constraint1:
  Const_1_i1 <- matrix(c(1,rep(0,N),rep(0,N),(w1/(r1+l1))*(1/5),(w2/(r1))*(1/1),rep(0,sum_m),(w1/2)*(1/2),rep(0,sum_l)), byrow = TRUE, nrow = 1);
  Const_1_i2 <- matrix(c(1,rep(0,N),rep(0,N),(w1/(r1+l1))*(1/1),(w2/(r1))*(1/5),rep(0,sum_m),(w1/2)*(1/8),rep(0,sum_l)), byrow = TRUE, nrow = 1);
  Const_1_i3 <- matrix(c(1,rep(0,N),rep(0,N),(w1/(r1+l1))*(1/5),(w2/(r1))*(1/2),rep(0,sum_m),(w1/2)*(1/1),rep(0,sum_l)), byrow = TRUE, nrow = 1);

  obj_func_i1 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,sum_r),-(w1/(m1))*(1/3),-(w2/(m1+l1))*(1/8),rep(0,sum_l),-(w2/2)*(1/2)), byrow = TRUE, nrow = 1);
  obj_func_i2 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,sum_r),-(w1/(m1))*(1/7),-(w2/(m1+l1))*(1/4),rep(0,sum_l),-(w2/2)*(1/8)), byrow = TRUE, nrow = 1);
  obj_func_i3 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,sum_r),-(w1/(m1))*(1/2),-(w2/(m1+l1))*(1/3),rep(0,sum_l),-(w2/2)*(1/1)), byrow = TRUE, nrow = 1);

  Obj_func_and_Const_i1 <- list( "FOBJ" = obj_func_i1, "FCON" = Const_1_i1);
  Obj_func_and_Const_i2 <- list( "FOBJ" = obj_func_i2, "FCON" = Const_1_i2);
  Obj_func_and_Const_i3 <- list( "FOBJ" = obj_func_i3, "FCON" = Const_1_i3);


  #Constraint 2:
  Const_2_i1 <- matrix(c(-3,3,7,2,0,0,0,0,0,1,0,rep(0,(2*sum_l)),
                         -8,0,0,0,8,4,3,0,0,0,1,rep(0,(2*sum_l))), byrow = TRUE, nrow = 2);

  Const_2_i2 <- matrix(c(-7,3,7,2,0,0,0,0,0,1,0,rep(0,(2*sum_l)),
                         -4,0,0,0,8,4,3,0,0,0,1,rep(0,(2*sum_l))), byrow = TRUE, nrow = 2);

  Const_2_i3 <- matrix(c(-2,3,7,2,0,0,0,0,0,1,0,rep(0,(2*sum_l)),
                         -3,0,0,0,8,4,3,0,0,0,1,rep(0,(2*sum_l))), byrow = TRUE, nrow = 2);

  #Constraint 3:
  Const_3_i1 <- matrix(c(-5,5,1,5,0,0,0,-1,0,0,0,rep(0,(2*sum_l)),
                         -1,0,0,0,1,5,2,0,-1,0,0,rep(0,(2*sum_l))), byrow = TRUE, nrow = 2);

  Const_3_i2 <- matrix(c(-1,5,1,5,0,0,0,-1,0,0,0,rep(0,(2*sum_l)),
                         -5,0,0,0,1,5,2,0,-1,0,0,rep(0,(2*sum_l))), byrow = TRUE, nrow = 2);

  Const_3_i3 <- matrix(c(-5,5,1,5,0,0,0,-1,0,0,0,rep(0,(2*sum_l)),
                         -2,0,0,0,1,5,2,0,-1,0,0,rep(0,(2*sum_l))), byrow = TRUE, nrow = 2);

  #COnstraint 4:
  Const_4 <- matrix(c(-1,1,1,1,0,0,0,0,0,0,0,rep(0,(2*sum_l)),
                      -1,0,0,0,1,1,1,0,0,0,0,rep(0,(2*sum_l))), byrow = TRUE, nrow = 2);

  #Constraint 5:
  Const_5_i1 <- matrix(c(-2,2,8,1,0,0,0,0,0,0,0, -1,0,
                         -2,0,0,0,2,8,1,0,0,0,0, 0,1), byrow = TRUE, nrow = 2);

  Const_5_i2 <- matrix(c(-8,2,8,1,0,0,0,0,0,0,0, -1,0,
                         -8,0,0,0,2,8,1,0,0,0,0, 0,1), byrow = TRUE, nrow = 2);

  Const_5_i3 <- matrix(c(-1,2,8,1,0,0,0,0,0,0,0, -1,0,
                         -1,0,0,0,2,8,1,0,0,0,0, 0,1), byrow = TRUE, nrow = 2);

  #t Constraint:
  Const_t <- matrix(c(1,0,0,0,0,0,0,0,0,0,0,rep(0,(2*sum_l))), byrow = TRUE, nrow = 1);

  # weightsNSBM
  weightsNSBM <- matrix(c(1,rep(2,(K*N)),rep(3,(sum_r)), rep(4,(sum_m)), rep(5,(sum_l)), rep(6,(sum_l)),
                          1,rep(2,(K*N)),rep(7,(sum_r)), rep(8,(sum_m)), rep(9,(sum_l)), rep(10,(sum_l)),
                          1,rep(2,(K*N)),rep(11,(sum_r)), rep(12,(sum_m)), rep(13,(sum_l)), rep(14,(sum_l))), byrow = TRUE, nrow = 3);

  # slack_plus
  slack_plus <- matrix(c(rep(3,(sum_r)),rep(5,(sum_l)),
                         rep(7,(sum_r)),rep(9,(sum_l)),
                         rep(11,(sum_r)),rep(13,(sum_l))), byrow = TRUE, nrow = 3);

  # slack_minus
  slack_minus <- matrix(c(rep(4,(sum_r)),rep(6,(sum_l)),
                          rep(8,(sum_r)),rep(10,(sum_l)),
                          rep(12,(sum_r)),rep(14,(sum_l))), byrow = TRUE, nrow = 3);

  t_ = matrix(c(1,
                1,
                1), byrow = TRUE, nrow = 3);

  lambda_ <- matrix(c(rep(2,(K*N)),
                      rep(2,(K*N)),
                      rep(2,(K*N))), byrow = TRUE, nrow = 3);

  Slack_transformation_res <- list("t" = t_, "lambda" = lambda_, "slack_plus" = slack_plus, "slack_minus" = slack_minus);



  nsbm.div_res <- matrix(c(  ((-1/3)/(51/20)), (-3/4)/4 ,
                             (-1/7)/(81/16), (-10/16)/(12/5),
                             -5/8.6 , -8/(13/2)), byrow = TRUE, nrow = 3);

  #########################################
  #########################################
  #########################################


  #Object.Function and Constraint 1 and direction="non" and i=1:
  expect_equal(Obj.func_and_con(1, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i1, check.attributes = FALSE);

  #Object.Function and Constraint 2 and direction="non" and i=2:
  expect_equal(Obj.func_and_con(2, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i2, check.attributes = FALSE) ;

  #Object.Function and Constraint 2 and direction="non" and i=3:
  expect_equal(Obj.func_and_con(3, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i3, check.attributes = FALSE) ;



  #########################################

  #Constraint 2 and i=1:
  expect_equal(Constraint2(1, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i1, check.attributes = FALSE)

  #Constraint 2 and i=2:
  expect_equal(Constraint2(2, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i2, check.attributes = FALSE)

  #Constraint 2 and i=3:
  expect_equal(Constraint2(3, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i3, check.attributes = FALSE)

  ########################################

  #Constraint 3 and i=1:
  expect_equal(Constraint3(1, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i1, check.attributes = FALSE)

  #Constraint 3 and i=2:
  expect_equal(Constraint3(2, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i2, check.attributes = FALSE)

  #Constraint 3 and i=3:
  expect_equal(Constraint3(3, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i3, check.attributes = FALSE)


  ########################################

  #vrs constraint:
  expect_equal(vrs.constraint(direction, K, N, sum_m, sum_r, sum_l, Link_obj), Const_4, check.attributes = FALSE)


  ########################################

  #Constraint 5 and i=1:
  expect_equal(Constraint5(1, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i1, check.attributes = FALSE)

  #Constraint 5 and i=2:
  expect_equal(Constraint5(2, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i2, check.attributes = FALSE)

  #Constraint 5 and i=3:
  expect_equal(Constraint5(3, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i3, check.attributes = FALSE)


  ########################################

  #t Constraint
  expect_equal(t_constraint(K, N, sum_m, sum_r, sum_l, Link_obj), Const_t, check.attributes = FALSE)

  ########################################

  # slack_transformation
  expect_equal(slacks.transformation(direction, weightsNSBM, K, N, sum_m, sum_r, sum_l, Link_obj), Slack_transformation_res, check.attributes = FALSE)

  # nsbm division
  expect_equal(nsbm.division(direction, slack_plus, slack_minus, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, Link_obj), nsbm.div_res, check.attributes = FALSE)

})
