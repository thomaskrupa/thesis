#Test case 47:

Data_1 <- matrix(data=1:32,nrow=4,ncol=8);
Data_1 <- matrix(c(7,3,5,6,7,8,10,1,
                   4,8,2,1,3,2,4,5,
                   1,7,5,5,6,7,8,2,
                   2,1,4,8,3,5,4,1),byrow=TRUE, nrow=4);


Input = matrix(c(7,5,7,
                 4,2,3,
                 1,5,6,
                 2,4,3), byrow = TRUE, nrow = 4);

Output = matrix(c(3,6,8,
                  8,1,2,
                  7,5,7,
                  1,8,5), byrow = TRUE, nrow = 4);

Link = matrix(c(10,1,
                4,5,
                8,2,
                4,1), byrow = TRUE, nrow = 4);

weights = c(0.2, 0.3, 0.5);

w1 <- weights[1];
w2 <- weights[2];
w3 <- weights[3];


K = 3;
N = 4;
sum_m = 3;
sum_r = 3;
sum_l = 2;

Amount = c(1,1,1,1,1,1,1,1);
Amount_Input = c(1,1,1);
Amount_Output = c(1,1,1);
Amount_Link = c(1,1);

direction = "non";
link_con = 2; #free
return_to_scale = "VRS"
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


test_that("Test case 46",{


  #Amount split function
  Amount_split_ <- list("Amount_IN" = Amount_Input, "Amount_OUT" = Amount_Output, "Amount_LINK" = Amount_Link);

  #Data split:
  data_split_ <- list( "In" = Input, "Out" = Output, "Link" = Link);


  #Righthand side and Direction:
  Righthandside <- c(1,rep(0,sum_m),rep(0,sum_r),rep(0,K),rep(0,(sum_l)),rep(0,1));
  Direction_ <- c(rep("=",1),rep("=",sum_m),rep("=",sum_r),rep("=",K),rep("=",(sum_l)), ">");
  Righthside_Direction_ <- list( "FDIR" = Direction_, "FRHS" = Righthandside);

  #objective function and Constraint1:
  Const_1_i1 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w1/3),(w2/6),(w3/8),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i2 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w1/8),(w2/1),(w3/2),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i3 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w1/7),(w2/5),(w3/7),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i4 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w1/1),(w2/8),(w3/5),rep(0,sum_m)), byrow = TRUE, nrow = 1);

  obj_func_i1 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/7),-(w2/5),-(w3/7)), byrow = TRUE, nrow = 1);
  obj_func_i2 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/4),-(w2/2),-(w3/3)), byrow = TRUE, nrow = 1);
  obj_func_i3 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/1),-(w2/5),-(w3/6)), byrow = TRUE, nrow = 1);
  obj_func_i4 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/2),-(w2/4),-(w3/3)), byrow = TRUE, nrow = 1);

  Obj_func_and_Const_i1 <- list( "FOBJ" = obj_func_i1, "FCON" = Const_1_i1);
  Obj_func_and_Const_i2 <- list( "FOBJ" = obj_func_i2, "FCON" = Const_1_i2);
  Obj_func_and_Const_i3 <- list( "FOBJ" = obj_func_i3, "FCON" = Const_1_i3);
  Obj_func_and_Const_i4 <- list( "FOBJ" = obj_func_i4, "FCON" = Const_1_i4);


  #Constraint 2:
  Const_2_i1 <- matrix(c(-7,7,4,1,2,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -5,rep(0,N),5,2,5,4,rep(0,N),rep(0,sum_r),0,1,0,
                         -7,rep(0,N),rep(0,N),7,3,6,3,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = 3);

  Const_2_i2 <- matrix(c(-4,7,4,1,2,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -2,rep(0,N),5,2,5,4,rep(0,N),rep(0,sum_r),0,1,0,
                         -3,rep(0,N),rep(0,N),7,3,6,3,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = 3);

  Const_2_i3 <- matrix(c(-1,7,4,1,2,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -5,rep(0,N),5,2,5,4,rep(0,N),rep(0,sum_r),0,1,0,
                         -6,rep(0,N),rep(0,N),7,3,6,3,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = 3);

  Const_2_i4 <- matrix(c(-2,7,4,1,2,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -4,rep(0,N),5,2,5,4,rep(0,N),rep(0,sum_r),0,1,0,
                         -3,rep(0,N),rep(0,N),7,3,6,3,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = 3);


  #Constraint 3:
  Const_3_i1 <- matrix(c(-3,3,8,7,1,rep(0,N),rep(0,N),-1,0,0,rep(0,sum_m),
                         -6,rep(0,N),6,1,5,8,rep(0,N),0,-1,0,rep(0,sum_m),
                         -8,rep(0,N),rep(0,N),8,2,7,5,0,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 3);

  Const_3_i2 <- matrix(c(-8,3,8,7,1,rep(0,N),rep(0,N),-1,0,0,rep(0,sum_m),
                         -1,rep(0,N),6,1,5,8,rep(0,N),0,-1,0,rep(0,sum_m),
                         -2,rep(0,N),rep(0,N),8,2,7,5,0,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 3);

  Const_3_i3 <- matrix(c(-7,3,8,7,1,rep(0,N),rep(0,N),-1,0,0,rep(0,sum_m),
                         -5,rep(0,N),6,1,5,8,rep(0,N),0,-1,0,rep(0,sum_m),
                         -7,rep(0,N),rep(0,N),8,2,7,5,0,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 3);

  Const_3_i4 <- matrix(c(-1,3,8,7,1,rep(0,N),rep(0,N),-1,0,0,rep(0,sum_m),
                         -8,rep(0,N),6,1,5,8,rep(0,N),0,-1,0,rep(0,sum_m),
                         -5,rep(0,N),rep(0,N),8,2,7,5,0,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 3);


  #COnstraint 4:
  Const_4 <- matrix(c(-1,rep(1,N),rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                      -1,rep(0,N),rep(1,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                      -1,rep(0,N),rep(0,N),rep(1,N),rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = 3);

  #Constraint 5:
  Const_5 <- matrix(c(0,-10,-4,-8,-4,10,4,8,4,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         0,rep(0,N),-1,-5,-2,-1,1,5,2,1,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = 2);


  #t Constraint:
  Const_t <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = 1);



  #########################################
  #########################################
  #########################################

  #Data split function:
  expect_equal(Amount.split(Amount, K), Amount_split_ , check.attributes = FALSE)


  #########################################

  #Data split function:
  expect_equal(data.split(Data_1, Amount, K, N), data_split_ , check.attributes = FALSE)



  #########################################

  #Righthandside and Direction:
  expect_equal(Righthandside_and_Direction(1, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_, check.attributes = FALSE)


  #########################################

  #Object.Function and Constraint 1 and direction="non" and i=1:
  expect_equal(Obj.func_and_con(1, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i1, check.attributes = FALSE);

  #Object.Function and Constraint 2 and direction="non" and i=2:
  expect_equal(Obj.func_and_con(2, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i2, check.attributes = FALSE) ;

  #Object.Function and Constraint 2 and direction="non" and i=3:
  expect_equal(Obj.func_and_con(3, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i3, check.attributes = FALSE) ;

  #Object.Function and Constraint 2 and direction="non" and i=4:
  expect_equal(Obj.func_and_con(4, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i4, check.attributes = FALSE) ;


  #########################################

  #Constraint 2 and i=1:
  expect_equal(Constraint2(1, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i1, check.attributes = FALSE)

  #Constraint 2 and i=2:
  expect_equal(Constraint2(2, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i2, check.attributes = FALSE)

  #Constraint 2 and i=3:
  expect_equal(Constraint2(3, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i3, check.attributes = FALSE)

  #Constraint 2 and i=4:
  expect_equal(Constraint2(4, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i4, check.attributes = FALSE)


  ########################################

  #Constraint 3 and i=1:
  expect_equal(Constraint3(1, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i1, check.attributes = FALSE)

  #Constraint 3 and i=2:
  expect_equal(Constraint3(2, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i2, check.attributes = FALSE)

  #Constraint 3 and i=3:
  expect_equal(Constraint3(3, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i3, check.attributes = FALSE)

  #Constraint 3 and i=4:
  expect_equal(Constraint3(4, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i4, check.attributes = FALSE)



  ########################################

  #VRS constraint:
  expect_equal(vrs.constraint(direction, K, N, sum_m, sum_r, sum_l, Link_obj), Const_4, check.attributes = FALSE)

  ########################################

  #Constraint 5:
  expect_equal(Constraint5(1, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5, check.attributes = FALSE)


  ########################################

  #t Constraint
  expect_equal(t_constraint(K, N, sum_m, sum_r, sum_l, Link_obj), Const_t, check.attributes = FALSE)


})














