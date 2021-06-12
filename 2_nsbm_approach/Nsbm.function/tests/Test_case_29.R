#Test case 29:

Data_1 <- matrix(c(3,5,8,1,2,
                   7,1,4,5,8,
                   2,5,3,2,1,
                   4,8,1,2,5),byrow=TRUE, nrow=4);

Input = matrix(c(3,8,
                 7,4,
                 2,3,
                 4,1), byrow = TRUE, nrow = 4);

Output = matrix(c(5,1,
                  1,5,
                  5,2,
                  8,2), byrow = TRUE, nrow = 4);

Link = matrix(c(2,
                8,
                1,
                5), byrow = TRUE, nrow = 4);

weights = c(0.5, 0.5);

K = 2;
N = 4;
sum_m = 2;
sum_r = 2;
sum_l = 1;

Amount = c(1,1,1,1,1);
Amount_Input = c(1,1);
Amount_Output = c(1,1);
Amount_Link = c(1);




direction = "input" ;
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




test_that("Test case 29",{

  #Amount split function
  Amount_split_ <- list("Amount_IN" = Amount_Input, "Amount_OUT" = Amount_Output, "Amount_LINK" = Amount_Link);

  #Data split:
  data_split_ <- list( "In" = Input, "Out" = Output, "Link" = Link);


  #Righthand side and Direction:
  Righthandside_i1 <- c(Input[1,], Output[1,], rep(1,K), rep(0,sum_l));
  Righthandside_i2 <- c(Input[2,], Output[2,], rep(1,K), rep(0,sum_l));
  Righthandside_i3 <- c(Input[3,], Output[3,], rep(1,K), rep(0,sum_l));
  Righthandside_i4 <- c(Input[4,], Output[4,], rep(1,K), rep(0,sum_l));

  Direction_ <- c(rep("=",sum_m), rep("=",sum_r),rep("=",K),rep("=",(sum_l)));

  Righthside_Direction_1 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i1);
  Righthside_Direction_2 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i2);
  Righthside_Direction_3 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i3);
  Righthside_Direction_4 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i4);


  #objective function and Constraint1:
  Const_1_i1 <- NaN;
  Const_1_i2 <- NaN;
  Const_1_i3 <- NaN;
  Const_1_i4 <- NaN;

  obj_func_i1 <- matrix(c(rep(0,N),rep(0,N),rep(0,sum_r),-(0.5/3),-(0.5/8)), byrow = TRUE, nrow = 1);
  obj_func_i2 <- matrix(c(rep(0,N),rep(0,N),rep(0,sum_r),-(0.5/7),-(0.5/4)), byrow = TRUE, nrow = 1);
  obj_func_i3 <- matrix(c(rep(0,N),rep(0,N),rep(0,sum_r),-(0.5/2),-(0.5/3)), byrow = TRUE, nrow = 1);
  obj_func_i4 <- matrix(c(rep(0,N),rep(0,N),rep(0,sum_r),-(0.5/4),-(0.5/1)), byrow = TRUE, nrow = 1);

  Obj_func_and_Const_i1 <- list( "FOBJ" = obj_func_i1, "FCON" = Const_1_i1);
  Obj_func_and_Const_i2 <- list( "FOBJ" = obj_func_i2, "FCON" = Const_1_i2);
  Obj_func_and_Const_i3 <- list( "FOBJ" = obj_func_i3, "FCON" = Const_1_i3);
  Obj_func_and_Const_i4 <- list( "FOBJ" = obj_func_i4, "FCON" = Const_1_i4);


  #Constraint 2:
  Const_2_i1 <- matrix(c(3,7,2,4,rep(0,N),rep(0,sum_r),1,0,
                         rep(0,N),8,4,3,1,rep(0,sum_r),0,1), byrow = TRUE, nrow = 2);

  Const_2_i2 <- matrix(c(3,7,2,4,rep(0,N),rep(0,sum_m),1,0,
                         rep(0,N),8,4,3,1,rep(0,sum_m),0,1), byrow = TRUE, nrow = 2);

  Const_2_i3 <- matrix(c(3,7,2,4,rep(0,N),rep(0,sum_r),1,0,
                         rep(0,N),8,4,3,1,rep(0,sum_r),0,1), byrow = TRUE, nrow = 2);

  Const_2_i4 <- matrix(c(3,7,2,4,rep(0,N),rep(0,sum_r),1,0,
                         rep(0,N),8,4,3,1,rep(0,sum_r),0,1), byrow = TRUE, nrow = 2);

  #Constraint 3:
  Const_3_i1 <- matrix(c(5,1,5,8,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),1,5,2,2,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 2);

  Const_3_i2 <- matrix(c(5,1,5,8,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),1,5,2,2,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 2);

  Const_3_i3 <- matrix(c(5,1,5,8,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),1,5,2,2,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 2);

  Const_3_i4 <- matrix(c(5,1,5,8,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),1,5,2,2,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 2);

  #VRS Constraint:
  vrs_constraint <- matrix(c(rep(1,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                           rep(0,N),rep(1,N),rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = 2);

  #Constraint 5:
  Const_5 <- matrix(c(-2,-8,-1,-5,2,8,1,5,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = 1);


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
#i=1
expect_equal(Righthandside_and_Direction(1, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_1, check.attributes = FALSE)

#i=2
expect_equal(Righthandside_and_Direction(2, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_2, check.attributes = FALSE)

#i=3
expect_equal(Righthandside_and_Direction(3, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_3, check.attributes = FALSE)

#i=4
expect_equal(Righthandside_and_Direction(4, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_4, check.attributes = FALSE)


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
expect_equal(vrs.constraint(direction, K, N, sum_m, sum_r, sum_l, Link_obj), vrs_constraint, check.attributes = FALSE)


########################################

#Constraint 5:
expect_equal(Constraint5(1, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5, check.attributes = FALSE)


})
