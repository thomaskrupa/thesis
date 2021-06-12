#Test case 20:

Data_1 <- matrix(c(7,2,3,1,5,4,6,2,7,5,8,2,10,5,1,4,
                   4,3,8,1,2,3,1,1,3,1,2,2,4,7,5,3,
                   1,8,7,4,5,7,5,4,6,4,7,1,8,4,2,2),byrow=TRUE, nrow=3);

Input = matrix(c(7,2,5,4,7,5,
                 4,3,2,3,3,1,
                 1,8,5,7,6,4), byrow = TRUE, nrow = 3);

Output = matrix(c(3,1,6,2,8,2,
                  8,1,1,1,2,2,
                  7,4,5,4,7,1), byrow = TRUE, nrow = 3);

Link = matrix(c(10,5,1,4,
                4,7,5,3,
                8,4,2,2), byrow = TRUE, nrow = 3);

weights = c(0.5, 0.5,0.5);

K = 3;
N = 3;
sum_m = 6;
sum_r = 6;
sum_l = 4;

Amount = c(2,2,2,2,2,2,2,2);
Amount_Input = c(2,2,2);
Amount_Output = c(2,2,2);
Amount_Link = c(2,2);




direction = "input" ;
link_con = 1; #fix
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



test_that("Test case 20",{


  #Amount split function
  Amount_split_ <- list("Amount_IN" = Amount_Input, "Amount_OUT" = Amount_Output, "Amount_LINK" = Amount_Link);

  #Data split:
  data_split_ <- list( "In" = Input, "Out" = Output, "Link" = Link);


  #Righthand side and Direction:
  Righthandside_i1 <- c(Input[1, ], Output[1, ], rep(1,K), 10,5,10,5,1,4,1,4);
  Righthandside_i2 <- c(Input[2, ], Output[2, ], rep(1,K), 4,7,4,7,5,3,5,3);
  Righthandside_i3 <- c(Input[3, ], Output[3, ], rep(1,K), 8,4,8,4,2,2,2,2);

  Direction_ <- c(rep("=", sum_m),rep("=", sum_r), rep("=", K), rep("=", (2*sum_l)));

  Righthside_Direction_1 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i1);
  Righthside_Direction_2 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i2);
  Righthside_Direction_3 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i3);


  #objective function and Constraint1:
  Const_1_i1 <- NaN;
  Const_1_i2 <- NaN;
  Const_1_i3 <- NaN;

  obj_func_i1 <- matrix(c(rep(0,(K*N)),rep(0,sum_r),-(0.5/14),-(0.5/4),-(0.5/10),-(0.5/8),-(0.5/14),-(0.5/10)), byrow = TRUE, nrow = 1);
  obj_func_i2 <- matrix(c(rep(0,(K*N)),rep(0,sum_r),-(0.5/8),-(0.5/6),-(0.5/4),-(0.5/6),-(0.5/6),-(0.5/2)), byrow = TRUE, nrow = 1);
  obj_func_i3 <- matrix(c(rep(0,(K*N)),rep(0,sum_r),-(0.5/2),-(0.5/16),-(0.5/10),-(0.5/14),-(0.5/12),-(0.5/8)), byrow = TRUE, nrow = 1);

  Obj_func_and_Const_i1 <- list( "FOBJ" = obj_func_i1, "FCON" = Const_1_i1);
  Obj_func_and_Const_i2 <- list( "FOBJ" = obj_func_i2, "FCON" = Const_1_i2);
  Obj_func_and_Const_i3 <- list( "FOBJ" = obj_func_i3, "FCON" = Const_1_i3);


  #Constraint 2:
  Const_2_i1 <- matrix(c(7,4,1,rep(0,((N-1)*K)),rep(0,sum_r),1,0,0,0,0,0,
                         2,3,8,rep(0,((N-1)*K)),rep(0,sum_r),0,1,0,0,0,0,
                         rep(0,N),5,2,5,rep(0,N),rep(0,sum_r),0,0,1,0,0,0,
                         rep(0,N),4,3,7,rep(0,N),rep(0,sum_r),0,0,0,1,0,0,
                         rep(0,((N-1)*K)),7,3,6,rep(0,sum_r),0,0,0,0,1,0,
                         rep(0,((N-1)*K)),5,1,4,rep(0,sum_r),0,0,0,0,0,1), byrow = TRUE, nrow = 6);

  Const_2_i2 <- matrix(c(7,4,1,rep(0,((N-1)*K)),rep(0,sum_r),1,0,0,0,0,0,
                         2,3,8,rep(0,((N-1)*K)),rep(0,sum_r),0,1,0,0,0,0,
                         rep(0,N),5,2,5,rep(0,N),rep(0,sum_r),0,0,1,0,0,0,
                         rep(0,N),4,3,7,rep(0,N),rep(0,sum_r),0,0,0,1,0,0,
                         rep(0,((N-1)*K)),7,3,6,rep(0,sum_r),0,0,0,0,1,0,
                         rep(0,((N-1)*K)),5,1,4,rep(0,sum_r),0,0,0,0,0,1), byrow = TRUE, nrow = 6);

  Const_2_i3 <- matrix(c(7,4,1,rep(0,((N-1)*K)),rep(0,sum_r),1,0,0,0,0,0,
                         2,3,8,rep(0,((N-1)*K)),rep(0,sum_r),0,1,0,0,0,0,
                         rep(0,N),5,2,5,rep(0,N),rep(0,sum_r),0,0,1,0,0,0,
                         rep(0,N),4,3,7,rep(0,N),rep(0,sum_r),0,0,0,1,0,0,
                         rep(0,((N-1)*K)),7,3,6,rep(0,sum_r),0,0,0,0,1,0,
                         rep(0,((N-1)*K)),5,1,4,rep(0,sum_r),0,0,0,0,0,1), byrow = TRUE, nrow = 6);





  #Constraint 3:
  Const_3_i1 <- matrix(c(3,8,7,rep(0,((N-1)*K)),-1,0,0,0,0,0,rep(0,sum_m),
                         1,1,4,rep(0,((N-1)*K)),0,-1,0,0,0,0,rep(0,sum_m),
                         rep(0,N),6,1,5,rep(0,N),0,0,-1,0,0,0,rep(0,sum_m),
                         rep(0,N),2,1,4,rep(0,N),0,0,0,-1,0,0,rep(0,sum_m),
                         rep(0,((N-1)*K)),8,2,7,0,0,0,0,-1,0,rep(0,sum_m),
                         rep(0,((N-1)*K)),2,2,1,0,0,0,0,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 6);

  Const_3_i2 <- matrix(c(3,8,7,rep(0,((N-1)*K)),-1,0,0,0,0,0,rep(0,sum_m),
                         1,1,4,rep(0,((N-1)*K)),0,-1,0,0,0,0,rep(0,sum_m),
                         rep(0,N),6,1,5,rep(0,N),0,0,-1,0,0,0,rep(0,sum_m),
                         rep(0,N),2,1,4,rep(0,N),0,0,0,-1,0,0,rep(0,sum_m),
                         rep(0,((N-1)*K)),8,2,7,0,0,0,0,-1,0,rep(0,sum_m),
                         rep(0,((N-1)*K)),2,2,1,0,0,0,0,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 6);

  Const_3_i3 <- matrix(c(3,8,7,rep(0,((N-1)*K)),-1,0,0,0,0,0,rep(0,sum_m),
                         1,1,4,rep(0,((N-1)*K)),0,-1,0,0,0,0,rep(0,sum_m),
                         rep(0,N),6,1,5,rep(0,N),0,0,-1,0,0,0,rep(0,sum_m),
                         rep(0,N),2,1,4,rep(0,N),0,0,0,-1,0,0,rep(0,sum_m),
                         rep(0,((N-1)*K)),8,2,7,0,0,0,0,-1,0,rep(0,sum_m),
                         rep(0,((N-1)*K)),2,2,1,0,0,0,0,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 6);


  #VRS Constraint:
  vrs_constraint <- matrix(c(rep(1,N),rep(0,((N-1)*K)),rep(0,sum_r), rep(0,sum_m),
                           rep(0,N),rep(1,N),rep(0,N),rep(0,sum_r), rep(0,sum_m),
                           rep(0,((N-1)*K)),rep(1,N),rep(0,sum_r), rep(0,sum_m)), byrow = TRUE, nrow = 3);

  #Constraint 5:
  Const_5_i1 <- matrix(c(10,4,8,rep(0,((N-1)*K)),rep(0,sum_r),rep(0,sum_m),
                         5,7,4,rep(0,((N-1)*K)),rep(0,sum_r),rep(0,sum_m),
                         rep(0,N),10,4,8,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         rep(0,N),5,7,4,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         rep(0,N),1,5,2,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         rep(0,N),4,3,2,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         rep(0,((N-1)*K)),1,5,2,rep(0,sum_r),rep(0,sum_m),
                         rep(0,((N-1)*K)),4,3,2,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = 8);

  Const_5_i2 <- matrix(c(10,4,8,rep(0,((N-1)*K)),rep(0,sum_r),rep(0,sum_m),
                         5,7,4,rep(0,((N-1)*K)),rep(0,sum_r),rep(0,sum_m),
                         rep(0,N),10,4,8,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         rep(0,N),5,7,4,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         rep(0,N),1,5,2,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         rep(0,N),4,3,2,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         rep(0,((N-1)*K)),1,5,2,rep(0,sum_r),rep(0,sum_m),
                         rep(0,((N-1)*K)),4,3,2,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = 8);

  Const_5_i3 <- matrix(c(10,4,8,rep(0,((N-1)*K)),rep(0,sum_r),rep(0,sum_m),
                         5,7,4,rep(0,((N-1)*K)),rep(0,sum_r),rep(0,sum_m),
                         rep(0,N),10,4,8,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         rep(0,N),5,7,4,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         rep(0,N),1,5,2,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         rep(0,N),4,3,2,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         rep(0,((N-1)*K)),1,5,2,rep(0,sum_r),rep(0,sum_m),
                         rep(0,((N-1)*K)),4,3,2,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = 8);


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
#i = 1
expect_equal(Righthandside_and_Direction(1, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_1, check.attributes = FALSE)

#i = 2
expect_equal(Righthandside_and_Direction(2, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_2, check.attributes = FALSE)

#i=3
expect_equal(Righthandside_and_Direction(3, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_3, check.attributes = FALSE)


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

#VRS constraint:
expect_equal(vrs.constraint(direction, K, N, sum_m, sum_r, sum_l, Link_obj), vrs_constraint, check.attributes = FALSE)


########################################

#Constraint 5 and i=1:
expect_equal(Constraint5(1, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i1, check.attributes = FALSE)

#Constraint 5 and i=2:
expect_equal(Constraint5(2, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i2, check.attributes = FALSE)

#Constraint 5 and i=3:
expect_equal(Constraint5(3, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i3, check.attributes = FALSE)


})

