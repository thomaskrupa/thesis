#Test case 22:

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


direction = "non" ;
link_con = 2;
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



test_that("Test case 22",{


  #Amount split function
  Amount_split_ <- list("Amount_IN" = Amount_Input, "Amount_OUT" = Amount_Output, "Amount_LINK" = Amount_Link);

  #Data split:
  data_split_ <- list( "In" = Input, "Out" = Output, "Link" = Link);


  #Righthand side and Direction:
  Righthandside <- c(1,rep(0,sum_m),rep(0,sum_r),rep(0,K),rep(0,(sum_l)),rep(0,1));
  Direction_ <- c(rep("=",1),rep("=",sum_m),rep("=",sum_r),rep("=",K),rep("=",(sum_l)), ">");
  Righthside_Direction_ <- list( "FDIR" = Direction_, "FRHS" = Righthandside);


  #objective function and Constraint1:
  Const_1_i1 <- matrix(c(1,rep(0,(K*N)),(0.5/6),(0.5/2),(0.5/12),(0.5/4),(0.5/16),(0.5/4),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i2 <- matrix(c(1,rep(0,(K*N)),(0.5/16),(0.5/2),(0.5/2),(0.5/2),(0.5/4),(0.5/4),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i3 <- matrix(c(1,rep(0,(K*N)),(0.5/14),(0.5/8),(0.5/10),(0.5/8),(0.5/14),(0.5/2),rep(0,sum_m)), byrow = TRUE, nrow = 1);

  obj_func_i1 <- matrix(c(1,rep(0,(K*N)),rep(0,sum_r),-(0.5/14),-(0.5/4),-(0.5/10),-(0.5/8),-(0.5/14),-(0.5/10)), byrow = TRUE, nrow = 1);
  obj_func_i2 <- matrix(c(1,rep(0,(K*N)),rep(0,sum_r),-(0.5/8),-(0.5/6),-(0.5/4),-(0.5/6),-(0.5/6),-(0.5/2)), byrow = TRUE, nrow = 1);
  obj_func_i3 <- matrix(c(1,rep(0,(K*N)),rep(0,sum_r),-(0.5/2),-(0.5/16),-(0.5/10),-(0.5/14),-(0.5/12),-(0.5/8)), byrow = TRUE, nrow = 1);

  Obj_func_and_Const_i1 <- list( "FOBJ" = obj_func_i1, "FCON" = Const_1_i1);
  Obj_func_and_Const_i2 <- list( "FOBJ" = obj_func_i2, "FCON" = Const_1_i2);
  Obj_func_and_Const_i3 <- list( "FOBJ" = obj_func_i3, "FCON" = Const_1_i3);


  #Constraint 2:
  Const_2_i1 <- matrix(c(-7,7,4,1,rep(0,((N-1)*K)),rep(0,sum_r),1,0,0,0,0,0,
                         -2,2,3,8,rep(0,((N-1)*K)),rep(0,sum_r),0,1,0,0,0,0,
                         -5,rep(0,N),5,2,5,rep(0,N),rep(0,sum_r),0,0,1,0,0,0,
                         -4,rep(0,N),4,3,7,rep(0,N),rep(0,sum_r),0,0,0,1,0,0,
                         -7,rep(0,((N-1)*K)),7,3,6,rep(0,sum_r),0,0,0,0,1,0,
                         -5,rep(0,((N-1)*K)),5,1,4,rep(0,sum_r),0,0,0,0,0,1), byrow = TRUE, nrow = 6);

  Const_2_i2 <- matrix(c(-4,7,4,1,rep(0,((N-1)*K)),rep(0,sum_r),1,0,0,0,0,0,
                         -3,2,3,8,rep(0,((N-1)*K)),rep(0,sum_r),0,1,0,0,0,0,
                         -2,rep(0,N),5,2,5,rep(0,N),rep(0,sum_r),0,0,1,0,0,0,
                         -3,rep(0,N),4,3,7,rep(0,N),rep(0,sum_r),0,0,0,1,0,0,
                         -3,rep(0,((N-1)*K)),7,3,6,rep(0,sum_r),0,0,0,0,1,0,
                         -1,rep(0,((N-1)*K)),5,1,4,rep(0,sum_r),0,0,0,0,0,1), byrow = TRUE, nrow = 6);

  Const_2_i3 <- matrix(c(-1,7,4,1,rep(0,((N-1)*K)),rep(0,sum_r),1,0,0,0,0,0,
                         -8,2,3,8,rep(0,((N-1)*K)),rep(0,sum_r),0,1,0,0,0,0,
                         -5,rep(0,N),5,2,5,rep(0,N),rep(0,sum_r),0,0,1,0,0,0,
                         -7,rep(0,N),4,3,7,rep(0,N),rep(0,sum_r),0,0,0,1,0,0,
                         -6,rep(0,((N-1)*K)),7,3,6,rep(0,sum_r),0,0,0,0,1,0,
                         -4,rep(0,((N-1)*K)),5,1,4,rep(0,sum_r),0,0,0,0,0,1), byrow = TRUE, nrow = 6);





  #Constraint 3:
  Const_3_i1 <- matrix(c(-3,3,8,7,rep(0,((N-1)*K)),-1,0,0,0,0,0,rep(0,sum_m),
                         -1,1,1,4,rep(0,((N-1)*K)),0,-1,0,0,0,0,rep(0,sum_m),
                         -6,rep(0,N),6,1,5,rep(0,N),0,0,-1,0,0,0,rep(0,sum_m),
                         -2,rep(0,N),2,1,4,rep(0,N),0,0,0,-1,0,0,rep(0,sum_m),
                         -8,rep(0,((N-1)*K)),8,2,7,0,0,0,0,-1,0,rep(0,sum_m),
                         -2,rep(0,((N-1)*K)),2,2,1,0,0,0,0,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 6);

  Const_3_i2 <- matrix(c(-8,3,8,7,rep(0,((N-1)*K)),-1,0,0,0,0,0,rep(0,sum_m),
                         -1,1,1,4,rep(0,((N-1)*K)),0,-1,0,0,0,0,rep(0,sum_m),
                         -1,rep(0,N),6,1,5,rep(0,N),0,0,-1,0,0,0,rep(0,sum_m),
                         -1,rep(0,N),2,1,4,rep(0,N),0,0,0,-1,0,0,rep(0,sum_m),
                         -2,rep(0,((N-1)*K)),8,2,7,0,0,0,0,-1,0,rep(0,sum_m),
                         -2,rep(0,((N-1)*K)),2,2,1,0,0,0,0,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 6);

  Const_3_i3 <- matrix(c(-7,3,8,7,rep(0,((N-1)*K)),-1,0,0,0,0,0,rep(0,sum_m),
                         -4,1,1,4,rep(0,((N-1)*K)),0,-1,0,0,0,0,rep(0,sum_m),
                         -5,rep(0,N),6,1,5,rep(0,N),0,0,-1,0,0,0,rep(0,sum_m),
                         -4,rep(0,N),2,1,4,rep(0,N),0,0,0,-1,0,0,rep(0,sum_m),
                         -7,rep(0,((N-1)*K)),8,2,7,0,0,0,0,-1,0,rep(0,sum_m),
                         -1,rep(0,((N-1)*K)),2,2,1,0,0,0,0,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = 6);


  #COnstraint 4:
  Const_4 <- matrix(c(-1,rep(1,N),rep(0,((N-1)*K)),rep(0,sum_r), rep(0,sum_m),
                      -1,rep(0,N),rep(1,N),rep(0,N),rep(0,sum_r), rep(0,sum_m),
                      -1,rep(0,((N-1)*K)),rep(1,N),rep(0,sum_r), rep(0,sum_m)), byrow = TRUE, nrow = 3);

  #Constraint 5:
  Const_5 <- matrix(c(0,-10,-4,-8,10,4,8,rep(0,K),rep(0,sum_r),rep(0,sum_m),
                      0,-5,-7,-4,5,7,4,rep(0,K),rep(0,sum_r),rep(0,sum_m),
                      0,rep(0,N),-1,-5,-2,1,5,2,rep(0,sum_r),rep(0,sum_m),
                      0,rep(0,N),-4,-3,-2,4,3,2,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = 4);



  #t Constraint:
  Const_t <- matrix(c(1,rep(0,(N*K)),rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = 1);


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
expect_equal(vrs.constraint(direction, K, N, sum_m, sum_r, sum_l, Link_obj), Const_4, check.attributes = FALSE)


########################################

#Constraint 5 and i=1:
expect_equal(Constraint5(1, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5, check.attributes = FALSE)


########################################

#t Constraint
expect_equal(t_constraint(K, N, sum_m, sum_r, sum_l, Link_obj), Const_t, check.attributes = FALSE)



})



















