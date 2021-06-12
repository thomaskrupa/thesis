#Test case 79:

Data_1 <- matrix(c(0.838, 0.277, 0.879, 0.962, 0.337, 0.894, 0.362,
                 1.233, 0.132, 0.538, 0.443, 0.180, 0.678, 0.188,
                 0.321, 0.045, 0.911, 0.482, 0.198, 0.836, 0.207,
                 1.483, 0.111, 0.570, 0.467, 0.491, 0.869, 0.516,
                 1.592, 0.208, 1.086, 1.073, 0.372, 0.693, 0.407,
                 0.790, 0.139, 0.722, 0.545, 0.253, 0.966, 0.269,
                 0.451, 0.075, 0.509, 0.366, 0.241, 0.647, 0.257,
                 0.408, 0.074, 0.619, 0.229, 0.097, 0.756, 0.103,
                 1.864, 0.061, 1.023, 0.691, 0.380, 1.191, 0.402,
                 1.222, 0.149, 0.769, 0.337, 0.178, 0.792, 0.187),byrow=TRUE, nrow=10);



Input = matrix(c(0.838,0.277,0.962,
                 1.233,0.132,0.443,
                 0.321,0.045,0.482,
                 1.483,0.111,0.467,
                 1.592,0.208,1.073,
                 0.790,0.139,0.545,
                 0.451,0.075,0.366,
                 0.408,0.074,0.229,
                 1.864,0.061,0.691,
                 1.222,0.149,0.337), byrow = TRUE, nrow = 10);

Output = matrix(c(0.879,0.337,
                  0.538,0.180,
                  0.911,0.198,
                  0.570,0.491,
                  1.086,0.372,
                  0.722,0.253,
                  0.509,0.241,
                  0.619,0.097,
                  1.023,0.380,
                  0.769,0.178), byrow = TRUE, nrow = 10);

Link = matrix(c(0.894,0.362,
                0.678,0.188,
                0.836,0.207,
                0.869,0.516,
                0.693,0.407,
                0.966,0.269,
                0.647,0.257,
                0.756,0.103,
                1.191,0.402,
                0.792,0.187), byrow = TRUE, nrow = 10);


weights = c(0.4, 0.2, 0.4);



w1 <- weights[1];
w2 <- weights[2];
w3 <- weights[3];

K = 3;
N = 10;
sum_m = 3;
sum_r = 2;
sum_l = 2;

Amount <- c(1,0,1,1,1,1,1,1);
Amount_Input = c(1,1,1);
Amount_Output = c(0,1,1);
Amount_Link = c(1,1);




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


# Tone example


test_that("Test case 79",{

  #Amount split function
  Amount_split_ <- list("Amount_IN" = Amount_Input, "Amount_OUT" = Amount_Output, "Amount_LINK" = Amount_Link);

  #Data split:
  data_split_ <- list( "In" = Input, "Out" = Output, "Link" = Link);


  #Righthand side and Direction:
  Righthandside_i1 <- c(Input[1,], Output[1,], rep(1,K), rep(0,sum_l));
  Righthandside_i2 <- c(Input[2,], Output[2,], rep(1,K), rep(0,sum_l));
  Righthandside_i3 <- c(Input[3,], Output[3,], rep(1,K), rep(0,sum_l));
  Righthandside_i4 <- c(Input[4,], Output[4,], rep(1,K), rep(0,sum_l));
  Righthandside_i5 <- c(Input[5,], Output[5,], rep(1,K), rep(0,sum_l));
  Righthandside_i6 <- c(Input[6,], Output[6,], rep(1,K), rep(0,sum_l));
  Righthandside_i7 <- c(Input[7,], Output[7,], rep(1,K), rep(0,sum_l));
  Righthandside_i8 <- c(Input[8,], Output[8,], rep(1,K), rep(0,sum_l));
  Righthandside_i9 <- c(Input[9,], Output[9,], rep(1,K), rep(0,sum_l));
  Righthandside_i10 <- c(Input[10,], Output[10,], rep(1,K), rep(0,sum_l));

  Direction_ <- c(rep("=",sum_m), rep("=",sum_r),rep("=",K),rep("=",(sum_l)));

  Righthside_Direction_1 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i1);
  Righthside_Direction_2 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i2);
  Righthside_Direction_3 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i3);
  Righthside_Direction_4 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i4);
  Righthside_Direction_5 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i5);
  Righthside_Direction_6 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i6);
  Righthside_Direction_7 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i7);
  Righthside_Direction_8 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i8);
  Righthside_Direction_9 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i9);
  Righthside_Direction_10 <- list( "FDIR" = Direction_, "FRHS" = Righthandside_i10);


  #objective function and Constraint1:
  Const_1_i1 <- NaN;
  Const_1_i2 <- NaN;
  Const_1_i3 <- NaN;
  Const_1_i4 <- NaN;
  Const_1_i5 <- NaN;
  Const_1_i6 <- NaN;
  Const_1_i7 <- NaN;
  Const_1_i8 <- NaN;
  Const_1_i9 <- NaN;
  Const_1_i10 <- NaN;


  obj_func_i1 <- matrix(c(rep(0,N),rep(0,N),rep(0,N),(w2/0.879),(w3/0.337),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  obj_func_i2 <- matrix(c(rep(0,N),rep(0,N),rep(0,N),(w2/0.538), (w3/0.180),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  obj_func_i3 <- matrix(c(rep(0,N),rep(0,N),rep(0,N),(w2/0.911), (w3/0.198),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  obj_func_i4 <- matrix(c(rep(0,N),rep(0,N),rep(0,N),(w2/0.570), (w3/0.491),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  obj_func_i5 <- matrix(c(rep(0,N),rep(0,N),rep(0,N),(w2/1.086), (w3/0.372),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  obj_func_i6 <- matrix(c(rep(0,N),rep(0,N),rep(0,N),(w2/0.722), (w3/0.253),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  obj_func_i7 <- matrix(c(rep(0,N),rep(0,N),rep(0,N),(w2/0.509), (w3/0.241),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  obj_func_i8 <- matrix(c(rep(0,N),rep(0,N),rep(0,N),(w2/0.619), (w3/0.097),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  obj_func_i9 <- matrix(c(rep(0,N),rep(0,N),rep(0,N),(w2/1.023), (w3/0.380),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  obj_func_i10 <- matrix(c(rep(0,N),rep(0,N),rep(0,N),(w2/0.769), (w3/0.178),rep(0,sum_m)), byrow = TRUE, nrow = 1);


  Obj_func_and_Const_i1 <- list( "FOBJ" = obj_func_i1, "FCON" = Const_1_i1);
  Obj_func_and_Const_i2 <- list( "FOBJ" = obj_func_i2, "FCON" = Const_1_i2);
  Obj_func_and_Const_i3 <- list( "FOBJ" = obj_func_i3, "FCON" = Const_1_i3);
  Obj_func_and_Const_i4 <- list( "FOBJ" = obj_func_i4, "FCON" = Const_1_i4);
  Obj_func_and_Const_i5 <- list( "FOBJ" = obj_func_i5, "FCON" = Const_1_i5);
  Obj_func_and_Const_i6 <- list( "FOBJ" = obj_func_i6, "FCON" = Const_1_i6);
  Obj_func_and_Const_i7 <- list( "FOBJ" = obj_func_i7, "FCON" = Const_1_i7);
  Obj_func_and_Const_i8 <- list( "FOBJ" = obj_func_i8, "FCON" = Const_1_i8);
  Obj_func_and_Const_i9 <- list( "FOBJ" = obj_func_i9, "FCON" = Const_1_i9);
  Obj_func_and_Const_i10 <- list( "FOBJ" = obj_func_i10, "FCON" = Const_1_i10);


  #Constraint 2:
  Const_2_i1 <- matrix(c(0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i2 <- matrix(c(0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i3 <- matrix(c(0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i4 <- matrix(c(0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i5 <- matrix(c(0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i6 <- matrix(c(0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i7 <- matrix(c(0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i8 <- matrix(c(0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i9 <- matrix(c(0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i10 <- matrix(c(0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);




  #Constraint 3:
  Const_3_i1 <- matrix(c(rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i2 <- matrix(c(rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i3 <- matrix(c(rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i4 <- matrix(c(rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i5 <- matrix(c(rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i6 <- matrix(c(rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i7 <- matrix(c(rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i8 <- matrix(c(rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i9 <- matrix(c(rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i10 <- matrix(c(rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);



  #VRS Constraint:
  vrs_constraint <- matrix(c(rep(1,N),rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                             rep(0,N),rep(1,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                             rep(0,N),rep(0,N),rep(1,N),rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = K);

  #Constraint 5:
  Const_5 <- matrix(c(-0.894,-0.678,-0.836,-0.869,-0.693,-0.966,-0.647,-0.756,-1.191,-0.792,0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                      rep(0,N),-0.362,-0.188,-0.207,-0.516,-0.407,-0.269,-0.257,-0.103,-0.402,-0.187,0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = sum_l);



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

#i=5
expect_equal(Righthandside_and_Direction(5, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_5, check.attributes = FALSE)

#i=6
expect_equal(Righthandside_and_Direction(6, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_6, check.attributes = FALSE)

#i=7
expect_equal(Righthandside_and_Direction(7, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_7, check.attributes = FALSE)

#i=8
expect_equal(Righthandside_and_Direction(8, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_8, check.attributes = FALSE)

#i=9
expect_equal(Righthandside_and_Direction(9, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_9, check.attributes = FALSE)

#i=10
expect_equal(Righthandside_and_Direction(10, direction, return_to_scale, link_con, NIRS, Input, Output, Link, Amount_Link, K, sum_m, sum_r, sum_l), Righthside_Direction_10, check.attributes = FALSE)


#########################################

#Object.Function and Constraint 1 and direction="non" and i=1:
expect_equal(Obj.func_and_con(1, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i1, check.attributes = FALSE);

#Object.Function and Constraint 2 and direction="non" and i=2:
expect_equal(Obj.func_and_con(2, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i2, check.attributes = FALSE) ;

#Object.Function and Constraint 2 and direction="non" and i=3:
expect_equal(Obj.func_and_con(3, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i3, check.attributes = FALSE) ;

#Object.Function and Constraint 2 and direction="non" and i=4:
expect_equal(Obj.func_and_con(4, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i4, check.attributes = FALSE) ;

#Object.Function and Constraint 2 and direction="non" and i=5:
expect_equal(Obj.func_and_con(5, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i5, check.attributes = FALSE) ;

#Object.Function and Constraint 2 and direction="non" and i=6:
expect_equal(Obj.func_and_con(6, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i6, check.attributes = FALSE) ;

#Object.Function and Constraint 2 and direction="non" and i=7:
expect_equal(Obj.func_and_con(7, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i7, check.attributes = FALSE) ;

#Object.Function and Constraint 2 and direction="non" and i=8:
expect_equal(Obj.func_and_con(8, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i8, check.attributes = FALSE) ;

#Object.Function and Constraint 2 and direction="non" and i=9:
expect_equal(Obj.func_and_con(9, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i9, check.attributes = FALSE) ;

#Object.Function and Constraint 2 and direction="non" and i=10:
expect_equal(Obj.func_and_con(10, direction, Input, Output, Link, Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj), Obj_func_and_Const_i10, check.attributes = FALSE) ;


#########################################

#Constraint 2 and i=1:
expect_equal(Constraint2(1, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i1, check.attributes = FALSE)

#Constraint 2 and i=2:
expect_equal(Constraint2(2, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i2, check.attributes = FALSE)

#Constraint 2 and i=3:
expect_equal(Constraint2(3, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i3, check.attributes = FALSE)

#Constraint 2 and i=4:
expect_equal(Constraint2(4, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i4, check.attributes = FALSE)

#Constraint 2 and i=5:
expect_equal(Constraint2(5, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i5, check.attributes = FALSE)

#Constraint 2 and i=6:
expect_equal(Constraint2(6, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i6, check.attributes = FALSE)

#Constraint 2 and i=7:
expect_equal(Constraint2(7, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i7, check.attributes = FALSE)

#Constraint 2 and i=8:
expect_equal(Constraint2(8, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i8, check.attributes = FALSE)

#Constraint 2 and i=9:
expect_equal(Constraint2(9, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i9, check.attributes = FALSE)

#Constraint 2 and i=10:
expect_equal(Constraint2(10, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj), Const_2_i10, check.attributes = FALSE)

########################################

#Constraint 3 and i=1:
expect_equal(Constraint3(1, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i1, check.attributes = FALSE)

#Constraint 3 and i=2:
expect_equal(Constraint3(2, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i2, check.attributes = FALSE)

#Constraint 3 and i=3:
expect_equal(Constraint3(3, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i3, check.attributes = FALSE)

#Constraint 3 and i=4:
expect_equal(Constraint3(4, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i4, check.attributes = FALSE)

#Constraint 3 and i=5:
expect_equal(Constraint3(5, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i5, check.attributes = FALSE)

#Constraint 3 and i=6:
expect_equal(Constraint3(6, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i6, check.attributes = FALSE)

#Constraint 3 and i=7:
expect_equal(Constraint3(7, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i7, check.attributes = FALSE)

#Constraint 3 and i=8:
expect_equal(Constraint3(8, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i8, check.attributes = FALSE)

#Constraint 3 and i=9:
expect_equal(Constraint3(9, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i9, check.attributes = FALSE)

#Constraint 3 and i=10:
expect_equal(Constraint3(10, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj), Const_3_i10, check.attributes = FALSE)


########################################

#VRS constraint:
expect_equal(vrs.constraint(direction, K, N, sum_m, sum_r, sum_l, Link_obj), vrs_constraint, check.attributes = FALSE)


########################################

#Constraint 5:
expect_equal(Constraint5(1, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5, check.attributes = FALSE)

})

