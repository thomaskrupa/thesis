#Test case 74

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




direction = "non" ;
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




# Tone example


test_that("Test case 74",{

  #Amount split function
  Amount_split_ <- list("Amount_IN" = Amount_Input, "Amount_OUT" = Amount_Output, "Amount_LINK" = Amount_Link);

  #Data split:
  data_split_ <- list( "In" = Input, "Out" = Output, "Link" = Link);


  #Righthand side and Direction:
  Righthandside <- c(1,rep(0,sum_m),rep(0,sum_r),rep(0,K),rep(0,(sum_l*2)),rep(0,1));
  Direction_ <- c(rep("=",1),rep("=",sum_m),rep("=",sum_r),rep("=",K),rep("=",(sum_l*2)), ">");
  Righthside_Direction_ <- list( "FDIR" = Direction_, "FRHS" = Righthandside);


  #objective function and Constraint1:
  Const_1_i1 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w2/0.879),(w3/0.337),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i2 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w2/0.538), (w3/0.180),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i3 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w2/0.911), (w3/0.198),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i4 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w2/0.570), (w3/0.491),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i5 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w2/1.086), (w3/0.372),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i6 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w2/0.722), (w3/0.253),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i7 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w2/0.509), (w3/0.241),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i8 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w2/0.619), (w3/0.097),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i9 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w2/1.023), (w3/0.380),rep(0,sum_m)), byrow = TRUE, nrow = 1);
  Const_1_i10 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),(w2/0.769), (w3/0.178),rep(0,sum_m)), byrow = TRUE, nrow = 1);

  obj_func_i1 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/0.838),-(w2/0.277),-(w3/0.962)), byrow = TRUE, nrow = 1);
  obj_func_i2 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/1.233),-(w2/0.132),-(w3/0.443)), byrow = TRUE, nrow = 1);
  obj_func_i3 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/0.321),-(w2/0.045),-(w3/0.482)), byrow = TRUE, nrow = 1);
  obj_func_i4 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/1.483),-(w2/0.111),-(w3/0.467)), byrow = TRUE, nrow = 1);
  obj_func_i5 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/1.592),-(w2/0.208),-(w3/1.073)), byrow = TRUE, nrow = 1);
  obj_func_i6 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/0.790),-(w2/0.139),-(w3/0.545)), byrow = TRUE, nrow = 1);
  obj_func_i7 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/0.451),-(w2/0.075),-(w3/0.366)), byrow = TRUE, nrow = 1);
  obj_func_i8 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/0.408),-(w2/0.074),-(w3/0.229)), byrow = TRUE, nrow = 1);
  obj_func_i9 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/1.864),-(w2/0.061),-(w3/0.691)), byrow = TRUE, nrow = 1);
  obj_func_i10 <- matrix(c(1,rep(0,N),rep(0,N),rep(0,N),rep(0,sum_r),-(w1/1.222),-(w2/0.149),-(w3/0.337)), byrow = TRUE, nrow = 1);

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
  Const_2_i1 <- matrix(c(-0.838,0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -0.277,rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         -0.962,rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i2 <- matrix(c(-1.233,0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -0.132,rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         -0.443,rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i3 <- matrix(c(-0.321,0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -0.045,rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         -0.482,rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i4 <- matrix(c(-1.483,0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -0.111,rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         -0.467,rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i5 <- matrix(c(-1.592,0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -0.208,rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         -1.073,rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i6 <- matrix(c(-0.790,0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -0.139,rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         -0.545,rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i7 <- matrix(c(-0.451,0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -0.075,rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         -0.366,rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i8 <- matrix(c(-0.408,0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -0.074,rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         -0.229,rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i9 <- matrix(c(-1.864,0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -0.061,rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         -0.691,rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);

  Const_2_i10 <- matrix(c(-1.222,0.838,1.233,0.321,1.483,1.592,0.790,0.451,0.408,1.864,1.222,rep(0,N),rep(0,N),rep(0,sum_r),1,0,0,
                         -0.149,rep(0,N),0.277,0.132,0.045,0.111,0.208,0.139,0.075,0.074,0.061,0.149,rep(0,N),rep(0,sum_r),0,1,0,
                         -0.337,rep(0,N),rep(0,N),0.962,0.443,0.482,0.467,1.073,0.545,0.366,0.229,0.691,0.337,rep(0,sum_r),0,0,1), byrow = TRUE, nrow = sum_m);




  #Constraint 3:
  Const_3_i1 <- matrix(c(-0.879,rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         -0.337,rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i2 <- matrix(c(-0.538,rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         -0.180,rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i3 <- matrix(c(-0.911,rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         -0.198,rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i4 <- matrix(c(-0.570,rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         -0.491,rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i5 <- matrix(c(-1.086,rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         -0.372,rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i6 <- matrix(c(-0.722,rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         -0.253,rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i7 <- matrix(c(-0.509,rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         -0.241,rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i8 <- matrix(c(-0.619,rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         -0.097,rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i9 <- matrix(c(-1.023,rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         -0.380,rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);

  Const_3_i10 <- matrix(c(-0.769,rep(0,N),0.879,0.538,0.911,0.570,1.086,0.722,0.509,0.619,1.023,0.769,rep(0,N),-1,0,rep(0,sum_m),
                         -0.178,rep(0,N),rep(0,N),0.337,0.180,0.198,0.491,0.372,0.253,0.241,0.097,0.380,0.178,0,-1,rep(0,sum_m)), byrow = TRUE, nrow = sum_r);



  #COnstraint 4:
  Const_4 <- matrix(c(-1,rep(1,N),rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                      -1,rep(0,N),rep(1,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                      -1,rep(0,N),rep(0,N),rep(1,N),rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = K);

  #Constraint 5:
  Const_5_i1 <- matrix(c(-0.894,0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.894,rep(0,N),0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.362,rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.362,rep(0,N),rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = (2*sum_l));

  Const_5_i2 <- matrix(c(-0.678,0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.678,rep(0,N),0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.188,rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.188,rep(0,N),rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = (2*sum_l));

  Const_5_i3 <- matrix(c(-0.836,0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.836,rep(0,N),0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.207,rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.207,rep(0,N),rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = (2*sum_l));

  Const_5_i4 <- matrix(c(-0.869,0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.869,rep(0,N),0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.516,rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.516,rep(0,N),rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = (2*sum_l));

  Const_5_i5 <- matrix(c(-0.693,0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.693,rep(0,N),0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.407,rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.407,rep(0,N),rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = (2*sum_l));

  Const_5_i6 <- matrix(c(-0.966,0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.966,rep(0,N),0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.269,rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.269,rep(0,N),rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = (2*sum_l));

  Const_5_i7 <- matrix(c(-0.647,0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.647,rep(0,N),0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.257,rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.257,rep(0,N),rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = (2*sum_l));

  Const_5_i8 <- matrix(c(-0.756,0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.756,rep(0,N),0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.103,rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.103,rep(0,N),rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = (2*sum_l));

  Const_5_i9 <- matrix(c(-1.191,0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -1.191,rep(0,N),0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.402,rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.402,rep(0,N),rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = (2*sum_l));

  Const_5_i10 <- matrix(c(-0.792,0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.792,rep(0,N),0.894,0.678,0.836,0.869,0.693,0.966,0.647,0.756,1.191,0.792,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.187,rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,N),rep(0,sum_r),rep(0,sum_m),
                         -0.187,rep(0,N),rep(0,N),0.362,0.188,0.207,0.516,0.407,0.269,0.257,0.103,0.402,0.187,rep(0,sum_r),rep(0,sum_m)), byrow = TRUE, nrow = (2*sum_l));

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

#VRS constraint
expect_equal(vrs.constraint(direction, K, N, sum_m, sum_r, sum_l, Link_obj), Const_4, check.attributes = FALSE)


########################################

#Constraint 5 and i=1:
expect_equal(Constraint5(1, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i1, check.attributes = FALSE)

#Constraint 5 and i=2:
expect_equal(Constraint5(2, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i2, check.attributes = FALSE)

#Constraint 5 and i=3:
expect_equal(Constraint5(3, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i3, check.attributes = FALSE)

#Constraint 5 and i=4:
expect_equal(Constraint5(4, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i4, check.attributes = FALSE)

#Constraint 5 and i=5:
expect_equal(Constraint5(5, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i5, check.attributes = FALSE)

#Constraint 5 and i=6:
expect_equal(Constraint5(6, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i6, check.attributes = FALSE)

#Constraint 5 and i=7:
expect_equal(Constraint5(7, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i7, check.attributes = FALSE)

#Constraint 5 and i=8:
expect_equal(Constraint5(8, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i8, check.attributes = FALSE)

#Constraint 5 and i=9:
expect_equal(Constraint5(9, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i9, check.attributes = FALSE)

#Constraint 5 and i=10:
expect_equal(Constraint5(10, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj), Const_5_i10, check.attributes = FALSE)


########################################

#t Constraint
expect_equal(t_constraint(K, N, sum_m, sum_r, sum_l, Link_obj), Const_t, check.attributes = FALSE)

})
