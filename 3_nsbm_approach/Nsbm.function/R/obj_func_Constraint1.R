#' Obj.func_Constraint1
#'
#' @description The Obj.func_Constraint1 function creates the objective function and the first constraint
#' for the nsbm approach.
#
#' @param i_loop Integer containing the current DMU where the NSBM approach is.
#' @param direction String which contains the orientation of the problem (e.g. "non", "input" or "output").
#' @param Input Matrix with the data of the input variables.
#' @param Output Matrix with the data of the output variables.
#' @param Link Matrix with the data of the Link variables.
#' @param Amount_Input Vector containing the number of input variables of each division.
#' @param Amount_Output Vector containing the number of output variables of each division.
#' @param Amount_Link Vector containing the number of link variables between the divisions.
#' @param K Integer which contains the number of division.
#' @param N Integer which contains the total amount of the DMU's.
#' @param sum_m Integer which included the total number of all input variables.
#' @param sum_r Integer which included the total number of all output variables.
#' @param sum_l Integer which included the total number of all link variables.
#' @param weights Vector which contains the weights of each division K.
#' @param Link_obj Flag which is 0 for no link variable in the objective function 1 otherwise.
#'
#' @return Return a list
#' \itemize{
#'       \item FOBJ - Matrix which contains the objective function of the nsbm approach.
#'       \item FCON - Matrix which contains the first constraint for the nsbm approach.
#' }
#' @examples i = 1
#' dir = "non"
#' Input = matrix(c(3, 8,7, 4,2, 3), byrow = TRUE, nrow = 3)
#' Output = matrix(c(5, 1,1, 5,5, 2), byrow = TRUE, nrow = 3)
#' Link = matrix(c(1,2,3), byrow = TRUE, nrow = 3)
#' Amount_In = c(1,1)
#' Amount_Out = c(1,1)
#' Amount_Li = c(1)
#' K = 2
#' N = 3
#' sum_m = 2
#' sum_r = 2
#' sum_l = 1
#' weights = c(0.5, 0.5)
#' Link_obj = 0
#' Obj.func_and_con(i, dir, Input, Output, Link, Amount_In, Amount_Out,
#' Amount_Li, K, N, sum_m, sum_r, sum_l, weights, Link_obj)
#' @export

Obj.func_and_con <- function(i_loop, direction, Input, Output, Link, Amount_Input, Amount_Output,
                             Amount_Link, K, N, sum_m, sum_r, sum_l, weights, Link_obj){

# Case "non" problem
if(direction == "non"){
  # No Link variable in the objective function
  if(Link_obj == 0){
    # Initialize two auxiliary variables for the determination
    # of the objective function (Place_1) and the first constraint (Place_2)
    Place_1 = 1;
    Place_2 = 1;
    # For loop over the K divisions
    for(i in 1:K){
      # Determination of the last part of the objective function
      if(Amount_Input[i] > 0){
        aux_var1 <- c(-(weights[i]/ Amount_Input[i])*
                        (1/Input[i_loop, (Place_1: (Place_1 + Amount_Input[i]-1))]));
        aux_var1 <- t(aux_var1);
        # Case distinction whether it is the first determination of the objective function
        # or already one has been determined
        if(Place_1 ==1){
          f.obj <- aux_var1;
        }else if(Place_1 >1){
          f.obj <- cbind(f.obj, aux_var1);
        }
        Place_1 = Place_1 + Amount_Input[i];
      }
      # Determination of a part of the first constraint
      if(Amount_Output[i] > 0){
        #
        aux_var2 <- c((weights[i]/Amount_Output[i])*
                        (1/Output[i_loop, (Place_2 : (Place_2 + Amount_Output[i]-1))]));
        aux_var2 <- t(aux_var2);
        # Case distinction whether it is the first determination of the first constraint
        # or already one has been determined
        if(Place_2 == 1){
          f.con <- aux_var2;
        }else{
          f.con <- cbind(f.con, aux_var2);
        }
        Place_2 = Place_2 + Amount_Output[i];
      }
    }
    # Composition of the final objective function for the case of non-oriented problem
    f.obj <- rbind(c(1, rep(0,1,(K*N)), rep(0,1,sum_r), f.obj));
    # Composition of the final first constraint for the case of non-oriented problem
    f.con <- rbind(c(1, rep(0,1,(K*N)), f.con, rep(0,1,sum_m)));
  # Use Link variable in the objective function
  }else if(Link_obj == 1){
    # Initialize two auxiliary variables for the determination
    # of the objective function (Place_1) and the first constraint (Place_2)
    Place_1 = 1;
    Place_2 = 1;
    Place_3 = 1;
    # For loop over the K divisions
    for(i in 1:K){
      # Determination of the last part of the objective function
      if(Amount_Input[i] > 0){
        # In the case of K=1, their is no Link as input
        if(i==1){
          aux_var1 <- c(-(weights[i]/ Amount_Input[i])*
                          (1/Input[i_loop, (Place_1: (Place_1 + Amount_Input[i]-1))]));
          aux_var1 <- t(aux_var1);
        # Case K>1
        }else if(i>1){
          aux_var1 <- c(-(weights[i]/ (Amount_Input[i]+Amount_Link[i-1])) *
                          (1/Input[i_loop, (Place_1: (Place_1 + Amount_Input[i]-1))]));
          aux_var1 <- t(aux_var1);
        }
        # Case distinction whether it is the first determination of the objective function
        # or already one has been determined
        if(Place_1 ==1){
          f.obj <- aux_var1;
        }else if(Place_1 >1){
          f.obj <- cbind(f.obj, aux_var1);
        }
        Place_1 = Place_1 + Amount_Input[i];
      }
      # Determination of a part of the first constraint
      if(Amount_Output[i] > 0){
        # In the output case, their are only Link variable for i=1 to i=K-1
        if(i<K){
          aux_var2 <- c((weights[i]/(Amount_Output[i]+Amount_Link[i]))*
                          (1/Output[i_loop, (Place_2 : (Place_2 + Amount_Output[i]-1))]));
          aux_var2 <- t(aux_var2);
        # Case of  i=K and no Link variable for division i
        }else if(i==K){
          aux_var2 <- c((weights[i]/Amount_Output[i])*
                          (1/Output[i_loop, (Place_2 : (Place_2 + Amount_Output[i]-1))]));
          aux_var2 <- t(aux_var2);
        }
        # Case distinction whether it is the first determination of the first constraint
        # or already one has been determined
        if(Place_2 == 1){
          f.con <- aux_var2;
        }else{
          f.con <- cbind(f.con, aux_var2);
        }
        Place_2 = Place_2 + Amount_Output[i];
      }
      # Link variable
      if(i<K){
        # Check if their is a Link between this division i and i+1. If not go to the next division
        if(Amount_Link[i]>0){
          # Link variable in the objective function as Output
          aux_var2_1 <- c((weights[i]/(Amount_Output[i]+Amount_Link[i]))*
                            (1/Link[i_loop, (Place_3 : (Place_3 + Amount_Link[i]-1))]));
          aux_var2_1 <- t(aux_var2_1);
          # Link variable in the objective function as Input
          aux_var1_1 <- c(-(weights[i+1]/ (Amount_Input[i+1]+Amount_Link[i])) *
                            (1/Link[i_loop, (Place_3: (Place_3 + Amount_Link[i]-1))]));
          aux_var1_1 <- t(aux_var1_1);
          if(Place_3 == 1){
            f.obj_1 <- aux_var1_1;
            f.con_1 <- aux_var2_1;
          }else{
            f.obj_1 <- cbind(f.obj_1, aux_var1_1);
            f.con_1 <- cbind(f.con_1, aux_var2_1);
          }
          # Increase the number of Place_3
          Place_3 = Place_3 + Amount_Link[i];
        }
      }
    }
    if(sum_l>0){
      # Composition of the final objective function for the case of non-oriented problem
      f.obj <- rbind(c(1, rep(0,1,(K*N)), rep(0,1,sum_r), f.obj, rep(0,1,sum_l), f.obj_1));

      # Composition of the final first constraint for the case of non-oriented problem
      f.con <- rbind(c(1, rep(0,1,(K*N)), f.con, rep(0,1,sum_m), f.con_1, rep(0,1,sum_l)));
    }else{
      # Composition of the final objective function for the case of non-oriented problem
      f.obj <- rbind(c(1, rep(0,1,(K*N)), rep(0,1,sum_r), f.obj));

      # Composition of the final first constraint for the case of non-oriented problem
      f.con <- rbind(c(1, rep(0,1,(K*N)), f.con, rep(0,1,sum_m)));
    }
  }
# Case "input" problem
}else if(direction == "input"){
  # Non link variable for the objective function
  if(Link_obj == 0){
    # For the "input" and "output" problem, the first constraint is not available
    # and is therefore set it to NaN
    f.con = NaN;
    # Auxiliary variable for the objective function calculation
    Place_1 = 1;
    # For loop over the K divisions:
    for(i in 1:K){
      # Determination of the last part of the objective function
      if(Amount_Input[i] > 0){
        aux_var1 <- c(-(weights[i]/ Amount_Input[i])*
                        (1/Input[i_loop, (Place_1 : (Place_1 + Amount_Input[i]-1))]));
        aux_var1 <- t(aux_var1);
        # Case distinction whether it is the first determination of the objective function
        # or already one has been determined
        if(Place_1 ==1){
          f.obj <- aux_var1;
        }else if(Place_1 >1){
          f.obj <- cbind(f.obj, aux_var1);
        }
        Place_1 = Place_1 + Amount_Input[i];
      }
    }
    # Composition of the final objective function for the case of input-oriented problem
    f.obj <- rbind(c(rep(0,1,(K*N)), rep(0,1,sum_r), f.obj));
  # Use Link variable in the objective function
  }else if(Link_obj == 1){
    # For the "input" and "output" problem, the first constraint is not available
    # and is therefore set it to NaN
    f.con = NaN;
    # Auxiliary variable for the objective function calculation
    Place_1 = 1;
    Place_3 = 1;
    # For loop over the K divisions:
    for(i in 1:K){
      # Determination of the last part of the objective function
      if(Amount_Input[i] > 0){
        # In the case of i =1, their is no Link as input
        if(i == 1){
          aux_var1 <- c(-(weights[i]/ Amount_Input[i])*
                          (1/Input[i_loop, (Place_1 : (Place_1 + Amount_Input[i]-1))]));
          aux_var1 <- t(aux_var1);
        # Case i > 1
        }else if(i > 1){
          aux_var1 <- c(-(weights[i]/ (Amount_Input[i] + Amount_Link[i-1]))*
                          (1/Input[i_loop, (Place_1 : (Place_1 + Amount_Input[i]-1))]));
          aux_var1 <- t(aux_var1);
        }
        # Case distinction whether it is the first determination of the objective function
        # or already one has been determined
        if(Place_1 ==1){
          f.obj <- aux_var1;
        }else if(Place_1 >1){
          f.obj <- cbind(f.obj, aux_var1);
        }
        Place_1 = Place_1 + Amount_Input[i];
      }
      if(i<K){
        if(Amount_Link[i] > 0){
          # Link variable in the objective function as Input
          aux_var1_1 <- c(-(weights[i]/ (Amount_Input[i+1]+Amount_Link[i])) *
                            (1/Link[i_loop, (Place_3: (Place_3 + Amount_Link[i]-1))]));
          aux_var1_1 <- t(aux_var1_1);
          if(Place_3 == 1){
            f.obj_1 <- aux_var1_1;
          }else{
            f.obj_1 <- cbind(f.obj_1, aux_var1_1);
          }
          # Increase the number of Place_3
          Place_3 = Place_3 + Amount_Link[i];
        }
      }
    }
    # Composition of the final objective function for the case of input-oriented problem
    f.obj <- rbind(c(rep(0,1,(K*N)), rep(0,1,sum_r), f.obj, f.obj_1));
  }
# Case "output" problem
}else if(direction == "output"){
  # Non link variable for the objective function
  if(Link_obj == 0){
    # For the "input" and "output" problem, the first constraint is not available
    # and is therefore set it to NaN
    f.con <- NaN;
    # Auxiliary variable
    Place_2 = 1;
    # For loop over the K divisions
    for(i in 1:K){
      # Determination of the last part of the objective function
      if(Amount_Output[i] > 0){
        aux_var2 <- c((weights[i]/Amount_Output[i])*
                        (1/Output[i_loop, (Place_2 : (Place_2 + Amount_Output[i]-1))]));
        aux_var2 <- t(aux_var2);
        # Case distinction whether it is the first determination of the objective function
        # or already one has been determined
        if(Place_2 == 1){
          f.obj <- aux_var2;
        }else{
          f.obj <- cbind(f.obj, aux_var2);
        }
        Place_2 = Place_2 +Amount_Output[i];
      }
    }
    # Composition of the final objective function for the case of output-oriented problem
    f.obj <- rbind(c(rep(0,1,(K*N)), f.obj, rep(0,1,sum_m)));
  # Use Link variable in the objective function
  }else if(Link_obj == 1){
    # For the "input" and "output" problem, the first constraint is not available
    # and is therefore set it to NaN
    f.con <- NaN;
    # Auxiliary variable
    Place_2 = 1;
    Place_3 = 1;
    # For loop over the K divisions
    for(i in 1:K){
      # Determination of the last part of the objective function
      if(Amount_Output[i] > 0){
        # In the output case, their are only Link variable for i=1 to i=K-1
        if(i<K){
          aux_var2 <- c((weights[i]/(Amount_Output[i]+Amount_Link[i]))*
                          (1/Output[i_loop, (Place_2 : (Place_2 + Amount_Output[i]-1))]));
          aux_var2 <- t(aux_var2);
          # Case of  i=K and no Link variable for division i
        }else if(i==K){
          aux_var2 <- c((weights[i]/Amount_Output[i])*
                          (1/Output[i_loop, (Place_2 : (Place_2 + Amount_Output[i]-1))]));
          aux_var2 <- t(aux_var2);
        }
        # Case distinction whether it is the first determination of the objective function
        # or already one has been determined
        if(Place_2 == 1){
          f.obj <- aux_var2;
        }else{
          f.obj <- cbind(f.obj, aux_var2);
        }
        Place_2 = Place_2 +Amount_Output[i];
      }
      # Link variable
      if(i<K){
        # Check if their is a Link between this division i and i+1. If not go to the next division
        if(Amount_Link[i]>0){
          # Link variable in the objective function as Output
          aux_var2_1 <- c((weights[i]/(Amount_Output[i]+Amount_Link[i]))*
                            (1/Link[i_loop, (Place_3 : (Place_3 + Amount_Link[i]-1))]));
          aux_var2_1 <- t(aux_var2_1);
          if(Place_3 == 1){
            f.obj_1 <- aux_var2_1;
          }else{
            f.obj_1 <- cbind(f.obj_1, aux_var2_1);
          }
          # Increase the number of Place_3
          Place_3 = Place_3 + Amount_Link[i];
        }
      }
    }
    # Composition of the final objective function for the case of output-oriented problem
    f.obj <- rbind(c(rep(0,1,(K*N)), f.obj, rep(0,1,sum_m), f.obj_1));
  }
}
# Save the results of the objective function and the first constraint into the variable results
results <- list( "FOBJ" = f.obj, "FCON" = f.con)
# Outputs the variable results
return(results)
}
