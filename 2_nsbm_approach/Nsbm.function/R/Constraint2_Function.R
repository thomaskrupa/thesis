#' Constraint2
#'
#' @description The Constraints2 function creates the second constraint for the NSBM approach.
#'
#' @param i_loop Integer containing the current DMU where the NSBM approach is.
#' @param direction String which contains the orientation of the problem (e.g. "non", "input" or "output").
#' @param Input Matrix with the data of the input variables.
#' @param Amount_Input Vector containing the number of the input variables of each division.
#' @param K Integer which contains the number of division.
#' @param N Integer which contains the total amount of the DMUs.
#' @param sum_m Integer which included the total number of all input variables.
#' @param sum_r Integer which included the total number of all output variables.
#' @param sum_l Integer which included the total number of all link variables.
#' @param Link_obj Flag which is 0 for no link variable in the objective function 1 otherwise.
#'
#' @return Return a matrix which contains the second constraint for the NSBM approach.
#' @examples i_loop = 1
#' direction = "non"
#' Input = matrix(c(3, 8,7, 4,2, 3), byrow = TRUE, nrow = 3)
#' Amount_Input = c(1,1)
#' K = 2
#' N = 3
#' sum_m = 2
#' sum_r = 2
#' sum_l = 1
#' Link_obj = 0
#' Constraint2(i_loop, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj)
#' @export

Constraint2 <- function(i_loop, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj){

# Initialization of auxiliaries variables
Place <- 1;
Help_1 <- 1;

# Start for loop over the K divisions
for(i in 1:K){
  # Initialization of auxiliaries variables
  Help_2 <- 1;
  # Check if there is a Input for division K
  if((Amount_Input[i]>0)){
    # Only for the non-oriented problem:
    if(direction == "non"){
      aux_var1<- -Input[i_loop, (Place : (Place + Amount_Input[i]-1))];
      aux_var1 <- t(t(aux_var1));
    }
    # If i is greater than 1, creates a matrix with zeros
    if(i>1){
      for(j in 1:(i-1)){
        aux_var2 <- matrix(0, Amount_Input[i], N);
        # Distinguish the results between non-oriented problems and the others (input, output)
        if(direction == "non"){
          aux_var1 <- cbind(aux_var1, aux_var2);
        }else{
          # Input or output-oriented problem:
          # Distinguish between first round with j =1 and else
          if(j==1){
            aux_var1 <- aux_var2;
            Help_2 = Help_2 + 1;
          }else{
            aux_var1 <- cbind(aux_var1, aux_var2);
          }
        }
      }
    }
    # Save the input values into the aux_var3 variable
    aux_var3 <- Input[,Place : (Place + Amount_Input[i]-1)];
    aux_var3 <- t(aux_var3);
    if(direction == "non"){
      aux_var <- cbind(aux_var1, aux_var3);
    }else{
      # Input or output-oriented case. Distinguish between first loop and else:
      if(Help_2 == 1){
        aux_var <- aux_var3
      }else{
        aux_var <- cbind(aux_var1, aux_var3);
      }
    }
    Place <- Place + Amount_Input[i];
    # If i is smaller than K, create a matrix with zeros
    if(i<K){
      for(k in (i+1):K){
        aux_var3 <- matrix(0, Amount_Input[i], N);
        aux_var <- cbind(aux_var, aux_var3);
      }
    }
    # Combine the results and distinguish between the first round of the loop and otherwise
    if(Help_1 ==1){
      res <- aux_var;
      Help_1 <- Help_1 + 1;
    }else{
      res <- rbind(res, aux_var)
    }
  }
}
# No Link variable in the objective function
if(Link_obj == 0){
  #save the results into the res matrix
  res <- cbind(res, matrix(0, sum_m, sum_r), diag(sum_m));
# Case: Link variable in the objective function
}else if(Link_obj == 1){
  # Distinguish the Link variable between non-oriented case the other ones (input, output)
  if(direction == "non"){
    #save the results into the res matrix
    res <- cbind(res, matrix(0, sum_m, sum_r), diag(sum_m), matrix(0, sum_m, (2*sum_l)));
  }else{
    #save the results into the res matrix
    res <- cbind(res, matrix(0, sum_m, sum_r), diag(sum_m), matrix(0, sum_m, (sum_l)));
  }
}
# Output the res matrix
return(res);
}
