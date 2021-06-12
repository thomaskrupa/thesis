#' vrs.constraint
#'
#' @description The vrs_constraint function creates the VRS constraint for the NBSM approach
#'
#' @param direction String which contains the orientation of the problem (e.g. "non", "input" or "output").
#' @param K Integer which contains the number of division.
#' @param N Integer which includes the number of DMU's.
#' @param sum_m Integer which included the total number of all input variables.
#' @param sum_r Integer which included the total number of all output variables.
#' @param sum_l Integer which included the total number of all link variables.
#' @param Link_obj Flag which is 0 for no link variable in the objective function 1 otherwise.
#'
#' @return Return a matrix with the VRS constraint
#' @examples direction = "non"
#'  K = 2
#'  N = 3
#'  sum_m = 2
#'  sum_r = 2
#'  sum_l = 1
#'  Link_obj = 0
#'  vrs.constraint(direction, K, N, sum_m, sum_r, sum_l, Link_obj)
#'@export

vrs.constraint <- function(direction, K, N, sum_m, sum_r, sum_l, Link_obj){

  #This function creates the VRS constraint for the nsbm approach and save them into a matrix.
  for(i in 1:K){
    # Auxiliary variable
    Help_1 <- 1;
    # Only for the non-oriented problem
    if(direction == "non"){
      aux_var1 <- c(-1);
      aux_var1 <- t(aux_var1);
    }

    if(i>1){
      for(j in 1:(i-1)){
        aux_var2 <- matrix(0, 1, N);
        # Non-oriented problem:
        if(direction == "non"){
          aux_var1 <- cbind(aux_var1, aux_var2);
        }else{
          #input or output-oriented problem
          #distinguish between first round with j =1 and otherwise
          if(j==1){
            aux_var1 <- aux_var2;
            Help_1 = Help_1 + 1;
          }else{
            aux_var1 <- cbind(aux_var1, aux_var2);
          }
        }
      }
    }
    # Auxiliary matrix:
    aux_var3 <- matrix(1,1,N);
    # Non-oriented problem
    if(direction == "non"){
      aux_var <- cbind(aux_var1, aux_var3);
    }else{
      # Input and output-oriented case
      if(Help_1 == 1){
        aux_var <- aux_var3;
      }else{
        aux_var <- cbind(aux_var1, aux_var3);
      }
    }
    if(i<K){
      for(k in (i+1):K){
        aux_var3 <- matrix(0, 1, N);
        aux_var <- cbind(aux_var, aux_var3);
      }
    }
    #combine all the parts together:
    if(i==1){
      res <- aux_var;
    }else{
      res <- rbind(res, aux_var)
    }
  }
  # No Link variable in the objective function
  if(Link_obj == 0){
    # combine all results with additional zeros matrix
    res <- cbind(res, matrix(0,K, sum_r), matrix(0, K, sum_m));
  # Case: Link variable in the objective function
  }else if(Link_obj == 1){
    # Distinguish the Link variable between non-oriented case the other ones (input, output)
    if(direction == "non"){
      # combine all results with additional zeros matrix
      res <- cbind(res, matrix(0,K, sum_r), matrix(0, K, sum_m), matrix(0,K,(2*sum_l)));
    }else{
      # combine all results with additional zeros matrix
      res <- cbind(res, matrix(0,K, sum_r), matrix(0, K, sum_m), matrix(0,K,sum_l));
    }
  }
  # Output res
  return(res);
}
