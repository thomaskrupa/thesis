#' Constraint3
#'
#' @description The Constraint3 function creates the third constraint for the nsbm approach.
#'
#' @param i_loop Integer containing the current DMU where the NSBM approach is.
#' @param direction String which contains the orientation of the problem (e.g. "non", "input" or "output").
#' @param Output Matrix with the data of the output variables.
#' @param Amount_Output Vector containing the number of output variables of each division.
#' @param K Integer which contains the number of division.
#' @param N Integer which contains the total amount of the DMU's.
#' @param sum_m Integer which included the total number of all input variables.
#' @param sum_r Integer which included the total number of all output variables.
#' @param sum_l Integer which included the total number of all link variables.
#' @param Link_obj Flag which is 0 for no link variable in the objective function 1 otherwise.
#'
#' @return Return a matrix which contains the third constraint for the NSBM approach.
#' @examples i_loop = 1
#' direction = "non"
#' Output = matrix(c(5, 1,1, 5,5, 2), byrow = TRUE, nrow = 3)
#' Amount_Output = c(1,1)
#' K = 2
#' N = 3
#' sum_m = 2
#' sum_r = 2
#' sum_l = 1
#' Link_obj = 0
#' Constraint3(i_loop, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj)
#' @export

Constraint3 <- function(i_loop, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj){

  # Initialization of auxiliaries variables
  Place <- 1;
  Help_1 <- 1;
  # For loop over all K divisions
  for(i in 1:K){
    # Initialization of auxiliaries variables
    Help_2 <- 1;
    # Check of their is an Output for this division i
    if((Amount_Output[i]>0)){
      # Only for non-oriented problem
      if(direction == "non"){
        aux_var1<- -Output[i_loop, (Place : (Place + Amount_Output[i]-1))];
        aux_var1 <- t(t(aux_var1));
      }
      # If i is greater than 1, creates a zero matrix and combine it with the results before
      if(i>1){
        for(j in 1:(i-1)){
          aux_var2 <- matrix(0, Amount_Output[i], N);
          # Distinguish the results between non-oriented problems and the others (input, output)
          if(direction == "non"){
            aux_var1 <- cbind(aux_var1, aux_var2);
          }else{
            # Input or output-oriented problem
            # distinguish between first round with j =1 and else
            if(j==1){
              aux_var1 <- aux_var2;
              Help_2 = Help_2 + 1;
            }else{
              aux_var1 <- cbind(aux_var1, aux_var2);
            }
          }
        }
      }

      # Save the output values into the aux_var3 variable and transform it in the right dimension
      aux_var3 <- Output[,Place : (Place + Amount_Output[i]-1)];
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
      Place <- Place + Amount_Output[i];
      # If i is smaller than K, creates again zero matrix and combine it with the results before
      if(i<K){
        for(k in (i+1):K){
          aux_var3 <- matrix(0, Amount_Output[i], N);
          aux_var <- cbind(aux_var, aux_var3);
        }
      }
      # Combine the results and distinguish between the first round of the loop and otherwise
      if(Help_1 == 1){
        res <- aux_var;
        Help_1 <- Help_1 +1;
      }else{
        res <- rbind(res, aux_var)
      }
    }
  }
  # No Link variable in the objective function
  if(Link_obj == 0){
    #Save all the results into the variable res
    res <- cbind(res, -diag(sum_r), matrix(0, sum_r, sum_m));
  # Case: Link variable in the objective function
  }else if(Link_obj == 1){
    # Distinguish the Link variable between non-oriented case the other ones (input, output)
    if(direction == "non"){
      #Save all the results into the variable res
      res <- cbind(res, -diag(sum_r), matrix(0, sum_r, sum_m), matrix(0, sum_r, (2*sum_l)));
    }else{
      #Save all the results into the variable res
      res <- cbind(res, -diag(sum_r), matrix(0, sum_r, sum_m), matrix(0, sum_r, (sum_l)));
    }
  }
  # Output res
  return(res);
}
