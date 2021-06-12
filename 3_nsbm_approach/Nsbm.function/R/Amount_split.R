#' Amount.split
#'
#' @description This Amount.split function split the Amount vector into the vectors Amount_Input,
#' Amount_Output and Amount_Link
#'
#' @param Amount Vector containing the number of inputs and outputs variables of each division and
#' the number of link variables.
#' @param K Integer which contains the number of division.
#'
#' @return Return a list
#' \itemize{
#'    \item Amount_IN - Vector which contains the number of inputs of each division K.
#'    \item Amount_OUT - Vector which contains the number of outputs of each division K.
#'    \item Amount_LINK - Vector which includes the amount of Link variable.
#' }
#' @examples Amount = c(1,1,1,1,1)
#'  K = 2
#'  Amount.split(Amount, K)
#' @export

Amount.split <- function(Amount, K){

  # Case for K>1 and Link variables
  if(K>1){
    # Creates a vector Amount_Input and saves the number of Inputs of each division in it
    Amount_Input <- rep(0,K)
    k = 1;
    for(i in 1:K){
      Amount_Input[i] = Amount[k]
      k = k+2;
    }

    # Creates a vector Amount_Output and saves the number of Outputs of each division in it
    Amount_Output <- rep(0,K)
    k = 2;
    for(i in 1:K){
      Amount_Output[i] = Amount[k]
      k = k+2;
    }
    # Creates a vector Amount_Link and saves the number of Link of each division in it
    Amount_Link <- rep(0,(K-1));
    k = (2*K)+1;

    for(i in 1:(K-1)){
      Amount_Link[i] = Amount[k]
      k = k+1;
    }
    # Case K=1:
  }else{
    # Creates a vector Amount_Input and saves the number of Inputs of each division in it
    Amount_Input <- rep(0,K)
    k = 1;
    for(i in 1:K){
      Amount_Input[i] = Amount[k]
      k = k+2;
    }
    # Creates a vector Amount_Output and saves the number of Outputs of each division in it
    Amount_Output <- rep(0,K)
    k = 2;
    for(i in 1:K){
      Amount_Output[i] = Amount[k]
      k = k+2;
    }
  }

  if(K>1){
    # Save all results in the variable results and output it
    results <- list( "Amount_IN" = Amount_Input, "Amount_OUT" = Amount_Output,
                     "Amount_LINK" = Amount_Link);
    return(results)
    # Case K=1:
  }else{
    # Save all results in the variable results and output it
    results <- list( "Amount_IN" = Amount_Input, "Amount_OUT" = Amount_Output, "Amount_LINK" = NaN);
    return(results)
  }
}
