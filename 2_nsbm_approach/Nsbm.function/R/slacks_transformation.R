#' slacks.transformation
#'
#'@description The slacks_transformation function uses the the solution of the variables (weightsNSBM)
#' and decompose them into the individual solution (t, lambda, slack_plus and slack_minus).
#'
#'@param direction String which contains the orientation of the problem (e.g. "non", "input" or "output").
#'@param weightsNSBM Matrix which contains all solution of the variables from the NSBM approach.
#'@param K Integer which contains the number of division
#'@param N Integer which includes the number of DMU's.
#'@param sum_m Integer which included the total number of all input variables.
#'@param sum_r Integer which included the total number of all output variables.
#'@param sum_l Integer which included the total number of all link variables.
#'@param Link_obj Flag which is 0 for no link variable in the objective function 1 otherwise.
#'1 works only in the non-oriented case.
#'
#'@return return a list
#'  \itemize{
#'       \item t - Is a vector with the optimal solution of variable t.
#'       \item lambda - Contains a matrix with the intensity vector lambda.
#'       \item slack_plus - Contains a matrix with the optimal slack plus solution.
#'       \item slack_minus - Contains a matrix with the optimal slack minus solution.
#'  }
#'@examples direction = "non"
#'  weightsNSBM <- matrix(c(0.8,0,0,0.3,0,0.4,0.6,0,0,0.3,0.6,0,0,
#'  0.4,0,0.2,1.4,0,0,0.4,1,0,0,1,0,0,0,0,0,0,0.6,0,0,0.6,0,0,0,0.6,
#'  0,0,1,0,0,0,0,1,0,0,0,0), byrow = TRUE, nrow = 5)
#'  K = 1
#'  N = 5
#'  sum_m = 2
#'  sum_r = 2
#'  sum_l = 0
#'  Link_obj = 0
#'  slacks.transformation(direction, weightsNSBM, K, N, sum_m, sum_r, sum_l, Link_obj)
#'@export

slacks.transformation <- function(direction, weightsNSBM, K, N, sum_m, sum_r, sum_l, Link_obj){

  #This function inputs the optimal solution of the variables (weightsNSBM) and decompose them
  # into the individual solution variable (e.g. t, lambda, slack_plus, slack_minus).

  #Case non-oriented problem:
  if(direction == "non"){
    # estimate t and save it into t_:
    t_ <- weightsNSBM[ ,1];
    #The variable is transposed twice to avoid problems with dimensions
    t_ <- t(t(t_));
    # intensity vector
    lambda_ <- weightsNSBM[,2:(1+K*N)];
    #The variable is transposed twice to avoid problems with dimensions
    lambda_ <- t(t(lambda_));
  }else{
    # Input or output-oriented case
    # no t variable in the input and output-oriented case
    t_ <- NaN;
    # intensity vector
    lambda_ <- weightsNSBM[,1:(K*N)];
    #The variable is transposed twice to avoid problems with dimensions
    lambda_ <- t(t(lambda_));
  }
  # Estimate slack_minus and slack_plus:
  if(direction == "non"){
    # No link variables in the objective function
    if(Link_obj == 0){
      #Decompose the slack_plus and slack_minus variable from the weightNSBM matrix
      slack_plus <- weightsNSBM[ , (1+1+(K*N)):(1+sum_r+(K*N))];
      #The variable is transposed twice to avoid problems with dimensions
      slack_plus <- t(t(slack_plus));

      slack_minus <- weightsNSBM[ , (1+K*N+sum_r+1):(1+K*N+sum_r+sum_m)];
      #The variable is transposed twice to avoid problems with dimensions
      slack_minus <- t(t(slack_minus));
    # Link variable in the objective function
    }else if(Link_obj == 1){
      #Decompose the slack_plus and slack_minus variable from the weightNSBM matrix
      slack_plus_1 <- weightsNSBM[ , (1+1+(K*N)):(1+sum_r+(K*N))];
      slack_plus_2 <- weightsNSBM[ ,(1+(K*N)+sum_r+sum_m+1) : (1+(K*N)+sum_r+sum_m+sum_l) ];
      #The variable is transposed twice to avoid problems with dimensions
      slack_plus_1 <- t(t(slack_plus_1));
      slack_plus_2 <- t(t(slack_plus_2));
      # Combine slack_plus from the outputs and links
      slack_plus <- cbind(slack_plus_1, slack_plus_2);
      #The variable is transposed twice to avoid problems with dimensions
      #slack_plus <- t(t(slack_plus));
      slack_minus_1 <- weightsNSBM[ , (1+K*N+sum_r+1):(1+K*N+sum_r+sum_m)];
      slack_minus_2 <- weightsNSBM[ , (1+(K*N)+sum_r+sum_m+sum_l+1) : (1+(K*N)+sum_r+sum_m+2*sum_l)];
      #The variable is transposed twice to avoid problems with dimensions
      slack_minus_1 <- t(t(slack_minus_1));
      slack_minus_2 <- t(t(slack_minus_2));
      # Combine slack_minus from the inputs and links
      slack_minus <- cbind(slack_minus_1, slack_minus_2);
      #The variable is transposed twice to avoid problems with dimensions
      #slack_minus <- t(t(slack_minus));
    }
  }else{
    # input or output-oriented case:
    #Decompose the slack_plus and slack_minus variable from the weightNSBM matrix
    slack_plus <- weightsNSBM[ , (K*N+1):(K*N+sum_r)];
    #The variable is transposed twice to avoid problems with dimensions
    slack_plus <- t(t(slack_plus));
    slack_minus <- weightsNSBM[ , (K*N+sum_r+1):(K*N+sum_r+sum_m)];
    #The variable is transposed twice to avoid problems with dimensions
    slack_minus <- t(t(slack_minus));
  }
  # In the non-oriented case, the lambdas and slacks must be transformed into the real values using t
  if(direction == "non"){
    for(i in seq_along(slack_plus[1,])){
      slack_plus[,i] <- slack_plus[,i] / t_ ;
    }
    for(j in seq_along(slack_minus[1,])){
      slack_minus[,j] <- slack_minus[,j] / t_ ;
    }
    for(k in 1:(K*N)){
      lambda_[,k] <- lambda_[,k] / t_;
    }
  }
  # Safe the all variables (t, lambda, slack_plus and slack_minus) into a list and output it:
  results <- list("t" = t_, "lambda" = lambda_, "slack_plus" = slack_plus, "slack_minus" = slack_minus)
  return(results)
}
