#' nsbm
#'
#' @description This nsbm function Calculates the technical efficiency score by applying the
#' network slacks-based measure (NSBM). It also outputs the division scores, the optimal variable
#' solution and the projection to the frontier.
#'
#' @param direction String which contains the orientation of the problem. Possible are "non", "input"
#' or "output". Default value is "non".
#' @param return_to_scale String which contains the return to scale variant. Possible are
#' "CRS" (constant return to scale) or "VRS" (variant return to scale).
#' @param link_con Integer which contains the information of the link constraint variant. Possible
#' are 1 for fix constraint and 2 for free constraint.
#' @param Data Matrix which contains all the data of the problem. The data matrix has a special form.
#' First inputs and outputs come from the first division. Then from the second and following division.
#' Finally there are the link variables.
#' @param Amount Vector containing the number of inputs and outputs variables of each division and
#' the number of link variables. Start with the number of the division and at the end the number of the
#' link variables.
#' @param K Integer which contains the number of division. The number should be an even number.
#' @param weights Vector which contains the weights of each division K.
#' @param NIRS Flag which is 0 for the normal calculation and 1 for the Non-increasing return to scale (NIRS)
#' calculation. Default value is 0. NIRS is only calculating when return_to_scale = "VRS".
#' @param Link_obj Flag which is 0 for no link variable in the objective function 1 otherwise.
#' 1 works only in the non-oriented case.
#'
#' @return Return a list of different results. There are different results for K >1 and K=1
#' \itemize{
#'    \item Return for the K>1 case:
#'       \itemize{
#'          \item Return for Link_obj = 0 case:
#'              \itemize{
#'                 \item Eff - Vector which contains the efficiency scores.
#'                 \item divEff - Matrix which contains the division efficiency scores.
#'                 \item weightNSBM - Matrix which contains all the optimal solutions of the variable.
#'                 \item t - Vector which includes the optimal solution of the variable t.
#'                 \item lambda - Matrix which includes the optimal intensity vector lambda.
#'                 \item slacks_plus - Matrix which contains the optimal slack plus solution.
#'                 \item slack_minus - Matrix which contains the optimal slack minus solution.
#'                 \item Input_proj - Matrix which includes the optimal projection of the input
#'                 variable to the frontier.
#'                 \item Output_proj - Matrix which includes the optimal projection of the output
#'                 variable to the frontier.
#'                \item Link_proj - Matrix which includes the optimal projection of the link
#'                variable to the frontier.
#'              }
#'          \item Return for Link_obj = 1 case:
#'              \itemize{
#'                 \item Eff - Vector which contains the efficiency scores.
#'                 \item divEff - Matrix which contains the division efficiency scores.
#'                 \item weightNSBM - Matrix which contains all the optimal solutions of the variable.
#'                 \item t - Vector which includes the optimal solution of the variable t.
#'                 \item lambda - Matrix which includes the optimal intensity vector lambda.
#'                 \item slacks_plus - Matrix which contains the optimal slack plus solution.
#'                 \item slack_minus - Matrix which contains the optimal slack minus solution.
#'             }
#'        }
#'    \item Return for the K=1 case:
#'       \itemize{
#'          \item Eff - Vector which contains the efficiency scores.
#'          \item weightNSBM - Matrix which contains all the optimal solutions of the variable.
#'          \item t - Vector which includes the optimal solution of the variable t.
#'          \item lambda - Matrix which includes the optimal intensity vector lambda.
#'          \item slacks_plus - Matrix which contains the optimal slack plus solution
#'          \item slack_minus - Matrix which contains the optimal slack minus solution.
#'          \item Input_proj - Matrix which includes the optimal projection of the input
#'          variable to the frontier.
#'          \item Output_proj - Matrix which includes the optimal projection of the output
#'          variable to the frontier.
#'        }
#' }
#' @examples direction = "non"
#' return_to_scale = "VRS"
#' link_con = 1
#' Data <- matrix(c(3, 5, 8, 1, 2, 7, 1, 4, 5, 8,2, 5, 3, 2, 1), byrow=TRUE, nrow=3)
#' Amount = c(1,1,1,1,1)
#' K = 2
#' weights = matrix(c(0.5, 0.5),byrow=TRUE, nrow=1)
#' NIRS = 0
#' Link_obj = 0
#' nsbm(direction, return_to_scale, link_con, Data, Amount, K, weights, NIRS, Link_obj)
#' @import "lpSolve"
#' @export

nsbm <- function(direction="non", return_to_scale, link_con, Data, Amount, K, weights, NIRS=0, Link_obj){

  ### The function 'nsbm' is the primary function for the NSBM approach and is divided into six parts.
  ### First, some plausibilities are checked. Second, the variables are transformed and defined (initialization).
  ### Third, the opimisation problem is setted up by performing most of the secondary functions. Then,
  ### the calculation of the NSBM approach is performed by using the R function lp(). Finally, some
  ### adjustments are done and the results are outputed.
  library(lpSolve);

  ###########################################
  # Checking the input values for plausibility
  ###########################################

  # The second dimension of Data should be the sum of Amount
  if (dim(Data)[2]!=sum(Amount)){
    stop("The dimension of Data and the Amount vector do not fit!")
  }
  # Checking if K is an integer
  if(all.equal(K, as.integer(K)) != "TRUE"){
    stop("The dimension K should be an integer!")
  }
  # Checking if link_con is 1 or 2
  if(K>1 & link_con!=1 & link_con!= 2){
    stop("link_con should be 1 or 2!")
  }
  # Checking if the sum of weights is equal to one
  if (sum(weights)!=1){
    stop("The sum of the weights are not equal to 1")
  }
  # Checking if the number of weights is equal to the number of divisions K
  if (dim(weights)[2]!= K){
    stop("The number of weights must be equal to the number of divisions K")
  }
  # Checking if direction is equal to "non", "input" or "output"
  if (direction!= "non" & direction!= "input" & direction!= "output"){
   stop("direction should be equal to non, input or output")
  }
  # Checking if Link_obj is equal to 0 or 1
  if (Link_obj != 0 & Link_obj != 1){
    stop("Link_obj should be equal to 0 or 1")
  }
  # Checking if Link_obj is equal to 0 or 1
  if (Link_obj == 1 & direction != "non"){
    stop("Link variable in the objective function is only available for a non-oriented problem")
  }

  ###########################################
  # Initialization
  ###########################################

  # Creation of the vector amount of inputs, outputs and links of each divisions:
  T <- Amount.split(Amount, K);
  # Distinguish the results:
  Amount_Input <- T$Amount_IN;
  Amount_Output <- T$Amount_OUT;
  Amount_Link <- T$Amount_LINK;
  # Creation of the total amount of input, output and link variable and save them into
  # sum_m, sum_r and sum_l:
  sum_m = sum(Amount_Input);
  sum_r = sum(Amount_Output);
  # sum_l and Amount_Link exist only in the K>1 case:
  if(K>1){
    sum_l = sum(Amount_Link);
  }
  # Checking if K=1, sum_m and sum_r are greater than 0
  if(K == 1){
    if(sum_m == 0 | sum_r == 0){
      stop("The SBM model needs at least one input and one output variable")
    }
  }else{
    # Checking if sum_m or sum_r > 0
    if(sum_m == 0 | sum_r == 0){
      stop("The NSBM approach needs at least one input and one output for the calculation")
    }
  }
  # Save the total amount of DMUs into the variable N
  N <- dim(Data)[1];
  # Call data.split function to creates the matrices input, output and link
  A <- data.split(Data, Amount, K, N);
  # Distinguish the results
  Input <- A$In;
  Output <- A$Out;
  Link <- A$Link;
  # Check if the input matrix has any negative or zero values. If yes, stop the nsbm function
  for(i in 1:sum_m){
    if(length(subset(Input[,i], Input[,i]<=0))>0){
      stop("The input matrix should only contains positive values")
    }
  }
  # Checking, if K>1 and the link matrix has zero or negative values
  if(K>1){
    # If yes, stop the nsbm function:
    for(i in 1:sum_l){
      if(length(subset(Link[,i], Link[,i]<=0))>0){
        stop("The link matrix should only contains positive values")
      }
    }
  }
  Output_Adj <- Output;
  # Only for the direction non or output, it is possible to adjust the output values if there are
  # zero or negative:
  if(direction == "non" | direction == "output"){
    # loop over the columns of the output matrix
    for(i in 1:sum_r){
      #check if the output matrix contains zeros or negative values in column i
      if(length(subset(Output[,i], Output[,i]<=0))>0){
        # Save the column with the negative or zero values into Output_vec
        Output_vec <- Output[,i];
        # The variable is transposed twice to avoid problems with dimensions
        Output_vec <- t(t(Output_vec));
        # Adjust the zeros and negative values of the column i in the Output vector and save them again
        Output_Adj[,i] <- negative_zero_value(Output_vec, N);
      }
    }
  }

  ###########################################
  # Setting up the optimisation problem
  ###########################################

# Main for loop over all DMUs
for(i in 1:N){
  # Call the function 'Righthandside_and_Direction' and save the results into Y
  Y <- Righthandside_and_Direction(i, direction, return_to_scale, link_con, NIRS, Input, Output, Link,
                                   Amount_Link, K, sum_m, sum_r, sum_l);
  # Distinguish the results and save them into f.dir and f.rhs
  f.dir <- Y$FDIR;
  f.rhs <- Y$FRHS;

# If the direction is non-oriented
if(direction == "non"){
  # Call the function "Obj.func_and_con" to calculate the objective function and the first constraint
  # and save them into Z
  Z <- Obj.func_and_con(i, direction, Input, Output_Adj, Link, Amount_Input, Amount_Output, Amount_Link,
                        K, N, sum_m, sum_r, sum_l, weights, Link_obj);
  # Distinguish the results and save them into f.obj and f.con
  f.obj <- Z$FOBJ;
  f.con <- Z$FCON;
  # Call the function 'Constraint2' and save the results in f.con
  A <- Constraint2(i, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj);
  f.con <- rbind(f.con, A);
  #
  #If the direction is input- or output-oriented
}else{
  # Call the function 'Obj.func_and_con' to calculate the objective function and the first constraint
  Z <- Obj.func_and_con(i, direction, Input, Output_Adj, Link, Amount_Input, Amount_Output, Amount_Link,
                        K, N, sum_m, sum_r, sum_l, weights, Link_obj);
  # Save only the f.obj because input and output-oriented problems have not the first constraint
  f.obj <- Z$FOBJ;
  # Call the function "Constraint2" and save the results in f.con
  A <- Constraint2(i, direction, Input, Amount_Input, K, N, sum_m, sum_r, sum_l, Link_obj);
  f.con <- A;
}

# Call the function "Constraint3" and save the results in f.con
B <- Constraint3(i, direction, Output, Amount_Output, K, N, sum_m, sum_r, sum_l, Link_obj);
f.con <- rbind(f.con, B);
# Checking if the problem use VRS or CRS. If the the problem use VRS, then
# the "vrs.constraint" function creates the constraints.
if(return_to_scale == "VRS"){
  # Call the "vrs.constraint" function  and save the results into f.con
  C <- vrs.constraint(direction, K, N, sum_m, sum_r, sum_l, Link_obj);
  f.con <- rbind(f.con, C);
}

# Only for the case K>1:
if(K>1){
  # If no link variable exist, no constraints should be made in the "Constraint5" function
  if(sum_l >0){
    # Call the "Constraint5" function and save the results into f.con
    D <- Constraint5(i, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj);
    f.con <- rbind(f.con, D);
  }
}

# Call the function "t_constraint" (only in the non-oriented problem) and save the results into f.con,
if(direction == "non"){
  E <- t_constraint(K, N, sum_m, sum_r, sum_l, Link_obj);
  f.con <- rbind(f.con, E);
}

###########################################
# Calculation
###########################################

# Calculates the efficiency scores and save them into results_NSBM
  if(direction == "non"){
   results_NSBM <- lp("min", as.numeric(f.obj), f.con, f.dir, f.rhs, scale=0, compute.sens=TRUE);
  }else if(direction == "input"){
   results_NSBM <- lp("min", as.numeric(f.obj), f.con, f.dir, f.rhs, scale=0, compute.sens=TRUE);
  }else if(direction == "output"){
    results_NSBM <- lp("max", as.numeric(f.obj), f.con, f.dir, f.rhs, scale=0, compute.sens=TRUE);
  }
# Distinguish the results from the nsbm approach and saved them into weightsNSBM, efficiencyNSBM
# and lambdasNSBM
if(i==1){
  weightsNSBM <- results_NSBM$solution
  efficiencyNSBM <- results_NSBM$objval
  lambdasNSBM <- results_NSBM$duals[seq(1,N)]
}else{
  weightsNSBM <- rbind(weightsNSBM, results_NSBM$solution)
  efficiencyNSBM <- rbind(efficiencyNSBM, results_NSBM$objval)
  lambdasNSBM <- rbind(lambdasNSBM, results_NSBM$duals[seq(1,N)])
}
}

  ###########################################
  # Adjustments
  ###########################################

  # Adjustment of the Efficiency score in the input-oriented case
  if(direction == "input"){
    efficiencyNSBM <- 1+ efficiencyNSBM;
  }
  # Adjustment of the Efficiency score in the output-oriented case
  if(direction == "output"){
    efficiencyNSBM <- 1/(efficiencyNSBM+1);
  }
  # Decomposed the optimal variable solution (weightNSBM)  and save the results into A
  A <- slacks.transformation(direction, weightsNSBM, K, N, sum_m, sum_r, sum_l, Link_obj);
  # Distinguish the results and save them into t, lambda_, slack_plus and slack_minus
  t_ <- A$t;
  lambda_ <- A$lambda;
  slack_plus <- A$slack_plus;
  slack_minus <- A$slack_minus;
  # The division efficiency should only calculates when there are at least two division (K>1)
  if(K>1){
    # Calculation of the division efficiency with the help of "nsbm.division" function
    divisionNSBM <- nsbm.division(direction, slack_plus, slack_minus, Input, Output_Adj, Link,
                                  Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, Link_obj);
  }
  # Projection are only calculated for the case no Link variable in the objective function (k>1) or
  # in the K == 1 case
  if(Link_obj == 0 & K >1){
    # The "projection.frontier" function calculates the projection of the variables and save them into B
    B <- projection.frontier(link_con, slack_plus, slack_minus, lambda_, Input, Output_Adj, Link,
                             Amount_Input, Amount_Output, Amount_Link, N, K, sum_m, sum_r, sum_l)

    # Distinguish the results and save them into Input_proj, Output_proj and Link_proj
    Input_proj <- B$Input_Proj;
    Output_proj <- B$Output_Proj;
    Link_proj <- B$Link_Proj;
  }else if(K == 1){
    # The "projection.frontier" function calculates the projection of the variables and save them into B
    B <- projection.frontier(link_con, slack_plus, slack_minus, lambda_, Input, Output_Adj, Link,
                             Amount_Input, Amount_Output, Amount_Link, N, K, sum_m, sum_r, sum_l)
    # Distinguish the results and save them into Input_proj, Output_proj and Link_proj
    Input_proj <- B$Input_Proj;
    Output_proj <- B$Output_Proj;
    Link_proj <- B$Link_Proj;
  }

  ###########################################
  # Output
  ###########################################

# Distinguish the outputs between the nsbm approach (K>1) and the sbm approach (K=1)
if(K>1){
  # nsbm approach (K>1)
  if(Link_obj == 0){
    # Save the results in a list results and outputs the results
    results <- list( "Eff" = efficiencyNSBM, "divEff" = divisionNSBM, "weightNSBM" = weightsNSBM,
                     "t" = t_, "lambda" = lambda_, "slacks_plus" = slack_plus, "slack_minus" = slack_minus,
                     "Input_proj" = Input_proj, "Output_proj" = Output_proj, "Link_proj" = Link_proj)
    return(results)
  }else if(Link_obj == 1){
    # Save the results in a list results and outputs the results
    results <- list( "Eff" = efficiencyNSBM, "divEff" = divisionNSBM, "weightNSBM" = weightsNSBM,
                     "t" = t_, "lambda" = lambda_, "slacks_plus" = slack_plus, "slack_minus" = slack_minus)
    return(results)
  }
}else{
  # sbm approach (K=1)
  # Save the results in a list results (without the division efficiency) and outputs the results
  results <- list( "Eff" = efficiencyNSBM, "weightNSBM" = weightsNSBM, "t" = t_, "lambda" = lambda_,
                   "slacks_plus" = slack_plus, "slack_minus" = slack_minus, "Input_proj" = Input_proj,
                   "Output_proj" = Output_proj)
  return(results)
}
}
