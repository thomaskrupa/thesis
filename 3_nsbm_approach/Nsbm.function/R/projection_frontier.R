#' projection.frontier
#'
#'@description The projection.function calculates the projection for input, intermediate and
#'output variables to the frontier.
#'
#'@param link_con Integer which contains the information of the link constraint variant
#'(e.g. 1 for fix and 2 for free constraints).
#'@param slack_plus Matrix which contains the optimal slack plus solution.
#'@param slack_minus Matrix which contains the optimal slack minus solution.
#'@param lambda Matrix which contains the optimal solution for the intensity vector.
#'@param Input Matrix with the data of the input variables.
#'@param Output Matrix with the data of the output variables.
#'@param Link Matrix with the data of the link variables.
#'@param Amount_Input Vector containing the number of input variables of each division.
#'@param Amount_Output Vector containing the number of output variables of each division.
#'@param Amount_Link Vector containing the number of link variables between the divisions.
#'@param K Integer which contains the number of division.
#'@param N Integer which contains the total amount of the DMU's.
#'@param sum_m Integer which included the total number of all input variables.
#'@param sum_r Integer which included the total number of all output variables.
#'@param sum_l Integer which included the total number of all link variables.
#'
#'@return Return a list
#'  \itemize{
#'        \item Input_Proj - Optimal projection of the input variable to the frontier.
#'        \item Output_Proj - Optimal projection of the output variable to the frontier.
#'        \item Link_Proj - Optimal projection of the link variable to the frontier.
#'  }
#'@examples link_con = NaN
#'  slack_plus <- matrix(c(0.714,0,2.286,0,0,0,0,1,0,0), byrow = TRUE, nrow = 5)
#'  slack_minus <- matrix(c(0,0.357,0,0.643,0,0,0,0,0,0), byrow = TRUE, nrow = 5)
#'  lambda <- matrix(c(  0,0,0.3030303,0,0.4848485,
#'                       0,0,0.4090909,0,0.2727273,
#'                       0,0,1.0000000,0,0.0000000,
#'                       0,0,0.6666667,0,0.0000000,
#'                       0,0,0.0000000,0,1.0000000 ), byrow = TRUE, nrow = 5)
#'  Input = matrix(c(4,3,6,3,8,1,8,1,2,4), byrow = TRUE, nrow = 5)
#'  Output = matrix(c(2,3,2,3,6,2,6,1,1,4), byrow = TRUE, nrow = 5)
#'  Link <- NaN
#'  Amount_In =  c(2)
#'  Amount_Out = c(2)
#'  Amount_L = NaN
#'  N = 5
#'  K = 1
#'  sum_m = 2
#'  sum_r = 2
#'  sum_l = 0
#'  projection.frontier(link_con, slack_plus, slack_minus, lambda, Input, Output,
#'   Link, Amount_In, Amount_Out, Amount_L, N, K, sum_m, sum_r, sum_l)
#'@export

projection.frontier <- function(link_con, slack_plus, slack_minus, lambda, Input, Output, Link,
                                Amount_Input, Amount_Output, Amount_Link, N, K, sum_m, sum_r, sum_l){

  #This function calculations the projection of input, output and link variables to the frontier.
  #For this it uses the optimal solution variable of the nsbm function

  # Initialization of the results matrix
  Output_proj <- matrix(0, N, sum_r);
  Input_proj <- matrix(0, N, sum_m);
  # Projection for the nsbm calculation:
  if(K>1){
    # for loop over all DMU's
    for(i in 1:N){
      # Initialization of auxiliary variable
      Place_1 <- 1;
      Place_2 <- 1;
      # for loop over all divisions K
      for(j in 1:K){
        #Calculates the projection for the input variables if there exists one
        if(Amount_Input[j] > 0){
          Input_proj[i, (Place_1: (Place_1 + Amount_Input[j]-1))] =
            Input[i,(Place_1:(Place_1+Amount_Input[j]-1))] -
            slack_minus[i,(Place_1: (Place_1 + Amount_Input[j]-1))];
          Place_1 <- Place_1 +Amount_Input[j];
        }
        #Calculates the projection for the output variables if there exists one
        if(Amount_Output[j] >0){
          Output_proj[i, (Place_2: (Place_2 + Amount_Output[j]-1))] =
            Output[i,(Place_2:(Place_2+Amount_Output[j]-1))] +
            slack_plus[i, (Place_2: (Place_2 + Amount_Output[j]-1))];
          Place_2 = Place_2 +Amount_Output[j];
        }
      }
    }
    # There exist only a projection for Link in the free link case. In the fix case, the
    #projection is exactly the Link variable again.
    if(link_con == 2){
      # Initialization of the result matrix
      Link_proj <- matrix(0, N, sum_l);
      # Calculation of the Link projection:
      for(i in 1:N){
        # Initialization of the auxiliary variable
        Place_3 <- 1;
        Place_4 <- 1;
        for(j in 1:(K-1)){
          #Transform the found matrices into the right dimension for a matrix multiplication
          a <- t(lambda[i, ((j-1)*N+1) : (j*N)]);
          b <- t(t( Link[,Place_3 : (Place_3 + Amount_Link[j]-1)]  ));
          # Calculation is distinguish between 1 Link variable and 2 or more :
          if(Amount_Link[j]== 1){
            Link_proj[i,Place_4] <- a %*% b;
            Place_4 <- Place_4 + 1;
          }else if(Amount_Link[j] >1){
            for(k in 1: Amount_Link[j]){
              Link_proj[i,Place_4] <- a %*% b[,k];
              Place_4 <- Place_4 +1;
            }
          }
          Place_3 <- Place_3 + Amount_Link[j];
        }
      }
    }else{
      # fix case (link_con == 1):
      # The projection of the frontier for the Link variable is the Link variable
      Link_proj = Link;
    }
  #
  }else{
    # Calculation of the projection in the SBM case:
    # fix case (link_con == 1):
    # The projection of the frontier for the Link variable is the Link variable
    Link_proj <- Link;
    # for loop over all DMUs
    for(i in 1:N){
        #Calculates the projection for the input variables if there exists one
        if(Amount_Input[1] > 0){
          Input_proj[i, ] = Input[i, ] - slack_minus[i, ];
        }
        #Calculates the projection for the output variables if there exists one
        if(Amount_Output[1] >0){
          Output_proj[i, ] = Output[i, ] + slack_plus[i, ];
        }
    }
  }
  # Safe the projections of frontier
  results <- list("Input_Proj" = Input_proj,  "Output_Proj" = Output_proj, "Link_Proj" = Link_proj)
  return(results)
}
