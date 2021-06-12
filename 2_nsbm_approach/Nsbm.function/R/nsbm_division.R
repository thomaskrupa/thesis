#'  nsbm.division
#'
#'@description The nsbm.division function uses the results of the nsbm approach and calculates the
#'efficiency score of each division.
#'
#'@param direction String which contains the orientation of the problem (e.g. "non", "input" or "output").
#'@param slack_plus Matrix which contains the optimal slack plus solution.
#'@param slack_minus Matrix which contains the optimal slack minus solution.
#'@param Input Matrix with the data of the input variables.
#'@param Output Matrix with the data of the output variables.
#'@param Link Matrix with the data of the Link variables.
#'@param Amount_Input Vector containing the number of input variables of each division.
#'@param Amount_Output Vector containing the number of output variables of each division.
#'@param Amount_Link Vector containing the number of link variables between the divisions.
#'@param K Integer which contains the number of division.
#'@param N Integer which contains the total amount of the DMU's.
#'@param sum_m Integer which included the total number of all input variables.
#'@param sum_r Integer which included the total number of all output variables.
#'@param Link_obj Flag which is 0 for no link variable in the objective function 1 otherwise.
#'1 works only in the non-oriented case.
#'
#'@return Return a vector which contains the results of the division efficiency scores.
#'@examples direction = "non"
#'slack_plus <- matrix(c(3,3,3,3,3,3), byrow = TRUE, nrow = 3);
#'slack_minus <- matrix(c(4,4,4,4,4,4), byrow = TRUE, nrow = 3);
#'Input <- matrix(c(1,2,1,2,1,2), byrow = TRUE, nrow = 3);
#'Output <- matrix(c(3,4,3,4,3,4), byrow = TRUE, nrow = 3);
#'Link <- matrix(c(2,2,2), byrow = TRUE, nrow = 3);
#'Amount_Input =  c(1,1);
#'Amount_Output = c(1,1);
#'Amount_Link = c(1);
#'K = 2;
#'N = 3;
#'sum_m = 2;
#'sum_r = 2;
#'Link_obj = 0;
#'nsbm.division(direction, slack_plus, slack_minus, Input, Output, Link,
#'Amount_Input, Amount_Output, Amount_Link, K, N, sum_m, sum_r, Link_obj)
#'@export

nsbm.division <- function(direction, slack_plus, slack_minus, Input, Output, Link, Amount_Input,
                          Amount_Output, Amount_Link, K, N, sum_m, sum_r, Link_obj){

  # Initialization of auxiliary variable and result matrix
  divisionNSBM <- matrix(0,N,K)

  # Two for loops over the divisions K and DMUs N:
  for(i in 1:N){
    Place_1 <- 1;
    Place_2 <- 1;
    Place_3 <- 1; # Link outputs
    Place_4 <- 1; # Link inputs

    for(j in 1:K){
      # Different calculations of the three orientations
      if(direction == "non"){
        # Case where no link variable in the objective function
        if(Link_obj == 0){
          # Check if there exist inputs and outputs for division K. If both exists, go into the first if case.
          # If only input exist go into the second case and if only output exists go into the last case:
          if(Amount_Input[j]>0 & Amount_Output[j]>0){
            counter <- 1-(1/Amount_Input[j])*sum(slack_minus[i,(Place_1:(Place_1+Amount_Input[j]-1))]/
                                                       Input[i,(Place_1:(Place_1+Amount_Input[j]-1))]);
            denominator <-(1+(1/Amount_Output[j])*sum(slack_plus[i,(Place_2:(Place_2+Amount_Output[j]-1))]/
                                                             Output[i,(Place_2:(Place_2+Amount_Output[j]-1))]));
            # Calculation of the division efficiency
            divisionNSBM[i,j] <- counter / denominator;
            Place_1 = Place_1 + Amount_Input[j];
            Place_2 = Place_2 + Amount_Output[j];
          }else if(Amount_Input[j]>0){
            # case where Division j has no outputs
            divisionNSBM[i,j] <- 1-(1/Amount_Input[j])*
              sum(slack_minus[i,(Place_1:(Place_1+Amount_Input[j]-1))]/
                                            Input[i,(Place_1:(Place_1+Amount_Input[j]-1))]);
            Place_1 = Place_1 + Amount_Input[j];
          }else if(Amount_Output[j]>0){
            # Case where division j has no inputs
            divisionNSBM[i,j] <- 1/(1+(1/Amount_Output[j])*
                                      sum(slack_plus[i,(Place_2:(Place_2+Amount_Output[j]-1))]/
                                            Output[i,(Place_2:(Place_2+Amount_Output[j]-1))]));
            Place_2 = Place_2 + Amount_Output[j];
          }
        # Case where link variables are in the objective function
        }else if(Link_obj == 1){
          # Case for the first division:
          if(j == 1){
            # Define the index for the link variable
            l = j;
            # Check if there exist inputs and outputs for division K. If both exists, go into the first if case.
            # If only input exist go into the second case and if only output exists go into the last case:
            if(Amount_Input[j]>0 & (Amount_Output[j]>0 | Amount_Link[l]>0)){
              # counter part can be calculated same for the three parts below:
              counter <- 1- (1/ Amount_Input[j]) * sum(slack_minus[i, (Place_1: (Place_1 + Amount_Input[j]-1))]/
                                                         Input[i, (Place_1: (Place_1 + Amount_Input[j]-1))]);
              # Case only outputs and no Link variable for the first division:
              if(Amount_Link[l] == 0){
                denominator <- (1+(1/Amount_Output[j])*sum(slack_plus[i,(Place_2:(Place_2+Amount_Output[j]-1))]/
                                                            Output[i,(Place_2:(Place_2+Amount_Output[j]-1))]));
                Place_2 = Place_2 + Amount_Output[j];
              # Case with Link variable in the first division
              }else if (Amount_Link[l]> 0){
                # And output variable
                if(Amount_Output[j]>0){
                  den_1 <- sum(slack_plus[i, (Place_2: (Place_2 + Amount_Output[j]-1))]/
                                 Output[i, (Place_2: (Place_2 + Amount_Output[j]-1))]);
                  den_2 <- sum(slack_plus[i, ((Place_3 + sum_r): (Place_3 + sum_r + Amount_Link[l]-1))]/
                                 Link[i, (Place_3: (Place_3 + Amount_Link[l]-1))]);
                  denominator <- (1+ (1/ (Amount_Output[j]+Amount_Link[l])) * (den_1 + den_2));
                  Place_2 = Place_2 + Amount_Output[j];
                  Place_3 = Place_3 + Amount_Link[l];
                # No output variable for the first division
                }else if(Amount_Output[j]==0){
                  den_2 <- sum(slack_plus[i, ((Place_3 + sum_r): (Place_3 + sum_r + Amount_Link[l]-1))]/
                                 Link[i, (Place_3: (Place_3 + Amount_Link[l]-1))]);
                  denominator <- (1+ (1/ (Amount_Output[j]+Amount_Link[l])) * (den_2));
                  Place_3 = Place_3 + Amount_Link[l];
                }
              }
              # Calculation of the division efficiency
              divisionNSBM[i,j] <- counter / denominator;
              Place_1 = Place_1 + Amount_Input[j];
            }else if(Amount_Input[j]>0){
              # case where Division j has no outputs
              divisionNSBM[i,j] <- 1-(1/Amount_Input[j])*
                sum(slack_minus[i,(Place_1: (Place_1 + Amount_Input[j]-1))]/
                                    Input[i, (Place_1: (Place_1 + Amount_Input[j]-1))]);
              Place_1 = Place_1 + Amount_Input[j];
            }else if(Amount_Output[j]>0 | Amount_Link[l]>0){
              if(Amount_Link[l]==0){
                # Case where division j has no inputs
                divisionNSBM[i,j] <- 1/(1+(1/Amount_Output[j])*
                                          sum(slack_plus[i, (Place_2: (Place_2 + Amount_Output[j]-1))]/
                                            Output[i, (Place_2: (Place_2 + Amount_Output[j]-1))]));
                Place_2 = Place_2 + Amount_Output[j];
              }else if(Amount_Link[l]==1){
                # Case where division j has no inputs
                den_1 <- sum(slack_plus[i, (Place_2: (Place_2 + Amount_Output[j]-1))]/
                               Output[i, (Place_2: (Place_2 + Amount_Output[j]-1))]);
                den_2 <- sum(slack_plus[i, (Place_2: (Place_2 + Amount_Output[j]-1))]/
                               Output[i, (Place_2: (Place_2 + Amount_Output[j]-1))]);
                divisionNSBM[i,j] <- 1/ (1+ (1/ Amount_Output[j]) * (den_1 + den_2)) ;
                Place_2 = Place_2 + Amount_Output[j];
                Place_3 = Place_3 + Amount_Link[l];
              }
            }
          # Case for the middle division
          }else if(j >1 & j < K){
            #Define the index for the link variable
            l = j;
            # Check if there exist inputs and outputs for division K. If both exists, go into the first if case.
            # If only input exist go into the second case and if only output exists go into the last case:
            if((Amount_Input[j]>0 | Amount_Link[l-1]>0) & (Amount_Output[j]>0 | Amount_Link[l]>0)){
              # No Links as input variable
              if(Amount_Link[l-1] == 0){
                counter <- 1-(1/Amount_Input[j])*sum(slack_minus[i,(Place_1:(Place_1+Amount_Input[j]-1))]/
                                                        Input[i,(Place_1:(Place_1+Amount_Input[j]-1))]);
                Place_1 <- Place_1 + Amount_Input[j];
              # Link as input variable
              }else if(Amount_Link[l-1]>0){
                # Case where inputs and links exist
                if(Amount_Input[j] >0){
                  count_1 <- sum(slack_minus[i, (Place_1: (Place_1 + Amount_Input[j]-1))]/
                                   Input[i, (Place_1: (Place_1 + Amount_Input[j]-1))])
                  count_2 <- sum(slack_minus[i, ((Place_4 + sum_m): (Place_4 + sum_m + Amount_Link[l-1]-1))]/
                                   Link[i, (Place_4: (Place_4 + Amount_Link[l-1]-1))])
                  counter <- 1- (1/ (Amount_Input[j] + Amount_Link[l-1])) *(count_1 + count_2);
                  Place_1 <- Place_1 + Amount_Input[j];
                  Place_4 <- Place_4 + Amount_Link[l-1];
                # Case where links exist and no inputs
                }else if(Amount_Input[j] == 0){
                  count_2 <- sum(slack_minus[i, ((Place_4 + sum_m): (Place_4 + sum_m + Amount_Link[l-1]-1))]/
                                   Link[i, (Place_4: (Place_4 + Amount_Link[l-1]-1))])
                  counter <- 1- (1/ (Amount_Link[l-1])) *(count_2);
                  Place_4 <- Place_4 + Amount_Link[l-1];
                }
              }
              # No Links as output variable
              if(Amount_Link[l] == 0){
                denominator <- (1+(1/Amount_Output[j])*sum(slack_plus[i,(Place_2:(Place_2+Amount_Output[j]-1))]/
                                                            Output[i,(Place_2:(Place_2+Amount_Output[j]-1))]));
                Place_2 = Place_2 + Amount_Output[j];
              }else if(Amount_Link[l] > 0){
                # Case where Outputs and links exist
                if(Amount_Output[j] >0){
                  den_1 <- sum(slack_plus[i, (Place_2: (Place_2 + Amount_Output[j]-1))]/
                                 Output[i, (Place_2: (Place_2 + Amount_Output[j]-1))])
                  den_2 <- sum(slack_plus[i, ((Place_3 + sum_r): (Place_3 + sum_r + Amount_Link[l]-1))]/
                                 Link[i, (Place_3: (Place_3 + Amount_Link[l]-1))])
                  denominator <- (1+ (1/ (Amount_Output[j] + Amount_Link[l])) *(den_1 + den_2));
                  Place_2 = Place_2 + Amount_Output[j];
                  Place_3 <- Place_3 + Amount_Link[l];
                # Case where only Links exist
                }else if(Amount_Output[j] == 0){
                  den_2 <- sum(slack_plus[i, ((Place_3 + sum_r): (Place_3 + sum_r + Amount_Link[l]-1))]/
                                 Link[i, (Place_3: (Place_3 + Amount_Link[l]-1))])
                  denominator <- (1+ (1/ Amount_Link[l]) *(den_2));
                  Place_3 <- Place_3 + Amount_Link[l];
                }
              }
              # Calculation of the division efficiency
              divisionNSBM[i,j] <- counter / denominator;
            # case where the middle division j has no outputs
            }else if(Amount_Input[j]>0 | Amount_Link[l-1]>0) {
              # No Links as input variable
              if(Amount_Link[l-1] == 0){
                # Calculation of the division efficiency
                divisionNSBM[i,j] <- 1-(1/Amount_Input[j])*
                  sum(slack_minus[i,(Place_1:(Place_1+Amount_Input[j]-1))]/
                                        Input[i,(Place_1:(Place_1+Amount_Input[j]-1))]);
                Place_1 <- Place_1 + Amount_Input[j];
                # Link as input variable
              }else if(Amount_Link[l-1]>0){
                # Case where inputs and links exist
                if(Amount_Input[j] >0){
                  count_1 <- sum(slack_minus[i, (Place_1: (Place_1 + Amount_Input[j]-1))]/
                                   Input[i, (Place_1: (Place_1 + Amount_Input[j]-1))])
                  count_2 <- sum(slack_minus[i, ((Place_4 + sum_m): (Place_4 + sum_m + Amount_Link[l-1]-1))]/
                                   Link[i, (Place_4: (Place_4 + Amount_Link[l-1]-1))])
                  # Calculation of the division efficiency
                  divisionNSBM[i,j] <- 1- (1/ (Amount_Input[j] + Amount_Link[l-1])) *(count_1 + count_2);
                  Place_1 <- Place_1 + Amount_Input[j];
                  Place_4 <- Place_4 + Amount_Link[l-1];
                  # Case where links exist and no inputs
                }else if(Amount_Input[j] == 0){
                  count_2 <- sum(slack_minus[i, ((Place_4 + sum_m): (Place_4 + sum_m + Amount_Link[l-1]-1))]/
                                   Link[i, (Place_4: (Place_4 + Amount_Link[l-1]-1))])
                  # Calculation of the division efficiency
                  divisionNSBM[i,j]<- 1- (1/ (Amount_Link[l-1])) *(count_2);
                  Place_4 <- Place_4 + Amount_Link[l-1];
                }
              }
            # Case where division j has no inputs
            }else if(Amount_Output[j]>0 | Amount_Link[l]>0){
              # No Links as output variable
              if(Amount_Link[l] == 0){
                # Case where division j has no inputs
                divisionNSBM[i,j] <- 1/ (1+ (1/ Amount_Output[j])*
                                           sum(slack_plus[i, (Place_2: (Place_2 + Amount_Output[j]-1))]/
                                            Output[i, (Place_2: (Place_2 + Amount_Output[j]-1))]));
                Place_2 = Place_2 + Amount_Output[j];
              }else if(Amount_Link[l] > 0){
                # Case where Outputs and links exist
                if(Amount_Output[j] >0){
                  den_1 <- sum(slack_plus[i, (Place_2: (Place_2 + Amount_Output[j]-1))]/
                                 Output[i, (Place_2: (Place_2 + Amount_Output[j]-1))])
                  den_2 <- sum(slack_plus[i, ((Place_3 + sum_r): (Place_3 + sum_r + Amount_Link[l]-1))]/
                                 Link[i, (Place_3: (Place_3 + Amount_Link[l]-1))])
                  # Case where division j has no inputs
                  divisionNSBM[i,j] <- 1/ (1+ (1/ (Amount_Output[j] + Amount_Link[l])) *(den_1 + den_2));
                  Place_2 = Place_2 + Amount_Output[j];
                  Place_3 <- Place_3 + Amount_Link[l];
                  # Case where only Links exist
                }else if(Amount_Output[j] == 0){
                  den_2 <- sum(slack_plus[i, ((Place_3 + sum_r): (Place_3 + sum_r + Amount_Link[l]-1))]/
                                 Link[i, (Place_3: (Place_3 + Amount_Link[l]-1))])
                  # Case where division j has no inputs
                  divisionNSBM[i,j] <- 1/ (1+ (1/ Amount_Link[l]) *(den_2));
                  Place_3 <- Place_3 + Amount_Link[l];
                }
              }
            }
          # Case for the end division
          }else if(j == K){
            # Define the index for the link variable
            l = j;
            # Check if there exist inputs and outputs for division K. If both exists, go into the first if case.
            # If only input exist go into the second case and if only output exists go into the last case:
            if((Amount_Input[j]>0 | Amount_Link[l-1])  & Amount_Output[j]>0){
            denominator <- (1+(1/Amount_Output[j])*sum(slack_plus[i,(Place_2:(Place_2+Amount_Output[j]-1))]/
                                                             Output[i,(Place_2:(Place_2+Amount_Output[j]-1))]));
            Place_2 = Place_2 + Amount_Output[j];
            if(Amount_Link[l-1]==0){
              counter <- 1- (1/ Amount_Input[j]) * sum(slack_minus[i, (Place_1: (Place_1 + Amount_Input[j]-1))]/
                                                         Input[i, (Place_1: (Place_1 + Amount_Input[j]-1))]);
              Place_1 = Place_1 + Amount_Input[j];
            }else if(Amount_Link[l-1]>0){
              if(Amount_Input[j]>0){
                count_1 <- sum(slack_minus[i, (Place_1: (Place_1 + Amount_Input[j]-1))]/
                                 Input[i, (Place_1: (Place_1 + Amount_Input[j]-1))]);
                count_2 <- sum(slack_minus[i, ((Place_4 + sum_m): (Place_4 + sum_m + Amount_Link[l-1]-1))]/
                                 Link[i, (Place_4: (Place_4 + Amount_Link[l-1]-1))]);
                counter <- 1- (1/ (Amount_Input[j]+Amount_Link[l-1])) *(count_1 + count_2);
                Place_1 = Place_1 + Amount_Input[j];
                Place_4 = Place_4 + Amount_Link[l-1];
              }else if (Amount_Input[j]==0){
                count_2 <- sum(slack_minus[i, ((Place_4 + sum_m): (Place_4 + sum_m + Amount_Link[l-1]-1))]/
                                 Link[i, (Place_4: (Place_4 + Amount_Link[l-1]-1))]);
                counter <- 1- (1/ Amount_Link[l-1]) *(count_2);
                Place_4 = Place_4 + Amount_Link[l-1];
              }
            }
           }
            # Calculation of the division efficiency
            divisionNSBM[i,j] <- counter / denominator;
        }
}
      # Input-oriented problem:
      }else if(direction == "input"){
        # Check if division K has inputs. If yes, calculate the input division efficiency
        # and save it into divisionNSBM:
        if(Amount_Input[j]>0){
          # Calculation of the division efficiency
          divisionNSBM[i,j] <- 1- (1/ Amount_Input[j])
          *sum(slack_minus[i,(Place_1:(Place_1+Amount_Input[j]-1))]/
                                  Input[i, (Place_1: (Place_1 + Amount_Input[j]-1))]);
          Place_1 = Place_1 + Amount_Input[j];
        }
        # Output-oriented problem:
      }else if(direction == "output"){
        # Check if division K has outputs. If yes, calculate the output division efficiency
        #and save it into divisionNSBM:
        if(Amount_Output[j]>0){
          divisionNSBM[i,j] = 1/(1+(1/Amount_Output[j])*
                                   sum(slack_plus[i, (Place_2: (Place_2 + Amount_Output[j]-1))]/
                                   Output[i, (Place_2: (Place_2 + Amount_Output[j]-1))]));
          Place_2 = Place_2 + Amount_Output[j];
        }
      }
    }
  }
  # Return the results of the division efficiency calculation:
  return(divisionNSBM);
}
