#' data.split
#'
#' @description The data.split function split the Data matrix into the input, output and link vector
#'
#' @param Data Matrix which contains all the data of the problem.
#' @param Amount Vector containing the number of inputs and outputs variables of each division and
#' the number of link variables.
#' @param K Integer which contains the number of division.
#' @param N Integer which includes the number of DMUs.
#'
#' @return Return a list
#' \itemize{
#'     \item In - Matrix with the data of input variables.
#'     \item Out - Matrix with the data of output variables.
#'     \item Link - Matrix with the data of the Link variables.
#' }
#' @examples Data <- matrix(c(3, 5, 8, 1, 2, 7, 1, 4, 5, 8,2, 5, 3, 2, 1),byrow=TRUE, nrow=3)
#' Amount = c(1,1,1,1,1)
#' K = 2
#' N = 3
#' data.split(Data, Amount, K, N)
#' @export

data.split <- function(Data, Amount, K, N){

  ###########################################
  # Initialization
  ###########################################

  Count =1;
  Place = 1;
  Help_in = 0;
  Help_out = 0;

  # For loop over the K divisions
  for(i in 1:K){
    # Filling the Input vector:
    # First the Amount vector is used to check if there is an input vector for the respective division.
    # Additionally there are two cases which are the first time one finds an input vector and
    # the next time one distinguishes between them ("Help_in" is the responsible variable)
    if(Amount[Count]>0 && Help_in == 0){
      Input <- matrix(0,N,1);
      Input <- Data[,(Place):(Place + Amount[Count] -1)];
      Place = Place + Amount[Count];
      Count = Count +1;
      Help_in = 1;
    # Case of founding second Input vector
    }else if(Amount[Count]>0 && Help_in >0){
      var <- Data[,(Place):(Place + Amount[Count] -1)];
      Input <- cbind(Input, var);
      Place = Place + Amount[Count];
      Count = Count +1;
    # Case when there is no Input vector for division K
    }else{
      Count = Count +1;
    }

    # Filling the Output vector:
    # First the Amount vector is used to check if there is an output vector for the respective division.
    # Additionally there are two cases which are the first time one finds an output vector and
    # the next time one distinguishes between them ("Help_out" is the responsible variable)
    if(Amount[Count]>0 && Help_out == 0){
      Output <- matrix(0,N,1);
      Output <- Data[,(Place):(Place + Amount[Count] -1)];
      Place = Place + Amount[Count];
      Count = Count +1;
      Help_out = 1;
    # Second or more time found an Output vector
    }else if(Amount[Count]>0 && Help_out >0){
      var <- Data[,(Place):(Place + Amount[Count] -1)];
      Output <- cbind(Output, var);
      Place = Place + Amount[Count];
      Count = Count +1;
    # Case: No Output vector for this division
    }else{
      Count = Count +1;
    }
  }

# Filling the Link vector
if(Place <= (dim(Data)[2])){
 Link <- Data[,(Place):(dim(Data)[2])];
 # The variables are transposed twice to avoid problems with dimensions
 Input = t(t(Input));
 Output = t(t(Output));
 Link = t(t(Link));
}else{
  Link <- NULL;
  # The variables are transposed twice to avoid problems with dimensions
  Input = t(t(Input));
  Output = t(t(Output));
}
  # Save the results in a list results and outputs the results
  results <- list( "In" = Input, "Out" = Output, "Link" = Link)
  return(results)
}
