#' negative_zero_values
#'
#' @description The negative_zero_values function adjust negative or zero output values to use them
#' in the nsbm approach calculation. This function runs only in the non- and output-oriented case.
#'
#' @param Output_vec Vector which contain one column of the Output matrix. This vector includes zero/s
#' or negative values.
#' @param N Integer which contains the total amount of the DMU's.
#'
#' @return Return the adjusted output vector with only positive values.
#' @examples Output_vec = matrix(c(3,2,1,0,-1,-2,-3), byrow=TRUE, nrow=7)
#' N = 7
#'negative_zero_value(Output_vec, N)
#'@export

negative_zero_value <- function(Output_vec, N){

# Initialization of auxiliary variable
Help <- 0;
# If all values in Output_vec are negative than set y_head = y_bottom = 1
# else find max and min of positive values
if(sort(Output_vec)[N] <= 0){
  y_head <- 1;
  y_bottom <- 1;
  # Initialization of large number B
  B = 100*N;
  # Identification of this case by increasing the Help variable
  Help <- Help + 1;
}else{
  # Estimate y_head and y_bottom
  y_head <- max(subset(Output_vec, Output_vec >0));
  y_bottom <- min(subset(Output_vec, Output_vec >0));
  # If y_head is equal to y_bottom:
  if(y_head == y_bottom){
    # Initialization of large number B
    B = 100*N;
    # Identification of this case by increasing the Help variable
    Help <- Help + 1;
  }
}

for(i in 1:N){
  # If the Output value is smaller or equal to zero and y_head > y_bottom:
  if(Output_vec[i] <=0 & Help == 0){
    Output_vec[i] <- (y_bottom * (y_head - y_bottom)) / (y_head - Output_vec[i]);
  # If Output value is smaller or equal to zero and y_head = y_bottom:
  }else if(Output_vec[i] <=0 & Help == 1){
    Output_vec[i] <- (y_bottom)^2 / (B*(y_head - Output_vec[i]));
  }
}
# Return the adjusted Output_vec
return(Output_vec)
}
