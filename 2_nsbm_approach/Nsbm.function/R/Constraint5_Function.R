#' Constraint5
#'
#' @description The Constraint5 function creates the constraint for the link variables
#'
#' @param i_loop Integer containing the current DMU where the NSBM approach is.
#' @param direction String which contains the orientation of the problem (e.g. "non", "input" or "output").
#' @param link_con Integer which contains the information of the link constraint variant
#' (e.g. 1 for fix and 2 for free constraints).
#' @param Link Matrix with the data of the Link variables.
#' @param Amount_Link Vector containing the number of link variables between the divisions.
#' @param K Integer which contains the number of division.
#' @param N Integer which contains the total amount of the DMU's.
#' @param sum_m Integer which included the total number of all input variables.
#' @param sum_r Integer which included the total number of all output variables.
#' @param sum_l Integer which included the total number of all link variables.
#' @param Link_obj Flag which is 0 for no link variable in the objective function 1 otherwise.
#'
#' @return Return a matrix with the constraints for the link variables (fix or free variant).
#' @examples i_loop = 1
#' direction = "non"
#' link_con = 1
#' Link = matrix(c(2,8,1), byrow = TRUE, nrow = 3)
#' Amount_Link = c(1)
#' K = 2
#' N = 3
#' sum_m = 2
#' sum_r = 2
#' sum_l = 1
#' Link_obj = 0
#' Constraint5(i_loop, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj)
#' @export

Constraint5 <- function(i_loop, direction, link_con, Link, Amount_Link, K, N, sum_m, sum_r, sum_l, Link_obj){

  # Initialization of auxiliary variables
  Place <- 1;
  Help_1 <- 1;
  Help_5 <- 1;
  # First "fixed" link constraint
  if(link_con ==1){
    # For loop over the K divisions
    for(i in 1:(K-1)){
      # Initialization of auxiliary variables
      Number <- i;
      Help_2 <- 1;
      if((Amount_Link[i]>0)){
        # Non-oriented problem:
        if(direction == "non"){
          aux_var12<- -Link[i_loop, (Place: (Place + Amount_Link[Number]-1))];
          aux_var12 <- t(t(aux_var12));
          aux_var1 <- aux_var12;
        }
        # If i>1 creates zeros matrices for the link constraint
        if(i>1){
          for(j in 1:(i-1)){
            aux_var2 <- matrix(0, Amount_Link[Number], N);
            # Non-oriented problem:
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
        # Save the link values into aux_var3 and transform them
        aux_var3 <- Link[,Place: (Place + Amount_Link[Number]-1)];
        aux_var3 <- t(aux_var3);
        # Non-oriented problem:
        if(direction == "non"){
          aux_var <- cbind(aux_var1, aux_var3);
        }else{
          # Input and output-oriented case
          if(Help_2 == 1){
            aux_var <- aux_var3;
          }else{
            aux_var <- cbind(aux_var1, aux_var3);
          }
        }
        # If i is smaller than K, creates a zeros matrix again:
        if(i<K){
          for(k in (i+1):K){
            aux_var3 <- matrix(0, Amount_Link[Number], N);
            aux_var <- cbind(aux_var, aux_var3);
          }
        }
        if(Help_1 ==1){
          res <- aux_var;
          Help_1 <- Help_1 + 1;
        }else{
          res <- rbind(res, aux_var);
        }

        # If Link variable are in the objective function
        if(Link_obj == 1){
          # Initialization of auxiliary variables
          Help_3 <- 1;
          Help_4 <- 1;
          # If i>1 creates zeros matrices for the link constraint
          if(i>1){
            for(j in 1:(i-1)){
              aux_var2 <- matrix(0,Amount_Link[j+1],Amount_Link[j]);
              if(Help_3 == 1){
                aux_var <- aux_var2;
                Help_3 <- Help_3 +1;
              }else{
                aux_var <- cbind(aux_var, aux_var2);
              }
            }
          }
          # Middle part :
          aux_var3 <- diag(-1,Amount_Link[Number],Amount_Link[Number]);
          aux_var3 <- t(aux_var3);
          if(Help_3 == 1){
            aux_var <- aux_var3;
            Help_3 <- Help_3 +1;
          }else{
            aux_var <- cbind(aux_var, aux_var3);
          }
          # If i is smaller than K, creates a zeros matrix again:
          if(i<(K-1)){
            for(k in (i+1):(K-1)){
              aux_var3 <- matrix(0, Amount_Link[k-1], Amount_Link[k]);
              aux_var <- cbind(aux_var, aux_var3);
            }
          }
          if(direction == "non"){
            # Safe the solution for the next calculation
            aux_var_link <- aux_var * (-1);
            # combine the zero matrix with the solution before
            aux_var3 <- matrix(0, Amount_Link[i], sum_l);
            aux_var_link2 <- aux_var3;
            aux_var <- cbind(aux_var, aux_var3);
          }
          if(Help_5 == 1){
            # Combine all Link_obj calculation together and save them in res_1
            res_1 <- aux_var;
            Help_5 <- Help_5 +1;
          }else{
            res_1 <- rbind(res_1, aux_var);
          }
        }
        # Initialization of auxiliary variables
        i_1 = i + 1;
        Help_3 <- 1;
        # Non-oriented problem:
        if(direction == "non"){
          aux_var1 <- aux_var12;
        }
        if((i_1)>1){
          for(j in 1:((i_1) - 1)){
            aux_var2 <- matrix(0, Amount_Link[Number], N);
            # Non-oriented problem
            if(direction == "non"){
              aux_var1 <- cbind(aux_var1, aux_var2);
            }else{
              # Input and output-oriented problem distinguish
              # between first time round with j =1 and else
              if(j==1){
                aux_var1 <- aux_var2;
                Help_3 <- Help_3 +1;
              }else{
                aux_var1 <- cbind(aux_var1, aux_var2);
              }
            }
          }
        }
        aux_var3 <- Link[,Place: (Place + Amount_Link[Number]-1)];
        aux_var3 <- t(aux_var3);
        if(direction == "non"){
          aux_var <- cbind(aux_var1, aux_var3);
        }else{
          # Input or output-oriented case. Distinguish between first loop and else:
          if(Help_3 == 1){
            aux_var <- aux_var3
          }else{
            aux_var <- cbind(aux_var1, aux_var3);
          }
        }
        if((i_1)< K){
          for(k in ((i_1) +1):K){
            aux_var3 <- matrix(0, Amount_Link[Number], N);
            aux_var <- cbind(aux_var, aux_var3);
          }
        }
        #
        if(Link_obj == 1){
          if(direction == "non"){
            aux_var5 <- cbind(aux_var_link2, aux_var_link);
            res_1 <- rbind(res_1, aux_var5);
          }
        }
        res <- rbind(res, aux_var)
      }
      Place <- Place + Amount_Link[Number];
    }
    if(Link_obj == 0){
      # Combine the results with some zeros matrices
      res <- cbind(res, matrix(0, (2*sum_l), sum_r), matrix(0, (2*sum_l), sum_m));
    }else if(Link_obj == 1){
      # Combine the results with some zeros matrices
      res <- cbind(res, matrix(0, (2*sum_l), sum_r), matrix(0, (2*sum_l), sum_m), res_1);
    }
  }else{
    #Second "free" link constraint
    Place <- 1;
    Help <- 1;
    for(i in 1:(K-1)){
      # Initialization of auxiliary variables
      Help_2 <- 1;
      Number <- i;
      if((Amount_Link[i]>0)){
        # Only for the non-oriented case
        if(direction == "non"){
          aux_var1 <- matrix(0, 1, Amount_Link[i]);
          aux_var1 <- t(aux_var1);
        }
        if(i>1){
          for(j in 1:(i-1)){
            aux_var2 <- matrix(0, Amount_Link[Number], N);
            if(direction == "non"){
              aux_var1 <- cbind(aux_var1, aux_var2);
            }else{
              #input or output-oriented problem
              #distinguish between first round with j =1 and else
              if(j==1){
                aux_var1 <- aux_var2;
                Help_2 = Help_2 + 1;
              }else{
                aux_var1 <- cbind(aux_var1, aux_var2);
              }
            }
          }
        }
        aux_var3 <- Link[,Place: (Place + Amount_Link[Number]-1)];
        aux_var3 <- t(aux_var3);
        # Non-oriented problem:
        if(direction == "non"){
          aux_var <- cbind(aux_var1, - aux_var3, aux_var3);
        }else{
          # Input and output-oriented case
          if(Help_2 == 1){
            aux_var <- cbind(- aux_var3, aux_var3);
          }else{
            aux_var <- cbind(aux_var1, - aux_var3, aux_var3);
          }
        }
        if(i<(K-1)){
          for(k in (i+1):(K-1)){
            aux_var3 <- matrix(0, Amount_Link[Number], N);
            aux_var <- cbind(aux_var, aux_var3);
          }
        }
        # Combine the results and distinguish between the first round of the loop and otherwise
        if(Help_1 ==1){
          res <- aux_var;
          Help_1 <- Help_1 + 1;
        }else{
          res <- rbind(res, aux_var);
        }
      }
      Place <- Place + Amount_Link[Number];
    }
    # Save the results into the variable res:
    res <- cbind(res, matrix(0, sum_l, sum_r), matrix(0, sum_l, sum_m));
    # If Link variable are in the objective function
    if(Link_obj == 1){
      # Distinguish between non-oriented case and the other two orientation
      if(direction == "non"){
        aux_var5 <- matrix(0,sum_l, (2*sum_l))
        res <- cbind(res, aux_var5);
      }else{
        aux_var5 <- matrix(0,sum_l, (sum_l))
        res <- cbind(res, aux_var5);
      }
    }
  }
  # return the variable res
  return(res);
}
