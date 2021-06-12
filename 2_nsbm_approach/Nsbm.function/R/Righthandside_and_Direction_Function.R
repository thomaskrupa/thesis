#' Righthandside_and_Direction
#'
#' @description The Righthandside_and_Direction function creates the righthandside (f.rhs)
#' and the direction (f.dir) for the nsbm approach.
#'
#' @param i_loop Integer containing the current DMU where the NSBM approach is.
#' @param direction String which contains the orientation of the problem (e.g. "non", "input" or "output").
#' @param return_to_scale String which contains the return to scale variant (e.g. "CRS" or "VRS").
#' @param link_con Integer which contains the information of the link constraint variant
#' (e.g. 1 for fix and 2 for free constraints).
#' @param NIRS Flag which is 0 for the normal calculation and 1 for the Non-increasing return to scale
#' (NIRS) calculation.
#' @param Input Matrix with the data of the input variables.
#' @param Output Matrix with the data of the output variables.
#' @param Link Matrix with the data of the link variables.
#' @param Amount_Link Vector containing the number of link variables between the divisions.
#' @param K Integer which contains the number of division.
#' @param sum_m Integer which included the total number of all input variables.
#' @param sum_r Integer which included the total number of all output variables.
#' @param sum_l Integer which included the total number of all link variables.
#'
#' @return Returns a list
#' \itemize{
#'      \item FDIR - Matrix which contains the sign for the nsbm approach.
#'      \item FRHS - Matrix which contains the right hand side of the nsbm approach.
#' }
#' @examples i = 1
#' direction = "non"
#' return_to_scale = "VRS"
#' link_con = 1
#' NIRS = 0
#' Input = matrix(c(3, 8,7, 4,2, 3), byrow = TRUE, nrow = 3)
#' Output = matrix(c(5, 1,1, 5,5, 2), byrow = TRUE, nrow = 3)
#' Link = matrix(c(2,8,1), byrow = TRUE, nrow = 3)
#' Amount_L = c(1)
#' K = 2
#' sum_m = 2
#' sum_r = 2
#' sum_l = 1
#' Righthandside_and_Direction(i, direction, return_to_scale, link_con, NIRS, Input,
#' Output, Link, Amount_L, K, sum_m, sum_r, sum_l)
#' @export

Righthandside_and_Direction <- function(i_loop, direction, return_to_scale, link_con, NIRS, Input, Output,
                                        Link, Amount_Link, K, sum_m, sum_r, sum_l){

  # This function calculates the right hand side and sign for the nsbm approach. The first approach is
  # distinguished between the nsbm (K>1) and the sbm (K=1). Then the right hand side is stored in f.rhs
  # and the direction in f.dir for each case. The function can distinguish between the different orientation
  # ("non", "input" and "output"), return to scale variants ("VRS" and "CRS"), the different link variants
  # (1 for fix and 2 for free) and non-increasing return to scale (NIRS) calculation ( flag equals 1).

  # Calculation for the NSBM approach:
  if(K>1){
    # Non-oriented direction:
    if(direction == "non"){
      # VRS case
      if(return_to_scale == "VRS"){
        # link_con fix
        if(link_con == 1){
          # NIRS equal zero is the standard case with direction equal to "=" in the VRS constraint
          if(NIRS == 0){
            # Non-oriented direction with VRS and fix link_con (NIRS == 0)
            f.dir <- c("=", rep("=", 1, sum_m), rep("=", 1, sum_r), rep("=", 1, K), rep("=", 1, (2*sum_l)), ">");
            f.rhs <- c(1, rep(0,1,sum_m), rep(0,1,sum_r), rep(0, 1, K), rep(0, 1, (2*sum_l)), 0);
          }else if(NIRS == 1){
            # Non-oriented direction with VRS and fix link_con (NIRS == 1, VRS constraint has sign "<=")
            f.dir <- c("=", rep("=", 1, sum_m), rep("=", 1, sum_r), rep("<=", 1, K), rep("=", 1, (2*sum_l)), ">");
            f.rhs <- c(1, rep(0,1,sum_m), rep(0,1,sum_r), rep(0, 1, K), rep(0, 1, (2*sum_l)), 0);
          }
        # link_con =2
        }else{
          # NIRS equal zero is the standard case with direction equal to "=" in the VRS constraint
          if(NIRS == 0){
            # Non-oriented direction with VRS and free link_con (NIRS == 0)
            f.dir <- c("=", rep("=", 1, sum_m), rep("=", 1, sum_r), rep("=", 1, K), rep("=", 1, sum_l), ">");
            f.rhs <- c(1, rep(0,1,sum_m), rep(0,1,sum_r), rep(0, 1, K), rep(0, 1, sum_l), 0);
          }else if(NIRS == 1){
            # Non-oriented direction with VRS and free link_con (NIRS == 1, VRS constraint has sign "<=")
            f.dir <- c("=", rep("=", 1, sum_m), rep("=", 1, sum_r), rep("<=", 1, K), rep("=", 1, sum_l), ">");
            f.rhs <- c(1, rep(0,1,sum_m), rep(0,1,sum_r), rep(0, 1, K), rep(0, 1, sum_l), 0);
          }
        }
      }else{
        # Case return to scale == "CRS"
        if(link_con == 1){
          # Non-oriented direction with CRS and fix link_con
          f.dir <- c("=", rep("=", 1, sum_m), rep("=", 1, sum_r), rep("=", 1, (2*sum_l)), ">");
          f.rhs <- c(1, rep(0,1,sum_m), rep(0,1,sum_r),  rep(0, 1, (2*sum_l)), 0);
        }else{
          # Non-oriented direction with CRS and free link_con
          f.dir <- c("=", rep("=", 1, sum_m), rep("=", 1, sum_r), rep("=", 1, sum_l), ">");
          f.rhs <- c(1, rep(0,1,sum_m), rep(0,1,sum_r), rep(0, 1, sum_l), 0);
        }
      }
      # Case: Input and Output direction
    }else{
      # Case VRS return to scale
      if(return_to_scale == "VRS"){
        if(link_con == 1){
          # NIRS equal zero is the standard case with direction equal to "=" in the VRS constraint
          if(NIRS == 0){
            # Input or Output-oriented direction with VRS and fix link_con (NIRS == 0)
            f.dir <- c(rep("=", 1, sum_m), rep("=", 1, sum_r), rep("=", 1, K), rep("=", 1, (2*sum_l)));
          }else if(NIRS == 1){
            # Input or Output-oriented direction with VRS and fix link_con
            # (NIRS == 1, VRS constraint has sign "<=")
            f.dir <- c(rep("=", 1, sum_m), rep("=", 1, sum_r), rep("<=", 1, K), rep("=", 1, (2*sum_l)));
          }
          # Auxiliary variables
          Place_l<- 1;
          Help_l <- 1;
          # Calculation of the first column of Link vector
          for(i in 1:(K-1)){
            # Another auxiliary variable
            Number <- i;
            # create the vectors only if Link variable exists
            if((Amount_Link[i]>0)){
              aux_var12<- Link[i_loop, (Place_l: (Place_l + Amount_Link[Number]-1))];
              #The variable is transposed twice to avoid problems with dimensions
              aux_var12 <- t(t(aux_var12));
              aux_var1 <- aux_var12;
              if(Help_l == 1){
                aux_var2 <- c(rep(aux_var1,2));
                Help_l = Help_l +1;
              }else{
                aux_var2 <- c(aux_var2, rep(aux_var1,2));
              }
            }
            Place_l <- Place_l + Amount_Link[Number];
          }
          # If this problem has link variables or not
          if(sum_l >0){
            f.rhs <- c( Input[i_loop,], Output[i_loop,], rep(1, 1, K), aux_var2);
          }else{
            f.rhs <- c( Input[i_loop,], Output[i_loop,], rep(1, 1, K));
          }
        }else{
          # NIRS equal zero is the standard case with direction equal to "=" in the VRS constraint
          if(NIRS == 0){
            # Input or Output-oriented direction with VRS and free link_con (NIRS == 0)
            f.dir <- c(rep("=", 1, sum_m), rep("=", 1, sum_r), rep("=", 1, K), rep("=", 1, sum_l));
            f.rhs <- c(Input[i_loop,], Output[i_loop,], rep(1, 1, K), rep(0, 1, sum_l));
          }else if(NIRS == 1){
            # Input or Output-oriented direction with VRS and free link_con
            # (NIRS == 1, VRS constraint has sign "<=")
            f.dir <- c(rep("=", 1, sum_m), rep("=", 1, sum_r), rep("<=", 1, K), rep("=", 1, sum_l));
            f.rhs <- c(Input[i_loop,], Output[i_loop,], rep(1, 1, K), rep(0, 1, sum_l));
          }
        }
        # CRS return to scale
      }else{
        if(link_con == 1){
          # Input or Output-oriented direction with CRS and fix link_con
          f.dir <- c(rep("=", 1, sum_m), rep("=", 1, sum_r), rep("=", 1, (2*sum_l)));
          # Auxiliary variables
          Place_l<- 1;
          Help_l <- 1;
          # Calculation of the first column of Link vector
          for(i in 1:(K-1)){
            #Another auxiliary variable
            Number <- i;
            # Only go into this part when LInk variable exists
            if((Amount_Link[i]>0)){
              aux_var12<- Link[i_loop, (Place_l: (Place_l + Amount_Link[Number]-1))];
              #The variable is transposed twice to avoid problems with dimensions
              aux_var12 <- t(t(aux_var12));
              aux_var1 <- aux_var12;
              if(Help_l == 1){
                aux_var2 <- c(rep(aux_var1,2));
                Help_l = Help_l +1;
              }else{
                aux_var2 <- c(aux_var2, rep(aux_var1,2));
              }
            }
            Place_l <- Place_l + Amount_Link[Number];
          }
          # If the problem has link variables or not
          if(sum_l >0){
            f.rhs <- c(Input[i_loop,], Output[i_loop,],  aux_var2);
          }else{
            f.rhs <- c(Input[i_loop,], Output[i_loop,]);
          }
        }else{
          # Input or Output-oriented direction with CRS and free link_con
          f.dir <- c(rep("=", 1, sum_m), rep("=", 1, sum_r),  rep("=", 1, sum_l));
          f.rhs <- c(Input[i_loop,], Output[i_loop,], rep(0, 1, sum_l));
        }
      }
    }
  }else{
  # SBM approach case:
    # Non-oriented direction:
    if(direction == "non"){
      # VRS case
      if(return_to_scale == "VRS"){
        # NIRS equal zero is the standard case with direction equal to "=" in the VRS constraint
        if(NIRS == 0){
          # Non-oriented direction with VRS and free link_con (NIRS == 0)
          f.dir <- c("=", rep("=", 1, sum_m), rep("=", 1, sum_r), rep("=", 1, K), ">");
          f.rhs <- c(1, rep(0,1,sum_m), rep(0,1,sum_r), rep(0, 1, K), 0);
        }else if(NIRS == 1){
          # Non-oriented direction with VRS and free link_con
          # (NIRS == 1, VRS constraint has sign "<=")
          f.dir <- c("=", rep("=", 1, sum_m), rep("=", 1, sum_r), rep("<=", 1, K), ">");
          f.rhs <- c(1, rep(0,1,sum_m), rep(0,1,sum_r), rep(0, 1, K), 0);
        }
      }else{
        # SBM Non-oriented direction with CRS
        f.dir <- c("=", rep("=", 1, sum_m), rep("=", 1, sum_r), ">");
        f.rhs <- c(1, rep(0,1,sum_m), rep(0,1,sum_r), 0);
      }
      # Case: Input and Output direction
    }else{
      # Case VRS return to scale
      if(return_to_scale == "VRS"){
        # NIRS equal zero is the standard case with direction equal to "=" in the VRS constraint
        if(NIRS == 0){
          # Input or Output-oriented direction with VRS (NIRS == 0)
          f.dir <- c(rep("=", 1, sum_m), rep("=", 1, sum_r), rep("=", 1, K));
          f.rhs <- c(Input[i_loop,], Output[i_loop,], rep(1, 1, K));
        }else if(NIRS == 1){
          # Input or Output-oriented direction with VRS (NIRS == 1, VRS constraint has sign "<=")
          f.dir <- c(rep("=", 1, sum_m), rep("=", 1, sum_r), rep("<=", 1, K));
          f.rhs <- c(Input[i_loop,], Output[i_loop,], rep(1, 1, K));
        }
      }else{
        # Input or Output-oriented direction with CRS
        f.dir <- c(rep("=", 1, sum_m), rep("=", 1, sum_r));
        f.rhs <- c(Input[i_loop,], Output[i_loop,]);
      }
    }
  }
  # Save the results of f.dir and f.rhs in the variable "results" and output these
  results <- list("FDIR" = f.dir, "FRHS" = f.rhs)
  return(results)
}
