#' t_constraint
#'
#' @description The t_constraint function creates a matrix which contains the constraint that t>0.
#'
#' @param K Integer which contains the number of division.
#' @param N Integer which includes the number of DMUs.
#' @param sum_r Integer which included the total number of all output variables.
#' @param sum_m Integer which included the total number of all input variables.
#' @param sum_l Integer which included the total number of all link variables.
#' @param Link_obj Flag which is 0 for no link variable in the objective function 1 otherwise.
#'
#' @return Return a matrix with the constraint that t>0.
#' @examples K = 2;
#' N = 3;
#' sum_r = 2;
#' sum_m = 2;
#' sum_l = 1
#' Link_obj = 0
#' t_constraint(K, N, sum_r, sum_m, sum_l, Link_obj)
#' @export

t_constraint <- function(K, N, sum_r, sum_m, sum_l, Link_obj){

  #This function creates an additional constraint for the orientation "non" because
  #for the transformation into a linear program the additional variable t>0 was introduced.

  # No Link variable in the objective function
  if(Link_obj == 0){
    # Creates the t constraint and save them into f.con
    f.con <- rbind(c(1, rep(0,1, (K*N)), rep(0,1,sum_r), rep(0,1,sum_m)));
  # Case: Link variable in the objective function
  }else if(Link_obj == 1){
    # Creates the t constraint and save them into f.con
    f.con <- rbind(c(1, rep(0,1, (K*N)), rep(0,1,sum_r), rep(0,1,sum_m), rep(0,1,(2*sum_l))));
  }
  #Return the f.con variable
  return(f.con);
}
