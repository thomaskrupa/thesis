#'load_all_func
#'
#'@description The load_all_func function load all the function of the nsbm approach.
#'
#'@return Load all the function in the "Nsbm.function" package.
#'@import "testthat"
#'@export


load_all_func <- function(){

  # load testthat package
  library(testthat);

  #path where the functions are:
  setwd(getwd())
  #setwd("C:/Users/tkrup/Documents/R/Nsbm.function/R")

  #Loading the function:
  source("Function_NSBM.R");
  source("Amount_split.R");
  source("Data_split.R");
  source("Righthandside_and_Direction_Function.R");
  source("obj_func_Constraint1.R");
  source("Constraint2_Function.R");
  source("Constraint3_Function.R");
  source("VRS_Constraint_Function.R");
  source("Constraint5_Function.R");
  source("t_constraint_Function.R");

  source("nsbm_division.R");
  source("slacks_transformation.R");
  source("projection_frontier.R");
  source("negative_zero_value.R");

}
