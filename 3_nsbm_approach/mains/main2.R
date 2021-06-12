######################################################
# Main function
######################################################

main2 <- function(Path, Amount, sec){


  library(tidyverse)
  library(fs)
  library("Nsbm.function")


  ######################################################
  # Read data
  ######################################################

  # "C:/Users/tkrup/Documents/R/Calculation/life"
  # Amount = c(3,0,0,1,2);

  file_paths <- fs::dir_ls(Path)
  file_paths

  # For loop: read files

  file_contents <- list();

  for(i in seq_along(file_paths)){
    # read the files of the file_paths and save them into file_contents
    file_contents[[i]] <- read.csv(file = file_paths[[i]], header=TRUE, sep = ";");
  }

  # Rename the file names in the list file_contents with the paths in file_paths:
  file_contents <- set_names(file_contents, file_paths)



  ######################################################
  # Initialization of the inputs
  ######################################################

  # Inputs
  direction = "non";

  # only fix link constraint is used for the calculation
  link_con = 1;

  # Number of divisions
  K = 2;

  # weights
  weights = matrix(c(0.5, 0.5),byrow=TRUE, nrow=1);

  # This thesis used only the case where link variable are used for the objective function
  Link_obj = 1;

  #
  #
  #
  # Inputs which changed:

  # distinguish calculation between life/ non-life and robustness check cases
  if(sec == "l" | sec == "nl"){

    # NIRS vector for the three cases. 0 is for the CRS and VRS case and 1 for the NIRS cases
    # in combination with return_to_scale = "VRS"
    NIRS_vec <- matrix(c(0,0,1),byrow=TRUE, nrow=1);

    # change between CRS and VRS:
    return_to_scale_vec <- matrix(c("CRS","VRS","VRS"),byrow=TRUE, nrow=1);

  }else{

    # NIRS vector for the three cases. 0 is for the CRS and VRS case and 1 for the NIRS cases
    # in combination with return_to_scale = "VRS"
    NIRS_vec <- matrix(c(0,0),byrow=TRUE, nrow=1);

    # change between CRS and VRS:
    return_to_scale_vec <- matrix(c("CRS","VRS"),byrow=TRUE, nrow=1);

  }


  ######################################################
  # Calculation and save the results
  ######################################################

  # For loop: calculation:

  res_list <- list();


  # for loop over the three or two cases (robustness check) of return_to_scale and NIRS
  for(j in seq_along(return_to_scale_vec)){

    # define inputs:
    return_to_scale <- return_to_scale_vec[j];

    NIRS <- NIRS_vec[j];


    # for loop over the years
    for(i in seq_along(file_contents)){

      Data <- file_contents[[i]]

      # Calculation and save the results in the variable res
      res <- nsbm(direction, return_to_scale, link_con, Data, Amount, K, weights, NIRS, Link_obj)


      # Save all the results in the same results dataframe
      if(i==1){
        res_eff<- res$Eff
        res_div1 <- res$divEff[,1]
        res_div2 <- res$divEff[,2]
      }else{
        res_eff<- cbind(res_eff, res$Eff)
        res_div1 <- cbind(res_div1, res$divEff[,1])
        res_div2 <- cbind(res_div2, res$divEff[,2])
      }

    }

    res_list[[j]] <- res_eff;

    # Create different files names
    H1 <-paste("res_eff_", sec, "_", toString(j),".csv", sep="")
    H2 <-paste("res_div1_", sec, "_", toString(j),".csv", sep="")
    H3 <-paste("res_div2_", sec, "_", toString(j),".csv", sep="")

    # output the different results in different csv files:
    write.table(res_eff, file = H1, row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
    write.table(res_div1, file = H2, row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
    write.table(res_div2, file = H3, row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
  }


  # only in the life and non-life case:
  if(sec == "l" | sec == "nl"){

    # Calculation of scale efficiency and output it:
    H4 <- paste("scale_eff_", sec,".csv", sep=";")
    scale_eff <- res_list[[1]] / res_list[[2]]
    write.table(scale_eff, file = H4, row.names = FALSE, dec = ".", sep = ";", quote = FALSE)


    N <- dim(res_eff)[1]
    year <- dim(res_eff)[2]

    RTS <- matrix(0, dim(res_eff)[1], dim(res_eff)[2])

    VRS_1 <- res_list[[2]];
    NIRS_1 <- res_list[[3]];

    # For loop over all 10 years
    for(j in 1:year){

      # for loop over all DMUs
      for(i in 1:N){

        if(scale_eff[i,j] == 1){
          RTS[i,j] <- "CRS";
        }else if(scale_eff[i,j] != 1 & (VRS_1[i,j] == NIRS_1[i,j])){
          RTS[i,j] <- "DRS";
        }else if(scale_eff[i,j] != 1 & (VRS_1[i,j] != NIRS_1[i,j])){
          RTS[i,j] <- "IRS";
        }
      }
    }

    # Calculation of RTS
    H5 <- paste("RTS_", sec,".csv", sep=";")
    write.table(RTS, file = H5, row.names = FALSE, dec = ".", sep = ";", quote = FALSE)

  }

}
