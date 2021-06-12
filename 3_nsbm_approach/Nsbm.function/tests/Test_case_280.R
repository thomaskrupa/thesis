#Test case 280
#Data from Cooper2006 page 12
#Solution on page 143


Data_1 <- matrix(c(20,151,100,90,
                   19,131,150,50,
                   25,160,160,55,
                   27,168,180,72,
                   22,158,94,66,
                   55,255,230,90,
                   33,235,220,88,
                   31,206,152,80,
                   30,244,190,100,
                   50,268,250,100,
                   53,306,260,147,
                   38,284,250,120), byrow=TRUE, nrow=12);


Input = matrix(c(20,151,
                 19,131,
                 25,160,
                 27,168,
                 22,158,
                 55,255,
                 33,235,
                 31,206,
                 30,244,
                 50,268,
                 53,306,
                 38,284), byrow=TRUE, nrow=12);

Output = matrix(c(100,90,
                  150,50,
                  160,55,
                  180,72,
                  94,66,
                  230,90,
                  220,88,
                  152,80,
                  190,100,
                  250,100,
                  260,147,
                  250,120), byrow=TRUE, nrow=12);

Link <- NaN;


K = 1; # divisions
N = 12; # Amount of DMU's
sum_m = 2; # Amount of inputs
sum_r = 2; # Amount of outputs
sum_l = NaN; # Amount of Link variables


# Distinguish the Amount vector:
Amount = matrix(c(2,2,0), byrow=TRUE, nrow=1);
Amount_Input =  c(2);
Amount_Output = c(2);
Amount_Link = NaN;

weights = matrix(c(1), byrow=TRUE, nrow=1);


direction = "input";
link_con = NaN;
return_to_scale = "CRS" ;
NIRS <- 0;
Link_obj = 0; # No Link variable in the objective function



#Loading all the functioN:
setwd(getwd())
setwd("..")
setwd("00_pkg_src")
setwd("Nsbm.function")
setwd("R")
source("load_all_func.R");
load_all_func();
setwd("..")
setwd("..")
setwd("..")
setwd("tests")





test_that("Test case 280",{


  #Results of the NSBM function
  EfficiencyNSBM <- matrix(c(1.000,1.000,0.852,1.000,0.756,0.704,0.895,0.774,0.905,0.781,0.866,0.936), byrow = TRUE, nrow = 12);



  #########################################
  #########################################
  #########################################



  # Results of the nsbm function:
  expect_equal(round(nsbm(direction, return_to_scale, link_con, Data_1, Amount, K, weights, NIRS, Link_obj)$Eff,3), EfficiencyNSBM, check.attributes = FALSE)

})
