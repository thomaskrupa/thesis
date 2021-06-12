

main1 <- function(){

  ######################################################
  # Calculation Life
  ######################################################

  #
  # life insurance
  Path_l <- "C:/Users/tkrup/Documents/phd thesis/Calculation/Life"
  Amount_l <- c(3,0,0,1,2);
  sec_l <- "l"

  # Perform the calculation with the NSBM approach
  main2(Path_l, Amount_l, sec_l)


  #
  # Robustness check life insurance RC1
  Path_rc_l <- "C:/Users/tkrup/Documents/phd thesis/Calculation/Robustness_Check/Life"
  Amount_rc_l <- c(3,0,0,1,2);
  sec_rc_l <- "rc_l_Earned_premium"

  # Perform the calculation with the NSBM approach
  main2(Path_rc_l, Amount_rc_l, sec_rc_l)


  ######################################################
  # Calculation Non-Life
  ######################################################


  #
  # non-life insurance
  Path_nl <- "C:/Users/tkrup/Documents/phd thesis/Calculation/Non-life"
  Amount_nl <- c(3,1,0,2,1);
  sec_nl <- "nl"

  # Perform the calculation with the NSBM approach
  main2(Path_nl, Amount_nl, sec_nl)


  #
  # Robustness check non-life insurance RC1
  Path_rc_nl_1 <- "C:/Users/tkrup/Documents/phd thesis/Calculation/Robustness_Check/Non_life/RC1"
  Amount_rc_nl_1 <- c(3,1,0,2,1);
  sec_rc_nl_1 <- "rc_nl_Earned_premium"

  # Perform the calculation with the NSBM approach
  main2(Path_rc_nl_1, Amount_rc_nl_1, sec_rc_nl_1)


  #
  # Robustness check non-life insurance RC2
  Path_rc_nl_2 <- "C:/Users/tkrup/Documents/phd thesis/Calculation/Robustness_Check/Non_life/RC2"
  Amount_rc_nl_2 <- c(3,1,0,2,1);
  sec_rc_nl_2 <- "rc_nl_excpected_loss"

  # Perform the calculation with the NSBM approach
  main2(Path_rc_nl_2, Amount_rc_nl_2, sec_rc_nl_2)



}
