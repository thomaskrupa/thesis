##########################################################################################
##########################################################################################
##########################################################################################
#
#  Unit test
#
##########################################################################################
##########################################################################################
##########################################################################################



##########################################################################################
# Test cases: General
##########################################################################################

#K=2, Direction = "non", link_con = 1, #Input = 1, #Output = 1, #Link = 1
source("Test_case_1.R");

#K=2, Direction = "input", link_con = 1, #Input = 1, #Output = 1, #Link = 1
source("Test_case_2.R");

#K=2, Direction = "output", link_con = 1, #Input = 1, #Output = 1, #Link = 1
source("Test_case_3.R");

#K=2, Direction = "non", link_con = 2, #Input = 1, #Output = 1, #Link = 1
source("Test_case_4.R");

#K=2, Direction = "input", link_con = 2, #Input = 1, #Output = 1, #Link = 1
source("Test_case_5.R");

#K=2, Direction = "output", link_con = 2, #Input = 1, #Output = 1, #Link = 1
source("Test_case_6.R");

#K=3, Direction = "non", link_con = 1, #Input = 1, #Output = 1, #Link = 1
source("Test_case_7.R");

#K=3, Direction = "input", link_con = 1, #Input = 1, #Output = 1, #Link = 1
source("Test_case_8.R");

#K=3, Direction = "output", link_con = 1, #Input = 1, #Output = 1, #Link = 1
source("Test_case_9.R");

#K=3, Direction = "non", link_con = 2, #Input = 1, #Output = 1, #Link = 1
source("Test_case_10.R");

#K=3, Direction = "input", link_con = 2, #Input = 1, #Output = 1, #Link = 1
source("Test_case_11.R");

#K=3, Direction = "output", link_con = 2, #Input = 1, #Output = 1, #Link = 1
source("Test_case_12.R");

#K=2, Direction = "non", link_con = 1, #Input = 2, #Output = 2, #Link = 2
source("Test_case_13.R");

#K=2, Direction = "input", link_con = 1, #Input = 2, #Output = 2, #Link = 2
source("Test_case_14.R");

#K=2, Direction = "output", link_con = 1, #Input = 2, #Output = 2, #Link = 2
source("Test_case_15.R");

#K=2, Direction = "non", link_con = 2, #Input = 2, #Output = 2, #Link = 2
source("Test_case_16.R");

#K=2, Direction = "input", link_con = 2, #Input = 2, #Output = 2, #Link = 2
source("Test_case_17.R");

#K=2, Direction = "output", link_con = 2, #Input = 2, #Output = 2, #Link = 2
source("Test_case_18.R");

# K=3, Direction = "non", link_con = 1, #Input = 2, #Output = 2, #Link = 2
source("Test_case_19.R");

# K=3, Direction = "input", link_con = 1, #Input = 2, #Output = 2, #Link = 2
source("Test_case_20.R");

# K=3, Direction = "output", link_con = 1, #Input = 2, #Output = 2, #Link = 2
source("Test_case_21.R");

# K=3, Direction = "non", link_con = 2, #Input = 2, #Output = 2, #Link = 2
source("Test_case_22.R");

# K=3, Direction = "input", link_con = 2, #Input = 2, #Output = 2, #Link = 2
source("Test_case_23.R");

# K=3, Direction = "output", link_con = 2, #Input = 2, #Output = 2, #Link = 2
source("Test_case_24.R");





##########################################################################################
# Test cases for N = 4
##########################################################################################

#K=2, N=4
#Direction = "non", link_con = 1, #Input = 1, #Output = 1, #Link = 1
source("Test_case_25.R");

#K=2, N=4
#Direction = "input", link_con = 1, #Input = 1, #Output = 1, #Link = 1
source("Test_case_26.R");

#K=2, N=4
#Direction = "output", link_con = 1, #Input = 1, #Output = 1, #Link = 1
source("Test_case_27.R");

#K=2, N=4
#Direction = "non", link_con = 2, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_28.R");

#K=2, N=4
#Direction = "input", link_con = 2, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_29.R");

#K=2, N=4
#Direction = "output", link_con = 2, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_30.R");

#K=3, N=4
#Direction = "non", link_con = 1, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_31.R");

#K=3, N=4
#Direction = "input", link_con = 1, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_32.R");

#K=3, N=4
#Direction = "output", link_con = 1, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_33.R");

#K=3, N=4
#Direction = "non", link_con = 2, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_34.R");

#K=3, N=4
#Direction = "input", link_con = 2, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_35.R");

#K=3, N=4
#Direction = "output", link_con = 2, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_36.R");




##########################################################################################
# Test cases for different weights
##########################################################################################

#K=2, N=4, weight = c(0.2,0.8);
#Direction = "non", link_con = 1, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_37.R");

#K=2, N=4, weight = c(0.2,0.8);
#Direction = "input", link_con = 1, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_38.R");

#K=2, N=4, weight = c(0.2,0.8);
#Direction = "output", link_con = 1, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_39.R");

#K=2, N=4, weight = c(0.2,0.8);
#Direction = "non", link_con = 2, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_40.R");

#K=2, N=4, weight = c(0.2,0.8);
#Direction = "input", link_con = 2, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_41.R");

#K=2, N=4, weight = c(0.2,0.8);
#Direction = "output", link_con = 2, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_42.R");

#K=3, N=4, weight = c(0.2,0.3, 0.5);
#Direction = "non", link_con = 1, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_43.R");

#K=3, N=4, weight = c(0.2,0.3, 0.5);
#Direction = "input", link_con = 1, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_44.R");

#K=3, N=4, weight = c(0.2,0.3, 0.5);
#Direction = "output", link_con = 1, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_45.R");

#K=3, N=4, weight = c(0.2,0.3, 0.5);
#Direction = "non", link_con = 2, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_46.R");

#K=3, N=4, weight = c(0.2,0.3, 0.5);
#Direction = "input", link_con = 2, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_47.R");

#K=3, N=4, weight = c(0.2,0.3, 0.5);
#Direction = "output", link_con = 2, Alle #Input = 1, #Output = 1, #Link = 1
source("Test_case_48.R");




##########################################################################################
# Test cases for input 1 is missing
##########################################################################################

#K=2, N=4
#Direction = "non", link_con = 1, #Input1 = 0, Alle #Input2 = 1, #Output = 1, #Link = 1
source("Test_case_49.R");

#K=2, N=4
#Direction = "input", link_con = 1, #Input1 = 0, Alle #Input2 = 1, #Output = 1, #Link = 1
source("Test_case_50.R");

#K=2, N=4
#Direction = "output", link_con = 1, #Input1 = 0, Alle #Input2 = 1, #Output = 1, #Link = 1
source("Test_case_51.R");

#K=2, N=4
#Direction = "non", link_con = 2, #Input1 = 0, Alle #Input2 = 1, #Output = 1, #Link = 1
source("Test_case_52.R");

#K=2, N=4
#Direction = "input", link_con = 2, #Input1 = 0, Alle #Input2 = 1, #Output = 1, #Link = 1
source("Test_case_53.R");

#K=2, N=4
#Direction = "output", link_con = 2, #Input1 = 0, Alle #Input2 = 1, #Output = 1, #Link = 1
source("Test_case_54.R");




##########################################################################################
# Test cases for input 2 is missing
##########################################################################################

#K=2, N=4
#Direction = "non", link_con = 1, #Input2 = 0, Alle #Input1 = 1, #Output = 1, #Link = 1
source("Test_case_55.R");

#K=2, N=4
#Direction = "input", link_con = 1, #Input2 = 0, Alle #Input1 = 1, #Output = 1, #Link = 1
source("Test_case_56.R");

#K=2, N=4
#Direction = "output", link_con = 1, #Input2 = 0, Alle #Input1 = 1, #Output = 1, #Link = 1
source("Test_case_57.R");

#K=2, N=4
#Direction = "non", link_con = 2, #Input2 = 0, Alle #Input1 = 1, #Output = 1, #Link = 1
source("Test_case_58.R");

#K=2, N=4
#Direction = "input", link_con = 2, #Input2 = 0, Alle #Input1 = 1, #Output = 1, #Link = 1
source("Test_case_59.R");

#K=2, N=4
#Direction = "output", link_con = 2, #Input2 = 0, Alle #Input1 = 1, #Output = 1, #Link = 1
source("Test_case_60.R");




##########################################################################################
# Test cases for output 1 is missing
##########################################################################################

#K=2, N=4
#Direction = "non", link_con = 1, #Output1 = 0, Alle #Input = 1, #Output2 = 1, #Link = 1
source("Test_case_61.R");

#K=2, N=4
#Direction = "input", link_con = 1, #Output1 = 0, Alle #Input = 1, #Output2 = 1, #Link = 1
source("Test_case_62.R");

#K=2, N=4
#Direction = "output", link_con = 1, #Output1 = 0, Alle #Input = 1, #Output2 = 1, #Link = 1
source("Test_case_63.R");

#K=2, N=4
#Direction = "non", link_con = 2, #Output1 = 0, Alle #Input = 1, #Output2 = 1, #Link = 1
source("Test_case_64.R");

#K=2, N=4
#Direction = "input", link_con = 2, #Output1 = 0, Alle #Input = 1, #Output2 = 1, #Link = 1
source("Test_case_65.R");

#K=2, N=4
#Direction = "output", link_con = 2, #Output1 = 0, Alle #Input = 1, #Output2 = 1, #Link = 1
source("Test_case_66.R");






##########################################################################################
# Test cases for output 2 is missing
##########################################################################################

#K=2, N=4
#Direction = "non", link_con = 1, #Output2 = 0, Alle #Input = 1, #Output1 = 1, #Link = 1
source("Test_case_67.R");

#K=2, N=4
#Direction = "input", link_con = 1, #Output2 = 0, Alle #Input = 1, #Output1 = 1, #Link = 1
source("Test_case_68.R");

#K=2, N=4
#Direction = "output", link_con = 1, #Output2 = 0, Alle #Input = 1, #Output1 = 1, #Link = 1
source("Test_case_69.R");

#K=2, N=4
#Direction = "non", link_con = 2, #Output2 = 0, Alle #Input = 1, #Output1 = 1, #Link = 1
source("Test_case_70.R");

#K=2, N=4
#Direction = "input", link_con = 2, #Output2 = 0, Alle #Input = 1, #Output1 = 1, #Link = 1
source("Test_case_71.R");

#K=2, N=4
#Direction = "output", link_con = 2, #Output2 = 0, Alle #Input = 1, #Output1 = 1, #Link = 1
source("Test_case_72.R");





##########################################################################################
# Test cases for Link 1 is missing
##########################################################################################

#K=2, N=4
#Direction = "non", link_con = 1, #Link = 0, Alle #Input = 1, #Output = 1
source("Test_case_73.R");




##########################################################################################
# Test cases for Tone NSBM cases without results
##########################################################################################

#K=3, N=10
#Direction = "non", link_con = 1, Alle #Link = 1, #Input = 1, #Output = 1
source("Test_case_74.R");

#K=3, N=10
#Direction = "input", link_con = 1, Alle #Link = 1, #Input = 1, #Output = 1
source("Test_case_75.R");

#K=3, N=10
#Direction = "output", link_con = 1, Alle #Link = 1, #Input = 1, #Output = 1
source("Test_case_76.R");

#K=3, N=10
#Direction = "non", link_con = 2, Alle #Link = 1, #Input = 1, #Output = 1
source("Test_case_77.R");

#K=3, N=10
#Direction = "input", link_con = 2, Alle #Link = 1, #Input = 1, #Output = 1
source("Test_case_78.R");

#K=3, N=10
#Direction = "output", link_con = 2, Alle #Link = 1, #Input = 1, #Output = 1
source("Test_case_79.R");





##########################################################################################
# Test cases for slack_transformation, nsbm_division and projection_frontier (non-oriented)
##########################################################################################

#K=2, N=3
#Direction = "non", link_con = 2,
source("Test_case_80.R");

#K=2, N=3
#Direction = "non", link_con = 2,
source("Test_case_81.R");

#K=2, N=3
#Direction = "non", link_con = 2, Amount_Output = (0,1),
source("Test_case_82.R");

#K=2, N=3
#Direction = "non", link_con = 2, Amount_Output = (1,0),
source("Test_case_83.R");

#K=2, N=3
#Direction = "non", link_con = 2, Amount_Input = (0,1),
source("Test_case_84.R");

#K=2, N=3
#Direction = "non", link_con = 2, Amount_Input = (1,0),
source("Test_case_85.R");

#K=2, N=3
#Direction = "non", link_con = 2, Amount_Input = (2,1),
source("Test_case_86.R");

#K=2, N=3
#Direction = "non", link_con = 2, Amount_Input = (1,2),
source("Test_case_87.R");


#K=2, N=3
#Direction = "non", link_con = 2, Amount_Output = (2,1),
source("Test_case_88.R");

#K=2, N=3
#Direction = "non", link_con = 2, Amount_Output = (1,2),
source("Test_case_89.R");

#K=2, N=3
#Direction = "non", link_con = 1, Amount_Link = (1),
source("Test_case_90.R");

#K=2, N=3
#Direction = "non", link_con = 1, Amount_Link = (2),
source("Test_case_91.R");

#K=3, N=3
#Direction = "non", link_con = 1, Amount_Link = (1,1),
source("Test_case_92.R");

#K=3, N=3
#Direction = "non", link_con = 1, Amount_Link = (2,1),
source("Test_case_93.R");

#K=3, N=3
#Direction = "non", link_con = 1, Amount_Link = (1,2),
source("Test_case_94.R");

#K=2, N=3
#Direction = "non"
source("Test_case_95.R");




##########################################################################################
# Test cases for slack_transformation, nsbm_division and projection_frontier (input-oriented)
##########################################################################################

#K=2, N=3
#Direction = "input", link_con = 1
source("Test_case_96.R");

#K=2, N=3
#Direction = "input", link_con = 1, Amount_Output = c(1,0)
source("Test_case_97.R");

#K=2, N=3
#Direction = "input", link_con = 1, Amount_Input =  c(0,1)
source("Test_case_98.R");

#K=2, N=3
#Direction = "input", link_con = 1, Amount_Input =  c(1,0)
source("Test_case_99.R");

#K=2, N=3
#Direction = "input", link_con = 1, Amount_Input =  c(2,1)
source("Test_case_100.R");

#K=2, N=3
#Direction = "input", link_con = 1, Amount_Input =  c(1,2)
source("Test_case_101.R");

#K=2, N=3
#Direction = "input", link_con = 1, Amount_Output = c(2,1)
source("Test_case_102.R");

#K=2, N=3
#Direction = "input", link_con = 1, Amount_Output = c(1,2)
source("Test_case_103.R");

#K=2, N=3
#Direction = "input", link_con = 2
source("Test_case_104.R");

#K=2, N=3
#Direction = "input", link_con = 2, Amount_Link = c(2)
source("Test_case_105.R");

#K=3, N=3
#Direction = "input", link_con = 2
source("Test_case_106.R");

#K=3, N=3
#Direction = "input", link_con = 2, Amount_Link = c(2,1)
source("Test_case_107.R");

#K=3, N=3
#Direction = "input", link_con = 2, Amount_Link = c(1,2)
source("Test_case_108.R");




##########################################################################################
# Test cases for slack_transformation, nsbm_division and projection_frontier (output-oriented)
##########################################################################################

#K=2, N=3
#Direction = "output", link_con = 1
source("Test_case_109.R");

#K=2, N=3
#Direction = "output", link_con = 1, Amount_Output = c(0,1)
source("Test_case_110.R");

#K=2, N=3
#Direction = "output", link_con = 1, Amount_Output = c(1,0)
source("Test_case_111.R");

#K=2, N=3
#Direction = "output", link_con = 1, Amount_Input =  c(0,1)
source("Test_case_112.R");

#K=2, N=3
#Direction = "output", link_con = 1, Amount_Input =  c(1,0)
source("Test_case_113.R");

#K=2, N=3
#Direction = "output", link_con = 1, Amount_Input =  c(2,1)
source("Test_case_114.R");

#K=2, N=3
#Direction = "output", link_con = 1, Amount_Input =  c(1,2)
source("Test_case_115.R");

#K=2, N=3
#Direction = "output", link_con = 1, Amount_Output = c(2,1)
source("Test_case_116.R");

#K=2, N=3
#Direction = "output", link_con = 1, Amount_Output = c(1,2)
source("Test_case_117.R");

#K=2, N=3
#Direction = "output", link_con = 2
source("Test_case_118.R");

#K=2, N=3
#Direction = "output", link_con = 2, Amount_Link = c(2)
source("Test_case_119.R");

#K=3, N=3
#Direction = "output", link_con = 2, Amount_Link = c(1,1)
source("Test_case_120.R");

#K=3, N=3
#Direction = "output", link_con = 2, Amount_Link = c(2,1)
source("Test_case_121.R");

#K=3, N=3
#Direction = "output", link_con = 2, Amount_Link = c(1,2)
source("Test_case_122.R");




##########################################################################################
# Test cases for negative_zero_value function
##########################################################################################

#N=3
# one negative value
source("Test_case_123.R");

#N=3
# all negative values in column 1
source("Test_case_124.R");

#N=3
# zero and negative values in column 1,
# one negative value in column 2 and 3
source("Test_case_125.R");

#N=3
# in each column one zero value
source("Test_case_126.R");

#N=3
# First column with only one value greater zero, second column with one negative value,
# third column with only zeros
source("Test_case_127.R");

#N=5
# First column: one one and otherwise only zero
# third column: 1 and 8 and otherwise negative values
source("Test_case_128.R");




##########################################################################################
# Test cases for NIRS flag in Righthandside_and_Direction function
##########################################################################################

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "VRS", NIRS = 1
source("Test_case_129.R");

#N=3, K = 2, direction = "non", link_con = 2, return_to_scale = "VRS", NIRS = 1
source("Test_case_130.R");

#N=3, K = 2, direction = "input", link_con = 1, return_to_scale = "VRS", NIRS = 1
source("Test_case_131.R");

#N=3, K = 2, direction = "input", link_con = 2, return_to_scale = "VRS", NIRS = 1
source("Test_case_132.R");

#N=3, K = 2, direction = "output", link_con = 1, return_to_scale = "VRS", NIRS = 1
source("Test_case_133.R");

#N=3, K = 2, direction = "output", link_con = 2, return_to_scale = "VRS", NIRS = 1
source("Test_case_134.R");


#N=3, K= 1, direction = "non", return_to_scale = "VRS", NIRS = 1
source("Test_case_135.R");

#N=3, K= 1, direction = "input", return_to_scale = "VRS", NIRS = 1
source("Test_case_136.R");

#N=3, K= 1, direction = "output", return_to_scale = "VRS", NIRS = 1
source("Test_case_137.R");




##########################################################################################
# Test cases for return_to_scale = "CRS" and direction = "non" (Righthandside_and_Direction)
##########################################################################################

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS"
source("Test_case_138.R");

#N=3, K = 2, direction = "non", link_con = 2, return_to_scale = "CRS"
source("Test_case_139.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Input = c(0,1)
source("Test_case_140.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Input = c(1,0)
source("Test_case_141.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Output = c(0,1)
source("Test_case_142.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Output = c(1,0)
source("Test_case_143.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Link = c(0)
source("Test_case_144.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Input = c(2,1)
source("Test_case_145.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Input = c(1,2)
source("Test_case_146.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Output = c(2,1)
source("Test_case_147.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Output = c(1,2)
source("Test_case_148.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Link = c(2)
source("Test_case_149.R");

#N=3, K= 3, direction = "non", link_con = 1, return_to_scale = "CRS"
source("Test_case_150.R");

#N=3, K= 3, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Link = c(0,1)
source("Test_case_151.R");

#N=3, K= 3, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Link = c(1,0)
source("Test_case_152.R");

#N=3, K= 3, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Link = c(1,2)
source("Test_case_153.R");

#N=3, K= 2, direction = "non", link_con = 2, return_to_scale = "CRS", Amount_Input = c(0,1)
source("Test_case_154.R");

#N=3, K= 2, direction = "non", link_con = 2, return_to_scale = "CRS", Amount_Input = c(1,0)
source("Test_case_155.R");

#N=3, K= 2, direction = "non", link_con = 2, return_to_scale = "CRS", Amount_Output = c(0,1)
source("Test_case_156.R");

#N=3, K= 2, direction = "non", link_con = 2, return_to_scale = "CRS", Amount_Output = c(1,0)
source("Test_case_157.R");

#N=3, K= 2, direction = "non", link_con = 2, return_to_scale = "CRS", Amount_Link = c(0)
source("Test_case_158.R");

#N=3, K= 2, direction = "non", link_con = 2, return_to_scale = "CRS", Amount_Input = c(2,1)
source("Test_case_159.R");

#N=3, K= 2, direction = "non", link_con = 2, return_to_scale = "CRS", Amount_Input = c(1,2)
source("Test_case_160.R");

#N=3, K= 2, direction = "non", link_con = 2, return_to_scale = "CRS", Amount_Output = c(2,1)
source("Test_case_161.R");

#N=3, K= 2, direction = "non", link_con = 2, return_to_scale = "CRS", Amount_Output = c(1,2)
source("Test_case_162.R");

#N=3, K= 2, direction = "non", link_con = 2, return_to_scale = "CRS", Amount_Link = c(2)
source("Test_case_163.R");

#N=4, K= 3, direction = "non", link_con = 2, return_to_scale = "CRS"
source("Test_case_164.R");

#N=4, K= 3, direction = "non", link_con = 2, return_to_scale = "CRS", Amount_Link = c(0,1)
source("Test_case_165.R");

#N=4, K= 3, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Link = c(1,0)
source("Test_case_166.R");

#N=4, K= 3, direction = "non", link_con = 1, return_to_scale = "CRS", Amount_Link = c(1,2)
source("Test_case_167.R");



##########################################################################################
# Test cases for return_to_scale = "CRS" and direction = "input" or "output" (Righthandside_and_Direction)
##########################################################################################

#N=3, K= 2, direction = "input", link_con = 1, return_to_scale = "CRS"
source("Test_case_168.R");

#N=3, K = 2, direction = "input", link_con = 2, return_to_scale = "CRS"
source("Test_case_169.R");

#N=3, K= 2, direction = "output", link_con = 1, return_to_scale = "CRS", Amount_Input = c(0,1)
source("Test_case_170.R");

#N=3, K= 2, direction = "output", link_con = 1, return_to_scale = "CRS", Amount_Input = c(1,0)
source("Test_case_171.R");

#N=3, K= 2, direction = "input", link_con = 1, return_to_scale = "CRS", Amount_Output = c(0,1)
source("Test_case_172.R");

#N=3, K= 2, direction = "input", link_con = 1, return_to_scale = "CRS", Amount_Output = c(1,0)
source("Test_case_173.R");

#N=3, K= 2, direction = "output", link_con = 1, return_to_scale = "CRS", Amount_Link = c(0)
source("Test_case_174.R");

#N=3, K= 2, direction = "output", link_con = 1, return_to_scale = "CRS", Amount_Input = c(2,1)
source("Test_case_175.R");

#N=3, K= 2, direction = "input", link_con = 1, return_to_scale = "CRS", Amount_Input = c(1,2)
source("Test_case_176.R");

#N=3, K= 2, direction = "input", link_con = 1, return_to_scale = "CRS", Amount_Output = c(2,1)
source("Test_case_177.R");

#N=3, K= 2, direction = "output", link_con = 1, return_to_scale = "CRS", Amount_Output = c(1,2)
source("Test_case_178.R");

#N=3, K= 2, direction = "output", link_con = 1, return_to_scale = "CRS", Amount_Link = c(2)
source("Test_case_179.R");

#N=3, K= 3, direction = "input", link_con = 1, return_to_scale = "CRS"
source("Test_case_180.R");

#N=3, K= 3, direction = "input", link_con = 1, return_to_scale = "CRS", Amount_Link = c(0,1)
source("Test_case_181.R");

#N=3, K= 3, direction = "output", link_con = 1, return_to_scale = "CRS", Amount_Link = c(1,0)
source("Test_case_182.R");

#N=3, K= 3, direction = "output", link_con = 1, return_to_scale = "CRS", Amount_Link = c(1,2)
source("Test_case_183.R");

#N=3, K= 2, direction = "input", link_con = 2, return_to_scale = "CRS", Amount_Input = c(0,1)
source("Test_case_184.R");

#N=3, K= 2, direction = "input", link_con = 2, return_to_scale = "CRS", Amount_Input = c(1,0)
source("Test_case_185.R");

#N=3, K= 2, direction = "output", link_con = 2, return_to_scale = "CRS", Amount_Output = c(0,1)
source("Test_case_186.R");

#N=3, K= 2, direction = "output", link_con = 2, return_to_scale = "CRS", Amount_Output = c(1,0)
source("Test_case_187.R");

#N=3, K= 2, direction = "input", link_con = 2, return_to_scale = "CRS", Amount_Link = c(0)
source("Test_case_188.R");

#N=3, K= 2, direction = "input", link_con = 2, return_to_scale = "CRS", Amount_Input = c(2,1)
source("Test_case_189.R");

#N=3, K= 2, direction = "output", link_con = 2, return_to_scale = "CRS", Amount_Input = c(1,2)
source("Test_case_190.R");

#N=3, K= 2, direction = "output", link_con = 2, return_to_scale = "CRS", Amount_Output = c(2,1)
source("Test_case_191.R");

#N=3, K= 2, direction = "input", link_con = 2, return_to_scale = "CRS", Amount_Output = c(1,2)
source("Test_case_192.R");

#N=3, K= 2, direction = "input", link_con = 2, return_to_scale = "CRS", Amount_Link = c(2)
source("Test_case_193.R");

#N=4, K= 3, direction = "output", link_con = 2, return_to_scale = "CRS"
source("Test_case_194.R");

#N=4, K= 3, direction = "output", link_con = 2, return_to_scale = "CRS", Amount_Link = c(0,1)
source("Test_case_195.R");

#N=4, K= 3, direction = "input", link_con = 1, return_to_scale = "CRS", Amount_Link = c(1,0)
source("Test_case_196.R");

#N=4, K= 3, direction = "input", link_con = 1, return_to_scale = "CRS", Amount_Link = c(1,2)
source("Test_case_197.R");



##########################################################################################
# Test cases for return_to_scale = "CRS", K=1, and direction = "non" (Righthandside_and_Direction)
##########################################################################################

#N=3, K= 1, direction = "non", return_to_scale = "CRS"
source("Test_case_198.R");

#N=3, K= 1, direction = "non", return_to_scale = "CRS", Amount_Input = c(1)
source("Test_case_199.R");

#N=3, K= 1, direction = "non", return_to_scale = "CRS", Amount_Input = c(1), Amount_Output = c(1)
source("Test_case_200.R");

#N=3, K= 1, direction = "input", return_to_scale = "CRS"
source("Test_case_201.R");

#N=3, K= 1, direction = "output", return_to_scale = "CRS", Amount_Input = c(1)
source("Test_case_202.R");

#N=3, K= 1, direction = "input", return_to_scale = "CRS", Amount_Input = c(1), Amount_Output = c(1)
source("Test_case_203.R");



##########################################################################################
# Test cases for return_to_scale = "VRS" and direction = "input" or "output" (Righthandside_and_Direction)
##########################################################################################

#N=3, K= 2, direction = "input", link_con = 1, return_to_scale = "VRS", Amount_Link = c(0)
source("Test_case_204.R");

#N=3, K= 2, direction = "output", link_con = 2, return_to_scale = "VRS", Amount_Link = c(0)
source("Test_case_205.R");







##########################################################################################
##########################################################################################
##########################################################################################
#
# Integration test
#
##########################################################################################
##########################################################################################
##########################################################################################






##########################################################################################
# Test cases for obj_fun_and_constraint_function and negative_zero_value (non-oriented)
##########################################################################################


#N=3, K= 2, direction = "non",
#only negative and zeros in first column output
#one negative value in second column output
source("Test_case_206.R");

#N=3, K= 2, direction = "non",
#only zeros in first column output
#one negative value in second column output
source("Test_case_207.R");

#N=3, K= 2, direction = "non",
#only one negative value in the first column output
source("Test_case_208.R");

#N=3, K= 2, direction = "non",
#only one negative value in the second column output
source("Test_case_209.R");

#N=3, K= 2, direction = "non",
#only zeros in the first column output
#only zeros in the second column output
source("Test_case_210.R");


##########################################################################################
# Test cases for obj_fun_and_constraint_function and negative_zero_value (output-oriented)
##########################################################################################

#N=3, K= 2, direction = "output",
#only zeros in the first column output
#one negative value in the second column output
source("Test_case_211.R");

#N=3, K= 2, direction = "output",
#only negative and zeros in the first column output
#one negative value in the second column output
source("Test_case_212.R");

#N=3, K= 2, direction = "output",
#only negative values in the first column output
source("Test_case_213.R");

#N=3, K= 2, direction = "output",
#only negative values in the second column output
source("Test_case_214.R");

#N=3, K= 2, direction = "output",
#only zeros in the first column output
#only zeros in the second column output
source("Test_case_215.R");


##########################################################################################
# Test cases for obj_fun_and_constraint_function and negative_zero_value, K = 1
##########################################################################################

#N=3, K= 1, direction = "non",
#one negative value in first column
#one negative value in second column
source("Test_case_216.R");

#N=3, K= 1, direction = "non",
#one negative value and otherwise zeros in first column
#one negative value in second column
source("Test_case_217.R");

#N=3, K= 1, direction = "non",
#one negative value and otherwise zeros in first column
#only zeros in second column
source("Test_case_218.R");

#N=3, K= 1, direction = "output",
#one negative value in first column
#one negative value in second column
source("Test_case_219.R");

#N=3, K= 1, direction = "output",
#one negative value and otherwise zeros in first column
#one negative value in second column
source("Test_case_220.R");

#N=3, K= 1, direction = "output",
#one negative value and otherwise zeros in first column
#only zeros in second column
source("Test_case_221.R");


##########################################################################################
# Test cases for same dimension of the constraint, f.rhs, f.dir and f.obj
##########################################################################################

#N=3, K= 2, direction = "non",
#link_con =1, VRS
source("Test_case_222.R");

#N=3, K= 2, direction = "non",
#link_con =1, CRS
source("Test_case_223.R");

#N=3, K= 2, direction = "non",
#link_con =2, VRS
source("Test_case_224.R");

#N=3, K= 2, direction = "non",
#link_con =2, CRS
source("Test_case_225.R");

#N=3, K= 2, direction = "input",
#link_con =1, VRS
source("Test_case_226.R");

#N=3, K= 2, direction = "input",
#link_con =1, CRS
source("Test_case_227.R");

#N=3, K= 2, direction = "input",
#link_con =2, VRS
source("Test_case_228.R");

#N=3, K= 2, direction = "input",
#link_con =2, CRS
source("Test_case_229.R");

#N=3, K= 2, direction = "output",
#link_con =1, VRS
source("Test_case_230.R");

#N=3, K= 2, direction = "output",
#link_con =1, CRS
source("Test_case_231.R");

#N=3, K= 2, direction = "output",
#link_con =2, VRS
source("Test_case_232.R");

#N=3, K= 2, direction = "output",
#link_con =2, CRS
source("Test_case_233.R");



##########################################################################################
# Test cases for stop the main nsbm function
##########################################################################################

#Check dim of Data and Amount
source("Test_case_234.R");

#Check if K is an integer
source("Test_case_235.R");

#Check if link_con is 1 or 2 when K>1
source("Test_case_236.R");

#Check if the sum of weights is 1:
source("Test_case_237.R");

#Check if dim of weights is equal to K
source("Test_case_238.R");

#Check if direction in {non, input, output}
source("Test_case_239.R");

#Check if  sum_m or sum_r > 0, K>1
source("Test_case_240.R");

#Check if  sum_m or sum_r > 0, K=1
source("Test_case_241.R");

#Check if  Input values >0
source("Test_case_242.R");

#Check if  Link values >0
source("Test_case_243.R");

#Check if Link_obj is 0 or 1
source("Test_case_244.R");



##########################################################################################
# Test cases for link variable in the objective function (Link_obj = 1, direction = "non")
##########################################################################################

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "VRS", weights = (0.5, 0.5)
source("Test_case_245.R");

#N=3, K= 2, direction = "non", link_con = 2, return_to_scale = "VRS", weights = (0.5, 0.5)
source("Test_case_246.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "VRS", weights = (0.4, 0.6)
source("Test_case_247.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "VRS", weights = (0.5, 0.5), sum_l = 2
source("Test_case_248.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "VRS", weights = (0.5, 0.5), sum_l = 0
source("Test_case_249.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", weights = (0.5, 0.5)
source("Test_case_250.R");

#N=3, K=3, direction = "non", link_con = 1, return_to_scale = "VRS", weights = (0.2, 0.3, 0.5), Amount_Link = (2,2)
source("Test_case_251.R");

#N=3, K=3, direction = "non", link_con = 1, return_to_scale = "VRS", weights = (0.2, 0.3, 0.5), Amount_Link = (2,1)
source("Test_case_252.R");

#N=3, K=3, direction = "non", link_con = 1, return_to_scale = "VRS", weights = (0.2, 0.3, 0.5), Amount_Link = (1,2)
source("Test_case_253.R");

#N=3, K=3, direction = "non", link_con = 1, return_to_scale = "VRS", weights = (0.2, 0.3, 0.5), Amount_Link = (2,0)
source("Test_case_254.R");

#N=3, K=3, direction = "non", link_con = 1, return_to_scale = "VRS", weights = (0.2, 0.3, 0.5), Amount_Link = (0,2)
source("Test_case_255.R");

#N=3, K=3, direction = "non", link_con = 1, return_to_scale = "VRS", weights = (0.2, 0.3, 0.5), Amount_Link = (1,1)
source("Test_case_256.R");

#N=3, K=3, direction = "non", link_con = 2, return_to_scale = "VRS", weights = (0.2, 0.3, 0.5), Amount_Link = (1,1)
source("Test_case_257.R");

#N=3, K=3, direction = "non", link_con = 1, return_to_scale = "CRS", weights = (0.2, 0.3, 0.5), Amount_Link = (1,1)
source("Test_case_258.R");

#Check if Link_obj equal to 1 only works for the non-oriented problem
source("Test_case_259.R");

#N=3, K=3, direction = "non", link_con = 1, return_to_scale = "VRS", weights = (0.2, 0.3, 0.5), Amount_Link = (1,0)
source("Test_case_260.R");

#N=3, K=3, direction = "non", link_con = 1, return_to_scale = "VRS", weights = (0.2, 0.3, 0.5), Amount_Link = (0,1)
source("Test_case_261.R");

#N=3, K=3, direction = "non", link_con = 2, return_to_scale = "VRS", weights = (0.2, 0.3, 0.5), Amount_Link = (2,2)
source("Test_case_262.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", weights = (0.5, 0.5), Amount = c(1,1,1,1,1);
source("Test_case_263.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", weights = (0.5, 0.5), Amount = c(1,0,1,1,1);
source("Test_case_264.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", weights = (0.5, 0.5), Amount = c(1,1,0,1,1);
source("Test_case_265.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", weights = (0.5, 0.5), Amount = c(3,0,0,1,2);
source("Test_case_266.R");

#N=3, K= 2, direction = "non", link_con = 1, return_to_scale = "CRS", weights = (0.5, 0.5), Amount = c(3,1,0,2,1);
source("Test_case_267.R");

#N=3, K= 3, direction = "non", link_con = 1, return_to_scale = "CRS", weights = (0.5, 0.25, 0.25), Amount = c(1,1,1,1,1,1,1,1);
source("Test_case_268.R");

#N=3, K= 3, direction = "non", link_con = 1, return_to_scale = "CRS", weights = (0.5, 0.25, 0.25), Amount = c(1,0,1,1,1,1,1,1);
source("Test_case_269.R");

#N=3, K= 3, direction = "non", link_con = 1, return_to_scale = "CRS", weights = (0.5, 0.25, 0.25), Amount = c(1,1,0,0,1,1,1,1);
source("Test_case_270.R");

#N=3, K= 3, direction = "non", link_con = 1, return_to_scale = "CRS", weights = (0.5, 0.25, 0.25), Amount = c(1,1,0,0,1,1,1,2);
source("Test_case_271.R");

#N=3, K= 3, direction = "non", link_con = 1, return_to_scale = "CRS", weights = (0.5, 0.25, 0.25), Amount = c(1,1,0,0,0,1,1,2);
source("Test_case_272.R");

#N=3, K= 3, direction = "non", link_con = 1, return_to_scale = "CRS", weights = (0.5, 0.25, 0.25), Amount = c(1,1,0,0,0,1,2,1);
source("Test_case_273.R");

#N=3, K= 3, direction = "non", t = c(2,4,5)
source("Test_case_274.R");

#N=3, K= 3, direction = "non", t = c(2,1,4)
source("Test_case_275.R");


##########################################################################################
##########################################################################################
##########################################################################################
#
# System test
#
##########################################################################################
##########################################################################################
##########################################################################################


# Tone2009: Data table 1
# oriented = "input", link_con = 1; VRS
source("Test_case_276.R");

# Tone2009: Data table 1
# oriented = "input", link_con = 2; VRS
source("Test_case_277.R");

# Tone2009: Data table 5
# oriented = "input", link_con = 2; CRS
source("Test_case_278.R");

# Tone2001: Data table 1
# oriented = "non", K=1, CRS
source("Test_case_279.R");

# Cooper2006: Data page 12
#solution page 143
# oriented = "input", K=1, CRS
source("Test_case_280.R");

# Cooper2006: Data page 102
#solution page 102
# oriented = "input", K=1, CRS
source("Test_case_281.R");

# Chen2016: Data table 1
# oriented = "input", K=2, link_con = fix, CRS
source("Test_case_282.R");

# Chen2016: Data table 1
# oriented = "input", K=2, link_con = free, CRS
source("Test_case_283.R");

# Tone2004
# oriented = "non", K=1, VRS, negative and zero values in Output
source("Test_case_284.R");














































