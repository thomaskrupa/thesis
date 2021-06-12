###########################################
# Intersection
###########################################

### R Code find the DMUs which are in all ten years of data
library(funprog)
library(tidyverse)
library(dplyr)
library(SpaDES)

# Path where the input file is located
Path <- "xxxx.csv"

# Read the data inside a dataframe df
df <- read.csv2(Path, header=T,sep = ";");

# Separate the ten columns (ten years data) and save them in different variables (w,x,y,z)
a <- df %>% select(1);
a <- t(t(a));
b <- df %>% select(2);
b <- t(t(b));
c <- df %>% select(3);
c <- t(t(c));
d <- df %>% select(4);
d <- t(t(d));
e <- df %>% select(5);
e <- t(t(e));
f <- df %>% select(6);
f <- t(t(f));
g <- df %>% select(7);
g <- t(t(g));
h <- df %>% select(8);
h <- t(t(h));
i <- df %>% select(9);
i <- t(t(i));
j <- df %>% select(10);
j <- t(t(j));

# Find the intersection between the ten years column and save them into the variable A
A <- Reduce(intersect, list(a,b,c,d,e,f,g,h,i,j));
A <- t(t(A));

# Outputs the variable A
write.csv2(A, "Results.csv")