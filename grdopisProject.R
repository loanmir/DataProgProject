library(tidyverse)
library(skimr)
mtcars
skim(mtcars)

dataset <- read.csv("student_data.csv", header = T)
