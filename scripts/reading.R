library(tidyverse)
library(mlr)
library(ucimlr)
library(actools)
library(dummies)
library(pander)

#########################################
# Load Data
#########################################
german <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

adult  <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")

red <- read_delim("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", delim = ";")
white <- read_delim("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", delim = ";'")

# Join into one data set and remove quality = 9 due to low number of observations
wine <- bind_rows(red, white) %>% 
  clean_names() %>% 
  filter(quality %in% c(3:8)) %>% 
  mutate(quality = as_factor(quality))

#########################################
# Data Summaries
#########################################
header_info <- c("Dataset", "# of Rows", "# of Columns", "# of Classes")
german_info <- c("German", nrow(german), 20, 2)
adult_info  <- c("Adult", nrow(adult), length(colnames(adult)) - 1, 2)
iris_info   <- c("Iris", nrow(iris), length(colnames(iris)) - 1, 3)
wine_info   <- c("Wine", nrow(wine), length(colnames(wine)) - 1, 6)

df <- rbind.data.frame(adult_info, german_info, iris_info, wine_info)
colnames(df) <- header_info

df %>% pander()


