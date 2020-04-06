library(gbm)
library(tidyverse)

poker  <- read_csv("data/poker-hand.data")

poker <- poker %>% 
  rename(suit_1 = "1",
         rank_1 = "1_1",
         suit_2 = "1_2",
         rank_2 = "13",
         suit_3 = "2",
         rank_3 = "4",
         suit_4 = "2_1",
         rank_4 = "3",
         suit_5 = "1_3",
         rank_5 = "12",
         target = "0") %>% 
  mutate(suit_1 = recode(suit_1,
                         '1' = "heart",
                         '2' = "spade",
                         '3' = "diamond",
                         '4' = "club"),
         suit_2 = recode(suit_2,
                         '1' = "heart",
                         '2' = "spade",
                         '3' = "diamond",
                         '4' = "club"),
         suit_3 = recode(suit_3,
                         '1' = "heart",
                         '2' = "spade",
                         '3' = "diamond",
                         '4' = "club"),
         suit_4 = recode(suit_4,
                         '1' = "heart",
                         '2' = "spade",
                         '3' = "diamond",
                         '4' = "club"),
         suit_5 = recode(suit_5,
                         '1' = "heart",
                         '2' = "spade",
                         '3' = "diamond",
                         '4' = "club"),
         suit_1 = as_factor(suit_1)) 

poker[sapply(poker, is.character)] <- lapply(poker[sapply(poker, is.character)], 
                                             as.factor)
poker <- poker %>% 
  mutate(target = as_factor(target))

## 70% of the sample size
smp_size <- floor(0.70 * nrow(poker))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(poker)), size = smp_size)

poker_train <- poker[train_ind, ]
poker_test  <- poker[-train_ind, ]

# train GBM model
set.seed(123)

# train GBM model
gbm.fit <- gbm(
  formula = target ~ .,
  distribution = "gaussian",
  data = poker_train,
  n.trees = 1000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 10,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

