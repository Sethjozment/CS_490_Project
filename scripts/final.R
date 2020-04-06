library(tidyverse)
library(pander)

dat <- read_csv("data/full_data.csv")

# Split up into the different metrics and clean

auc <- dat %>% 
  select(Dataset, Trial, Algorithm, AUC, AUCPlacement) %>% 
  mutate(metric = "AUC") %>% 
  rename(place = AUCPlacement,
         score = AUC)

log <- dat %>% 
  select(Dataset, Trial, Algorithm, LogLoss, LogLossPlacement) %>% 
  mutate(metric = "LogLoss") %>% 
  rename(place = LogLossPlacement,
         score = LogLoss)

acc <- dat %>% 
  select(Dataset, Trial, Algorithm, Accuracy, AccuracyPlacement) %>% 
  mutate(metric = "Accuracy") %>% 
  rename(place = AccuracyPlacement,
         score = Accuracy)

f1s <- dat %>% 
  select(Dataset, Trial, Algorithm, f1score, f1scorePlacement) %>% 
  mutate(metric = "F1Score") %>% 
  rename(place = f1scorePlacement, 
         score = f1score)

# Compute percentages of each place for each algorithm
get_percents <- function(data)
{
  percent <- data %>% 
    group_by(Algorithm) %>% 
    summarise(first  = sum(place == 1) / nrow(data),
              second = sum(place == 2) / nrow(data),
              third  = sum(place == 3) / nrow(data),
              fourth = sum(place == 4) / nrow(data))
}

auc_perc <- get_percents(auc)
log_perc <- get_percents(log)
acc_perc <- get_percents(acc)
f1s_perc <- get_percents(f1s)

# Join datasets back together
perc <- bind_rows(auc_perc, log_perc, acc_perc, f1s_perc)

# Group by algorithm to get the final results
results <- perc %>% 
  group_by(Algorithm) %>% 
  summarise(first  = sum(first) %>% round(3),
            second = sum(second) %>% round(3),
            third  = sum(third) %>% round(3),
            fourth = sum(fourth) %>% round(3)) %>% 
  arrange(desc(first)) 

###################################################

# Make datasets for visualization

# Wrangle and join into one dataset with all 4 metrics

auc_clean <- auc %>% 
  rename(AUC = score) %>% 
  select(Dataset, Algorithm, Trial, AUC)

log_clean <- log %>% 
  rename(LogLoss = score) %>% 
  select(Dataset, Algorithm, Trial, LogLoss)

acc_clean <- acc %>% 
  rename(Accuracy = score) %>% 
  select(Dataset, Algorithm, Trial, Accuracy)

f1s_clean <- f1s %>% 
  rename(F1_Score = score) %>% 
  select(Dataset, Algorithm, Trial, F1_Score)

full_clean <- auc_clean %>% 
  left_join(log_clean, by = c('Dataset', 'Algorithm', 'Trial')) %>% 
  left_join(acc_clean, by = c('Dataset', 'Algorithm', 'Trial')) %>%
  left_join(f1s_clean, c('Dataset', 'Algorithm', 'Trial'))

full_clean <- full_clean %>% 
  mutate(Algorithm = as_factor(Algorithm),
         Dataset   = as_factor(Dataset))

#######################################
# Plots
#######################################

# AUC

full_clean %>% 
  group_by(Algorithm) %>% 
  mutate(mean = mean(AUC)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(Dataset, AUC, .fun = max, .desc = TRUE), y = AUC)) +
  geom_boxplot() +
  geom_point() +
  geom_hline(aes(yintercept = mean), color = "firebrick") +
  facet_wrap(~fct_reorder(Algorithm, mean, .desc = TRUE), nrow = 1) +
  labs(title = "AUC",
       subtitle = "red line indicates average AUC by algorithm",
       x = "") +
  theme_bw()

ggsave("plots/auc_alg.png", width = 12, height = 6)

# LogLoss

full_clean %>% 
  group_by(Algorithm) %>% 
  mutate(mean = mean(LogLoss)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(Dataset, LogLoss, .fun = min), y = LogLoss)) +
  geom_boxplot() +
  geom_point() +
  geom_hline(aes(yintercept = mean), color = "firebrick") +
  facet_wrap(~fct_reorder(Algorithm, mean), nrow = 1) +
  labs(title = "Logloss",
       subtitle = "red line indicates average LogLoss by algorithm",
       x = "") +
  theme_bw()

ggsave("plots/logloss_alg.png", width = 12, height = 6)

# Accuracy

full_clean %>% 
  group_by(Algorithm) %>% 
  mutate(mean = mean(Accuracy)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(Dataset, Accuracy, .fun = max, .desc = TRUE), y = Accuracy)) +
  geom_boxplot() +
  geom_point() +
  geom_hline(aes(yintercept = mean), color = "firebrick") +
  facet_wrap(~fct_reorder(Algorithm, mean, .desc = TRUE), nrow = 1) +
  labs(title = "Accruacy",
       subtitle = "red line indicates average accruacy by algorithm",
       x = "") +
  theme_bw()

ggsave("plots/accuracy_alg.png", width = 12, height = 6)

# F1 Score

full_clean %>% 
  group_by(Algorithm) %>% 
  mutate(mean = mean(F1_Score)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(Dataset, F1_Score, .fun = max, .desc = TRUE), y = F1_Score)) +
  geom_boxplot() +
  geom_point() +
  geom_hline(aes(yintercept = mean), color = "firebrick") +
  facet_wrap(~fct_reorder(Algorithm, mean, .desc = TRUE), nrow = 1) +
  labs(title = "F1 Score",
       subtitle = "red line indicates average F1 Score by algorithm",
       x = "") +
  theme_bw()

ggsave("plots/f1score_alg.png", width = 12, height = 6)


# full_clean %>% 
#   group_by(Dataset) %>% 
#   mutate(mean = mean(AUC)) %>% 
#   ungroup() %>% 
#   ggplot(aes(x = fct_reorder(Algorithm, AUC, .fun = median, .desc = TRUE), y = AUC)) +
#   geom_boxplot() +
#   geom_point() +
#   geom_hline(aes(yintercept = mean), color = "firebrick") +
#   facet_wrap(~fct_reorder(Dataset, mean, .desc = TRUE), nrow = 1) +
#   labs(title = "AUC",
#        subtitle = "red line indicates average AUC per dataset",
#        x = "") +
#   theme_bw()
# 
# ggsave("plots/auc.png", width = 12, height = 6)

# make_plots <- function(data, metric, name)
# {
#   data %>% 
#     glimpse() %>% 
#     group_by(Algorithm) %>% 
#     mutate(mean = mean(metric)) %>% 
#     glimpse() %>% 
#     ungroup() %>% 
#     ggplot(aes(x = fct_reorder(data$Dataset, metric, .fun = max, .desc = TRUE), y = metric)) +
#     geom_boxplot() +
#     geom_point() +
#     geom_hline(aes(yintercept = mean), color = "firebrick") +
#     facet_wrap(~fct_reorder(Algorithm, mean, .desc = TRUE), nrow = 1) +
#     labs(title = name,
#          subtitle = paste("red line indicates average ", name, " by algorithm"),
#          x = "") +
#     theme_bw()
#   
#   ggsave(paste("plots/", name, "_alg.png"), width = 12, height = 6)
# }
# 
# make_plots(full_clean, full_clean$AUC, "AUC")
# make_plots(full_clean, full_clean$LogLoss, "LogLoss")

# # Spread the data
# collect <- function(data)
# {
#   data %>% 
#     pivot_longer(cols = c("first", "second", "third", "fourth"),
#                  names_to = "place",
#                  values_to = "percent")
# }
# 
# auc_long <- collect(auc_perc)
# log_long <- collect(log_perc)
# acc_long <- collect(log_perc)
# f1s_long <- collect(f1s_perc)
# 
# long <- bind_rows(auc_long, log_long, acc_long, f1s_long)
# 
# wide <- long %>% 
#   group_by(Algorithm, place) %>%
#   summarise(frequency = sum(percent)) %>% 
#   ungroup() %>% 
#   pivot_wider(id_cols = c(Algorithm, place), 
#               names_from = place,
#               values_from = frequency) %>% 
#   select(Algorithm, first, second, third, fourth) %>% 
#   arrange(desc(first))

  
