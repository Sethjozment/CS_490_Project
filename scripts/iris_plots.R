library(tidyverse)

iris %>% 
  ggplot(aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Species), size = 5) +
  labs(title = "Iris dataset") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("plots/iris.png", width = 8, height = 8)
