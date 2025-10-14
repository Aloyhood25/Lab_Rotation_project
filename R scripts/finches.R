finches <- read.csv(file.choose())
str(finches)
library(tidyverse)


head(finches)


finches %>%
  ggplot(aes(x = sex, y = length)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5, color = "blue") +
  theme_minimal()

finches %>%
  ggplot(aes(x = length, y = mass, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_bw()

