library(ggplot2)
library(dplyr)
library(tidyr)
library(FNN)

concrete.df <- read.csv("concrete.csv")
concrete.df <- concrete.df %>%  # Ditch some useless data
  select(-SLUMP, -FLOW, -No)

concrete.df <- mutate(concrete.df, LowSlag = Slag == 0) # Add LowSlag

# the linear model, created from a string that gets converted to a function
maxDeg <- 1
formStr <- "Strength~Cement" #starter string
for(d in 1:maxDeg) {
  formStr <- sprintf("%s+I(Cement^%s)", formStr, d)
}
linearModel <- lm(formula(formStr), data = concrete.df)

concrete.df$pred.lm <- predict(linearModel) # plot the linear model, just a check
ggplot(concrete.df, aes(Cement, Strength)) +
  geom_point() +
  geom_line(aes(Cement, pred.lm))  
