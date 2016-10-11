library(ggplot2)
library(dplyr)

#######################################################
N <- 1000

# Build some training data
x <- runif(N, -2, 2)
# bigger b1 is more separation of the classes
b1 <- 2
y <- 1 - b1 * x + rnorm(N, 0, .5)
p <- exp(y) / (1 + exp(y))
data.df <- data.frame(x, class = ifelse(runif(N) > p, "Y", "N"))

mod1 <- glm(class~x, data = data.df, family = "binomial")
summary(mod1)
ggplot(data.df, aes(x, class == "Y", color = class)) + geom_point()

# build some test data
x <- runif(N, -2, 2)
y <- 1 - b1 * x + rnorm(N, 0, .5)
p <- exp(y) / (1 + exp(y))

class <- ifelse(runif(N) > p, "Y", "N")
test.df <- data.frame(x, class)

# Predictions
test.df$prob <- predict(mod1, newdata = test.df, type = "response")


#######################################################
# Pick a treshhold and classifiy
yes.thresh <- 0.5
test.df <- test.df%>%
  mutate(class.pred = ifelse(prob >= yes.thresh, "Y", "N"))
with(test.df, table(class, class.pred))

