library(ggplot2)
library(dplyr)
library(caTools) # for trapz

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
ratesMatrix <- matrix(ncol = 3, nrow = 100)

for (i in 1:100) {
    yes.thresh <- i / 100

    test.df <- test.df%>%
        mutate(class.pred = ifelse(prob >= yes.thresh, "Y", "N"))
    with(test.df, table(class, class.pred))

    # Get the true positive numbers
    truePositives <- sum(test.df$class == "Y")
    falsePositves <- sum(test.df$class == "N" & test.df$class.pred == "Y")
    trueNegatives <- sum(test.df$class == "N")
    falseNegatives <- sum(test.df$class == "Y" & test.df$class.pred == "N")

    # Get the rates
    tpRate <- sum(test.df$class == "Y" & test.df$class.pred == "Y") / truePositives
    fpRate <- sum(test.df$class == "N" & test.df$class.pred == "Y") / trueNegatives

    ratesMatrix[i,] = c(yes.thresh, tpRate, fpRate)
}

rates.df <- data.frame(ratesMatrix)
colnames(rates.df) <- c("p", "truePositiveRate", "falsePositiveRate")
ggplot(rates.df, aes(falsePositiveRate, truePositiveRate)) + geom_point()

# get the area under that curve
area <- 0
for(i in 1:100) {
    area <- area + trapz(rates.df$falsePositiveRate, rates.df$truePositiveRate)
}
