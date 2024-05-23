# Make a matrix of random data
m <- matrix(rnorm(100*5), 100, 5)
head(m)

y = rnorm(100)

r = rbinom(100,1,0.5)
y = y + r*3 + m[,1] + m[,2]
plot(m[,1],y)
plot(m[,1], y, cex = r)
points(m[,1], y, cex = 1 - r, pch = 21)
points(m[,1], y, cex = 1 - r, pch=19)


# install.packages('lme4')
# library(lme4)
# install.packages('lmerTest')
# library(lmerTest)
# install.packages('MuMIn')
# library(MuMIn)

summary(lmer(y ~ m + (1|r), data = data.frame(y, m, r)))


# Try on the possum data
head(data)

# Island type is annoying and categorical
# use island type as thing in parenthesis
plot(data$`Island type`, data$CBL)
data = data.frame(data$CBL)
summary(lmer(data$CBL ~ data + (1|r), data = data.frame(data$CBL)))
data$CBL
data
