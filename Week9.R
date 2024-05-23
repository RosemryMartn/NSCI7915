# Make two random matrices
# 100 rows each, 30 and 15 columns
x <- matrix(rnorm(100*30), 100, 30)
y <- matrix(rnorm(100*15), 100, 15)


# Add structure
x[,1:15] <- x[,1:15] + rnorm(100)
x[,16:30] <- x[,16:30] + rnorm(100)
y <- y + 100*x[,1:15]


# Hierarchical (tree-based) clustering
# Makes a tree with things like species or individuals on tips
# y-axis is proportional to similarity between things
# Simple, built-in function is hclust, only useful inside plot() call
# Matrices must be distances, usually using dist() function
# Multiple methods, trees looks different
# Default is "complete" linkage, "average" linkage is more traditional
# ann=F removes annoying labels
d <- dist(x)
# Default
plot(hclust(d), cex = 0.5, ann =F)
# Average
plot(hclust(d, method = "average"), cex = 0.5, ann =F)


# Problem: no variance explained
# Non-hierarchical clustering simply creates groups, no trees
# Useful for plotting groups on PCA plots
# Standard method is k-means clustering, but there are others like partitioning
# around medoids (PAM: package cluster, function pam)
# k-means maximise between-cluster variance, minimises within-cluster
kmeans(x, centers = 3, nstart = 1000)
# Search is random, so set nstart to a high number
# between_SS/ total_SS = variance explained, SS means sum of squares


# A visual test of "significance": the scree plot
# Idea is to vary the cluster count from 2 to 10
# Get the variance explained by each count
# Plot the differences of the variances
# Look for a break in the slope
v <- array()
for (i in 2:20) {
  k <- kmeans(x, centers = i, nstart = 100)
  v[i] <- k$betweenss / k$totss
}
plot(diff(v), type = 'o', cex = 0.5)
# Too smooth
# Add in another command: log = 'y'
plot(diff(v), type = 'o', cex = 0.5, log = 'y')
# Still too smooth, bad if you don't know what the clusters should be


# Principal Components Analysis (PCA)
# Similar to drawing a line through an ellipse (first principle component)
# Then a second perpendicular line (second PC)
# If data are 3D, then second line also run through the "pancake"
z <- matrix(rnorm(1000*3), 1000, 3)
# Create relationships between the variables so there will be something to see
z[,2] <- z[,1]
z[,3] <- z[,1] + z[,3]
scatter3D(z[,1], z[,2], z[,3], cex = 0.1, col = 'black')
# Scatter 3D not working, couldn't load plot3D
# Update RStudio?


# Factor analysis is like PCA, but maybe easier to understand
# Also about rotating the points into a new coordinate system
# Each axis is still orthogonal (perpendicular)
# Have to specify how many axes (factors) you want
# "varimax" roatation separates "loadings" of variables
# This gives you a fuzzy clarification of the variables
# Built-in chi-squared test of significance
# Syntax is easy:
f <- factanal(x, factors = 2)
f
plot(f$loadings)


# Factor analysis output is kind of complicated but useful
# Uniquenesses tell you if variables are explained well: 0 is good, 1 bad 
# Loadings show which variables go with which factors
# Loadings < 0.1 are omitted, they're trivial
# The square of a loading is similar to an r-squared
# So a loading of 0.7 "explains" about half the variance (pretty good) 
# Total proportion of variance explained is reported
# p-value is of hypothesis that there are ENOUGH factors
# So a significant value means you need more
# Look to see if extra factors group at least two variables 
# Factor identities can swap as you add more


# Discriminant analysis evaluates hypothesised groups
# Prior probabilities are just a proportion of things in groups
k <- kmeans(x, centers = 2)
lda(k$cluster ~ x)
# lda not working, installing MASS not working. Update RStudio
# Posterior probabilities tell you how well it worked
pp < lda(k$cluster ~ x, CV = TRUE)$posterior
pp
plot(x, cex = pp)
points(x, cex = pp[,2] + 0.5, col = 'green3')
# If you can't see the difference, it didn't work
# If lots of things don't have any high posteriors, it didn't work


# Canonical correlations look for shared factors between matrices
# Repeat from before
x <- matrix(rnorm(100*30), 100, 30)
y <- matrix(rnorm(100*15), 100, 15)
x[,1:15] <- x[,1:15] + rnorm(100)
x[,16:30] <- x[,16:30] + rnorm(100)
y <- y + 100*x[,1:15]
# Get the canonical correlations
cc <- cancor(x, y)
# First one pulls out the X variables that are linked to Y
cc$xcoef[,1]


# Correspondence analysis is useful for data with lots of zeroes
# PCA and factor analysis assume linear relationships between variables 
# CA assumes that > 0 values come and go in bell curve patterns
# This is true of species distributions through space
# Great for identifying really long spatial or environmental gradients
# Eigenvalues are proportional to variance explained
download.packages('vegan')
library(vegan)
x <- matrix(rpois(100*30, 1), 100, 30)
x[1:40, 1:15] <- x[1:40, 1:15] + rpois(1,2)
x[31:70, 11:20] <- x[31:70, 11:20] + rpois(1,3)
x [61:100, 16:30] <- x[61:100, 16:30] + rpois(1,2)
summary(cca(x))


# You can plot "sites" and "species" at the same time
cc <- cca(x)$CA
# cc$u is for coordinates for the species
cc$u[,1]
# cc$v is for the sites (samples)
cc$v[,1]
plot(cc$u, col = 'blue')
points(cc$v, col = 'red')
# The lines connect sites in their irder across the matrix
lines(cc$v, col = 'red')
