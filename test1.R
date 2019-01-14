library(mvnfast)
library(ggplot2)
library(plotly)
library(tidyverse)
library(clues)
library(psych)

set.seed(2018)

#covariate matrix is 1000 x 50
x <- matrix(rnorm(50*1000), nrow=1000, ncol=50)

#the first 10 covariates combined with next 20 -> 4 clusters in X
x[1:500,1:10] <- x[1:500,1:10]+1
x[501:1000,1:10] <- x[501:1000,1:10]-1
cur.ndx <- sample(1:1000)
x[cur.ndx[1:500],11:30] <- x[cur.ndx[1:500],11:30]+2
x[cur.ndx[501:1000],11:30] <- x[cur.ndx[501:1000],11:30]-2

#Ys are clustered into 2 groups
#Why aren't they a function of X?
y = data.frame(y1 = c(rnorm(500, 1), rnorm(500)),
               y2 = c(rnorm(500, 1), rnorm(500)),
               y3 = c(rnorm(500, 1), rnorm(500)))
true.clus <- rep(c(1, 2), each = 500)


# cbind(y, true.clus) %>% 
#   data.frame() %>% 
#   plot_ly(x = ~y1, y = ~y2, z = ~y3, 
#           color = ~factor(true.clus), 
#           colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() 

#Get predicted Ys
yhat1 <- fitted(lm(y$y1~x))
yhat2 <- fitted(lm(y$y2~x))
yhat3 <- fitted(lm(y$y3~x))
yhat <- cbind(yhat1, yhat2, yhat3)

#Get cluster assignments
fitted.clus <- apply(yhat, 2, scale) %>% 
  dist() %>% 
  hclust() %>% 
  cutree(k = 2)
x.clus <- apply(x, 2, scale) %>% 
  dist() %>% 
  hclust() %>% 
  cutree(k = 2)
y.clus <- apply(y, 2, scale) %>% 
  dist() %>% 
  hclust() %>% 
  cutree(k = 2)







#Now for the crazy stuff.
#First let's try clustering on a bunch of distribution estimates.
#We already have E[Y]

#estimate the variances
var.y1 <- fitted(lm((y$y1 - yhat1)**2~x))
var.y2 <- fitted(lm((y$y2 - yhat2)**2~x))
var.y3 <- fitted(lm((y$y3 - yhat3)**2~x))
#set a lower bound so that you don't have negative or teeny tiny variance estimates
var <- data.frame(var.y1, var.y2, var.y3) %>% 
  apply(c(1, 2), function(x) max(x, 0.01)) %>% 
  as.data.frame()


#estimate the covariances
cov.y1.y2 <- fitted(lm((y$y1 - yhat1) * (y$y2 - yhat2) ~ x))
cov.y1.y3 <- fitted(lm((y$y1 - yhat1) * (y$y3 - yhat3) ~ x))
cov.y2.y3 <- fitted(lm((y$y2 - yhat2) * (y$y3 - yhat3) ~ x))
#convert covariance into correlation 
corr.y1.y2 <- cov.y1.y2 / sqrt(var$var.y1 * var$var.y2)
corr.y1.y3 <- cov.y1.y3 / sqrt(var$var.y1 * var$var.y3)
corr.y2.y3 <- cov.y2.y3 / sqrt(var$var.y2 * var$var.y3)
#13 of the correlations are outside the interval [-0.9999, 0.9999]
#Just force them back into the interval
corr <- data.frame(corr.y1.y2, corr.y1.y3, corr.y2.y3) %>% 
  apply(c(1, 2), function(x) ifelse(x < -0.9999, -0.9999,
                                    ifelse(x > 0.9999, 0.9999,
                                           x))) %>% 
  as.data.frame()

#For E[Y], we made it "unitless" by dividing by its sd
#We similarly want to make variance and correlation unit free as well
#Put all our estimated variables together
U <- data.frame(E.Y1 = yhat1 %>% scale(),
                E.Y2 = yhat2 %>% scale(),
                E.Y3 = yhat3 %>% scale(),
                Var.Y1 = sqrt(var$var.y1) / sd(yhat1),
                Var.y2 = sqrt(var$var.y2) / sd(yhat2),
                Var.y3 = sqrt(var$var.y3) / sd(yhat3),
                Corr.Y1.Y2 = fisherz(corr$corr.y1.y2),
                Corr.Y1.Y3 = fisherz(corr$corr.y1.y3),
                Corr.Y2.Y3 = fisherz(corr$corr.y2.y3))

U.clus <- cutree(hclust(dist(U)), k = 2)

#True cluster assignments should have mean c(1, 0)
data.frame(fitted.clus, x.clus, y.clus, U.clus, y) %>% 
  gather(Method, Cluster.Assignment, -y1, -y2, -y3) %>% 
  group_by(Method, Cluster.Assignment) %>% 
  summarise(m.y1 = mean(y1),
            m.y2 = mean(y2),
            m.y3 = mean(y3))

#misclassification for y-hat, x, y, and u clusterings
c(sum(fitted.clus != true.clus),
  sum(x.clus != true.clus),
  sum(y.clus != true.clus),
  sum(U.clus != true.clus)) / nrow(y)

#The U clustering was what we had planned initially. 
#Above is version 1 of kooky idea
#For this simulated dataset, it clearly does not perform that well

#Kosorok thought we had lost information in the modelling
#So version 2 of kooky idea was born:
#Find some way to incorporate both Y and U, including some kind of weight

#I have not tried this particular version of choosing w before
w <- cor(dist(U), cophenetic(hclust(dist(U)))) /
      cor(dist(y), cophenetic(hclust(dist(y))))
D <- dist(y) + (dist(U) ** w)
UY.clus <- cutree(hclust(D), k = 2)


#True cluster assignments should have mean c(1, 0)
data.frame(fitted.clus, x.clus, y.clus, U.clus, UY.clus, y) %>% 
  gather(Method, Cluster.Assignment, -y1, -y2, -y3) %>% 
  group_by(Method, Cluster.Assignment) %>% 
  summarise(m.y1 = mean(y1),
            m.y2 = mean(y2),
            m.y3 = mean(y3))

#misclassification for y-hat, x, y, u, and weighted u and y clusterings
c(sum(fitted.clus != true.clus),
  sum(x.clus != true.clus),
  sum(y.clus != true.clus),
  sum(U.clus != true.clus),
  sum(UY.clus != true.clus)) / nrow(y)

#It doesn't show any improvement from clustering on U alone for this simulation study either
