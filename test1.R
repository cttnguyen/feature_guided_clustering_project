library(mvnfast)
library(ggplot2)
library(plotly)
library(tidyverse)
library(clues)

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


cbind(y, true.clus) %>% 
  data.frame() %>% 
  plot_ly(x = ~y1, y = ~y2, z = ~y3, 
          color = ~factor(true.clus), 
          colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() 

#Get predicted Ys
yhat1 <- fitted(lm(y$y1~x)) %>% scale()
yhat2 <- fitted(lm(y$y2~x)) %>% scale() 
yhat3 <- fitted(lm(y$y3~x)) %>% scale()
yhat <- cbind(yhat1, yhat2, yhat3)

#Get cluster assignments
fitted.clus <- cutree(hclust(dist(yhat)), k = 2) 
x.clus <- apply(x, 1, scale) %>% 
  t() %>% 
  dist() %>% 
  hclust() %>% 
  cutree(k = 2)
y.clus <- apply(y, 1, scale) %>% 
  t() %>% 
  dist() %>% 
  hclust() %>% 
  cutree(k = 2)

#True cluster assignments should have mean c(1, 0)
data.frame(fitted.clus, x.clus, y.clus, y) %>% 
  gather(Method, Cluster.Assignment, -y1, -y2, -y3) %>% 
  group_by(Method, Cluster.Assignment) %>% 
  summarise(m.y1 = mean(y1),
            m.y2 = mean(y2),
            m.y3 = mean(y3))

#misclassification for y-hat, x, and y
c(sum(fitted.clus != true.clus),
  sum(x.clus != true.clus),
  sum(y.clus != true.clus)) / nrow(y)
