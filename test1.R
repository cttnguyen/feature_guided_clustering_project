x <- matrix(rnorm(50*1000), nrow=1000, ncol=50)
x[1:500,1:10] <- x[1:500,1:10]+1
x[501:1000,1:10] <- x[501:1000,1:10]-1
cur.ndx <- sample(1:1000)
x[cur.ndx[1:500],11:30] <- x[cur.ndx[1:500],11:30]+2
x[cur.ndx[501:1000],11:30] <- x[cur.ndx[501:1000],11:30]-2

y1 <- c(rnorm(500,1), rnorm(500))
y2 <- c(rnorm(500,1), rnorm(500))
y3 <- c(rnorm(500,1), rnorm(500))
y <- cbind(y1, y2, y3)

yhat1 <- fitted(lm(y1~x))
yhat2 <- fitted(lm(y2~x))
yhat3 <- fitted(lm(y3~x))
yhat <- cbind(yhat1, yhat2, yhat3)

rm(cur.ndx)
