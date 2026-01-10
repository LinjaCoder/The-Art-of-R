#Chapter 8 Doing math and simulations in R
#8.1.1 Extended example: Calculating a probability
exactlyone <- function(p) {
  notp <- 1 - p 
  tot <- 0.0
  for (i in 1:length(p))
    tot <- tot + p[i] * prod(notp[-i])
  return(tot)
}

vec <- c(1,0,1)

exactlyone(vec)

#8.1.2 Cumulative Sums and Products
x <- c(12,5,13)
cumsum(x)
cumprod(x)

#8.1.3 Minima and Maxima
z <- matrix(c(1,2,5,3,6,2), nrow=3, ncol=2, byrow = TRUE)
z

min(z[,1], z[,2]) #finds minimum in the columns & rows 
pmin(z[,1], z[,2]) # finds the pair wise minimum i.e the minimum result for each Row
pmin(z[1,], z[2,], z[3,])

max(z[,1], z[,2]) #finds minimum in the columns & rows 
pmax(z[,1], z[,2]) # finds the pair wise minimum i.e the minimum result for each Row
pmax(z[1,], z[2,], z[3,])

a<-nlm(function(x) return(x^2-sin(x)),8)

#8.1.4 Calculus
D(expression(exp(x^2)), "x")
exp(x^2) * (2 * x)
integrate(function(x) x^2,0,1)


#8.2 functions for statistical distributions
mean(rchisq(1000,df=2))
qchisq(0.95,2)

qchisq(c(0.5,0.95), df=2)

#8.3 Sorting
x <- c(13,5,12,5)
sort(x)
x

order(x)

y <- data.frame(list(V1=c("def","ab","zzzz"), V2=c(2,5,1)))
y

r <- order(y$V2)
r

z <- y[r,]
z

d <- data.frame(list(kids=c("Jack","Jill","Billy"), age=c(12,10,13)))
d

d[order(d$kids),]
d[order(d$age),]


x <- c(13,5,12,5)
rank(x)

#8.4 Linear algebra operations on vectors and Matrices
y <- c(1,3,4,10)
2*y

crossprod(1:3,c(5,12,13))

a <- matrix(c(1:4), nrow=2, ncol=2, byrow = TRUE)
a
b <- matrix(c(1,-1,0,1), nrow=2, ncol=2 ,byrow = TRUE)
b

a %*% b

a <- matrix(c(1,1,-1,1),nrow=2,ncol=2)
b<-c(2,4)
solve(a,b)
solve(a)

m <- matrix(c(1,2,7,8),nrow=2,ncol=2)
dm <- diag(m)
dm
diag(dm)
diag(3)

m <- matrix(c(1:9),nrow=3,ncol=3)
m
sweep(m,1,c(1,4,7),"+")

#8.4.1 Extended example:Vector Cross Product
xprod <- function(x,y){
  m <- rbind(rep(NA,3),x,y)
  xp <- vector(length=3)
  for (i in 1:3)
    xp[i] <- -(-1)^i * det(m[2:3. -i])
  return(xp)
}

#8.4.2 Extended Example: Finding Stationary Distributions of Markov Chains
findpi1 <- function(p){
  n <- nrow(p)
  imp <- diag(n) - t(p)
  imp[n,] <- rep(1,n)
  rhs <- c(rep(0,n-1),1)
  pivec <- solve(imp,rhs)
  return(pivec)
}

findpi2 <- function(p) {
  n <- nrow(p)
  # find first eigenvector of P transpose
  pivec <- eigen(t(p))$vectors[,1]
  # guaranteed to be real, but could be negative
  if (pivec[1] < 0) pivec <- -pivec
  # normalize to sum to 1
  pivec <- pivec / sum(pivec)
  return(pivec)
}

#8.5 Set Operations














