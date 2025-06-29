#3.1 Creating matrices
y <- matrix(c(1,2,3,4), nrow=2,ncol=2)
y

y[,2]

y <- matrix(nrow=2, ncol=2)
y[1,1] <- 1
y[2,1] <- 2
y[1,2] <- 3
y[2,2] <- 4
y

#default is to populate a matrix by column but it can be set to byrow using the byrow argument to true
m <-  matrix(c(1,2,3,4,5,6), nrow=2, byrow=T)
m

#3.2 General Matrix Operations
#3.2.1 Performing Linear Algebra Operations on Matrices
y %*% y #Mathematical matrix multiplication
3*y #Mathematical multiplication of matrix by scalar
y+y #mathematical matrix addition

#3.2.2 Matrix indexing
z <- matrix(c(1,2,3,4,1,1,0,0,1,0,1,0), nrow=4,ncol=3)
z
z[,2:3]

y <- matrix(c(11,21,31,12,22,32), nrow=3,ncol=2)
y
y[2:3,]
y[2:3,2]

y <- matrix(c(1,2,3,4,5,6), nrow=3,ncol=2)
y

y[c(1,3),] <- matrix(c(1,1,8,12),nrow=2)
y

x <- matrix(nrow=3,ncol=3)
y <- matrix(c(4,5,2,3),nrow=2)
y

x[2:3,2:3] <- y
x

y <- matrix(c(1,2,3,4,5,6), nrow=3,ncol=2)
y

y[-2,]

#3.2.3 Extended example Image manipulation
library(pixmap)
mtrush1 <- read.pnm("pixmap/mtrush1.pgm")
mtrush1
plot(mtrush1)
str(mtrush1)
mtrush1@grey[28,88]

mtrush2 <- mtrush1
mtrush2@grey[84:163,135:177] <- 1
plot(mtrush2)

# adds random noise to img, at the range rows,cols of img; img and the
# return value are both objects of class pixmap; the parameter q
# controls the weight of the noise, with the result being 1-q times the
# original image plus q times the random noise
blurpart <- function(img,rows,cols,q) {
  lrows <- length(rows)
  lcols <- length(cols)
  newimg <- img
  randomnoise <- matrix(nrow=lrows, ncol=lcols,runif(lrows*lcols))
  newimg@grey[rows,cols] <- (1-q) * img@grey[rows,cols] + q * randomnoise
  return(newimg)
}
mtrush3 <- blurpart(mtrush1,84:163,135:177,0.65)
plot(mtrush3)

#3.2.4 filtering on Matrices
x <- matrix(c(1,2,3,2,3,4), nrow=3)
x

x[x[,2] >= 3,]

j <- x[,2] >= 3
j

x[j,]
x

z <- c(5,12,13)
z
x[z %% 2 == 1,]

m <- matrix(c(1,2,3,4,5,6), ncol=2)
m
m[m[,1] > 1 & m[,2] > 5,]

m
which(m>4)

#3.2.5 Extended Example: Generating a Covariane Matrix
makecov <- function(rho,n){
  m <- matrix(nrow=n,ncol=n)
  m <- ifelse(row(m) == col(m),1,rho)
  return(m)
}

z <- matrix(c(1,2,3,4,5,6), ncol=2)
z
row(z)

#3.3 Applying Functions to Matrix Rows and Columns
#3.3.1 Using the apply() Function
apply(z,2,mean)

z
f <- function(x) x/c(2,8)
y <- apply(z,1,f)
y
t(apply(z,1,f))

copymaj <-  function(rw,d){
  maj <- sum(rw[1:d]) / d
  return(if(maj > 0.5) 1 else 0)
}

x <- matrix(c(1,1,1,0,
              0,1,0,1,
              1,1,0,1,
              1,1,1,1,
              0,0,1,0), ncol=5, nrow=4)

x

apply(x,1,copymaj,3)
apply(x,1,copymaj,2)

#3.3.2 Extended Example Finding outliers

findols <- function(x){
  findol <- function(xrow){
    mdn <- median(xrow)
    devs <- abs(xrow-mdn)
    return(which.max(devs))
  }
  return(apply(x,1,findol))
}

findols(rs)

#3.4 Adding and deleting Matrix rows and columns
#3.4.1 Changing the sicze of a Matrix
x <- c(12,5,13,16,8)
x
x <- c(x,20) #appends 20 to the vector
x

x <- c(x[1:3], 20,x[4:6])
x

x <- x[-2:-4] #delete elements 2 through 4
x

one <- c(1,1,1,1)
one
z <- matrix(c(1,2,3,4,1,1,0,0,1,0,1,0), ncol=3, nrow=4)
z

cbind(one,z)

q <- cbind(c(1,2),c(3,4))
q

m <- matrix(1:6,nrow=3)
m

m <- m[c(1,3),]
m

#3.4.2 Extended Example:Finding the Closest Pair of Verices in a Graph
# returns the minimum value of d[i,j], i != j, and the row/col attaining
# that minimum, for square symmetric matrix d; no special policy on ties
mind <- function(d) {
 n <- nrow(d)
 # add a column to identify row number for apply()
 dd <- cbind(d,1:n)
 wmins <- apply(dd[-n,],1,imin)
 # wmins will be 2xn, 1st row being indices and 2nd being values
 i <- which.min(wmins[2,])
 j <- wmins[1,i]
 return(c(d[i,j],i,j))
}

# finds the location, value of the minimum in a row x
imin <- function(x) {
   lx <- length(x)
   i <- x[lx] # original row number
   j <- which.min(x[(i+1):(lx-1)])
   k <- i+j
   return(c(k,x[k]))
  }

q <- matrix(c(0,12,13,8,20,
              12,0,15,28,88,
              13,15,0,6,9,
              8,28,6,0,33,
              20,88,9,33,0), nrow=5,ncol=5)

q

mind(q)

#3.5 More on the Vector/Matrix Distinction
z <- matrix(1:8,nrow=4)
z

length(z)
class(z)
attributes(z)
dim(z)
nrow(z)
ncol(z)

nrow

#Avoiding Unintended Dimention Reduction
z <- matrix(1:8,nrow=4)
z

r <- z[2,]
r

attributes(z)
attributes(r)
str(z)
str(r)

r <- z[2,, drop=FALSE]
r
dim(r)

z[3,2]
"[" (z,3,2)

u <- c(1:3)
u
v <- as.matrix(u)
attributes(u)
attributes(v)

#3.7 Naming Matrix Rows and Columns
z <- matrix(1:4, nrow=2)
z
colnames(z)
colnames(z) <- c("a","b")
z
colnames(z)
z[,"a"]

#3.8 Higher-Dimensional Arrays

firsttest <- matrix(c(46,21,50,30,25,48), ncol=2)
firsttest

secondtest <- matrix(c(46,41,50,43,35,49), ncol=2)
secondtest

tests <- array(data=c(firsttest, secondtest),dim=c(3,2,2))
tests
attributes(tests)

tests[3,2,1]

