#scalars, vectors, Arrays and matrices
x <- c(88,5,12,13,1)
x <- c(x[1:3],168,x[4:5]) #insert 168 before the 13
x

length(x)

first1 <- function(x){
  for (i in 1:length(x)) {
    if (x[i] == 1) break #break out of the loop
  }
  return(i)
}
#call function first1
first1(x)

#2.1.3 Matrices and Arrays as Vectors
m

m + 10:13

z <- 3
z


y[1] <- 5
y[2] <- 12
y

y <- c(5,12)
y

x <- c(1,5)
x
x <- "äbc"
x

#recycling
c(1,2,4) + c(6,0,9,20,22)

#common vector operations
#2+3
#+(2,3)


#vector indexing
y <- c(1.2,3.9,0.4,0.12)
y
y[c(1,3)]
y[2:3]

v <- 3:4
y[v]

x <- c(4,2,17,5)
y <- x[c(1,1,3)]
y

#negative subscriptions mean you want to exclude the element 
z <- c(5,12,13)
z[-1] #exclude element 1

z[-1:-2] #exclude elements 1 through 2

#a more robust way of dropping the last item of a vector
z <- c(5,12,13)
z[1:(length(z)-1)]
#shorter example
z[-length(z)]

#Generating useful vectors with the : operator 
5:8
5:1

#Generatng Vector Sequences ith seq()
seq(from=12,to=30,by=3)
seq(from=1.1,to=2,length=10)

seq(z)

#repeating vector constants with rep
x <- rep(8,4)
x
rep(c(5,12,13),3)
rep(1:3,2)
rep(c(5,12,13),each=2)

#using all() and any()
x <- 1:10
any(x > 8)
any(x > 88)
all(x > 88)
all(x > 0)

#Extended Example: Finding Runs of Consecutive Ones 
findruns <- function(x,k){
  n <- length(x)
  runs <- NULL
  for (i in 1:(n-k+1)){
    if (all(x[i:(i+k-1)]==1)) runs <- c(runs,i)
  }
  return(runs)
}

y <- c(1,0,0,1,1,1,0,1,1)

findruns(y,3)
findruns(y,3)
findruns(y,6)

#2.5.2 Extended example:predicting discrete valued time serie
preda <- function(x,k){
  n <- length(x)
  print(n)
  k2 <- k/2
  print(k)
  #the vector pred will contain our predicted values
  pred <- vector(length = n-k)
  for (i in 1:(n-k)){
    if (sum(x[i:(i+(k-1))]) >= k2) pred[i] <- 1 else pred[i] <- 0
  }
  return(mean(abs(pred-x[(k+1):n])))
}
y <- c(1,0,0,1,1,1,0,1,1)
preda(y,3)


predb <- function(x,k){
  n <- length(x)
  k2 <- k/2
  pred <- vector(length=n-k)
  sm <- sum(x[1:k])
  if (sm >= k2) pred [1] <- 1 else pred[1] <- 0
  if (n-k >= 2) {
    for (i in 2:(n-k)){
      sm <- sm + x[i+k-1] - x[i-1]
      if (sm >= k2) pred[i] <- 1 else pred[i] <- 0
    }
  }
  return(mean(abs(pred-x[(k+1):n])))
}
y <- c(1,0,0,1,1,1,0,1,1)
predb(y,3)

#use cumsum cumulative sum function in R
predc <- function(x,k){
  n <- length(x)
  k2 <- k/2
  #the vector in red will contain our predicted values
  pred <- vector(length=n-k)
  csx <- c(0,cumsum(x))
  for (i in 1:(n-k)){
    if (csx[i+k] - csx[i] >= k2) pred[i] <- 1 else pred[i] <- 0
  }
  return(mean(abs(pred-x[(k+1):n])))
}
y <- c(5,-5,8,12,5,-3)
predb(y,3)

#2.6 Vectorized options
#vector in vetor out
u <- c(5,2,8)
v <- c(1,3,9)

u > v

w <- function(x) return (x+1)

w(u)

sqrt(1:9)

y <- c(1.2,3.9,0.4)
z <- round(y)
z

y <- c(12,5,13)
y+4

'+'(y+4)

#2.6.2 Vector in, matrix out
ź12 <- function(z) return(c(z,z**2))

x <- 1:8
ź12(x)

matrix(ź12(x),ncol=2)

sapply(1:8,ź12)

#2.7.1 Using NA values
x <- c(5,NA,12)
mode(x[1])
mode(x[2])
y <- c("äbc", "def", NA)
mode(y[2])
mode(y[3])

#using NULL Values
x <- c(88,NA,12,168,13)
x
mean(x)
mean(x,na.rm = T)
x <- c(88,NULL,12,168,13)
mean(x)

#build up a vecor of the even numbers in 1:10
z <- NULL
for (i in 1:10) if (i %%2 == 0) z <- c(z,i)
z

seq(2,10,2)
2*1:5

z <- NA
for (i in 1:10) if (i %%2 == 0) z <- c(z,i)
z

u <- NULL
length(u)
v <- NA
length(v)

#2.8 Filtering
z <- c(5,2,-3,8)
w <- z[z*z > 8]
w

z*z > 8

">"(2,1)
">"(2,5)

z <- c(5,2,-3,8)
j <- z*z > 8
j

y <- c(1,2,30,5)
y[j]


#more compactly 
z <- c(5,2,-3,8)
y <- c(1,2,30,5)
y[z*z > 8]

x[x > 3] <- 0
x <- c(1,3,8,2,20)
x[x>3] <- 0
x

#2.82 Filtering with the subset() function
x <- c(6, 1:3, NA, 12)
x
x[x > 5]

subset(x,x > 5)

#2.8.3 the selection function which()
z <- c(5,2,-3,8)
which(z*z > 8)

z*z > 8

first1 <- function(x) {
  for (i in 1:length(x)){
    if (x[i] == 1) break
  }
  return(i)
}

x <- c(1,3,8,2,20,1)
first1(x)

first1a <- function(x) return(which(x ==1)[1])
first1a(x)

#2.9 A vectorized if-then-else: The ifesle() Function

x <- 1:10
y <- ifelse(x %% 2 == 0,5,12)
y

x <- c(5,2,9,12)
ifelse(x > 6, 2*x, 3*x)

#2.9.1 Extended Ezmple: A measure of association
# findud() concerts vector v to 1s, 0s, representing an element 
# increasing or not, relative to the previous one; output length is 1
# less than input
findud <- function(v){
  vud <- v[-1] - v[-length(v)]
  return(ifelse(vud > 0, 1, -1))
}

udcorr <- function(x,y){
  ud <- lapply(list(x,y), findud)
  return(mean(ud[[1]]) == ud[[2]])
}

x < - c(5,12,13,3,6,0,1,15,16,8,88)
y < - c(4,2,3,23,6,10,11,12,6,3, 2)

udcorr(x,y)

u <- c(1,6,7,2,3,5)
u

diff(u)

sign(diff(u))

udcorr <- function(x,y) mean(sign(diff(x)) == sign(diff(y)))

x < - c(5,12,13,3,6,0,1,15,16,8,88)
y < - c(4,2,3,23,6,10,11,12,6,3, 2)

udcorr(x,y)

#2.9.2 Extended Example:Recording an abalone data set
g <- c("M", "F", "F", "I", "M", "M", "F")
g

ifelse(g== "M",1,ifelse(g== "F",2,3))

args(ifelse)

m <- which(g== "M")
m
f <- which(g== "F")
f
i <- which(g== "I")
i

#2.10 testing vector equality
x <- 1:3
y <- c(1,3,4)
x==y

all(x == y)

identical(x,y)

x <- 1:2
y <- c(1,2)
x
y
identical(x,y)
typeof(x)
typeof(y)

#2.11 vectolement names
x <- c(1,2,3)
names(x)
names(x) <- c("a","b","c")
names(x)
x

names(x) <- NULL
x

x <- c(1,2,4)
names(x) <- c("a", "b", "ab")
x["b"]

#more on c()
c(5,2,"abc")

c(5,2,list(a=1,b=4))

c(5,2,c(1.5,6))


