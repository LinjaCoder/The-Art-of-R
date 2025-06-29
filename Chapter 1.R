#counts the numbe of odd integers in x
addcount <- function(x) {
  k <- 0 #assign 0 to k
  for (n in x) {
    if (n %% 2 == 1) k <- k+1 
  }
  return(k) 
}

addcount(c(1,2,5))

addcount(c(1,2,3,7,9))

z <- c(2,6,7)
addcount(z)

f <- function(x) return(x+y)
y<-3
f(5)

g <- function(x,y=2,z=T)
g(12,z=FALSE)


#strings
x <- c(5,12,13)
x

length(x)

mode(x)
y <- "äbc"

length(y)
mode(y)

z<- c("äbc", "29 88")
length(z)
mode(z)

u <- paste("äbc", "de", "f")
u

v <- strsplit(u, " ")
v

#Matrices
m <- rbind(c(1,4), c(2,2))
m

m %*% c(1,1)

m[1,2]
m[2,2]

m[1,]
m[,2]

#Lists
x <- list(u=2, v="äbc")
x

x$u

hn <- hist(Nile)

print(hn)
str(hn)

#data frames
d <- data.frame(list(kids=c("Jack", "Jill"),ages=c(12,10)))
d

#Classes
print(hn)
summary(hn)

#extended example regression analysis exam grades
examsquiz <- read.table("ËxamsQuiz.txt",header=FALSE)

