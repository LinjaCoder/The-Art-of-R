#Chapter 7
#7.1.1 Loops
x <- c(5,12,13)
for (n in x) print(n ** 2)

i <- 1
while (i <= 10) i <- i+4
i

i <- 1
while (TRUE) {
  i <- i+4
  if (i > 10) break
}
i

i <- 1
repeat {
  i <- i+4
  if (i > 10) break
}
i

#7.1.2 Looping over Nonvector Sets
#create a matrix
u <- matrix(c(1,1,2,2,3,4), nrow=3, ncol=2, byrow = TRUE)
u
v <- matrix(c(8,15,12,10,20,2), nrow=3, ncol=2, byrow = TRUE)
v

for (m in c("u", "v")) {
  z <- get(m)
  print(lm(z[,2] ~ z[,1]))
}

#7.1.3 If-else
r <- 5
if (r == 4){
  x <- 1
  print(x)
} else {
  x <- 3
  print(x)
  y <- 4
  print(y)
}

x <- 2
y <- if(x == 2) x else x+1
y

x <- 3
y <- if(x == 2) x else x+1
y

#7.2 Arithmetic and Boolean Operators and Values
x <- c(TRUE,FALSE,TRUE)
x
y<- c(TRUE,TRUE,FALSE)
y

x & y
x[1] && y[1]

x && y #looks at just the first elements of each vector
if (x[1] && y[1]) print("both TRUE")
if (x & y) print("both TRUE")

1 < 2
(1 < 2) * (3 < 4)
(1 < 2) * (3 < 4) * (5 < 1)

(1 < 2) == TRUE
(1 < 2) == 1

#7.3 Default Values for Arguments
#only example in book
#7.4 Return values 
#only example in book
#7.5 Functions are Objects
? "{"

abline # you can just type the function name to see what the function does (code)
page(abline) # better view for longer functions

#7.6 Environment and Scope Issues
#7.6.1 The Top-Level Environment
w <- 12
f <- function(y) {
  d <- 8
  h <- function() {
    return(d*(w+y))
  }
  return(h())
}
environment(f)

ls()
ls.str()


#7.6.2 The Scope Hierarchy
f(2)
w <- 12
f <- function(y) {
  d <- 8
  return(h(d,y))
}
  h <- function(dee,yyy) {
    return(dee*(w+yyy))
  }

f(2)

f <- function(y,ftn){
  d <- 8
  print(environment(ftn))
  return(ftn(d,y))
}

h<- function(dee,yyy){
  return(dee*(w+yyy))
}

w <- 12
f(3,h)

#7.6.3 More on ls()
f <- function(y) {
  d <- 8
  return(h(d,y))
}

h<- function(dee,yyy){
  print(ls())
  print(ls(envir = parent.frame(n=1)))
  return(dee*(w+yyy))
}
f(2)

#7.6.4 Functions have (Almost) No Side Effects
w <- 12
f <- function(y){
  d <- 8
  w <- w + 1
  y <- y - 2
  print(w)
  h <- function(){
    return(d*(w+y))
  }
  return(h())
}

t <- 4
f(t)
w
t

#7.6.5 Extended Example: A Function to Display the Contents of a Call Frame
f <- function() {
  a <- 1
  return(g(a)+a)
}
g <- function(aa) {
  b <- 2
  aab <- h(aa+b)
  return(aab)
}
h <- function(aaa) {
  c <- 3
  return(aaa+c)
}

# shows the values of the local variables (including arguments) of the
# frame upn frames above the one from which showframe() is called; if
# upn < 0, the globals are shown; function objects are not shown
showframe <- function(upn) {
  # determine the proper environment
  if (upn < 0) {
    env <- .GlobalEnv
  } else {
    env <- parent.frame(n=upn+1)
  }
  # get the list of variable names
  vars <- ls(envir=env)
  # for each variable name, print its value
  for (vr in vars) {
    vrg <- get(vr,envir=env)
    if (!is.function(vrg)) {
      cat(vr,":\n",sep="")
      print(vrg)
    }
  }
}

g <- function(aa) {
  b <- 2
  showframe(0)
  showframe(1)
  aab <- h(aa+b)
  return(aab)
}
f()


m <- rbind(1:3, 20:22)
m

get("m")

#7.7 No Pointers in R
x <- c(13,5,12)
sort(x)
x

#to permanently apply the sort you need to reassign the values
x <- sort(x)
x

#7.8 Writing Upstairs
#7.8.1 Writing to Nonlocals with the Superassignment Operator
two <- function(u){
  u <<- 2*u
  z <-2*u
}
x <- 1
z <- 3

two(x)
x
z
u

f <- function(){
  inc <- function() {x <<- x + 1}
  x <- 3
  inc()
  return(x)
}

f()

#7.8.2 Writing to Nonlocals with assign()
x <- 1
two <- function(u) {
  assign("u", 2*u,pos = .GlobalEnv)
  z <- 2*z
}
two(x)
x
u

#7.8.3 Extended Example:Discrete-Event Simiulation in R
#check files DES.R and applic.R
#7.8.4 In book theory
#7.8.5 Closures
counter <- function() {
  ctr <- 0 
  f <- function() {
    ctr <<- ctr + 1
    cat("this count currently has value",ctr,"\n")
  }
  return(f)
}

c1 <- counter()
c2 <- counter()
c1 
c2

c1()
c1()
c2()
c2()
c2()
c1()

#7.9 Recursion
#7.9.1 A quicksort implementation
qs <- function(x){
  if(length(x) <= 1) return(x)
  pivot <- x[1]
  therest <- x[-1]
  sv1 <- therest[therest < pivot]
  sv2 <- therest[therest >= pivot]
  sv1 <- qs(sv1)
  sv2 <- qs(sv2)
  return(c(sv1,pivot,sv2))
}

v <- c(5,4,12,13,3,8,88)
qs(v)

#7.9.2 Extended Example: A Binary Search Tree
x <- newtree(8,3)
x

#7.10 replacement functions

x <- c(8,88,5,12,13)
x
x[2:3] <- 99:100
x

#7.10.2 Extended Example: A self-bookkeeping vector class
# class "bookvec" of vectors that count writes of their elements

# each instance of the class consists of a list whose components are the
# vector values and a vector of counts

# construct a new object of class bookvec
 newbookvec <- function(x) {
   tmp <- list()
   tmp$vec <- x # the vector itself
   tmp$wrts <- rep(0,length(x)) # counts of the writes, one for each element
   class(tmp) <- "bookvec"
   return(tmp)
   }

 # function to read
 "[.bookvec" <- function(bv,subs) {
   return(bv$vec[subs])
   }

 # function to write
 "[<-.bookvec" <- function(bv,subs,value) {
   bv$wrts[subs] <- bv$wrts[subs] + 1 # note the recycling
   bv$vec[subs] <- value
   return(bv)
   }

 
 b <- newbookvec(c(3,4,5,5,12,13))
 b
 
 b[2]
 b[2]<-88
 b[2]
 
 b$wrts
 
 #7.11 Tools for Composing Function Code
 g <- function(x){
   return(x+1)
 }
 
 
 f1 <- edit(schedevnt)