#4.1 Creating Lists
j <- list(name="Joe", salary=55000, uniion=T)
print(j)

jalt <- list("Joe", 55000, T)
print(jalt)


z <- vector(mode = "list")
z[["äbc"]] <- 3
z

#4.2 General List Operations
j$salary
j[["salary"]]
j[[2]]

j[1:2]
j2 <- j[2]
j2
class(j2)
str(j2)

#4.2.2 Adding and Deleting List Elements
z <- list(a="äbc", b=12)
z

z$c <- "sailing"# add a c component
z

#adding components via vector index
z[[4]] <- 28
z[5:7] <- c(FALSE,TRUE,TRUE)
z

#how to delete a list component by setting it to NULL
z$b <- NULL
z

c(list("Joe", 55000, T), list(5))
c

#4.2.3 Getting the Size of a list
length(j)

#4.2.4 Extended Example: Text Concorance.

findwords <- function(tf) {
  #read in the words from the file, into a vector of mode character
  txt <- scan(tf,"")
  wl <- list()
  for (i in 1:length(txt)){
    wrd <- txt[i] # ith word on the input file
    wl[[wrd]] <- c(wl[[wrd]], i)
  }
  return(wl)
}

wl <- findwords("testconcord.txt")

#4.3 Accessing List components and Values
names(j)
ulj <- unlist(j)
ulj
class(ulj)


z <- list(a=5, b=12, c=13)
y <- unlist(z)
class(y)
y

w <- list(a=5, b="xyz")
wu = unlist(w)
class(wu)
wu
names(wu) <- NULL
wu

wun <- unname(wu)
wun

#4.4 Applying functions to Lists
#4.4.1 Using the lapply() and sapply() functions
lapply(list(1:3, 25:29),median)
sapply(list(1:3, 25:29),median)

#4.4.2 Extended Example: Text Concordance, Continued
#sorts wrdlst, the output of findwords() alphabetically by word
alphawl <- function(wrdlst) {
  nms <- names(wrdlst) #the words
  sn <- sort(nms) #same words in alpha order
  return(wrdlst[sn]) #return rearranged version
}

alphawl(wl)

#orders the output of findwords() by word frequency
freqwl <- function(wrdlst){
  freqs <- sapply(wrdlst,length) #get word frequencies.
  return(wrdlst[order(freqs)])
}

freqwl(wl)

#4.4.3 Extended example:Back to the Abalone Data
g <- c("M","F","F","I","M","M","F")
g

g <- lapply(c("M", "F","I"), function(gender) which(g==gender))
g

#4.5 Recursive Lists
b <- list(u = 5, v = 12)
c <- list(w = 13)
a <- list(b,c)
a

length(a)

c(list(a=1,b=2,c=list(d=5,e=9)))
c(list(a=1,b=2,c=list(d=5,e=9)), recursive=T)

