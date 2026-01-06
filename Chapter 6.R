#Chapter 6 Factors and Tables
#6.1 Factors and Levels
x <- c(5,12,13,12)
xf <- factor(x)
xf
str(xf)
unclass(xf)
length(xf)
xff[2] <- 88
xff
xff[2] <- 28

#6.2 Common Functions Used with Factors
#6.2.1 The tapply() function
ages <- c(25,26,55,37,21,42)
affils <- c("R","D","D","R","U","D")
tapply(ages,affils,mean)

d <- data.frame(list(gender=c("M","M","F","M","F","F"), age=c(47,59,21,32,33,24),income=c(55000,88000,32450,76500,123000,45650)))
d

d$over25 <-ifelse(d$age >25,1,0)
d

tapply(d$income,list(d$gender,d$over25),mean)

#6.2.2 The split() function
split(d$income,list(d$gender,d$over25))

findwords <- function(tf){
  #read in the words from teh file, into a vector of mode character
  txt <- scan(tf,"")
  words <- split(1:length(txt),txt)
  return(words)
}

#6.2.3 The by() Function
aba <- read.csv("artofr_data/Abalone.data", header=F)
colnames(aba)
names(aba)[1] <- "Gender"
names(aba)[2] <- "Length"
names(aba)[3] <- "Diameter"
names(aba)[4] <- "Height"
names(aba)[5] <- "WholeWt"
names(aba)[6] <- "ShuckedWt"
names(aba)[7] <- "VisceraWt"
names(aba)[8] <- "ShellWt"
names(aba)[9] <- "Rings"
colnames(aba)

str(aba)
aba$Gender <- factor(aba$Gender)
str(aba)

by(aba,aba$Gender,function(m) lm(m[,2]~m[,3]))

#6.3 Working with Tables
u <- c(22,8,33,6,8,29,-2)
fl <- list(c(5,12,13,12,13,5,13),c("a","bc","a","a","bc","a","a"))
tapply(u,fl,length)

table(fl)

fl

#create a dataframe
ct <-data.frame(
  vote = c("Yes", "Yes", "No", "Not Sure", "No"),
  voted = c("Yes", "No","No","Yes","No")
)

cttab <- table(ct)
cttab
apply(cttab,1,sum)


#6.3.1 Matrix/Array-Like Operations on Tables
class(cttab)
cttab[1,1]
cttab[1,]

cttab/5

apply(cttab,1,sum)
addmargins(cttab)
dimnames(cttab)

#6.3.2 Extended Example Extracting a Subtable
vtlst <- list(Vote.for.X=c("No", "Yes"), Voted.for.X=c("No","Yes"))

subtable <- function(tbl, subnames){
  #get array of cell counts in tbl
  tblarray <- unclass(tbl)
  #We'll get the subarray of cell counts corresponding to subnames by calling
  #do.call() on the "["function; we need to build up a list of arguments first
  dcargs <- list(tblarray)
  ndims <- length(subnames) #number of dimentions
  for (i in 1:ndims){
    dcargs[[i+1]]<-subnames[[i]]
  }
  subarray <- do.call("[",dcargs)
  #now we'll build the new table, consisting of the subarray, the numbers of levels in each 
  #dimension, and the dimnames() value, plus the "tabe" class attribute
  dims <- lapply(subnames,length)
  subtbl <- array(subarray,dims,dimnames=subnames)
  class(subtbl) <- "table"
  return(subtbl)
}

subtable(cttab,list(Vote.for.X=c("No", "Yes"), Voted.for.X=c("No","Yes")))

#6.3.3 Extended Example: Finding the Largest Cells in a Table
d<-c(5,12,13,4,3,28,12,12,9,5,5,13,5,4,12)
dtab <- table(d)

#finds the cells in table tbl with the k highest frequencies; handling of ties unrefined
tabdom <- function(tbl,k) {
  #create a data frame representation of tbl, adding a Freq column
  tbldf <-as.data.frame(tbl)
  #determine the proper positions of the frequencies in a sorted order
  freqord <- order(tbldf$Freq,decreasing = TRUE)
  #rearrange the data fram in that order, and take the first k rows
  dom <- tbldf[freqord,][1:k,]
  return(dom)
}

tabdom(dtab,3)

#6.4.1 The aggregate() Function
aggregate(aba[,-1],list(aba$Gender),median)

