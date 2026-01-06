#Chapter 5 - Data Frames

#5.1 Creating Data Frames
kids <- c("Jack","Jill")
ages <- c(12,10)

d <- data.frame(kids, ages, stringsAsFactors = FALSE)
d #matrix-like version

#5.1.1 Accessing Data Frames
d[[1]]
d$kids
d[,1]

str(d)

#5.1.2 Extended Example:Regression Analysis of Exam Grades Continued.
examsquiz <- read.table("ExamsQuiz.txt",header=TRUE)

#5.2.1 Extracting Subdata Frames
examsquiz[2:5,] #First 5 records for all columns
examsquiz[2:5,2] #This keeps the second column and displays it as a row i.e it displays it as a vector
class(examsquiz[2:5,2])
examsquiz[2:5,2, drop=FALSE]#This keeps the second column and it's header and displays it as a column i.e it displays it as a dataframe
class(examsquiz[2:5,2, drop=FALSE])
#we can also do filtering
examsquiz[examsquiz$Exam.1 >= 3.8,]

#5.2.2 More on Treatment of NA Values
x <- c(2,NA,4)
mean(x)
mean(x, na.rm = TRUE)

subset(examsquiz, Exam.1 >= 3.8)

kids1   <- c("Jack",NA ,"Jillian","John")
states <- c("CA", "MA", "MA", NA)
d4 <- data.frame(kids1, states, stringsAsFactors = FALSE)
d4 #matrix-like version

complete.cases(d4) #removes records where at least one value is NA

d5 <- d4[complete.cases(d4),]#removes records where at least one value is NA and writes it out as d5
d5

#5.2.3 Using rbind() and cbind() Funtionsand alternatives
d

rbind(d,list("Laura", 19)) #rbind creates a new row 

eq <- cbind(examsquiz, examsquiz$Exam.2-examsquiz$Exam.1)
class(eq)
head(eq)

examsquiz$ExamDiff <- examsquiz$Exam.2 - examsquiz$Exam.1
head(examsquiz)

d
d$one <- 1
d

#5.2.4 Applying apply()
apply(examsquiz,1,max)

#5.2.5 Extended Example: A Salary Study
all2006 <- read.csv("artofr_data/2006.csv.short",header=TRUE, as.is=TRUE)
all2006 <- all2006[all2006$Wage_Per=="Year",] # xclude hourly-wagers
all2006 <- all2006[all2006$Wage_Offered_From > 20000,]# exclude weird cases
all2006 <- all2006[all2006$Prevailing_Wage_Amount > 200,]# exclude hrly prv wg

all2006$rat <- all2006$Wage_Offered_From / all2006$Prevailing_Wage_Amount

medrat <- function(dataframe){
  return(median(dataframe$rat,na.rm = TRUE))
}

se2006 <- all2006[grep("Software Engineer", all2006),] #grep identifies the row containing the specific values
prg2006 <- all2006[grep("Programmer", all2006),]
ee2006 <- all2006[grep("Electronics Engineer", all2006),]

makecorp <- function(corpname) {
  t <- all2006[all2006$Employer_Name == corpname,]
  return(t)
}

corplist <- c("MICROSOFT CORPORATION","ms","INTEL CORPORATION","intel","
SUN MICROSYSTEMS, INC.","sun","GOOGLE INC.","google")
for (i in 1:(length(corplist)/2)) {
  corp <- corplist[2*i-1]
  newdtf <- paste(corplist[2*i],"2006",sep="")
  assign(newdtf,makecorp(corp),pos=.GlobalEnv)
}


#5.3 Merging Data Frames
kids0   <- c("Jack","Jill" ,"Jillian","John")
states0 <- c("CA", "MA", "MA", "HI")
d1 <- data.frame(kids0, states0, stringsAsFactors = FALSE)
d1

kids0   <- c("Jill" ,"Lillian","Jack")
ages0    <- c(10,7,12)
d2 <- data.frame(ages0, kids0, stringsAsFactors = FALSE)
d2

dm <- merge(d1,d2)
dm

pals   <- c("Jack", "Jill", "Lillian")
ages0  <- c(12,10,7)
d3 <- data.frame(ages0,pals, stringsAsFactors = FALSE)
d3

merge(d1,d3,by.x="kids0", by.y="pals")

d2a <- rbind(d2,list(15, "Jill"))
d2a
  
merge(d1,d2a) #this gives a wrong result and assumes both Jills come from MA

#5.4 Applying Functions to Data Frames
d
d1 <- lapply(d,sort)
d1
  
as.data.frame(d1)
d1

#note that using this does break the correlation between the first and second column

#5.4.2 Extended Example: Applying Logistic Regression Models
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


abamf <- aba[aba$Gender != "I",] #exclude infants from the analysis
lftn <- function(clmn) {
  glm(abamf$Gender ~ clmn, family=binomial)$coef
}

loall <- sapply(abamf[,-1],lftn)
loall




  
examsquiz[examsquiz$Exam.1 >= 3.8,]