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
