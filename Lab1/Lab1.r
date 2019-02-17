dat <- read.table(file="f.txt", sep=",")
y <- t(dat)

a <- table(y)

f <- as.data.frame(a)

f[,3] <- 100*a/(sum(a)) 
f[,4] <- cumsum(a) 

f[,5]<- cumsum(f[,3])

colnames(f) <- c("Value","Frequency","Relative Freq.","Cumulative Freq.", "Cum. Relative Freq.")
f

plot(a,type="l",main="Frequencies Polygon",xlab="Value", ylab="Frequency") 

d <- as.numeric(y)
v <- sort(d)
x <- unique(v) 
y <- as.numeric(f[,4])
plot(x,as.numeric(f[,4]),type="l",xlab="Value",ylab="Cumulative Frequency", main= "Absolute Frequencies Cumulate") 
plot(x,as.numeric(f[,5]),type="l",xlab="Value",ylab="Cumulative Relative Frequency", main= "Relative Frequencies Cumulate") 