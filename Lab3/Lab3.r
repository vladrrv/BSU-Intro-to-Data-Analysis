dat = read.table("f.txt")
dat1 = read.table("f.txt")
disp <- var(dat[,1])
deviat <- sqrt(disp)
aver <- mean(dat[,1])

gr1 <- subset(dat[,1], ((aver - deviat) <= dat[,1]) & (dat[,1] <= (aver + deviat)))
gr2 <- subset(dat[,1], ((aver - 2 * deviat) <= dat[,1]) & (dat[,1] <= (aver + 2 * deviat)))
gr3 <- subset(dat[,1], ((aver - 3 * deviat) <= dat[,1]) & (dat[,1] <= (aver + 3 * deviat)))

tab2 <- matrix(0, 3, 3)
tab2[1:3,1] <- c(length (gr1), length (gr2), length(gr3))
tab2[1:3,2] <- tab2[1:3,1]/length(dat[,1]) * 100
tab2[1:3,3] <- c(68.3, 95.4, 99.7)

range <- max(dat[,1]) - min(dat[,1])
k <- 1 + floor(log(length(dat[,1]), 2))
h <- range / k
sa <- sort(dat[,1])
dat1[order(dat1$V1),]
tab3 <- matrix (0, k, 5)

for (i in 0:(k-1)) {
  l <- sa[1] + i * h
  r <- sa[1] + (i + 1) * h
  gr <- subset(sa, l <= sa & (sa < r | i == k - 1 & sa <= r))
  gr1<-0
  for(j in 1:length(sa)) {
    if(l <= dat1[j,1] & dat1[j,1] <= r)
      gr1<-gr1 + dat[j,2]
  }
  gr2<-gr1/length (gr)
  tab3[i + 1, 3:5] <- c(length (gr), gr1, gr2)
  tab3[i + 1, 1:2] <- c(l,r)
}

v <- length (dat[,1]) - 2
coefcor <- cor (dat[,1], dat[,2])
T <- abs(coefcor) * sqrt(v / (1 - coefcor^2))
coefcor
T

lm(dat[,2]~dat[,1])
tab2
k 
h
tab3
b<-coefcor*sqrt(var(dat[,2]))/sqrt(var(dat[,1]))
b
a<-mean(dat[,2])-b*mean(dat[,1])
a

plot(dat,type="p",main="Корреляционное поле",xlab="X", ylab="Y")
abline(a,b)

