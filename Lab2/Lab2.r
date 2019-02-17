dat <- read.table(file="f.txt", sep=",")
d <- as.numeric(dat)

# среднее значение
m <- mean(d)
# дисперсия
disp<-var(d)
# среднее кв. отклонения
s <- sd(d)
# мода и её позиция
which.max(table(d))
# медиана
median(d)

a <- table(d)
zn <- sort(unique(d))
n <- length(d)
# коэффициент асимметрии
coeff_asymm <- sum((zn-m)^3*a)/(n*disp^3)
# коэффициент эксцесса
coeff_exc <- sum((zn-m)^4*a)/(n*disp^4)-3
l <- length(d)-1
red_d <- sort(d[2:l])
# усечённое среднее
trunc_mean <- sum(red_d)/length(red_d)
# коэффициент вариации
coeff_var <- s/m

d_ <- sum(abs(d-m))/length(d)
# относительное линейное отклонение 
rel_dev <- d_/m