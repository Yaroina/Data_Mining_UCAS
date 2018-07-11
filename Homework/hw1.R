dat = read.csv("/Users/yaroina-kente/Desktop/Мои_Штукензии/PhD/Data_Mining/data_ex_2.csv", header=TRUE, sep=";")
age = summary(dat$age)
fat = summary(dat$X.fat)
dat$X.fat <- as.numeric(as.character(dat$X.fat))

age
sd(dat$age)
fat
sd(dat$X.fat)
boxplot(dat$X.fat, main = "% fat", las = 1)
plot(dat$age, dat$X.fat, main="Scatterplot", 
     xlab="Age", ylab="%fat", pch=19)

xmin <- min(dat$age)
xmax <- max(dat$age)
diff <- xmax - xmin

dat$age <- NULL
dat$norm_age <- (dat$age - xmin)/diff
?cor
pear <- cor(dat, use="all.obs", method="pearson") 
?order

?mean

x <- c(33.4, 34.1, 34.6, 35.7, 41.2, 42.5)
mean(x)
