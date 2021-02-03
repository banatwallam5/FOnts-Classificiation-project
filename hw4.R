data=c(7.65,7.60,7.65,7.70,7.55,7.55,7.40,7.40,7.50,7.50)
mean(data)
sd(data)

qqplot(data)
?qqplot

shapiro.test(data)
qqnorm(data)
qqline(data)

t.test(data,mu=7.50)
2*pt(-1.5395,9)
