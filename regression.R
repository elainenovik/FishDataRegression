library(readxl)
my_data<-read_excel("Exampleforclass.xlsx")
View(my_data)

keeps <- c("SPP", "LAKE", "WETHG", "COND")
cor_data <- my_data[keeps]

cor_data<-subset(cor_data, SPP=="LKWH")
View(cor_data)

#Testing normality
hist(cor_data$WETHG)
hist(exp(cor_data$COND))


shapiro.test(log(cor_data$WETHG))
shapiro.test(exp(cor_data$COND))

cor_data[,3]<-log(cor_data[3])
cor_data[,4]<-exp(cor_data[4])

View(cor_data)

attach(cor_data)
plot(COND, WETHG)
cor(COND, WETHG, method="pearson")

cor.test(COND, WETHG)

#Regression
LM<- lm(COND~WETHG)
summary(LM)
shapiro.test(LM$residuals)

#Second test
keeps2 <- c("DEL15N", "DEL13C", "WETHG", "FL")
cor_data2 <- subset(my_data[keeps2])

#cor_data2<-subset(cor_data2, SPP=="LKWH")


cor_data2[,3]<-log(cor_data2[3])
cor_data2[,4]<-log(cor_data2[4])

names(cor_data2)[3]<-"LWETHG"
names(cor_data2)[4]<-"LFL"


View(cor_data2)

attach(cor_data2)
summary(cor_data2)
cor(cor_data2, use ="complete.obs")
cor.test(DEL15N,LWETHG, use="complete.obs")
plot(DEL15N, LWETHG)

LM <- lm(DEL15N~LWETHG)
LM
summary(LM)
shapiro.test(LM$residuals)

