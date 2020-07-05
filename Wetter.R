Wetter <- read.table(file="Wetterdaten.txt",header=TRUE,sep=";")

Wetter[Wetter == -999.0] <- NA


plot(x=Wetter$LUFTTEMPERATUR)
points(max(Wetter$LUFTTEMPERATUR,na.rm=TRUE),col="red")
points(min(Wetter$LUFTTEMPERATUR,na.rm=TRUE),col="red")




sd(x = Wetter$LUFTTEMPERATUR,na.rm = TRUE)/sqrt(length(Wetter$LUFTTEMPERATUR))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Extract Termperature data of specific months (vectors)
indJa <- seq(from=1, to=length(Wetter$LUFTTEMPERATUR), by=12)
indFe <- seq(from=2, to=length(Wetter$LUFTTEMPERATUR), by=12)
indMz <- seq(from=3, to=length(Wetter$LUFTTEMPERATUR), by=12)
indAp <- seq(from=4, to=length(Wetter$LUFTTEMPERATUR), by=12)
indMa <- seq(from=5, to=length(Wetter$LUFTTEMPERATUR), by=12)
indJu <- seq(from=6, to=length(Wetter$LUFTTEMPERATUR), by=12)
indJi <- seq(from=7, to=length(Wetter$LUFTTEMPERATUR), by=12) 

Januar <- Wetter[indJa,7]
Februar <- Wetter[indFe,7]
Maerz <- Wetter[indMz,7]
April <- Wetter[indAp,7]
Mai <- Wetter[indMa,7]
Juni <- Wetter[indJu,7]
Juli <- Wetter[indJi,7]

par(mfrow=c(3,3))
hist(x=Januar)
hist(x=Februar )
hist(x=Maerz )
hist(x=April )
hist(x=Mai )
hist(x=Juni )
hist(x=Juli )


StJa <- sd(Januar)
StJi <- sd(Juli)

MJa <- mean(Januar)
MJi <- mean(Juli)

SterJa <-  StJa /sqrt(length(Januar))
SterJi <-  StJi /sqrt(length(Juli))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
# Extract Termperature data of specific months (loop)

j <- 1
ind <- 1
month <- 1
schleifenvar <- length(Wetter$LUFTTEMPERATUR) / 12

Months <- matrix(data=NA,nrow=7,ncol=schleifenvar)

for (ij in 1:7){
for (i in 1:schleifenvar ) 
{
Months[ij,ind] <- Wetter[j,7]
j = j+12
ind = ind+1
}
j <- 1+month
ind <- 1
month = month+1
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data fitting
Data  <- read.table(file="data_fit.txt",header=TRUE,sep=";")

data <- data.frame(x=Data$x,y=Data$y_1)
data2 <- data.frame(x=Data$x,y=Data$y_2)
data3 <- data.frame(x=Data$x,y=Data$y_3)
data4 <- data.frame(x=Data$x,y=Data$y_4)

temp <- lm(formula = x ~ y,data=data)
temp2 <- lm(formula = Data$x ~ Data$y_2,data=data2)
temp3 <- lm(formula = Data$x ~ Data$y_3,data=data3)
temp4 <- lm(formula = log(Data$x) ~ Data$y_4,data=data4)

par(mfrow=c(2,2))
plot(x=Data$x,y=Data$y_1)
abline(-0.2561, 0.4789)

plot(x=Data$x,y=Data$y_2)
abline(14.0622, -0.4115)

plot(x=Data$x,y=Data$y_3)
abline(3.6363, 0.4341)

plot(x=Data$x,y=Data$y_4)
abline(1.96881, 0.02792)






















 

