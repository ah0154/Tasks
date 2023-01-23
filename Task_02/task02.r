Data1 <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors = F)
Data2 <- read.csv('http://jonsmitchell.com/data/cyrus.csv', stringsAsFactors = F)
write.csv(Data1, 'rawdata.csv', quote = F)
length(Data1)
nrow(Data1)
ncol(Data1)
colnames(Data1)
head(Data1)
Data1[1,]
Data1[2,]
Data1[1:3,]
Data1[1:3, 4]
Data1[1:5, 1:3]
Feeds <- which(Data1[,9] == 'bottle')
head(berenMilk)
Feeds <- which(Data1[,'event'] == 'bottle')
Feeds <- which(Data1$event == 'bottle')
dayID <- apply(Data1, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-08")
Data1$age <- dateID - dateID[which(Data1$event == 'birth')]
head(Data1)
beren2 <- Data1
beren3 <- beren2[order(beren2$age),]
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
head(avgFeed)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab= "amount of milk consumed (oz)" )
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf(''r02b-totalMilkByDay.pdf'', height=4, width=4)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
dev.off()
source("http://jonsmitchell.com/code/plotFxn02b.R")
cyrus2<- Data2
Feeds<- which(Data2[,9] == 'trait_mass')
cyrusMass <- Data2[Feeds,]
head(cyrusMass)
Feeds<- which(Data2[,'event'] == 'trait_mass')
Feeds<- which(Data2$event == 'trait_mass')
dayID<- apply(Data2, 1, function(x) paste(x[1:3], collapse='-'))
dateID<- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data2$age<- dateID[which(Data2$event == 'birth')]
head(Data2)
cyrus2 <- Data2
cyrus3 <- cyrus2[order(cyrus2$age),]
Feeds <- which(cyrus3$event == "bottle")
avgMass <- mean(cyrus3$value[Feeds])
avgFeed <- tapply(cyrus3$value[Feeds], cyrus3$age[Feeds], mean)
varFeed <- tapply(cyrus3$value[Feeds], cyrus3$age[Feeds], var)
totalFeed<- tapply(cyrus3$value[Feeds], cyrus3$age[Feeds], sum)
numFeeds<- tapply(cyrus3$value[Feeds], cyrus3$age[Feeds], length)
cor(cyrus3$value[Feeds], cyrus3$age[Feeds])
cor.test(cyrus3$value[Feeds], cyrus3$age[Feeds])
cyrusCor<- cor.test(cyrus3$value[Feeds], cyrus3$age[Feeds])
boxplot( cyrus3$value[Feeds] ~ cyrus3$caregiver[Feeds], xlab="who gave the bottle", ylab="amount of milk consumed (oz)")
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
points(Data2)
col="blue"