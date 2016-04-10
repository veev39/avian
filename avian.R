install.packages("stringr")

data.birds <- read.csv(file="./downloads/avian/avianPointTransectSurvey_sewardPeninsula_McNew_2012.csv", header=T)
data.flora  <- read.csv(file="./downloads/avian/avianHabitat_sewardPeninsula_McNew_2012.csv")

#data.flora
data.flora$site_name <- factor(sub("\\d+$", "", data.flora$Site))
data.flora$sumP  <- rowSums(data.flora[grep("^P", names(data.flora))])
data.flora$Htsum  <- rowSums(data.flora[grep("Ht$", names(data.flora), ignore.case=T)])
head(data.flora)
#data.birds      
data.birds$mean  <- rowMeans(data.birds[,2:4], na.rm = T)
data.birds$site_name <- factor(sub("\\d+$", "", data.birds$Site))
birds_places <- setdiff(data.birds$Site, data.flora$Site)
data.less <- subset(data.birds, !(Site %in% birds_places))

bunkerHill.birds <- subset(data.less, site_name == "BunkerHill")
bunkerHill.birds  <- subset(bunkerHill.birds, V1+V2+V3>0)
unique(bunkerHill.birds$Species_code)

#bunkerHill.birds$mean  <- rowMeans(bunkerHill.birds[,2:4]) 
#dot
dotchart(bunkerHill.birds$mean,labels=bunkerHill.birds$site)
dotchart(data.less$mean,labels=data.less$site_name)


site_n  <- unique(data.less$site_name)

sum(data.less$mean[data.less$site_name == "CreteCreek"])

apply()
apply(data.less, sum(mean[site_name == site_n]))

df1  <-  aggregate(data.less$mean, by= data.less["site_name"], sum)
df2 <-  aggregate(data.flora$sumP, by= data.flora["site_name"], sum) 
df3  <-  aggregate(data.flora$Htsum, by= data.flora["site_name"], sum) 
cor(df1$x,df2$x)
cor(df1$x,df3$x)
#tdata
str(data.less)
data.less$Species_code
data.less$Site[data.less$Species_code == "AMPI"] == data.less$Site[data.less$Species_code == "AMGP"]
tdata.birds  <- aggregate(data.less, by= data.less$Species_code == "AMGP")

tdata.birds  <-  (data.less[c(which(data.less$Species_code == "AMGP")),c(1,8,9)])
tdata.birds2  <- (data.less[c(which(data.less$Species_code == "AMPI")),c(1,8,9)])
tdata.birds3  <- (data.less[c(which(data.less$Species_code == "AMRO")),c(1,8,9)])
names(tdata.birds)   
names(tdata.birds)[3]<-paste("AMGP")
all(tdata.birds$Site == tdata.birds3$Site)
tdb  <-   cbind.data.frame(tdata.birds,tdata.birds2, check.names = T)
  
