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
names(tdata.birds)   
names(tdata.birds)[3]<-paste("AMGP")
all(tdata.birds$Site == tdata.birds3$Site)
tdb  <-   cbind.data.frame(tdata.birds,tdata.birds2, check.names = T)
  
install.packages("ggplot2")
library("ggplot2")
install.packages("dplyr")
library("dplyr")
install.packages("tidyr")
library("tidyr")
library(stringr)
 df.birds <- data.less[data.less$mean >0,]
p3 <-  ggplot(df.birds, aes(site_name, Species_code, size= mean ))+ geom_point()
names(df.flora)
data.flora$BHt <- 0
data.flora2 <- data.flora[-c(1:4)]
str(data.flora2)
data.flora2$site_name <-  as.factor(data.flora2$site_name)
data.flora2$site_name <- c(1:5)
df.flora <-  aggregate(data.flora2,by= data.flora2["site_name"], sum)

grep("Ht$", names(df.flora))
df.flora
df.floraP <-  gather(df.flora[,c(1,grep("^P", names(df.flora)))],name,percent,-site_name)
df.floraHt <-  gather(df.flora[,c(1,grep("Ht$", names(df.flora)))],name,height,-site_name)
df_flora_tall <-  cbind(df.floraP,"height" = df.floraHt$height)
df_flora_tall <- df_flora_tall[df_flora_tall$percent >0 ,]
df_flora_tall$name <-  str_replace(df_flora_tall$name, "^P","")
#str(df_flora_tall)
p1 <-  ggplot(df_flora_tall, aes(site_name,name, size=percent))+ geom_point()
p2 <- ggplot(df_flora_tall, aes(site_name,name, size=height))+ geom_point()
#ggplot(df_flora_tall, aes(percent,height, color=name))+ geom_point()

grid.arrange(p1,p2,p3, layout_matrix= cbind(c(3,3),c(1,2)))
