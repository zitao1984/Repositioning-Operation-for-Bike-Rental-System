nov <- read.csv("C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/201811-niceride-tripdata.csv")
sep <- read.csv("C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/201809-niceride-tripdata.csv")
oct <- read.csv("C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/201810-niceride-tripdata.csv")
bike<-rbind(sep,oct,nov)
choice<- split(bike, bike$bike.type)
dockless<-choice$Dockless
dock<-choice$Classic
write.csv(dock,'C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/dock.csv')
write.csv(dockless,'C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/dockless.csv')
write.csv(bike,'C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/bike.csv')
############################
dock <- read.csv("C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/dock.csv")
dockless <- read.csv("C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/dockless.csv")
bike <- read.csv("C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/bike.csv")

library(tidyverse)
library(ggmap)

library(mapview)
library(ggplot2)
#Get the latest Install
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

#Load the library
library("ggmap")

#Set your API Key
ggmap::register_google(key = "AIzaSyBILC2a6qC8dkIpgz6fjuHH7Q6cYUMDL18")

#Notes: If you get still have a failure then I suggest to restart R and run the library and register google commands again.

p <- ggmap(get_googlemap(center = c(lon = -93.274620, lat = 44.975386),
                         zoom = 15, scale = 2,
                         maptype ='terrain',
                         color = 'color'))
p
##dockless
p + geom_leg(
  aes(x = dockless$start.station.longitude, y = dockless$start.station.latitude, xend = dockless$end.station.longitude, yend = dockless$end.station.latitude),
   data = dockless
)
p + geom_point(
  aes(x = dockless$start.station.longitude, y =dockless$start.station.latitude 
      ),shape="\u25D2",color="black",data = dockless, size = 0.5)+geom_point(
  aes(x = dockless$end.station.longitude, y =dockless$end.station.latitude 
    ),shape="\u25D3",color="red", data = dockless, size = 0.5) 

p+geom_density2d(data = dockless, aes(x = dockless$start.station.longitude, y =dockless$start.station.latitude ), size = 0.3) + 
  stat_density2d(data =dockless, 
                 aes(x = dockless$start.station.longitude, y =dockless$start.station.latitude , fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 5, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)


p+geom_density2d(data = dockless, aes(x = dockless$end.station.longitude, y =dockless$end.station.latitude ), size = 0.3) + 
  stat_density2d(data =dockless, 
                 aes(x = dockless$end.station.longitude, y =dockless$end.station.latitude , fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 5, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
###dock
p+geom_density2d(data = dock, aes(x = dock$start.station.longitude, y =dock$start.station.latitude ), size = 0.3) + 
  stat_density2d(data =dock, 
                 aes(x = dock$start.station.longitude, y =dock$start.station.latitude , fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 10, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)


p+geom_density2d(data = dock, aes(x = dock$end.station.longitude, y =dock$end.station.latitude ), size = 0.3) + 
  stat_density2d(data =dock, 
                 aes(x = dock$end.station.longitude, y =dock$end.station.latitude , fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 10, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

#####################################zipcode
library("ggmap")

ggmap::register_google(key = "AIzaSyBILC2a6qC8dkIpgz6fjuHH7Q6cYUMDL18")

library(stringi)
library(stringr)
x=str_split_fixed(bike.address$start.location, ",",5)
x=as.data.frame(x)
levels(x$V5)[1]="no"
x$V3=as.character(x$V3)
x$V4=as.character(x$V4)
for(i in 1: nrow(x)){
  if(x[i,5]!="no"){
    
    x[i,3]=x[i,4]
  }
}
names(x)[names(x)=="V3"]<-"zip"
bike.address.final=data.frame(bike.address,x$zip)
#######end.zip
merged.data.all <- merge(dock,bike.address.final, by=c("start.station.latitude","start.station.longitude"), all.x=TRUE)
names(bike.address.final)[names(bike.address.final)=="start.station.latitude"]<-"end.station.latitude"
names(bike.address.final)[names(bike.address.final)=="start.station.longitude"]<-"end.station.longitude"
names(bike.address.final)[names(bike.address.final)=="x.zip"]<-"zip.end"
merged.data.all1 <- merge(merged.data.all,bike.address.final, by=c("end.station.latitude","end.station.longitude"), all.x=TRUE)
final.bike<-merged.data.all1[,-c(18,20)]
names(final.bike)[names(final.bike)=="x.zip"]<-"zip.start"
write.csv(final.bike,'C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/bike_final.csv')
#######################
bike.final <- read.csv("C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/bike_final.csv")
library(tidyr)
bike.final=as.data.frame(bike.final)
bike.final$start_time=as.character(bike.final$start_time)
##new=separate(bike.final, bike.final$start_time, into = c("start.date", "start.time"), sep = " (?=[^ ]+$)")
new=strsplit(bike.final$start_time," ")

time=unlist(new)
odd <- function(x) x%%2 != 0 
even <- function(x) x%%2 == 0 
start.date=time[odd(1:length(time))]
start.time=time[even(1:length(time))]
new.time=data.frame(start.date,start.time)
new.time$start.time=as.character(new.time$start.time)
new.time$start.time=substr(new.time$start.time,1,2)

final.time=data.frame(bike.final,new.time)
write.csv(final.time,'C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/final_time.csv')
################
final.time$start.time=as.factor(final.time$start.time)
summary(final.time$start.time)
summary(final.time$zip.start)


