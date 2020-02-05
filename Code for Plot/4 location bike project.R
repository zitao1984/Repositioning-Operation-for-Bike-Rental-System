east <- read.csv('C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/east_bank_2.csv')
west <- read.csv('C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/west_bank_2.csv')

west$start.time[west$start.time == 0] <- 24
east$start.time[east$start.time == 0] <- 24
summary(as.factor(west$zip.start))
summary(as.factor(east$zip.start))
east.south.zip=c(55455)
west.north.zip=c(55401,55402,55403)
west.north=subset(west,west$zip.start%in% west.north.zip)
library(dplyr)
west.south=anti_join(west, west.north)
east.south=subset(east,east$zip.start%in% east.south.zip)
east.north=anti_join(east, east.south)


dock.count.east.south=east.south[,c(10,17,21)]
sum(unique(dock.count.east.south)[,3])
dock.count.east.north=east.north[,c(10,17,21)]
sum(unique(dock.count.east.north)[,3])
dock.count.west.south=west.south[,c(10,17,21)]
sum(unique(dock.count.west.south)[,3])
dock.count.west.north=west.north[,c(10,17,21)]
sum(unique(dock.count.west.north)[,3])



alfa.generator<-function(period,data){
  x=24/period
  alfa=matrix(rep(0,4*period),nrow=4,ncol=period)
  east.south.zip=c(55455)
  west.north.zip=c(55401,55402,55403)
  east.zip=c(55101,55102,55103,55104,55105,55107,55108,55114,55116,55155,55418,55413,55414,55455,55421)

    for(i in 1:period){
      m=x*i-(x-1)
      n=x*i
      start=subset(data,data$start.time %in%c(m:n))
      end.in.east=subset(start,start$zip.end%in% east.zip)
      end.in.east.south=subset(end.in.east,end.in.east$zip.end%in% east.south.zip)
      end.in.east.north=subset(end.in.east,!(end.in.east$zip.end%in% east.south.zip))
      
      end.in.west=subset(start,!(start$zip.end%in% east.zip))
      end.in.west.north=subset(end.in.west,end.in.west$zip.end%in% west.north.zip)
      end.in.west.south=subset(end.in.west,!(end.in.west$zip.end%in% west.north.zip))
      alfa[1,i]=nrow(end.in.west.north)/nrow(start)
      alfa[2,i]=nrow(end.in.west.south)/nrow(start)
      alfa[3,i]=nrow(end.in.east.north)/nrow(start)
      alfa[4,i]=nrow(end.in.east.south)/nrow(start)
      
    }
  
 
  return (alfa) 
}

alfa.generator(3,data=west.north)
alfa.generator(3,data=west.south)

alfa.generator(3,data=east.north)
alfa.generator(3,data=east.south)
######demand
hist(west.north$start.time)
hist(east.south$start.time)
west.north$day <- as.factor(weekdays(as.Date(west.north$start.date)))
west.south$day <- as.factor(weekdays(as.Date(west.south$start.date)))
east.north$day <- as.factor(weekdays(as.Date(east.north$start.date)))
east.south$day <- as.factor(weekdays(as.Date(east.south$start.date)))


barplot(prop.table(table(west.north$day)))
barplot(prop.table(table(east.north$day)))

############demand.west

daily.demand.generator<-function(period,data){
  x=24/period
  date=as.character(unique(data$start.date))
  result=matrix(rep(0,(period+1)*length(date)),nrow=length(date),ncol=period+1)
  w=rep(0,length(date))
  result[,1]=date
  for(j in 1:period){
    for (k in 1: length(date)){
      m=x*j-(x-1)
      n=x*j
      w[k]=sum(data[data$start.date == date[k] & data$start.time<=n &data$start.time>=m,4])
    }
    result[,j+1]=w
    
  }
  return(result)
}

temp.west=daily.demand.generator(3,west.weekday)
mean(as.numeric(temp.west[,3]))
length(temp.west[,3])
ncol(temp.west)

boothstrap.generator<-function(period,data,n){
  demand=daily.demand.generator(period,data)
  for(j in 2:ncol(demand)){
    demand[,j] =as.numeric(demand[,j])
    boot.dist<-rep(NA,n)
    for (i in 1:n){
      k=length(demand[,j])
      boot.sample<-sample(as.numeric(demand[,j]),k,replace=TRUE)
      boot.dist[i]<-mean(boot.sample)
    }
    hist(boot.dist)
    print(c(mean(as.numeric(demand[,j])),sd(boot.dist)))
  }
}
boothstrap.generator(3,west.weekday,n=5000)
boothstrap.generator(3,west.weekend,n=5000)

#######demand.west.south
library(plyr)
west.south.temp=count(west.south, vars=c("start.date","start.time","day"))
weekend=c("Sunday","Saturday")
west.south.weekend=subset(west.south.temp,west.south.temp$day%in%weekend)
west.south.weekday=subset(west.south.temp,!(west.south.temp$day%in%weekend))
temp.west.south.weekday=daily.demand.generator(3,west.south.weekday)
temp.west.south.weekend=daily.demand.generator(3,west.south.weekend)
boothstrap.generator(3,west.south.weekday,n=5000)
boothstrap.generator(3,west.south.weekend,n=5000)

#######demand.west.north
library(plyr)
west.north.temp=count(west.north, vars=c("start.date","start.time","day"))
weekend=c("Sunday","Saturday")
west.north.weekend=subset(west.north.temp,west.north.temp$day%in%weekend)
west.north.weekday=subset(west.north.temp,!(west.north.temp$day%in%weekend))
temp.west.north.weekday=daily.demand.generator(3,west.north.weekday)
temp.west.north.weekend=daily.demand.generator(3,west.north.weekend)
boothstrap.generator(3,west.north.weekday,n=5000)
boothstrap.generator(3,west.north.weekend,n=5000)


#######demand.east.south
library(plyr)
east.south.temp=count(east.south, vars=c("start.date","start.time","day"))
weekend=c("Sunday","Saturday")
east.south.weekend=subset(east.south.temp,east.south.temp$day%in%weekend)
east.south.weekday=subset(east.south.temp,!(east.south.temp$day%in%weekend))
temp.east.south.weekday=daily.demand.generator(3,east.south.weekday)
temp.east.south.weekend=daily.demand.generator(3,east.south.weekend)
boothstrap.generator(3,east.south.weekday,n=5000)
boothstrap.generator(3,east.south.weekend,n=5000)

#######demand.east.north
library(plyr)
east.north.temp=count(east.north, vars=c("start.date","start.time","day"))
weekend=c("Sunday","Saturday")
east.north.weekend=subset(east.north.temp,east.north.temp$day%in%weekend)
east.north.weekday=subset(east.north.temp,!(east.north.temp$day%in%weekend))
temp.east.north.weekday=daily.demand.generator(3,east.north.weekday)
temp.east.north.weekend=daily.demand.generator(3,east.north.weekend)
boothstrap.generator(3,east.north.weekday,n=5000)
boothstrap.generator(3,east.north.weekend,n=5000)

######################trip dusrition time


duration.time.generator<-function(start){
  
  east.south.zip=c(55455)
  west.north.zip=c(55401,55402,55403)
  east.zip=c(55101,55102,55103,55104,55105,55107,55108,55114,55116,55155,55418,55413,55414,55455,55421)
  
  end.in.east=subset(start,start$zip.end%in% east.zip)
  end.in.east.south=subset(end.in.east,end.in.east$zip.end%in% east.south.zip)
  end.in.east.north=subset(end.in.east,!(end.in.east$zip.end%in% east.south.zip))
  
  end.in.west=subset(start,!(start$zip.end%in% east.zip))
  end.in.west.north=subset(end.in.west,end.in.west$zip.end%in% west.north.zip)
  end.in.west.south=subset(end.in.west,!(end.in.west$zip.end%in% west.north.zip))
  hist(log(end.in.east.south$tripduration/3600),n=300)
  abline(v=median(log(end.in.east.south$tripduration/3600)),col="red")
  hist(log(end.in.west.south$tripduration/3600),n=300)
  abline(v=median(log(end.in.west.south$tripduration/3600)),col="red")
  hist(log(end.in.east.north$tripduration/3600),n=300)
  abline(v=median(log(end.in.east.north$tripduration/3600)),col="red")
  hist(log(end.in.west.north$tripduration/3600),n=300)
  abline(v=median(log(end.in.west.north$tripduration/3600)),col="red")
  
  print(summary(end.in.east.south$tripduration/3600))
  print(summary(end.in.west.south$tripduration/3600))
  print(summary(end.in.east.north$tripduration/3600))
  print(summary(end.in.west.north$tripduration/3600))
  
}
duration.time.generator(west.south) 
duration.time.generator(east.south)
duration.time.generator(west.north) 
duration.time.generator(east.north)

#########demand.weekday
x1=data.frame(temp.east.north.weekday,temp.east.south.weekday,temp.west.north.weekday,temp.west.south.weekday)
write.csv(x1,'C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/weekday_demand.csv')
x2=data.frame(temp.east.north.weekend,temp.east.south.weekend,temp.west.north.weekend,temp.west.south.weekend)
write.csv(x2,'C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/weekend_demand.csv')



###############bike

route.generator<-function(period,data){
  x=24/period
  alfa=matrix(rep(0,4*period),nrow=4,ncol=period)
  east.south.zip=c(55455)
  west.north.zip=c(55401,55402,55403)
  east.zip=c(55101,55102,55103,55104,55105,55107,55108,55114,55116,55155,55418,55413,55414,55455,55421)
  
  for(i in 1:period){
    m=x*i-(x-1)
    n=x*i
    start=subset(data,data$start.time %in%c(m:n))
    end.in.east=subset(start,start$zip.end%in% east.zip)
    end.in.east.south=subset(end.in.east,end.in.east$zip.end%in% east.south.zip)
    end.in.east.north=subset(end.in.east,!(end.in.east$zip.end%in% east.south.zip))
    
    end.in.west=subset(start,!(start$zip.end%in% east.zip))
    end.in.west.north=subset(end.in.west,end.in.west$zip.end%in% west.north.zip)
    end.in.west.south=subset(end.in.west,!(end.in.west$zip.end%in% west.north.zip))
    alfa[1,i]=nrow(end.in.west.north)
    alfa[2,i]=nrow(end.in.west.south)
    alfa[3,i]=nrow(end.in.east.north)
    alfa[4,i]=nrow(end.in.east.south)
    
  }
  
  
  return (alfa) 
}

weekend=c("Sunday","Saturday")
east.north.weekend.long=subset(east.north,east.north$day%in%weekend)
east.north.weekday.long=subset(east.north,!(east.north$day%in%weekend))
route.generator(3,east.north.weekend.long)
route.generator(3,east.north.weekday.long)

east.south.weekend.long=subset(east.south,east.south$day%in%weekend)
east.south.weekday.long=subset(east.south,!(east.south$day%in%weekend))
route.generator(3,east.south.weekend.long)
route.generator(3,east.south.weekday.long)

west.north.weekend.long=subset(west.north,west.north$day%in%weekend)
west.north.weekday.long=subset(west.north,!(west.north$day%in%weekend))
route.generator(3,west.north.weekend.long)
route.generator(3,west.north.weekday.long)

west.south.weekend.long=subset(west.south,west.south$day%in%weekend)
west.south.weekday.long=subset(west.south,!(west.south$day%in%weekend))
route.generator(3,west.south.weekend.long)
route.generator(3,west.south.weekday.long)





###############graph

library(tidyverse)
library(ggmap)
library(mapview)
library(ggplot2)
library("ggmap")

plot.east=east[!duplicated(east$start.station.id), ]
plot.west=west[!duplicated(west$start.station.id), ]
#Set your API Key
ggmap::register_google(key = "AIzaSyDG32YYUgGFjZuXWKFHTnMbhapKfMDlmDY")

#Notes: If you get still have a failure then I suggest to restart R and run the library and register google commands again.




p + geom_point(
aes(x = plot.east$start.station.longitude, y =plot.east$start.station.latitude
,size= plot.east$start.station.dock),color="blue",data = plot.east,alpha = 1/5)+labs(size="dock size")+geom_point(
aes(x = plot.west$start.station.longitude, y =plot.west$start.station.latitude
,size = plot.west$start.station.dock),color="red",data = plot.west,alpha = 1/5)+labs(size="dock size")



east.zip=c(55101,55102,55103,55104,55105,55107,55108,55114,55116,55155,55418,55413,55414,55455,55421)
##The alfa_i_j_t: the prob that a customer orignating from i at perios t ends at j

map.heat.generator<-function(period,data){
  x=24/period
  p <- ggmap(get_googlemap(center = c(lon = -93.238667, lat = 44.973157),
                           zoom = 11, scale = 4,
                           maptype ='terrain',
                           color = 'color'))
  title=c("Period 1","Period 2","Period 3")
    for(i in 1:period){
      m=x*i-(x-1)
      n=x*i
      start=subset(data,data$start.time %in%c(m:n))
      end.in.east=subset(start,start$zip.end%in% east.zip)
      end.in.west=subset(start,!(start$zip.end%in% east.zip))
      print(p+geom_density2d(data = end.in.east, aes(x = end.in.east$end.station.longitude, y =end.in.east$end.station.latitude ), size = 0.3) +
        stat_density2d(data =end.in.east,aes(x = end.in.east$end.station.longitude, y =end.in.east$end.station.latitude , fill = ..level.., alpha = ..level..), size = 0.01,
                       bins = 10, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
        scale_alpha(range = c(0, 0.3), guide = FALSE)+ ggtitle(title[i]))
      print(p+geom_density2d(data = end.in.west, aes(x = end.in.west$end.station.longitude, y =end.in.west$end.station.latitude ), size = 0.3) +
              stat_density2d(data =end.in.west,aes(x = end.in.west$end.station.longitude, y =end.in.west$end.station.latitude , fill = ..level.., alpha = ..level..), size = 0.01,
                             bins = 10, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
              scale_alpha(range = c(0, 0.3), guide = FALSE)+ ggtitle(title[i]))
    }
  
}
map.heat.generator(3,west)

map3.heat.generator<-function(data1){
  peak.morning.time=c(5,6,7,8,9)
  peak.evening.time=c(15,16,17,18)
  p <- ggmap(get_googlemap(center = c(lon = -93.238667, lat = 44.973157),
                           zoom = 12, scale = 4,
                           maptype ='terrain',
                           color = 'color'))
  title=c("bike.peak.hour.morning","bike.peak.hour.evening","bike.off.peak")
  

    start1=subset(data1,data1$start.time %in%peak.morning.time)
    start3=subset(data1,data1$start.time %in%peak.evening.time)
    start2=subset(data1,!(data1$start.time %in%c(peak.evening.time,peak.morning.time)))
    print(p+geom_density2d(data = start1, aes(x = start1$start.station.longitude, y =start1$start.station.latitude ), size = 0.3) +
            stat_density2d(data =start1,aes(x =start1$start.station.longitude, y =start1$start.station.latitude , fill = ..level.., alpha = ..level..), size = 0.01,
                           bins = 50, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
            scale_alpha(range = c(0, 0.3), guide = FALSE)+ ggtitle(title[1]))
    print(p+geom_density2d(data = start2, aes(x = start2$start.station.longitude, y =start2$start.station.latitude ), size = 0.3) +
            stat_density2d(data =start2,aes(x = start2$start.station.longitude, y =start2$start.station.latitude , fill = ..level.., alpha = ..level..), size = 0.01,
                           bins = 50, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
            scale_alpha(range = c(0, 0.3), guide = FALSE)+ ggtitle(title[3]))
    print(p+geom_density2d(data = start3, aes(x = start3$start.station.longitude, y =start3$start.station.latitude ), size = 0.3) +
            stat_density2d(data =start3,aes(x = start3$start.station.longitude, y =start3$start.station.latitude , fill = ..level.., alpha = ..level..), size = 0.01,
                           bins = 50, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
            scale_alpha(range = c(0, 0.3), guide = FALSE)+ ggtitle(title[2]))
}
map3.heat.generator(data)

map4.heat.generator<-function(data1){
  peak.morning.time=c(5,6,7,8,9,10)
  peak.evening.time=c(15,16,17,18,19)
  p <- ggmap(get_googlemap(center = c(lon = -93.238667, lat = 44.973157),
                           zoom = 12, scale = 4,
                           maptype ='terrain',
                           color = 'color'))
  title=c("dock.peak.hour.morning","dock.peak.hour.evening","dock.off.peak")
  
  
  start1=subset(data1,data1$start.time %in%peak.morning.time)
  start3=subset(data1,data1$start.time %in%peak.evening.time)
  start2=subset(data1,!(data1$start.time %in%c(peak.evening.time,peak.morning.time)))
  print(p+geom_density2d(data = start1, aes(x = start1$end.station.longitude, y =start1$end.station.latitude ), size = 0.3) +
          stat_density2d(data =start1,aes(x =start1$end.station.longitude, y =start1$end.station.latitude , fill = ..level.., alpha = ..level..), size = 0.01,
                         bins = 50, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
          scale_alpha(range = c(0, 0.3), guide = FALSE)+ ggtitle(title[1]))
  print(p+geom_density2d(data = start2, aes(x = start2$end.station.longitude, y =start2$end.station.latitude ), size = 0.3) +
          stat_density2d(data =start2,aes(x = start2$end.station.longitude, y =start2$end.station.latitude , fill = ..level.., alpha = ..level..), size = 0.01,
                         bins = 50, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
          scale_alpha(range = c(0, 0.3), guide = FALSE)+ ggtitle(title[3]))
  print(p+geom_density2d(data = start3, aes(x = start3$end.station.longitude, y =start3$end.station.latitude ), size = 0.3) +
          stat_density2d(data =start3,aes(x = start3$end.station.longitude, y =start3$end.station.latitude , fill = ..level.., alpha = ..level..), size = 0.01,
                         bins = 50, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
          scale_alpha(range = c(0, 0.3), guide = FALSE)+ ggtitle(title[2]))
}
map4.heat.generator(data)

