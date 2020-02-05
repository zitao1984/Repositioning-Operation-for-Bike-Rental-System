data <- read.csv('C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/final_time.csv')
dock<-read.csv('C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Nice_Ride_2017_Station_Locations.csv')
dock=rename(dock, c("Name"="start.station.name"))
data <- merge(data,dock[,c(2,5)],by=c("start.station.name"))
summary(as.factor(data$Total.docks))
data=rename(data, c("Total.docks"="start.station.dock"))
dock=rename(dock, c("start.station.name"="end.station.name"))
data <- merge(data,dock[,c(2,5)],by=c("end.station.name"))
summary(as.factor(data$Total.docks))
data=rename(data, c("Total.docks"="end.station.dock"))

data=data[,-c(1,2)]
summary(data$zip.start)
data$zip.start=as.character(data$zip.start)
data$zip.start=substr(data$zip.start,4,9)
data$zip.end=substr(data$zip.end,4,9)
data$zip.start=as.numeric(data$zip.start)
data$zip.end=as.numeric(data$zip.end)
east=c(55101,55102,55103,55104,55105,55107,55108,55114,55116,55155,55418,55413,55414,55455,55421)
east.bank.2 <- data[data$zip.start %in% east,]
west.bank.2<-data[!(data$zip.start%in% east),]
summary(east.bank.2 $zip.start)
summary(west.bank.2 $zip.start)
write.csv(east.bank.2[,-c(1:2)],'C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/east_bank_2.csv')
write.csv(west.bank.2[,-c(1:2)],'C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/west_bank_2.csv')
############################################ 3 period

east <- read.csv('C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/east_bank_2.csv')
west <- read.csv('C:/Users/zitao_000/Dropbox/homework/IE 8552 operation and supply chain/IE inventory project/Data/west_bank_2.csv')

west$start.time[west$start.time == 0] <- 24
east$start.time[east$start.time == 0] <- 24
T=3
east.zip=c(55101,55102,55103,55104,55105,55107,55108,55114,55116,55155,55418,55413,55414,55455,55421)
##The alfa_i_j_t: the prob that a customer orignating from i at perios t ends at j

alfa.generator<-function(period,data,position="west"){
  x=24/period
  alfa=matrix(rep(0,2*period),nrow=2,ncol=period)
  if( position == "west"){##west origin
  for(i in 1:period){
    m=x*i-(x-1)
    n=x*i
    start=subset(data,data$start.time %in%c(m:n))
    end.in.east=subset(start,start$zip.end%in% east.zip)
    end.in.west=subset(start,!(start$zip.end%in% east.zip))
    alfa[1,i]=nrow(end.in.west)/nrow(start)
    alfa[2,i]=nrow(end.in.east)/nrow(start)

    }
  }
  else{###east origin
    for(i in 1:period){
      m=x*i-(x-1)
      n=x*i
    start=subset(data,data$start.time %in%c(m:n))
     end.in.east=subset(start,start$zip.end%in% east.zip)
     end.in.west=subset(start,!(start$zip.end%in% east.zip))
     alfa[2,i]=nrow(end.in.west)/nrow(start)
     alfa[1,i]=nrow(end.in.east)/nrow(start)
    }
    
  }
  return (alfa) 
  }


alfa.generator(3,data=west)
alfa.generator(3,data=east,position="east")
######demand
hist(west$start.time)
hist(east$start.time)
west$day <- weekdays(as.Date(west$start.date))
east$day <- weekdays(as.Date(east$start.date))
west$day=as.factor(west$day)
east$day=as.factor(east$day)

barplot(prop.table(table(west$day)))
barplot(prop.table(table(east$day)))

############demand.west
library(plyr)
west.temp=count(west, vars=c("start.date","start.time","day"))
weekend=c("Sunday","Saturday")
west.weekend=subset(west.temp,west.temp$day%in%weekend)
west.weekday=subset(west.temp,!(west.temp$day%in%weekend))




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
##################demand.east
east.temp=count(east, vars=c("start.date","start.time","day"))
weekend=c("Sunday","Saturday")
east.weekend=subset(east.temp,east.temp$day%in%weekend)
east.weekday=subset(east.temp,!(east.temp$day%in%weekend))
boothstrap.generator(3,east.weekday,n=5000)
boothstrap.generator(3,east.weekend,n=5000)


######################trip dusrition time
east.zip=c(55101,55102,55103,55104,55105,55107,55108,55114,55116,55155,55418,55413,55414,55455,55421)

duration.time.generator<-function(data){



      end.in.east=subset(data,data$zip.end%in% east.zip)
      end.in.west=subset(data,!(data$zip.end%in% east.zip))
     hist(log(end.in.east$tripduration/3600),n=300)
     abline(v=median(log(end.in.east$tripduration/3600)),col="red")
      hist(log(end.in.west$tripduration/3600),n=300)
      abline(v=median(log(end.in.west$tripduration/3600)),col="red")
      
      print(summary(end.in.east$tripduration/3600))
      print(summary(end.in.west$tripduration/3600))
 
}
duration.time.generator(west) 
duration.time.generator(east) 
summary(west$tripduration/3600)
