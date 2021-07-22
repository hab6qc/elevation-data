#### Build graph corresponding to 24 x 24 grid

ele <- read.table("elevation.dat",header=TRUE)
Lon <- colnames(ele)
Lon <- unlist(lapply(Lon,function(x)gsub("X.","",x)))
names(ele) <- Lon

ele.Lon <- as.numeric(Lon)
ele.Lat <- as.numeric(rownames(ele))

metric.Lon <- scan("Lon.txt",what="",sep="\t")
metric.Lon <- unlist(lapply(metric.Lon,function(x) gsub("W","",x)))
metric.Lon <- as.numeric(metric.Lon)

metric.Lat <- scan("Lat.txt",what="",sep="\n")
metric.Lat <- unlist(lapply(metric.Lat,function(x){
    x <- gsub("N","",x)
    x <- gsub("S","",x)
})                            )
metric.Lat <- as.numeric(metric.Lat)
metric.Lat[16:24] <- -metric.Lat[16:24]

approx.elevation <- matrix(0,24,24)

for(i in 1:24){
    lat <- metric.Lat[i]
    dist.seq <- abs(ele.Lat-lat)
    lat.index <- which.min(dist.seq)

    for(j in 1:24){
        lon <- metric.Lon[j]
        dist.seq <- abs(ele.Lon-lon)
        lon.index <- which.min(dist.seq)
        approx.elevation[i,j] <- ele[lat.index,lon.index]
    }
}

background<-image(log(t(approx.elevation[24:1,]+1)))

image(log(approx.elevation+1))

elevation <- approx.elevation

save(elevation,file="ApproxElevation.Rda")
load("ApproxElevation.Rda")


m <- 24
Adj <- matrix(0,m^2,m^2)

grid.to.graph <- function(i,j,m){
	return((j-1)*m + i)
}
graph.to.grid <- function(v,m){
	i <- v%%m
	j <- (v-i)/m + 1
	if(i==0) i<- 24
	return(c(i,j))
}

for(i in 1:m){
	for(j in 1:m){
		v <- grid.to.graph(i,j,m)
		if(i>1){
			v.upper <- grid.to.graph(i-1,j,m)
			Adj[v,v.upper] <- 1
		}
		if(i<m){
			v.lower <- grid.to.graph(i+1,j,m)
			Adj[v,v.lower] <- 1
		}
		if(j>1){
			v.left <- grid.to.graph(i,j-1,m)
			Adj[v,v.left] <- 1
		}
		if(j<m){
			v.right <- grid.to.graph(i,j+1,m)
			Adj[v,v.right] <- 1
		}
	}
}



GridTimeSeries <- list()

for(t in 1:72){
##pressure <- read.table(paste("pressure",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
## pressure <- as.numeric(as.matrix(pressure[,-(1:3)]))

ozone <- read.table(paste("ozone",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
ozone <- as.numeric(as.matrix(ozone[,-(1:3)]))


## surftemp <- read.table(paste("surftemp",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
## surftemp <- as.numeric(as.matrix(surftemp[,-(1:3)]))

## cloudhigh <- read.table(paste("cloudhigh",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
## cloudhigh <- as.numeric(as.matrix(cloudhigh[,-(1:3)]))


## cloudmid <-  read.table(paste("cloudmid",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
## cloudmid <- as.numeric(as.matrix(cloudmid[,-(1:3)]))

## cloudlow <- read.table(paste("cloudlow",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
## cloudlow <- as.numeric(as.matrix(cloudlow[,-(1:3)]))

## temperature <- read.table(paste("temperature",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
## temperature <- as.numeric(as.matrix(temperature[,-(1:3)]))



## load("ApproxElevation.Rda")
## elevation <- as.numeric(elevation)

## X <- data.frame(pressure=pressure,ozone=ozone,surftemp=surftemp,cloudhigh=cloudhigh,cloudmid=cloudmid,cloudlow=cloudlow,temperature=temperature,elevation=elevation)
X <- data.frame(ozone=ozone)

with.missing <- sum(is.na(X)) > 0

GridTimeSeries[[t]] <- list(X=X)
}

Latitude<-rep(metric.Lat,times=24)
Longitude<-rep(-metric.Lon,each=24)
save(GridTimeSeries,file="NASAGridTimeSeries.Rda")

GridTimeSeries[[1]]

January<-data.frame(Latitude,Longitude,GridTimeSeries[[1]],GridTimeSeries[[13]],GridTimeSeries[[25]],GridTimeSeries[[37]],GridTimeSeries[[49]],GridTimeSeries[[61]])
January_means<-data.frame(Latitude=January[,1],Longitude=January[,2], Means=rowMeans(January[,-1:-2]))
February<-data.frame(Latitude,Longitude,GridTimeSeries[[2]],GridTimeSeries[[14]],GridTimeSeries[[26]],GridTimeSeries[[38]],GridTimeSeries[[50]],GridTimeSeries[[62]])
February_means<-data.frame(Latitude=February[,1],Longitude=February[,2], Means=rowMeans(February[,-1:-2]))
March<-data.frame(Latitude,Longitude,GridTimeSeries[[3]],GridTimeSeries[[15]],GridTimeSeries[[27]],GridTimeSeries[[39]],GridTimeSeries[[51]],GridTimeSeries[[63]])
March_means<-data.frame(Latitude=March[,1],Longitude=March[,2], Means=rowMeans(March[,-1:-2]))
April<-data.frame(Latitude,Longitude,GridTimeSeries[[4]],GridTimeSeries[[16]],GridTimeSeries[[28]],GridTimeSeries[[40]],GridTimeSeries[[52]],GridTimeSeries[[64]])
April_means<-data.frame(Latitude=April[,1],Longitude=April[,2], Means=rowMeans(April[,-1:-2]))
May<-data.frame(Latitude,Longitude,GridTimeSeries[[5]],GridTimeSeries[[17]],GridTimeSeries[[29]],GridTimeSeries[[41]],GridTimeSeries[[53]],GridTimeSeries[[65]])
May_means<-data.frame(Latitude=May[,1],Longitude=May[,2], Means=rowMeans(May[,-1:-2]))
June<-data.frame(Latitude,Longitude,GridTimeSeries[[6]],GridTimeSeries[[18]],GridTimeSeries[[30]],GridTimeSeries[[42]],GridTimeSeries[[54]],GridTimeSeries[[66]])
June_means<-data.frame(Latitude=June[,1],Longitude=June[,2], Means=rowMeans(June[,-1:-2]))
July<-data.frame(Latitude,Longitude,GridTimeSeries[[7]],GridTimeSeries[[19]],GridTimeSeries[[31]],GridTimeSeries[[43]],GridTimeSeries[[55]],GridTimeSeries[[67]])
July_means<-data.frame(Latitude=July[,1],Longitude=July[,2], Means=rowMeans(July[,-1:-2]))
August<-data.frame(Latitude,Longitude,GridTimeSeries[[8]],GridTimeSeries[[20]],GridTimeSeries[[32]],GridTimeSeries[[44]],GridTimeSeries[[56]],GridTimeSeries[[68]])
August_means<-data.frame(Latitude=August[,1],Longitude=August[,2], Means=rowMeans(August[,-1:-2]))
September<-data.frame(Latitude,Longitude,GridTimeSeries[[9]],GridTimeSeries[[21]],GridTimeSeries[[33]],GridTimeSeries[[45]],GridTimeSeries[[57]],GridTimeSeries[[69]])
September_means<-data.frame(Latitude=September[,1],Longitude=September[,2], Means=rowMeans(September[,-1:-2]))
October<-data.frame(Latitude,Longitude,GridTimeSeries[[10]],GridTimeSeries[[22]],GridTimeSeries[[34]],GridTimeSeries[[46]],GridTimeSeries[[58]],GridTimeSeries[[70]])
October_means<-data.frame(Latitude=October[,1],Longitude=October[,2], Means=rowMeans(October[,-1:-2]))
November<-data.frame(Latitude,Longitude,GridTimeSeries[[11]],GridTimeSeries[[23]],GridTimeSeries[[35]],GridTimeSeries[[47]],GridTimeSeries[[59]],GridTimeSeries[[71]])
November_means<-data.frame(Latitude=November[,1],Longitude=November[,2], Means=rowMeans(November[,-1:-2]))
December<-data.frame(Latitude,Longitude,GridTimeSeries[[12]],GridTimeSeries[[24]],GridTimeSeries[[36]],GridTimeSeries[[48]],GridTimeSeries[[60]],GridTimeSeries[[72]])
December_means<-data.frame(Latitude=December[,1],Longitude=December[,2], Means=rowMeans(December[,-1:-2]))


ggplot() +
  geom_polygon(data = ele, aes(x=Longitude, y = Latitude), fill="grey", alpha=0.4) +
  geom_point( data = January_means, aes(x=Longitude,y=Latitude,color=Means)) +
  theme_void() 

map <- get_stamenmap( bbox = c(left = -113.8, bottom = -21.2, right = -56.2, top = 36.2), zoom = 1, maptype = "watercolor")
ggmap(map) +
  theme_void() +
  theme(
    plot.title = element_text(colour = "orange"),
    panel.border = element_rect(colour = "grey", fill=NA, size=2)
  )


min(metric.Lat)

library(ggplot2)
library(viridis)

test_data = read.csv("test.csv", sep = "")

map <- get_stamenmap( bbox = c(left = -113.8, bottom = -21.2, right = -56.2, top = 36.2), zoom = 0, style = 'feature:administrative.continent|element:labels|visibility:off',color="bw",maptype="toner-lite")


January_map<-ggmap(map) + geom_raster(data = January_means, aes(x=Longitude, y = Latitude, fill=Means)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.7) +
  theme_void() + ggtitle("January")+theme(legend.position = "none",plot.title = element_text(size=25)) 

Febuary_map<-ggmap(map) + geom_raster(data = February_means, aes(x=Longitude, y = Latitude, fill=Means)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.7) +
  theme_void() + ggtitle("February")+theme(legend.position = "none",plot.title = element_text(size=25)) 

March_map<-ggmap(map) + geom_raster(data = March_means, aes(x=Longitude, y = Latitude, fill=Means)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.7) +
  theme_void() + ggtitle("March")+theme(legend.position = "none",plot.title = element_text(size=25)) 

April_map<-ggmap(map) + geom_raster(data = April_means, aes(x=Longitude, y = Latitude, fill=Means)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.7) +
  theme_void() + ggtitle("April") +theme(legend.position = "none",plot.title = element_text(size=25)) 

May_map<-ggmap(map) + geom_raster(data = May_means, aes(x=Longitude, y = Latitude, fill=Means)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.7) +
  theme_void() + ggtitle("May") +theme(legend.position = "none",plot.title = element_text(size=25)) 

June_map<-ggmap(map) + geom_raster(data = June_means, aes(x=Longitude, y = Latitude, fill=Means)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.7) +
  theme_void() + ggtitle("June") +theme(legend.position = "none",plot.title = element_text(size=25)) 

July_map<-ggmap(map) + geom_raster(data = July_means, aes(x=Longitude, y = Latitude, fill=Means)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.7) +
  theme_void()+ ggtitle("July") +theme(legend.position = "none",plot.title = element_text(size=25)) 

August_map<-ggmap(map) + geom_raster(data = August_means, aes(x=Longitude, y = Latitude, fill=Means)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.7) +
  theme_void() + ggtitle("August") +theme(legend.position = "none",plot.title = element_text(size=25)) 

Sept_map<-ggmap(map) + geom_raster(data = September_means, aes(x=Longitude, y = Latitude, fill=Means)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.7) +
  theme_void() + ggtitle("September") +theme(legend.position = "none",plot.title = element_text(size=25)) 

Oct_map<-ggmap(map) + geom_raster(data = October_means, aes(x=Longitude, y = Latitude, fill=Means)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.7) +
  theme_void() + ggtitle("October") +theme(legend.position = "none",plot.title = element_text(size=25)) 

Nov_map<-ggmap(map) + geom_raster(data = November_means, aes(x=Longitude, y = Latitude, fill=Means)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.7) +
  theme_void() + ggtitle("November") +theme(legend.position = "none",plot.title = element_text(size=25)) 

Dec_map<-ggmap(map) + geom_raster(data = December_means, aes(x=Longitude, y = Latitude, fill=Means)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.7) +
  theme_void() + ggtitle("December") +theme(plot.title = element_text(size=25))




ggmap(map) + geom_point(data=January_means, aes(x=Longitude, y=Latitude, size=Means, color=Means, alpha=Means), shape=20, stroke=FALSE) + 
  scale_size_continuous(name="Ozone abundance", trans="log", range=c(1,12), breaks=mybreaks) +
  scale_alpha_continuous(name="Ozone abundance", trans="log", range=c(0.1, .9), breaks=mybreaks) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks, name="Ozone Abundance" ) +
  theme_void()   + theme(legend.position="none")


trend1995<-data.frame(Latitude,Longitude,GridTimeSeries[[1]],GridTimeSeries[[2]],GridTimeSeries[[3]],GridTimeSeries[[4]],GridTimeSeries[[5]],GridTimeSeries[[6]],GridTimeSeries[[7]],GridTimeSeries[[8]],GridTimeSeries[[9]],GridTimeSeries[[10]],GridTimeSeries[[11]],GridTimeSeries[[12]])
trend1995_means<-data.frame(Latitude=trend1995[,1],Longitude=trend1995[,2], Means=rowMeans(trend1995[,-1:-2]))
trend1996<-data.frame(Latitude,Longitude,GridTimeSeries[[13]],GridTimeSeries[[14]],GridTimeSeries[[15]],GridTimeSeries[[16]],GridTimeSeries[[17]],GridTimeSeries[[18]],GridTimeSeries[[19]],GridTimeSeries[[20]],GridTimeSeries[[21]],GridTimeSeries[[22]],GridTimeSeries[[23]],GridTimeSeries[[24]])
trend1996_means<-data.frame(Latitude=trend1996[,1],Longitude=trend1996[,2], Means=rowMeans(trend1996[,-1:-2]))
trend1997<-data.frame(Latitude,Longitude,GridTimeSeries[[25]],GridTimeSeries[[26]],GridTimeSeries[[27]],GridTimeSeries[[28]],GridTimeSeries[[29]],GridTimeSeries[[30]],GridTimeSeries[[31]],GridTimeSeries[[32]],GridTimeSeries[[33]],GridTimeSeries[[34]],GridTimeSeries[[35]],GridTimeSeries[[36]])
trend1997_means<-data.frame(Latitude=trend1997[,1],Longitude=trend1997[,2], Means=rowMeans(trend1997[,-1:-2]))
trend1998<-data.frame(Latitude,Longitude,GridTimeSeries[[37]],GridTimeSeries[[38]],GridTimeSeries[[39]],GridTimeSeries[[40]],GridTimeSeries[[41]],GridTimeSeries[[42]],GridTimeSeries[[43]],GridTimeSeries[[44]],GridTimeSeries[[45]],GridTimeSeries[[46]],GridTimeSeries[[47]],GridTimeSeries[[48]])
trend1998_means<-data.frame(Latitude=trend1998[,1],Longitude=trend1998[,2], Means=rowMeans(trend1998[,-1:-2]))
trend1999<-data.frame(Latitude,Longitude,GridTimeSeries[[49]],GridTimeSeries[[50]],GridTimeSeries[[51]],GridTimeSeries[[52]],GridTimeSeries[[53]],GridTimeSeries[[54]],GridTimeSeries[[55]],GridTimeSeries[[56]],GridTimeSeries[[57]],GridTimeSeries[[58]],GridTimeSeries[[59]],GridTimeSeries[[60]])
trend1999_means<-data.frame(Latitude=trend1999[,1],Longitude=trend1999[,2], Means=rowMeans(trend1999[,-1:-2]))
trend2000<-data.frame(Latitude,Longitude,GridTimeSeries[[61]],GridTimeSeries[[62]],GridTimeSeries[[63]],GridTimeSeries[[64]],GridTimeSeries[[65]],GridTimeSeries[[66]],GridTimeSeries[[67]],GridTimeSeries[[68]],GridTimeSeries[[69]],GridTimeSeries[[70]],GridTimeSeries[[71]],GridTimeSeries[[72]])
trend2000_means<-data.frame(Latitude=trend2000[,1],Longitude=trend2000[,2], Means=rowMeans(trend2000[,-1:-2]))
all_means<-data.frame(Latitude=trend1995[,1],Longitude=trend1995[,2],Means1995=rowMeans(trend1995[,-1:-2]),Means1996=rowMeans(trend1996[,-1:-2]),Means1997=rowMeans(trend1997[,-1:-2]),Means1998=rowMeans(trend1998[,-1:-2]),Means1999=rowMeans(trend1999[,-1:-2]),Means2000=rowMeans(trend2000[,-1:-2]))
slopes<-data.frame(Latitude=trend1995[,1],Longitude=trend1995[,2],Slope95_96=(all_means[,4]-all_means[,3]),Slope96_97=(all_means[,5]-all_means[,4]),Slope97_98=(all_means[,6]-all_means[,5]),Slope98_99=(all_means[,7]-all_means[,6]),Slope99_00=(all_means[,8]-all_means[,7]))


mybreaks <- c(260, 275, 290,305 , 320)

map1995<-ggmap(map)+geom_point(data=all_means,aes(x=Longitude,y=Latitude,size=Means1995,color=Means1995,alpha=Means1995))+ggtitle("1995")
map1996<-ggmap(map)+geom_point(data=all_means,aes(x=Longitude,y=Latitude,size=Means1996,color=Means1996,alpha=Means1996))+ggtitle("1996")
map1997<-ggmap(map)+geom_point(data=all_means,aes(x=Longitude,y=Latitude,size=Means1997,color=Means1997,alpha=Means1997))+ggtitle("1997")
map1998<-ggmap(map)+geom_point(data=all_means,aes(x=Longitude,y=Latitude,size=Means1998,color=Means1998,alpha=Means1998))+ggtitle("1998")
map1999<-ggmap(map)+geom_point(data=all_means,aes(x=Longitude,y=Latitude,size=Means1999,color=Means1999,alpha=Means1999))+ggtitle("1999")
map2000<-ggmap(map)+geom_point(data=all_means,aes(x=Longitude,y=Latitude,size=Means2000,color=Means2000,alpha=Means2000))+ggtitle("2000")

mybreaks<-c(-10,-5,0,5,10,15)

map1995_96<-ggmap(map)+geom_raster(data = slopes, aes(x=Longitude, y = Latitude, fill=Slope95_96)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.65,limits=c(-10,10)) +
  theme_void() + ggtitle("Trend between 1995-1996") +theme(plot.title = element_text(size=25))


map1996_97<-ggmap(map)+geom_raster(data = slopes, aes(x=Longitude, y = Latitude, fill=Slope96_97)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.65,limits=c(-10,10)) +
  theme_void() + ggtitle("Trend between 1996-1997") +theme(plot.title = element_text(size=25))


map1997_98<-ggmap(map)+geom_raster(data = slopes, aes(x=Longitude, y = Latitude, fill=Slope97_98)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.65,limits=c(-10,10)) +
  theme_void() + ggtitle("Trend between 1997-1998") +theme(plot.title = element_text(size=25))

map1998_99<-ggmap(map)+geom_raster(data = slopes, aes(x=Longitude, y = Latitude, fill=Slope98_99)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.65,limits=c(-10,10)) +
  theme_void() + ggtitle("Trend between 1998-1999") +theme(plot.title = element_text(size=25))

map1999_00<-ggmap(map)+geom_raster(data = slopes, aes(x=Longitude, y = Latitude, fill=Slope99_00)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1,alpha=0.65,limits=c(-10,10)) +
  theme_void() + ggtitle("Trend between 1999-2000") +theme(plot.title = element_text(size=25))








