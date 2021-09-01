

################################
#Is Brazil reaching its limit of rainfed agriculture?
#Ludmila Rattis et al
#Jul 2021
#lrattis@woodwellclimate.org
################################

##---------------------------------------------------------------------------------##
#----------------------------------------------------------

#----------------------------------------------------------
###loading packages
library(rgdal)
library(SPEI)
library(raster)
library(tidyverse)
library(manipulate)
library(sp)
library(grDevices)
library(rgdal)

#----------------------------------------------------------


#dir.models = "D:/Faraca_Copy/RAW_DATA/CMIP5_AVG_PB/"
dir.models =  setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/Dados/Clima/CMIP5_AVG_PB/")

i.rcp45T <- brick(paste0(dir.models,"/", 'rcp45', "/tas", "/", 'multimodel_mean_rcp45_tas.nc'))
i.rcp45P <- brick(paste0(dir.models,"/", 'rcp45', "/pr", "/", 'multimodel_mean_rcp45_pr.nc'))
extent(i.rcp45T)=extent(i.rcp45P) = c(xmin = 271 - 360, xmax = 335 - 360, ymin = -44.5, ymax = 11.5)

i.rcp85T <- brick(paste0(dir.models,"/", 'rcp85', "/tas", "/", 'multimodel_mean_rcp85_tas.nc'))
i.rcp85P <- brick(paste0(dir.models,"/", 'rcp85', "/pr", "/", 'multimodel_mean_rcp85_pr.nc'))
extent(i.rcp85T)=extent(i.rcp85P) = c(xmin = 271 - 360, xmax = 335 - 360, ymin = -44.5, ymax = 11.5)





#----------------------------------------------------------

#----------------------------------------------------------

setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/3 - spera")
ocupa = readOGR('limit_latlong.shp')
limit = ocupa
limit.states = readOGR("spera_states_latlong.shp");plot(limit.states)

#----------------------------------------------------------

#----------------------------------------------------------
#recorte dos modelos para as AP's ocupadas

i.rcp45P.am <- crop(i.rcp45P,ocupa)*30;plot(i.rcp45P.am[[1:6]]) 
i.rcp85P.am <- crop(i.rcp85P,ocupa)*30;plot(i.rcp85P.am[[1:6]])

i.rcp45P.am <- mask(i.rcp45P.am,ocupa);plot(i.rcp45P.am[[1:6]]) 
i.rcp85P.am <- mask(i.rcp85P.am,ocupa);plot(i.rcp85P.am[[1:6]])


i.rcp45T.am <- crop(i.rcp45T,ocupa);plot(i.rcp45T.am[[1:6]]) 
i.rcp85T.am <- crop(i.rcp85T,ocupa);plot(i.rcp85T.am[[1:6]])

i.rcp45T.am <- mask(i.rcp45T.am,ocupa);plot(i.rcp45T.am[[1:6]]) 
i.rcp85T.am <- mask(i.rcp85T.am,ocupa);plot(i.rcp85T.am[[1:6]])



########
##Latitude
coord_amaz<-coordinates(i.rcp45P.am)
LAT_AMAZ<-coord_amaz[,2]
#LAT_raster<-raster(nrow=90,ncol=120,xmn=-89,xmx= -29,ymn= -33.5,ymx= 11.5)
#LAT_raster<-raster(nrow=90,ncol=120,xmn=-89,
#                   xmx= -29,ymn= -33.5,ymx= 11.5)
LAT_raster <- raster(nrow=30,ncol=38,xmn=-61.5,
                     xmx= -42.5, ymn= -19.5,ymx= -4.5)

LAT_r<-setValues(LAT_raster,LAT_AMAZ)

climate45.all = stack(i.rcp45P.am,i.rcp45T.am,LAT_r)
climate85.all = stack(i.rcp85P.am,i.rcp85T.am,LAT_r)

####function
#################
PET.lud = function(RAIN,TMP, LAT){
  PET=thornthwaite(TMP,LAT,na.rm=TRUE)
  # spei.temp= spei(RAIN-PET, scale=3,na.rm=TRUE,
  #                 ref.start=c(1961,9),ref.end=c(1990,7)) #add ref.start
  # spei.final=as.vector(spei.temp[[2]])
  return(PET)}

library(SPEI)
#library(raster)
library(rasterVis)
library(maptools)
library(ggplot2)
#library(rgdal)


PET.85 = calc(climate85.all,function(x){PET.lud(as.vector(x[1:1800]),
                                                as.vector(x[1801:3600]),
                                                LAT=x[3601])})

input.85 = i.rcp85P.am - PET.85

PET.45 = calc(climate45.all,function(x){PET.lud(as.vector(x[1:1800]),
                                                as.vector(x[1801:3600]),
                                                LAT=x[3601])})

input.45 = i.rcp45P.am - PET.45


names(PET.45) = unlist(namesL)

PET.45_comp = PET.45[[217:816]]

###########################
#
#
## CWD per season
months = c('jan_','feb_','mar_','apr_',
           'may_','jun_','jul_','aug_',
           'sep_','oct_','nov_','dec_')
years = 1950:2099

namesL = list()
for(i in 1: length(years)){
  namesL[[i]] = paste0(months,
                       years[i])}

names(input.45) = unlist(namesL)
names(input.85) = unlist(namesL)



####
####
############
#crops 2016
############

setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/3 - spera/spera_latlong")
#setwd("~/Dropbox/IPAM/climate_agriculture/analises/3 - spera")
spera = stack(list.files('.','.tif')[1:16]);spera
spera
rcl3=matrix(c(NA,0 ,1,4,99,6 ,2,5 ,11,13,
              NA,NA,1,1,NA,1,1,1,NA,NA),ncol=2);rcl3
agri16 = reclassify(spera[[16]],rcl3) 


fun.lud3 <- function(x,y) { x[y!=1] <- NA; return(x) }
agri16.res = aggregate(agri16,180,method='ngb')
agri16.res
plot(agri16.res)

#####
#####
#####

##CWD for the pre-growing season - 45

j = seq(7,1800,12)
a = seq(8,1800,12)
seqL = sort(c(j,a));head(seqL)


#####
#####
#####

names(input.85) = unlist(namesL)


####
####
####

##CWD for the growing season
full_seq = 1:1800
j = seq(7,1800,12)
a = seq(8,1800,12)

seqL = sort(c(j,a));head(seqL)

growing_indices = full_seq[! full_seq %in% seqL] 

m.grow.45 = input.45[[growing_indices]]
m.grow.85 = input.85[[growing_indices]]

m.grow.45 = m.grow.45[[7:1496]] #from Sep 1950 to jun 2099 
m.grow.85 = m.grow.85[[7:1496]]



seqA = list()
for(i in 1:149){seqA[[i]] = c(rep(i,10),rep(0,2))}
seqA2 = c(rep(0,8),unlist(seqA),rep(0,4));length(seqA2)



# MCWD Function
MCWDin.fun <- function(x) {
  
  mt <- c(rep(1:10,times=2))
  cm <- which.max(x) # max of the year is considered that the soil is saturated
  
  
  if(length(cm)==0){
    
    return(NA)
    
  } else {
    if(cm==1){cm=11}
    cwd <- c(rep(0, times=10))
    
    for (i in cm:(cm+9)){
      
      cwd[mt[i]] <- cwd[mt[i-1]] + x[mt[i]]
      ifelse(cwd[mt[i]]>0, cwd[mt[i]] <- 0,cwd[mt[i]] <- cwd[mt[i]])
    }
    return(min(cwd))
  }
}

fun.MCWD <- function(x) {
  z<-matrix(x, ncol=10, byrow=10) 
  mcwd <- apply(z,1,FUN=MCWDin.fun)
  return(mcwd)
}


# Applying the Function
wd.growing.45 <-calc(m.grow.45,function(x){fun.MCWD(x)})

wd.growing.85 <-calc(m.grow.85,function(x){fun.MCWD(x)})

names(wd.growing.45) = paste0("cwd_45_",1951:2099)

names(wd.growing.85) = paste0("cwd_85_",1951:2099)

wd.growing.dec.45 = stackApply(wd.growing.45,
                               indices = c(rep(1:14,each=10),rep(15,each = 9)),
                               fun=mean)

wd.growing.dec.85 = stackApply(wd.growing.85,
                               indices = c(rep(1:14,each=10),rep(15,each = 9)),
                               fun=mean)
names(wd.growing.dec.45) = paste0('decade_',seq(1950,2090,10))
names(wd.growing.dec.85) = paste0('decade_',seq(1950,2090,10))

levelplot(wd.growing.dec.45)
levelplot(wd.growing.dec.85)

####
####
####


## precipitation for the growing season
ppt.growing.45 = i.rcp45P.am[[growing_indices]]
ppt.growing.85 = i.rcp85P.am[[growing_indices]]

ppt.growing.45 = ppt.growing.45[[7:1496]]
ppt.growing.85 = ppt.growing.85[[7:1496]]


ppt.growing.yearly.45 = stackApply(ppt.growing.45,
                                   indices = rep(1:149,each=10),
                                   fun=sum)

ppt.growing.yearly.85 = stackApply(ppt.growing.85,
                                   indices = rep(1:149,each=10),
                                   fun=sum)

ppt.growing.dec.45 = stackApply(ppt.growing.yearly.45,
                                indices = c(rep(1:14,each=10),rep(15,each = 9)),
                                fun=mean)

ppt.growing.dec.85 = stackApply(ppt.growing.yearly.85,
                                indices = c(rep(1:14,each=10),rep(15,each = 9)),
                                fun=mean)

names(ppt.growing.dec.45) = paste0('decade_',seq(1950,2090,10))
names(ppt.growing.dec.85) = paste0('decade_',seq(1950,2090,10))

ppt.growing.dec.45 = mask(ppt.growing.dec.45,ocupa)
ppt.growing.dec.85 = mask(ppt.growing.dec.85,ocupa)

levelplot(ppt.growing.dec.45)
levelplot(ppt.growing.dec.85)


####
####
####

dir.fun <- 'C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/future/'
source(paste0(dir.fun,"bivariate_map_bioclimate3.R"))
name='Brazil - 1950-2099 - 19112020'

timep = c('spera_1950-1959','spera_1960-1969','spera_1970-1979',
          'spera_1980-1989','spera_1990-1999','spera_2000-2009',
          'spera_2010-2019','spera_2020:2029',
          'spera_2030:2039','spera_2040:2049',
          'spera_2050:2059','spera_2060:2069',
          'spera_2070:2079','spera_2080:2089',
          'spera_2090:2099')
mc1= -3000    #-3000
mc2= -0
p2=3000
p1=0
a <- list()
k=1    #data.frame(row.names=c(seq(1,16),NaN))
myraster <- stack(bivmap)


#############################################
#1) extracting climate information for crop data in 2016 


agri16.res = resample(agri16.res,ppt.growing.dec.45)
agri16.res <- mask(agri16.res,ppt.growing.dec.45[[1]])

###
###
###




ppt.45.growing.agri <- overlay(ppt.growing.dec.45,agri16.res,fun=fun.lud3)
cwd.45.growing.agri <- overlay(wd.growing.dec.45,agri16.res,fun=fun.lud3)

ppt.85.growing.agri <- overlay(ppt.growing.dec.85,agri16.res,fun=fun.lud3)
cwd.85.growing.agri <- overlay(wd.growing.dec.85,agri16.res,fun=fun.lud3)


names (ppt.45.growing.agri) = paste0("ppt_",seq(1950,2090,10))
names (cwd.45.growing.agri) = paste0("cwd_",seq(1950,2090,10))

names (ppt.85.growing.agri) = paste0("ppt_",seq(1950,2090,10))
names (cwd.85.growing.agri) = paste0("cwd_",seq(1950,2090,10))

###
###
###



ppt.growing.agri.45.df = as.data.frame(ppt.45.growing.agri,
                                       na.rm=TRUE,
                                       xy=TRUE);head(ppt.growing.agri.45.df)
cwd.growing.agri.45.df = as.data.frame(cwd.45.growing.agri,
                                       na.rm=TRUE,
                                       xy=TRUE);head(cwd.growing.agri.45.df)

ppt.growing.agri.85.df = as.data.frame(ppt.85.growing.agri,
                                       na.rm=TRUE,
                                       xy=TRUE);head(ppt.growing.agri.85.df)
cwd.growing.agri.85.df = as.data.frame(cwd.85.growing.agri,
                                       na.rm=TRUE,
                                       xy=TRUE);head(cwd.growing.agri.85.df)

####
####
####


df.growing.45 = as.data.frame(cbind(ppt.growing.agri.45.df,
                                    cwd.growing.agri.45.df))

df.growing.85 = as.data.frame(cbind(ppt.growing.agri.85.df,
                                    cwd.growing.agri.85.df))



df.growing.45$scenario = "RCP45"
df.growing.85$scenario = "RCP85"

#dd.pre.all = rbind(df.pre.45,df.pre.85)
dd.growing.all = rbind(df.growing.45,df.growing.85)

#dd.pre.all = dd.pre.all[,-c(18,19)]
dd.growing.all = dd.growing.all[,-c(18,19)]

library(tidyverse)



library(scales)

df.tidy = as_tibble(dd.growing.all) %>%
  gather('key','value', ppt_1950:cwd_2090) %>%
  separate(key,c('var', 'decade'))

nomes = c(`cwd`= 'Water Deficit',
          `ppt` = 'Precipitation',
          `decade` = 'Decade',
          `RCP45`='RCP 4.5 W/m2',
          `RCP85`='RCP 8.5 W/m2',
          `var` = 'Climate Variable')

scaleFUN <- function(x) sprintf("%.1f", x)

colfunc3<-colorRampPalette(c("darkred","royalblue"))
colfunc<-colorRampPalette(c("royalblue","green4","orange","darkred"))


df.tidy %>%
  ggplot(aes(x = decade,
             y = value,
             col = var, fill = var)) +
  geom_jitter(alpha = 0.02) +
  geom_boxplot(alpha = 0.5) +
  facet_wrap(~scenario,
             labeller=as_labeller(nomes)) +
  theme_light(22) +
  #theme(axis.text.x = element_text(angle=45)) +
  scale_color_manual(values = colfunc3(3),
                     name='Climate Variable',
                     breaks=c('ppt','cwd'),
                     labels=c('Precipitation','Water Deficit'))+
  scale_fill_manual(values = colfunc3(3),
                    name='Climate Variable',
                    breaks=c('ppt','cwd'),
                    labels=c('Precipitation','Water Deficit'))+
  scale_x_discrete(breaks=seq(1950,2090,30))
####
####
####

#Plot

nq=8 # color map resolution


# this define the extreme colors of the bivariatemap
col.matrix <- colmat(nquantiles =nq, 
                     upperleft="cadetblue1",
                     upperright="darkblue",
                     bottomleft="coral3",
                     bottomright="khaki1",
                     xlab="Drought intensity",
                     ylab="Precipitation (mm/year)",
                     main="Pre-growing season",
                     mc1=-1000,mc2=0,p1=0,p2=100)
par(new=TRUE)

#####
#####
#Plot Growing season
#####
#####


ddf.70 = as.data.frame(cbind(cwd1 = df.growing.45$cwd_1970,
                             ppt1 = df.growing.45$ppt_1970))
head(ddf.70)

ddf.70 = subset(ddf.70, cwd1 < 0)
t1970.growing = matrix(c(ddf.70$cwd1,ddf.70$ppt1),ncol=2)
t1970.growing = t1970.growing[complete.cases(t1970.growing), ]
chull1970.growing = chull(t1970.growing)
chull1970.growing = c(chull1970.growing,chull1970.growing[1])
plot(t1970.growing)
lines(t1970.growing[chull1970.growing,])


#####
#####
#####

ddf.growing.80 = as.data.frame(cbind(cwd1 = df.growing.45$cwd_1980,
                                     ppt1 = df.growing.45$ppt_1980))

head(ddf.growing.80)

ddf.growing.80 = subset(ddf.growing.80, cwd1 < 0)
t1980.growing = matrix(c(ddf.growing.80$cwd1,ddf.growing.80$ppt1),ncol=2)
t1980.growing = t1980.growing[complete.cases(t1980.growing), ]
chull1980.growing = chull(t1980.growing)
chull1980.growing = c(chull1980.growing,chull1980.growing[1])
plot(t1980.growing)
lines(t1970.growing[chull1970.growing,])

plot(t1970.growing, col = 'red')
par(new=TRUE)
plot(t1980.growing, add = T, pch = 11)
#####
#####
#####

##CH de 2010
ddf.growing10 = as.data.frame(cbind(cwd2 = df.growing.45$cwd_2010,
                                    ppt2 = df.growing.45$ppt_2010));head(ddf.growing10)

ddf.growing10 = subset(ddf.growing10, cwd2 < 0)
t2010 = matrix(c(ddf.growing10$cwd2,ddf.growing10$ppt2),ncol=2)
t2010 = t2010[complete.cases(t2010), ]
chull2010 = chull(t2010)
chull2010 = c(chull2010,chull2010[1])
plot(t2010)
lines(t2010[chull2010,])

####
####
####

##CH de 2030

ddf.growing30 = as.data.frame(cbind(cwd2 = df.growing.45$cwd_2030,
                                    ppt2 = df.growing.45$ppt_2030))
head(ddf.growing30)

ddf.growing30 = subset(ddf.growing30, cwd2 < 0)
t2030 = matrix(c(ddf.growing30$cwd2,ddf.growing30$ppt2),ncol=2)
t2030 = t2030[complete.cases(t2030), ]
chull2030 = chull(t2030)
chull2030 = c(chull2030,chull2030[1])
plot(t2030)
lines(t2030[chull2030,])

####
####
####

##CH de 2060
ddf.growing60 = as.data.frame(cbind(cwd2 = df.growing.45$cwd_2060,
                                    ppt2 = df.growing.45$ppt_2060))
head(ddf.growing60)

ddf.growing60 = subset(ddf.growing60, cwd2 < 0)
t2060 = matrix(c(ddf.growing60$cwd2,ddf.growing60$ppt2),ncol=2)
t2060 = t2060[complete.cases(t2060), ]
chull2060 = chull(t2060)
chull2060 = c(chull2060,chull2060[1])
plot(t2060)
lines(t2060[chull2060,])

####
####
####

##CH de 2090
ddf.growing90 = as.data.frame(cbind(cwd2 = df.growing.45$cwd_2090,
                                    ppt2 = df.growing.45$ppt_2090))
head(ddf.growing90)

ddf.growing90 = subset(ddf.growing90, cwd2 < 0)
t2090 = matrix(c(ddf.growing90$cwd2,ddf.growing90$ppt2),ncol=2)
t2090 = t2090[complete.cases(t2090), ]
chull2090 = chull(t2090)
chull2090 = c(chull2090,chull2090[1])
plot(t2090)
lines(t2090[chull2090,])



nq=8 # color map resolution


# this define the extreme colors of the bivariatemap
col.matrix <- colmat(nquantiles =nq, 
                     upperleft="cadetblue1",
                     upperright="darkblue",
                     bottomleft="coral3",
                     bottomright="khaki1",
                     xlab="Drought intensity (Sept-June)",
                     ylab="Precipitation (mm/growing season)",
                     main="",
                     mc1=-1900,mc2=0,p1=0,p2=2500)
par(new=TRUE)

res1.70.45 <- point.in.polygon(df.growing.45$cwd_1970,
                               df.growing.45$ppt_1970, 
                               t1970.growing[,1], t1970.growing[,2])
table(res1.70.45)
plot(df.growing.45$ppt_1970~df.growing.45$cwd_1970,
     col = "black",
     xlim=c(-1900,0),ylim=c(0,2500),
     xlab = "",
     ylab = "",
     main = " ",
     frame.plot = FALSE,axes = FALSE, cex = 1.5)

par(new=TRUE)

plot(df.growing.45$ppt_2010~df.growing.45$cwd_2010,
     col="gray40",
     xlim=c(-1900,0),ylim=c(0,2500),
     xlab = "",
     ylab = "",
     main = " ", pch = 4,
     frame.plot = FALSE,axes = FALSE, cex = 1.3)

par(new=TRUE)

plot(df.growing.45$ppt_2030~df.growing.45$cwd_2030,
     col="gray60",
     xlim=c(-1900,0),ylim=c(0,2500),
     xlab = "",
     ylab = "",
     main = " ", pch = 5,
     frame.plot = FALSE,axes = FALSE, cex = 1.3)

par(new=TRUE)

plot(df.growing.45$ppt_2060~df.growing.45$cwd_2060,
     col = "beige",
     xlim=c(-1900,0),ylim=c(0,2500),
     xlab = "",
     ylab = "",
     main = " ", pch = 2,
     frame.plot = FALSE,axes = FALSE, cex = 1.3)



lines(t1970.growing[chull1970.growing,],lwd=2,lty="solid",col="black")
lines(t2010[chull2010,],lwd=2,lty="solid",col="gray40")
lines(t2030[chull2030,],lwd=2,lty= "dotted",col="gray60")
lines(t2060[chull2060,],lwd=2,lty= "dotted",col="beige")




text(x = -160, y = 1200,labels = "1970",col="black",
     cex=1.6)

text(x = -250, y = 2360,labels = "2010",col="gray40",
     cex=1.6)

text(x = -600, y = 2300,labels = "2030",col="gray60",
     cex=1.6)

text(x = -1000, y = 1900,labels = "2060",col="beige",
     cex=1.6)



####
####
####