################
##Land Use Transition
##April 2019
##Ludmila Rattis
################

library(raster)
library(rgdal)
library(tidyverse)
library(tabularaster)
library(hrbrthemes)
library(bpgmm)

setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/Dados/uso do solo/Spera")

#gy2002 = raster("GY2002.tif")

setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/3 - spera/spera_latlong")
setwd("~/Dropbox/IPAM/climate_agriculture/analises/3 - spera/spera_latlong")

spera = stack(list.files('.','.tif')[1:16]);spera ##it includes 2017
# ss = list.files('.','.tif')[3:19]
# spera = list()
# for(i in 1:length(ss)){spera[[i]] = raster(ss[i])}
# 
# spera[[17]] = resample(spera[[17]],spera[[16]],method='ngb')
# 
# writeRaster(spera[[17]],"spera_latlong_2017.tif",
#             options=c("COMPRESS=LZW", "TFW=NO"))

#spera = raster::mask(raster::crop(spera,mt),mt);spera


spera = mask(spera,limit);plot(spera[[1]])
spera.c = area(spera)


###
####
###
#Color palette

colfunc<-colorRampPalette(c("royalblue","green4","orange","darkred"))
plot(rep(1,16),col=(colfunc(16)), pch=19,cex=2)

colfunc1<-colorRampPalette(c("beige","royalblue","green4","orange","darkred"))

colfunc2<-colorRampPalette(c("royalblue","orange","darkred"))

colfunc3<-colorRampPalette(c("darkred","black","royalblue"))
####
####

#rr = raster(resolution=c(0.5,0.5),xmx=-52,xmn=-60,ymn=-1,ymx=0)
#rr[] = runif(32)


t.area = as_tibble(spera.c)


tt = as_tibble(spera)

ttX = left_join(tt,t.area,by="cellindex");ttX

View(ttX %>%
    filter(cellvalue.x %in% c(1,2,4,5,6)) %>%
    group_by(cellvalue.x,dimindex) %>%
    summarise(round(mean(cellvalue.y,na.rm=TRUE),3),
              round(median(cellvalue.y,na.rm=TRUE),3)))
    #mutate(area_km2 = cellindex * cellindex.y))

#double cropping
View(ttX %>%
       filter(cellvalue.x %in% c(5,6)) %>%
  group_by(dimindex) %>% 
  summarise(Ncat = length(cellvalue.x),
              Area = mean(cellvalue.y),
              AreaT = Ncat*Area))

#single cropping
View(ttX %>%
       filter(cellvalue.x %in% c(1,2,4)) %>%
       group_by(dimindex) %>% 
       summarise(Ncat = length(cellvalue.x),
                 Area = mean(cellvalue.y),
                 AreaT = Ncat*Area))





pixel.area = 0.09 # km2 (calculated in R and Spera)
study.area = #1967027     #1837616




setwd("~/Dropbox/IPAM/climate_agriculture/analises/3 - spera/spera_latlong/spera_transition")
setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/3 - spera/spera_latlong/spera_transition")


double.aband.2years <- stack(list.files()[2:15])

names(double.aband.2years) = paste0('DF_',2003:2016)

double.single.2years <- stack(list.files()[17:30])

names(double.single.2years) = paste0('DS_',2003:2016)

double.consis <- stack(list.files()[32:45])

names(double.consis) = paste0('SD_',2003:2016)

ssT = stack(double.aband.2years,double.single.2years,double.consis)

ssT = mask(ssT,limit)

ssT2 = as_tibble(ssT)

ssT2 = ssT2 %>% drop_na(cellvalue)

ssT3 = ssT2 %>%
  filter(cellvalue == 1) %>%
  group_by(dimindex) %>% 
  summarise(Ncat = length(cellvalue))

View(ssT3)

tab = ssT2;head(tab)

setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises")
tab = read_csv('table_LULUT.csv')
tab



####
####
####

A = ggplot(tab) +
  geom_line(aes(x=Year,y=sc,col="#8B0000"),lwd=1) +
  geom_line(aes(x=Year,y=dc,col='blue'),lwd=1) +
  geom_point(aes(x=Year,y=sc,col="#8B0000"),pch=16,size=3) +
  geom_point(aes(x=Year,y=dc,col='blue'),pch=17,size=3) +
  scale_color_manual(values=alpha(c("darkblue",
                                    "gray20"),.4),
                     name="Land Use",
                     #breaks=c("single-cropping",
                     #        "double-cropping"),
                     labels=c("double-cropping",
                              "single-cropping"))+ 
  scale_color_manual(values=alpha(c("darkblue",
                                    "gray20"),.4),
                     name="Land Use",
                     #breaks=c("single-cropping",
                     #        "double-cropping"),
                     labels=c("double-cropping",
                              "single-cropping"))+
  labs(x="Year",
       y="Area"~(km^2))+ 
  theme_classic(base_size = 20)+
  theme(legend.position=c(0.8,0.2)) +
  labs(tag = "a")

B = ggplot(tab) +
  #geom_area(aes(x=Year,y=dc_sg,col='red',fill='red'),lwd=1) +
  #  geom_area(aes(x=Year,y=sg_dc,col='blue',fill='blue'),lwd=1) +
  #geom_area(aes(x=Year,y=dc_f,col='orange',fill='orange'),lwd=1) +
  # geom_area(aes(x=Year,y=total_de,col='black',fill='black'),lwd=1) +
  geom_line(aes(x=Year,y=dc_sg,col='red'),linetype = 'dashed',lwd=1) +
  geom_line(aes(x=Year,y=sg_dc,col='blue'),lwd=1) +
  geom_line(aes(x=Year,y=dc_f,col='orange'),linetype = 'dashed',lwd=1) +
  geom_line(aes(x=Year,y=total_de,col='black'),lwd=1) +
  #geom_line(aes(x=Year,y=net,col='gray'),lwd=1) +
  geom_point(aes(x=Year,y=dc_sg,col='red'),pch=16,size=3) +
  geom_point(aes(x=Year,y=sg_dc,col='blue'),pch=17,size=3) +
  geom_point(aes(x=Year,y=dc_f,col='orange'),pch=18,size=3) +
  geom_point(aes(x=Year,y=total_de,col='black'),pch=19,size=3) +#geom_area(aes(x=Year,y=net,col='orange',a)) +
  #geom_bar(aes(x=net)) +
  scale_color_manual(values=alpha(c("orange",
                                    "darkblue",
                                    "#8B0000",
                                    "gray20"),.4),
                     name="Land Use Transition",
                     #breaks=c("single-cropping",
                     #        "double-cropping"),
                     labels=c( "total de-intensification",
                               "total intensification",
                               "de-intens: double to fallow",
                               "de-intens: double to single"
                     ))+ 
  scale_fill_manual(values=alpha(c("orange",
                                   "darkblue",
                                   "#8B0000",
                                   "gray20"),.4),
                    name="Land Use Transition",
                    #breaks=c("single-cropping",
                    #        "double-cropping"),
                    labels=c("total de-intensification",
                             "total intensification",
                             "de-intens: double to fallow",
                             "de-intens: double to single"))+
  labs(x="Year",
       y="")+ 
  theme_classic(base_size = 20)+
  theme(legend.position=c(0.3,0.8)) +
  labs(tag = "b")


####
####
####
ggplot(tab,aes(x=Year,y=net)) +
  geom_area()+
  labs(x="Year",
       y="Area"~(km^2))+
  theme_classic(base_size = 20)+
  theme(legend.position=c(0.3,0.8)) +
  labs(tag = "c")
####
####


p1 <- ggplotGrob(A)
p2 <- ggplotGrob(B)

# 
stripT <- subset(p2$layout, grepl("spacer", p2$layout$name))


grid.draw(cbind(p1, p2, size = "first")) 

library(grid)
library(gridExtra)
grid.arrange(A,B,ncol=2)


### average change and rate of change

lm1 = lm(dc ~ Year, data=tab)
summary(lm1)

lm2 = lm(sc ~ Year, data=tab)
summary(lm2)

lm3 = lm(sg_dc ~ Year, data = subset(tab,Year >= 2003))
summary(lm3)

lm3 = lm(total_de ~ Year, data = subset(tab,Year >= 2012))
summary(lm3)
# 


ggplot(tab) +
  #geom_area(aes(x=Year,y=dc_sg,col='red',fill='red'),lwd=1) +
  #  geom_area(aes(x=Year,y=sg_dc,col='blue',fill='blue'),lwd=1) +
  #geom_area(aes(x=Year,y=dc_f,col='orange',fill='orange'),lwd=1) +
  # geom_area(aes(x=Year,y=total_de,col='black',fill='black'),lwd=1) +
#  geom_line(aes(x=Year,y=dc_sg,col='red'),linetype = 'dashed',lwd=1) +
  geom_line(aes(x=Year,y=sg_dc,col='blue'),linetype = 'dashed',lwd=2) +
#  geom_line(aes(x=Year,y=dc_f,col='orange'),linetype = 'dashed',lwd=1) +
  geom_line(aes(x=Year,y=total_de,col='black'),linetype = 'dashed',lwd=2) +
  #geom_line(aes(x=Year,y=net,col='gray'),lwd=1) +
#  geom_point(aes(x=Year,y=dc_sg,col='red'),pch=16,size=3) +
  geom_point(aes(x=Year,y=sg_dc,col='blue'),pch=17,size=3.5) +
#  geom_point(aes(x=Year,y=dc_f,col='orange'),pch=18,size=3) +
  geom_point(aes(x=Year,y=total_de,col='black'),pch=19,size=3.5) +#geom_area(aes(x=Year,y=net,col='orange',a)) +
  #geom_bar(aes(x=net)) +
  scale_color_manual(values=alpha(c("orange",
                                    "darkblue"),.6),
                     name="",
                     #breaks=c("single-cropping",
                     #        "double-cropping"),
                     labels=c( "Ag de-intensification",
                               "Ag intensification"))+ 
  scale_fill_manual(values=alpha(c("orange",
                                   "darkblue",
                                   'red',
                                   "green4"),.6),
                    name="",
                    #breaks=c("single-cropping",
                    #        "double-cropping"),
                    labels=c("Ag de-intensification",
                             "Ag intensification"))+
  labs(x="Year",
       y="Area"~(km^2))+ 
  theme_classic(base_size = 24)+
  theme(legend.position=c(0.3,0.8)) 

####
####











setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/3 - spera")
setwd("~/Dropbox/IPAM/climate_agriculture/analises/3 - spera")

limit = readOGR("limit_latlong.shp");plot(limit)
########
########
########



 

single_to_double = double_to_single = single_aband = 
  single_first = double_aband = double_first = list();

for (i in 1:nlayers(double.consis)){
  print(i)
  single_to_double[[i]] = raster::freq(double.consis[[i]],value=1,
                               merge=TRUE)
double_to_single[[i]] = raster::freq(double.single.2years[[i]],value=1,
                        merge=TRUE)
#single_aband[[i]]     = freq(ss2.rec[[i]],value=-2,progress='window',
 #                       merge=TRUE)
double_aband[[i]]     = raster::freq(double.aband.2years[[i]],value=1,
                        merge=TRUE)
#single_first[[i]]     = freq(ss2.rec[[i]],value=2,progress='window',
 #                       merge=TRUE)
#double_first[[i]]     = freq(ss2.rec[[i]],value=3,progress='window',
 #                       merge=TRUE)
}

spera_pat = as.data.frame(cbind(gtable_combine(single_to_double),
                                gtable_combine(double_to_single),
                                #combine(single_aband),
                                gtable_combine(double_aband)
                                #combine(single_first),
                                #combine(double_first)
                                ));head(spera_pat)
#rownames(spera_pat)=paste0('crop_',2002:2015)
#names(spera_pat) = c('sg_db','db_sg','sg_aband','db_aband','sg_fst','db_fst')
spera_pat = apply(spera_pat,2,as.numeric)
spera_pat = as.data.frame(spera_pat)
names(spera_pat) = c('sg_db','db_sg','db_aband')



spera_pat$year = c(2003:2016)
head(spera_pat)


(df = as_tibble(spera_pat) %>%
  gather(type_change,number_change,-year) %>%
    mutate(number_change = as.numeric(number_change)) %>%
  mutate(area_km2 = number_change*pixel.area))

s.df = df %>%
  group_by(type_change, year) %>%
  summarise(area_km2) %>%
  filter(type_change == "sg_db") 

  
summary(s.df$area_km2)  
  

df %>%
  dplyr::group_by(type_change) %>%
  mutate(cumm = cumsum(area_km2)) %>%
  ungroup() %>%
  ggplot(aes(x=year,y=cumm,col=type_change,
             fill=type_change))+
  geom_line()+
  geom_point()+ 
  geom_smooth() + 
  scale_color_manual(values=c("#c85b53",
                              "#d8884f",
                              "#92603d"),
                     name="Type of transition",
                     breaks=c("db_aband",
                              "db_sg",
                              "sg_db"),
                     labels=c("double to fallow",
                              "double to single",
                              "single  to double"))+ 
  scale_fill_manual(values=c("#c85b53",
                             "#d8884f",
                             "#92603d"),
                    name="Type of transition",
                    breaks=c("db_aband",
                             "db_sg",
                             "sg_db"),
                    labels=c("double to fallow",
                             "double to single",
                             "single  to double"))+ 
  labs(x="Year",
       y="Cumulative area"~(km^2))+ 
  theme_classic(base_size = 20)+
  theme(legend.position=c(0.2,0.8)) 



###quantifying total desintensification
desint = df[15:28,4] + df[29:42,4]
des.df = as.data.frame(cbind(desint=desint, year=2003:2016));des.df
mod1 = lm(area_km2~year,data=des.df)
summary(mod1)

###quantifying total intensification
int = df[1:14,4]
int.df = as.data.frame(cbind(int=int, year=2003:2016))
mod1 = lm(area_km2~year,data=int.df)
summary(mod1)

##########
nomes = c(`sg_db`= 'single to double cropping',
          `db_sg` = 'double to single cropping',
          `sg_aband` = 'single abandoned',
          `db_aband` = 'double cropping abandoned',
          `sg_fst`='single expansion',
          `db_fst`="double expansion",
          `year` = "Year")
##########



ggplot(df,aes(x=year,
                  y=area_km2,
                  col=type_change,
                  fill=type_change))+
  #geom_line(size=1,alpha=0.4)+
  geom_point(size=2)+ 
  geom_smooth(se=FALSE,alpha=0.3) + 
  scale_color_manual(#values=c("#c85b53",
                             # "#d8884f",
                              #"#92603d"),
                     values=colfunc3(3),
                     name="Type of transition",
                     breaks=c("db_aband",
                              "db_sg",
                              "sg_db"),
                     labels=c("double to fallow",
                              "double to single",
                              "single  to double"))+ 
  scale_fill_manual(values=c("#c85b53",
                             "#d8884f",
                             "#92603d"),
                    name="Type of transition",
                    breaks=c("db_aband",
                             "db_sg",
                             "sg_db"),
                    labels=c("double to fallow",
                             "double to single",
                             "single  to double"))+ 
  labs(x="Year",
       y="Area"~(km^2))+ 
  theme_classic(base_size = 20)+
  theme(legend.position=c(0.2,0.8)) 
#facet_wrap(~type_change,nrow=1,
#          labeller=as_labeller(nomes)) +
theme(legend.position = "botton") +
  labs(x="Year",y="Transitioned area"~(km^2))+
  theme_bw(20)
  

p 

setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/Manuscritos/figures_R")

ggsave(filename = "transition_jul21.tiff",
         dpi = 600)








######################

###Consistent land use transition
double.aband.2years

######################
ds = double = list()

for (i in 1:nlayers(double.aband.2years)){
  print(i)
  ds[[i]] = freq(double.aband.2years[[i]],value=1,
                               merge=TRUE)
  
  double[[i]]     = freq(double.consis[[i]],value=1,progress='window',
                                merge=TRUE)
  
}

spera_pat = as.data.frame(cbind(
  combine(ds),
  combine(double)
));head(spera_pat)

names(spera_pat) = c('db_sg','db')

spera_pat$year = c(2003:2016)
head(spera_pat)


(df = spera_pat %>%
    gather(type_change,number_change,-year)%>%
    mutate(area_km2 = number_change*0.09))



##########
nomes = c(`sg_db`= 'single to double cropping',
          `db`= 'double cropping',
          `db_sg` = 'double cropping to single cropping',
          `sg_aband` = 'single abandoned',
          `db_aband` = 'double cropping abandoned',
          `sg_fst`='single expansion',
          `db_fst`="double expansion",
          `year` = "Year")
##########



ggplot(df)+
  geom_line(aes(x=year,y=log(area_km2),col=type_change), show.legend = FALSE)+
  geom_point(aes(x=year,y=log(area_km2),col=type_change), show.legend = FALSE)+
  geom_smooth(method='lm',aes(x=year,y=log(area_km2),col=type_change), show.legend = FALSE)+
  facet_wrap(~type_change,ncol=3,'free_y',
             labeller=as_labeller(nomes)) +
  labs(x="Year",y="Area"~(km^2)) +
  theme_bw(20)




###########################################################################################
###########################################################################################


######################################################################
###########################################################################################
###########################################################################################
##---------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------##
#TEMPERATURE
##---------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------##
#CRU
setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/9 - dados climáticos/CRU_4.02")
setwd("~/Dropbox/IPAM/climate_agriculture/analises/9 - dados climaÌticos/CRU_4.02")

list.files()
tmp = stack("cru_ts4.02.1901.2017.tmp.dat.nc");tmp
tmp.t = subset(tmp,1213:1404);tmp.t
#tmp.t.g = mask(crop(tmp.t,evi.spera[[1]]),mt);tmp.t.g
aa = list();for(i in 1:15){aa[[i]]=c(rep(i,9),rep(0,3))};unlist(aa)

tmp.years = stackApply(tmp.t,
                       indices=c(rep(0,8),unlist(aa),rep(0,4)),
                       fun=mean,na.rm=TRUE)

tmp.years=tmp.years[[2:16]]
names(tmp.years) = paste0("tmp_",2003:2017)
tmp.years = mask(tmp.years,limit)

##---------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------##
#PRECIPITATION
##---------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------##
#CRU
# ppt = stack("cru_ts4.02.1901.2017.pre.dat.nc");ppt
# ppt.t = subset(ppt,1213:1404);ppt.t
# #ppt.t.g = mask(crop(ppt.t,-100,100,-30,30),mt);ppt.t.g
# ppt.years = stackApply(ppt.t,indices=c(rep(0,8),unlist(aa),rep(0,4)),fun=sum)
# #ppt.cum.years = stackApply(ppt.t.g,indices=c(rep(0,8),unlist(aa),rep(0,4)),fun=cumsum)
# prec.years=ppt.years[[2:16]]
# names(prec.years) = paste0("ppt_",2003:2017)
# prec.years = mask(prec.years,limit)

#CHIRPS
setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/9 - dados climáticos/CHIRPS/growing_season_1982-2018/sum")
setwd("~/Dropbox/IPAM/climate_agriculture/analises/9 - dados climaÌticos/CHIRPS/growing_season_1982-2018/sum")

ppt = stack(list.files()[23:37]);ppt
ppt = mask(ppt,limit)
names(ppt) = paste0('chirps.',2003:2017)

###################
###################
##ONSET
###################
###################

#onset x transition
setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/9 - dados climáticos/CHIRPS")
setwd("~/Dropbox/IPAM/climate_agriculture/analises/9 - dados climaÌticos/CHIRPS")

onset = stack("chirps_onset_1982-2016.tif");onset 
onset = onset[[22:35]];onset
plot(onset[[2]])

onset = crop(onset,limit);onset;plot(onset[[2]])

###################
###################
##SOILS
###################
###################


setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/11 - soils/useful")
setwd("~/Dropbox/IPAM/climate_agriculture/analises/11 - soils/useful")

#setwd("D:/Ludmila-D/Dropbox/IPAM/11 - soils")
list.files()
carb = mask(raster("ORCDRC_M_sl1_1km_0cm_Brazil.tiff"),limit);carb;plot(carb)
bulk = mask(raster("BLDFIE_M_sl1_1km_0cm_Brazil.tiff"),limit);bulk;plot(bulk)
sand = mask(raster("SNDPPT_M_sl1_1km_0cm_Brazil.tiff"),limit);sand;plot(sand)
clay = mask(raster("CLYPPT_M_sl1_1km_0cm_Brazil.tiff"),limit);clay;plot(clay)

names(carb) = "carb"
names(bulk) = "bulk"
names(sand) = "sand"
names(clay) = "clay"



##---------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------##
#VPD
##---------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------##
#Terra Climate
setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/9 - dados climáticos/TerraClimate/vpd")
setwd("~/Dropbox/IPAM/climate_agriculture/analises/9 - dados climaÌticos/TerraClimate/vpd")

vpd <- stack(list.files()[27:41]);vpd
vpd = mask(vpd,limit)/100
plot(vpd[[10]])


###################
###################

#abandoned double
double.aband.2years 

#double to single 2003 to 2016
double.single.2years 

#single to double
double.consis




#double abandoned for two years x doubled for two years in a row.
##
xy=xy2=xy3=onset.dl.sg=onset.sg.dl=onset.dl.ab=list()
tmp.dl.sg=tmp.sg.dl=tmp.dl.ab=list()
ppt.dl.sg=ppt.sg.dl=ppt.dl.ab=list()
carb.dl.sg=carb.sg.dl=carb.dl.ab = list()
clay.dl.sg=clay.sg.dl=clay.dl.ab = list()
sand.dl.sg=sand.sg.dl=sand.dl.ab = list()
bulk.dl.sg=bulk.sg.dl=bulk.dl.ab = list()
vpd.dl.sg=vpd.sg.dl=vpd.dl.ab = list()
chirps.dl.sg=chirps.sg.dl=chirps.dl.ab = list()


for(i in 1:nlayers(double.consis)){
  print(i)
  xy[[i]]  <- rasterToPoints(double.single.2years[[i]], function(x) x == 1)
  onset.dl.sg[[i]] <- raster::extract(onset[[i]], xy[[i]][,1:2],cellnumbers=F)
  #ppt.dl.sg[[i]] <- raster::extract(prec.years[[i]], xy[[i]][,1:2],cellnumbers=F)
  chirps.dl.sg[[i]] <- raster::extract(ppt[[i]], xy[[i]][,1:2],cellnumbers=F)
 # tmp.dl.sg[[i]] <- raster::extract(tmp.years[[i]], xy[[i]][,1:2],cellnumbers=F)
  vpd.dl.sg[[i]] <- raster::extract(vpd[[i]], xy[[i]][,1:2],cellnumbers=F)
  carb.dl.sg[[i]] <- raster::extract(carb, xy[[i]][,1:2],cellnumbers=F)
  bulk.dl.sg[[i]] <- raster::extract(bulk, xy[[i]][,1:2],cellnumbers=F)
  sand.dl.sg[[i]] <- raster::extract(sand, xy[[i]][,1:2],cellnumbers=F)
  clay.dl.sg[[i]] <- raster::extract(clay, xy[[i]][,1:2],cellnumbers=F)
  
  xy2[[i]]  <- rasterToPoints(double.consis[[i]], function(x) x == 1)
  onset.sg.dl[[i]] <- raster::extract(onset[[i]], xy2[[i]][,1:2],cellnumbers=F)
  #ppt.sg.dl[[i]] <- raster::extract(prec.years[[i]], xy2[[i]][,1:2],cellnumbers=F)
  chirps.sg.dl[[i]] <- raster::extract(ppt[[i]], xy2[[i]][,1:2],cellnumbers=F)
 # tmp.sg.dl [[i]] <- raster::extract(tmp.years[[i]], xy2[[i]][,1:2],cellnumbers=F)
  vpd.sg.dl [[i]] <- raster::extract(vpd[[i]], xy2[[i]][,1:2],cellnumbers=F)
  carb.sg.dl[[i]] <- raster::extract(carb, xy2[[i]][,1:2],cellnumbers=F)
  bulk.sg.dl[[i]] <- raster::extract(bulk, xy2[[i]][,1:2],cellnumbers=F)
  sand.sg.dl[[i]] <- raster::extract(sand, xy2[[i]][,1:2],cellnumbers=F)
  clay.sg.dl[[i]] <- raster::extract(clay, xy2[[i]][,1:2],cellnumbers=F)
  
  xy3[[i]]  <- rasterToPoints(double.aband.2years[[i]], function(x) x == 1)
  onset.dl.ab[[i]] <- raster::extract(onset[[i]], xy3[[i]][,1:2],cellnumbers=F)
  #ppt.dl.ab[[i]] <- raster::extract(prec.years[[i]], xy3[[i]][,1:2],cellnumbers=F)
  chirps.dl.ab[[i]] <- raster::extract(ppt[[i]], xy3[[i]][,1:2],cellnumbers=F)
 # tmp.dl.ab[[i]] <- raster::extract(tmp.years[[i]], xy3[[i]][,1:2],cellnumbers=F)
  vpd.dl.ab[[i]] <- raster::extract(vpd[[i]], xy3[[i]][,1:2],cellnumbers=F)
  carb.dl.ab[[i]] <- raster::extract(carb, xy3[[i]][,1:2],cellnumbers=F)
  bulk.dl.ab[[i]] <- raster::extract(bulk, xy3[[i]][,1:2],cellnumbers=F)
  sand.dl.ab[[i]] <- raster::extract(sand, xy3[[i]][,1:2],cellnumbers=F)
  clay.dl.ab[[i]] <- raster::extract(clay, xy3[[i]][,1:2],cellnumbers=F)
}


year = 2003:2017

onset.dl.sg2 <- as.data.frame(unlist(onset.dl.sg))
ai = sapply(onset.dl.sg,length)
nomes1 = list();for(i in 1:nlayers(double.consis)){
  nomes1[[i]] = sort(rep(paste0("tosingle.",year[i]),
                         ai[i]))}

onset.dl.sg3<-as.data.frame(cbind(onset.dl.sg2,
                                  unlist(nomes1)))

names(onset.dl.sg3) = c('onset','var')




onset.sg.dl2 <- as.data.frame(unlist(onset.sg.dl))
ai = sapply(onset.sg.dl,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("todouble.",year[i]),
                         ai[i]))}

onset.sg.dl3<-as.data.frame(cbind(onset.sg.dl2,
                                  unlist(nomes2)))

names(onset.sg.dl3) = c('onset','var')


onset.dl.ab2 <- as.data.frame(unlist(onset.dl.ab))
ai = sapply(onset.dl.ab,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("doubleaband.",year[i]),
                         ai[i]))}

onset.dl.ab3<-as.data.frame(cbind(onset.dl.ab2,
                                  unlist(nomes2)))

names(onset.dl.ab3) = c('onset','var')

onset.df = as.tbl(rbind(onset.dl.sg3,onset.sg.dl3,onset.dl.ab3))

#rm(nomes1);rm(nomes2);rm(onset);rm(onset.dl.sg);rm(onset.dl.sg2)
#rm(onset.left);rm(onset.left2);rm(onset.left3);rm(onset.dl.sg3)


#######################

######################

######################
###########################################################
###########################################################
###########################################################

# prec.sg.dl2 <- as.data.frame(unlist(ppt.sg.dl))
# ai = sapply(ppt.sg.dl,length)
# nomes2 = list();for(i in 1:nlayers(double.consis)){
#   nomes2[[i]] = sort(rep(paste0("todouble.",year[i]),
#                          ai[i]))}
# 
# prec.sg.dl3<-as.data.frame(cbind(prec.sg.dl2,
#                                   unlist(nomes2)))
# 
# names(prec.sg.dl3) = c('prec','var')
# 
# 
# prec.dl.ab2 <- as.data.frame(unlist(ppt.dl.ab))
# ai = sapply(ppt.dl.ab,length)
# nomes2 = list();for(i in 1:nlayers(double.consis)){
#   nomes2[[i]] = sort(rep(paste0("doubleaband.",year[i]),
#                          ai[i]))}
# 
# prec.dl.ab3<-as.data.frame(cbind(prec.dl.ab2,
#                                   unlist(nomes2)))
# 
# names(prec.dl.ab3) = c('prec','var')
# 
# 
# prec.dl.sg2 <- as.data.frame(unlist(ppt.dl.sg))
# ai = sapply(ppt.dl.sg,length)
# nomes1 = list();for(i in 1:nlayers(double.consis)){
#   nomes1[[i]] = sort(rep(paste0("tosingle.",year[i]),
#                          ai[i]))}
# 
# prec.dl.sg3<-as.data.frame(cbind(prec.dl.sg2,
#                                   unlist(nomes1)))
# 
# names(prec.dl.sg3) = c('prec','var')
# 
# prec.df = as.tbl(rbind(prec.dl.sg3,prec.sg.dl3,prec.dl.ab3))


##########################

##########################

##########################


chirps.dl.sg2 <- as.data.frame(unlist(chirps.dl.sg))
ai = sapply(chirps.dl.sg,length)
nomes1 = list();for(i in 1:nlayers(double.consis)){
  nomes1[[i]] = sort(rep(paste0("tosingle.",year[i]),
                         ai[i]))}

chirps.dl.sg3<-as.data.frame(cbind(chirps.dl.sg2,
                                  unlist(nomes1)))

names(chirps.dl.sg3) = c('chirps','var')




chirps.sg.dl2 <- as.data.frame(unlist(chirps.sg.dl))
ai = sapply(chirps.sg.dl,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("todouble.",year[i]),
                         ai[i]))}

chirps.sg.dl3<-as.data.frame(cbind(chirps.sg.dl2,
                                  unlist(nomes2)))

names(chirps.sg.dl3) = c('chirps','var')


chirps.dl.ab2 <- as.data.frame(unlist(chirps.dl.ab))
ai = sapply(chirps.dl.ab,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("doubleaband.",year[i]),
                         ai[i]))}

chirps.dl.ab3<-as.data.frame(cbind(chirps.dl.ab2,
                                  unlist(nomes2)))

names(chirps.dl.ab3) = c('chirps','var')

chirps.df = as.tbl(rbind(chirps.dl.sg3,chirps.sg.dl3,chirps.dl.ab3))


##########################

##########################

##########################


# tmp.dl.sg2 <- as.data.frame(unlist(tmp.dl.sg))
# ai = sapply(tmp.dl.sg,length)
# nomes1 = list();for(i in 1:nlayers(double.consis)){
#   nomes1[[i]] = sort(rep(paste0("tosingle.",year[i]),
#                          ai[i]))}
# 
# tmp.dl.sg3<-as.data.frame(cbind(tmp.dl.sg2,
#                                   unlist(nomes1)))
# 
# names(tmp.dl.sg3) = c('tmp','var')
# 
# 
# 
# 
# tmp.sg.dl2 <- as.data.frame(unlist(tmp.sg.dl))
# ai = sapply(tmp.sg.dl,length)
# nomes2 = list();for(i in 1:nlayers(double.consis)){
#   nomes2[[i]] = sort(rep(paste0("todouble.",year[i]),
#                          ai[i]))}
# 
# tmp.sg.dl3<-as.data.frame(cbind(tmp.sg.dl2,
#                                   unlist(nomes2)))
# 
# names(tmp.sg.dl3) = c('tmp','var')
# 
# 
# tmp.dl.ab2 <- as.data.frame(unlist(tmp.dl.ab))
# ai = sapply(tmp.dl.ab,length)
# nomes2 = list();for(i in 1:nlayers(double.consis)){
#   nomes2[[i]] = sort(rep(paste0("doubleaband.",year[i]),
#                          ai[i]))}
# 
# tmp.dl.ab3<-as.data.frame(cbind(tmp.dl.ab2,
#                                   unlist(nomes2)))
# 
# names(tmp.dl.ab3) = c('tmp','var')
# 
# tmp.df = as.tbl(rbind(tmp.dl.sg3,tmp.sg.dl3,tmp.dl.ab3))
# 

####################

#####################

####################


vpd.dl.sg2 <- as.data.frame(unlist(vpd.dl.sg))
ai = sapply(vpd.dl.sg,length)
nomes1 = list();for(i in 1:nlayers(double.consis)){
  nomes1[[i]] = sort(rep(paste0("tosingle.",year[i]),
                         ai[i]))}

vpd.dl.sg3<-as.data.frame(cbind(vpd.dl.sg2,
                                  unlist(nomes1)))

names(vpd.dl.sg3) = c('vpd','var')




vpd.sg.dl2 <- as.data.frame(unlist(vpd.sg.dl))
ai = sapply(vpd.sg.dl,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("todouble.",year[i]),
                         ai[i]))}

vpd.sg.dl3<-as.data.frame(cbind(vpd.sg.dl2,
                                  unlist(nomes2)))

names(vpd.sg.dl3) = c('vpd','var')


vpd.dl.ab2 <- as.data.frame(unlist(vpd.dl.ab))
ai = sapply(vpd.dl.ab,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("doubleaband.",year[i]),
                         ai[i]))}

vpd.dl.ab3<-as.data.frame(cbind(vpd.dl.ab2,
                                  unlist(nomes2)))

names(vpd.dl.ab3) = c('vpd','var')

vpd.df = as.tbl(rbind(vpd.dl.sg3,vpd.sg.dl3,vpd.dl.ab3))


####################

#####################

####################
bulk.dl.sg2 <- as.data.frame(unlist(bulk.dl.sg))
ai = sapply(bulk.dl.sg,length)
nomes1 = list();for(i in 1:nlayers(double.consis)){
  nomes1[[i]] = sort(rep(paste0("tosingle.",year[i]),
                         ai[i]))}

bulk.dl.sg3<-as.data.frame(cbind(bulk.dl.sg2,
                                  unlist(nomes1)))

names(bulk.dl.sg3) = c('bulk','var')




bulk.sg.dl2 <- as.data.frame(unlist(bulk.sg.dl))
ai = sapply(bulk.sg.dl,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("todouble.",year[i]),
                         ai[i]))}

bulk.sg.dl3<-as.data.frame(cbind(bulk.sg.dl2,
                                  unlist(nomes2)))

names(bulk.sg.dl3) = c('bulk','var')


bulk.dl.ab2 <- as.data.frame(unlist(bulk.dl.ab))
ai = sapply(bulk.dl.ab,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("doubleaband.",year[i]),
                         ai[i]))}

bulk.dl.ab3<-as.data.frame(cbind(bulk.dl.ab2,
                                  unlist(nomes2)))

names(bulk.dl.ab3) = c('bulk','var')

bulk.df = as.tbl(rbind(bulk.dl.sg3,bulk.sg.dl3,bulk.dl.ab3))


#################

#################

#################


clay.dl.sg2 <- as.data.frame(unlist(clay.dl.sg))
ai = sapply(clay.dl.sg,length)
nomes1 = list();for(i in 1:nlayers(double.consis)){
  nomes1[[i]] = sort(rep(paste0("tosingle.",year[i]),
                         ai[i]))}

clay.dl.sg3<-as.data.frame(cbind(clay.dl.sg2,
                                  unlist(nomes1)))

names(clay.dl.sg3) = c('clay','var')




clay.sg.dl2 <- as.data.frame(unlist(clay.sg.dl))
ai = sapply(clay.sg.dl,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("todouble.",year[i]),
                         ai[i]))}

clay.sg.dl3<-as.data.frame(cbind(clay.sg.dl2,
                                  unlist(nomes2)))

names(clay.sg.dl3) = c('clay','var')


clay.dl.ab2 <- as.data.frame(unlist(clay.dl.ab))
ai = sapply(clay.dl.ab,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("doubleaband.",year[i]),
                         ai[i]))}

clay.dl.ab3<-as.data.frame(cbind(clay.dl.ab2,
                                  unlist(nomes2)))

names(clay.dl.ab3) = c('clay','var')

clay.df = as.tbl(rbind(clay.dl.sg3,clay.sg.dl3,clay.dl.ab3))

#################

#################

#################


sand.dl.sg2 <- as.data.frame(unlist(sand.dl.sg))
ai = sapply(sand.dl.sg,length)
nomes1 = list();for(i in 1:nlayers(double.consis)){
  nomes1[[i]] = sort(rep(paste0("tosingle.",year[i]),
                         ai[i]))}

sand.dl.sg3<-as.data.frame(cbind(sand.dl.sg2,
                                  unlist(nomes1)))

names(sand.dl.sg3) = c('sand','var')




sand.sg.dl2 <- as.data.frame(unlist(sand.sg.dl))
ai = sapply(sand.sg.dl,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("todouble.",year[i]),
                         ai[i]))}

sand.sg.dl3<-as.data.frame(cbind(sand.sg.dl2,
                                  unlist(nomes2)))

names(sand.sg.dl3) = c('sand','var')


sand.dl.ab2 <- as.data.frame(unlist(sand.dl.ab))
ai = sapply(sand.dl.ab,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("doubleaband.",year[i]),
                         ai[i]))}

sand.dl.ab3<-as.data.frame(cbind(sand.dl.ab2,
                                  unlist(nomes2)))

names(sand.dl.ab3) = c('sand','var')

sand.df = as.tbl(rbind(sand.dl.sg3,sand.sg.dl3,sand.dl.ab3))

#################

#################

#################


carb.dl.sg2 <- as.data.frame(unlist(carb.dl.sg))
ai = sapply(carb.dl.sg,length)
nomes1 = list();for(i in 1:nlayers(double.consis)){
  nomes1[[i]] = sort(rep(paste0("tosingle.",year[i]),
                         ai[i]))}

carb.dl.sg3<-as.data.frame(cbind(carb.dl.sg2,
                                  unlist(nomes1)))

names(carb.dl.sg3) = c('carb','var')




carb.sg.dl2 <- as.data.frame(unlist(carb.sg.dl))
ai = sapply(carb.sg.dl,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("todouble.",year[i]),
                         ai[i]))}

carb.sg.dl3<-as.data.frame(cbind(carb.sg.dl2,
                                  unlist(nomes2)))

names(carb.sg.dl3) = c('carb','var')


carb.dl.ab2 <- as.data.frame(unlist(carb.dl.ab))
ai = sapply(carb.dl.ab,length)
nomes2 = list();for(i in 1:nlayers(double.consis)){
  nomes2[[i]] = sort(rep(paste0("doubleaband.",year[i]),
                         ai[i]))}

carb.dl.ab3<-as.data.frame(cbind(carb.dl.ab2,
                                  unlist(nomes2)))

names(carb.dl.ab3) = c('carb','var')

carb.df = as.tbl(rbind(carb.dl.sg3,carb.sg.dl3,carb.dl.ab3))
#################

#################

#################

##plots
nomes = c(`todouble`= 'single to double-cropping',
          `tosingle` = 'double to single-cropping',
          `doubleaband` = 'double-cropping abandoned',
          `crops` = 'Crop Yields',
          `Yr`='Year')

library(ggpubr)


prec.df2 = prec.df%>%
  separate(var,c("var1", "Yr")) 


       

prec.df2 %>%
  ggplot(aes(x=Yr,y=prec, col=var1))+
  geom_boxplot()

p = ggplot(df,aes(x=year,
                  y=area_km2,
                  col=type_change,
                  fill=type_change))+
  geom_line()+
  geom_point()+ 
  geom_smooth() + 
  labs(x="Year",
       y="Transitioned area"~(km^2))+ 
  theme_classic(base_size = 20)+
  theme(legend.position=c(0.2,0.8)) 
#facet_wrap(~type_change,nrow=1,
#          labeller=as_labeller(nomes)) +
theme(legend.position = "botton") +
  labs(x="Year",y="Transitioned area"~(km^2))+
  theme_bw(20)



chirps.df2 = chirps.df%>%
  separate(var,c("var1", "Yr")) 
cc = chirps.df2 %>%
  ggplot(aes(x=Yr,y=chirps, col=var1, fill=var1))+
  geom_boxplot(notch = TRUE, alpha=.3)+
  labs(x="Year",y="Precipitation (mm/growing season)",
       fill='Type of transition')+ 
  scale_color_manual(values=c("#c85b53",
                              "#d8884f",
                              "#92603d"),
                     name="Type of transition",
                     breaks=c("doubleaband",
                              "tosingle",
                              "todouble"),
                     labels=c("double to fallow",
                              "double to single",
                              "single  to double"))+ 
  scale_fill_manual(values=c("#c85b53",
                             "#d8884f",
                             "#92603d"),
                    name="Type of transition",
                    breaks=c("doubleaband",
                             "tosingle",
                             "todouble"),
                    labels=c("double to fallow",
                             "double to single",
                             "single  to double")) +
  theme_light(base_size = 24)+
  theme(legend.position=c(.2, .9))

cc + 
  geom_hline(yintercept = mean(chirps.df2$chirps,na.rm=TRUE),
                linetype = 2) + 
  stat_compare_means(method = "kruskal.test", label.y = 1000,
                     label = "p.signif") 
  

View(chirps.df %>%
       separate(var,c("var1", "Yr")) %>%
       group_by(var1) %>%
       summarise(min(chirps,na.rm=TRUE),
                 max(chirps,na.rm=TRUE),
                 mean(chirps,na.rm=TRUE),
                 sd(chirps,na.rm=TRUE),
                 median(chirps,na.rm=TRUE),
                 Mode(chirps)))
####################

chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  drop_na() %>%
  #filter(Yr== "2003") %>%
  ggplot(aes(x=varF,y=chirps,
             #group = Yr,
             col=Yr, fill=Yr))+
  geom_boxplot(notch = TRUE, alpha=.3)+
  #facet_wrap(~Yr) +
  stat_compare_means(comparisons = my_comparisons,
                     method = "kruskal.test",
                     paired = TRUE)
                     #group.by = "Yr")#+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1000,label = "p.signif") 

#it works
x3 = chirps.df%>%
    separate(var,c("var1", "Yr"))%>%
    mutate(varF = as.factor(var1)) %>%
    filter(Yr== "2003") 
pairwise.wilcox.test(x3$chirps,x3$varF,
                     p.adjust.method = "bonferroni")

x4 = chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  filter(Yr== "2004") 
pairwise.wilcox.test(x4$chirps,x4$varF,
                     p.adjust.method = "bonferroni")

x5 = chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  filter(Yr== "2005") 
pairwise.wilcox.test(x5$chirps,x5$varF,
                     p.adjust.method = "bonferroni")

x6 = chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  filter(Yr== "2006") 
pairwise.wilcox.test(x6$chirps,x6$varF,
                     p.adjust.method = "bonferroni")

x7 = chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  filter(Yr== "2007") 
pairwise.wilcox.test(x7$chirps,x7$varF,
                     p.adjust.method = "bonferroni")

x8 = chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  filter(Yr== "2008") 
pairwise.wilcox.test(x8$chirps,x8$varF,
                     p.adjust.method = "bonferroni")

x9 = chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  filter(Yr== "2009") 
pairwise.wilcox.test(x9$chirps,x9$varF,
                     p.adjust.method = "bonferroni")

x10 = chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  filter(Yr== "2010") 
pairwise.wilcox.test(x10$chirps,x10$varF,
                     p.adjust.method = "bonferroni")

x11 = chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  filter(Yr== "2011") 
pairwise.wilcox.test(x11$chirps,x11$varF,
                     p.adjust.method = "bonferroni")

x12 = chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  filter(Yr== "2012") 
pairwise.wilcox.test(x12$chirps,x12$varF,
                     p.adjust.method = "bonferroni")

x13 = chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  filter(Yr== "2013") 
pairwise.wilcox.test(x13$chirps,x13$varF,
                     p.adjust.method = "bonferroni")

x14 = chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  filter(Yr== "2014") 
pairwise.wilcox.test(x14$chirps,x14$varF,
                     p.adjust.method = "bonferroni")

x15 = chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  filter(Yr== "2015") 
pairwise.wilcox.test(x15$chirps,x15$varF,
                     p.adjust.method = "bonferroni")

x16 = chirps.df%>%
  separate(var,c("var1", "Yr"))%>%
  mutate(varF = as.factor(var1)) %>%
  filter(Yr== "2016") 
pairwise.wilcox.test(x16$chirps,x16$varF,
                     p.adjust.method = "bonferroni")



#it works too, but not pairwised
chirps.df3 = chirps.df2 %>%
  drop_na() 
compare_means(chirps~var1, data=chirps.df3,
              method = "kruskal.test",
              paired = TRUE,
              group.by = "Yr")

#trying again
my_comparisons <- list( c("doubleaband", "todouble"),
                        c("tosingle", "todouble"))

  
chirps.df2$chirps = round(chirps.df2$chirps,2)
  
pairwise.wilcox.test(chirps.df2$chirps,chirps.df2$var1,
                     p.adjust.method = "bonferroni")
kruskal.test(chirps~var1,data = chirps.df2)

chirps.df2$var1 = as.factor(chirps.df2$var1)
chirps.df2$Yr = as.factor(chirps.df2$Yr)
  

chirps.df3 = chirps.df2 %>%
  filter(var1 %in% c("tosingle", "todouble")) 

chirps.df3 %>%
  ggplot(aes(x=Yr,y=chirps, col=var1, fill=var1))+
  geom_boxplot(notch = TRUE, alpha=.3)+
  labs(x="Year",y="Precipitation (mm/growing season)",
       fill='Land Use Transition')+
  scale_color_manual(name = "Land Use Transition",
                     labels = c("single to double",
                                "double to single"
                                ),
                     values = c("#88a752",
                                "#965ea7"
                                ))+
  scale_fill_manual(name = "Land Use Transition",
                    labels = c("single to double",
                               "double to single"
                    ),
                    values = c("#88a752",
                               "#965ea7"
                    ))+
  theme_light(base_size = 24)+
  theme(legend.position=c(.1, .9))
  


onset.df2 = onset.df%>%
  separate(var,c("var1", "Yr")) 
oo <- onset.df2 %>%
  filter(onset < 150) %>%
  ggplot(aes(x=Yr,y=onset, col=var1,fill=var1))+
  geom_boxplot(notch = FALSE, alpha=.3)+
         labs(x="Year",y="Onset Raining Season\n (days after Aug 28th)",
              fill='Type of transition')+ 
  scale_color_manual(values=c("#c85b53",
                              "#d8884f",
                              "#92603d"),
                     name="Type of transition",
                     breaks=c("doubleaband",
                              "tosingle",
                              "todouble"),
                     labels=c("double to fallow",
                              "double to single",
                              "single  to double"))+ 
  scale_fill_manual(values=c("#c85b53",
                             "#d8884f",
                             "#92603d"),
                    name="Type of transition",
                    breaks=c("doubleaband",
                             "tosingle",
                             "todouble"),
                    labels=c("double to fallow",
                             "double to single",
                             "single  to double")) +
  theme_light(base_size = 24)+
  theme(legend.position=c(.2, .9))

oo + 
  geom_hline(yintercept = mean(onset.df2$onset,na.rm=TRUE),
             linetype = 2) + 
  stat_compare_means(method = "kruskal.test", label.y = 100,
                     label = "p.signif")

View(onset.df2 %>%
       #separate(var,c("var1", "Yr")) %>%
       filter(onset < 120) %>%
       group_by(var1) %>%
       summarise(min(onset,na.rm=TRUE),
                 max(onset,na.rm=TRUE),
                 mean(onset,na.rm=TRUE),
                 sd(onset,na.rm=TRUE),
                 median(onset,na.rm=TRUE),
                 mode(onset)))
# 
# onset.df2 %>%
#   filter(var1 %in% c("tosingle", "todouble")) %>%
#   ggplot(aes(x=Yr,y=onset, col=var1,fill=var1))+
#   geom_boxplot(notch = FALSE, alpha=.3)+
#   labs(x="Year",y="Onset Raining Season\n (days after Aug 28th)",
#        fill='Land Use Transition')+
#   scale_color_manual(name = "Land Use Transition",
#                      labels = c("single to double",
#                                 "double to single"
#                      ),
#                      values = c("#88a752",
#                                 "#965ea7"
#                      ))+
#   scale_fill_manual(name = "Land Use Transition",
#                     labels = c("single to double",
#                                "double to single"
#                     ),
#                     values = c("#88a752",
#                                "#965ea7"
#                     ))+
#   theme_light(base_size = 24)+
#   theme(legend.position=c(.1, .9))

  




tmp.df2 = tmp.df%>%
  separate(var,c("var1", "Yr")) 
tmp.df2 %>%
  ggplot(aes(x=Yr,y=tmp, col=var1))+
  geom_boxplot()


View(tmp.df %>%
       separate(var,c("var1", "Yr")) %>%
       #filter(onset < 150) %>%
       group_by(var1) %>%
       summarise(min(tmp,na.rm=TRUE),
                 max(tmp,na.rm=TRUE),
                 mean(tmp,na.rm=TRUE),
                 sd(tmp,na.rm=TRUE),
                 median(tmp,na.rm=TRUE),
                 Mode(tmp)))


vpd.df2 = vpd.df%>%
  separate(var,c("var1", "Yr")) 
vv = vpd.df2 %>%
  ggplot(aes(x=Yr,y=vpd, col=var1, fill=var1))+
  geom_boxplot(notch = TRUE, alpha=.3)+
         labs(x="Year",y="Vapor Pressure Deficit (KPa)",
              fill='Type of transition')+ 
  scale_color_manual(values=c("#c85b53",
                              "#d8884f",
                              "#92603d"),
                     name="Type of transition",
                     breaks=c("doubleaband",
                              "tosingle",
                              "todouble"),
                     labels=c("double to fallow",
                              "double to single",
                              "single  to double"))+ 
  scale_fill_manual(values=c("#c85b53",
                             "#d8884f",
                             "#92603d"),
                    name="Type of transition",
                    breaks=c("doubleaband",
                             "tosingle",
                             "todouble"),
                    labels=c("double to fallow",
                             "double to single",
                             "single  to double")) +
  theme_light(base_size = 24)+
  theme(legend.position=c(.2, .9))

vv + 
  geom_hline(yintercept = mean(vpd.df2$vpd,na.rm=TRUE),
             linetype = 2) + 
  stat_compare_means(method = "kruskal.test", label.y = 2,
                     label = "p.signif")


View(vpd.df %>%
       separate(var,c("var1", "Yr")) %>%
       #filter(onset < 150) %>%
       group_by(var1) %>%
       summarise(min(vpd,na.rm=TRUE),
                 max(vpd,na.rm=TRUE),
                 mean(vpd,na.rm=TRUE),
                 sd(vpd,na.rm=TRUE),
                 median(vpd,na.rm=TRUE),
                 Mode(vpd)))


bulk.df2 = bulk.df%>%
  separate(var,c("var1", "Yr")) 
bulk.df2 %>%
  ggplot(aes(x=Yr,y=bulk, col=var1))+
  geom_boxplot()

View(bulk.df %>%
       separate(var,c("var1", "Yr")) %>%
       #filter(onset < 150) %>%
       group_by(var1) %>%
       summarise(min(bulk,na.rm=TRUE),
                 max(bulk,na.rm=TRUE),
                 mean(bulk,na.rm=TRUE),
                 sd(bulk,na.rm=TRUE),
                 median(bulk,na.rm=TRUE),
                 Mode(bulk)))



clay.df2 = clay.df%>%
  separate(var,c("var1", "Yr")) 
clay.df2 %>%
  ggplot(aes(x=Yr,y=clay, col=var1))+
  geom_boxplot()

View(clay.df %>%
       separate(var,c("var1", "Yr")) %>%
       #filter(onset < 150) %>%
       group_by(var1) %>%
       summarise(min(clay,na.rm=TRUE),
                 max(clay,na.rm=TRUE),
                 mean(clay,na.rm=TRUE),
                 sd(clay,na.rm=TRUE),
                 median(clay,na.rm=TRUE),
                 Mode(clay)))

carb.df2 = carb.df%>%
  separate(var,c("var1", "Yr")) 
carb.df2 %>%
  ggplot(aes(x=Yr,y=carb, col=var1))+
  geom_boxplot()

View(carb.df %>%
       separate(var,c("var1", "Yr")) %>%
       #filter(onset < 150) %>%
       group_by(var1) %>%
       summarise(min(carb,na.rm=TRUE),
                 max(carb,na.rm=TRUE),
                 mean(carb,na.rm=TRUE),
                 sd(carb,na.rm=TRUE),
                 median(carb,na.rm=TRUE),
                 Mode(carb)))

sand.df2 = sand.df%>%
  separate(var,c("var1", "Yr")) 
sand.df2 %>%
  ggplot(aes(x=Yr,y=sand, col=var1))+
  geom_boxplot()

View(sand.df %>%
       separate(var,c("var1", "Yr")) %>%
       #filter(onset < 150) %>%
       group_by(var1) %>%
       summarise(min(sand,na.rm=TRUE),
                 max(sand,na.rm=TRUE),
                 mean(sand,na.rm=TRUE),
                 sd(sand,na.rm=TRUE),
                 median(sand,na.rm=TRUE),
                 Mode(sand)))

ss = sand.df2 %>%
  ggplot(aes(x=Yr,y=sand, col=var1, fill=var1))+
  geom_boxplot(notch = TRUE, alpha=.3)+
  labs(x="Year",y="Sand content (g/kg)",
       fill='Type of transition')+ 
  scale_color_manual(values=c("#c85b53",
                              "#d8884f",
                              "#92603d"),
                     name="Type of transition",
                     breaks=c("doubleaband",
                              "tosingle",
                              "todouble"),
                     labels=c("double to fallow",
                              "double to single",
                              "single  to double"))+ 
  scale_fill_manual(values=c("#c85b53",
                             "#d8884f",
                             "#92603d"),
                    name="Type of transition",
                    breaks=c("doubleaband",
                             "tosingle",
                             "todouble"),
                    labels=c("double to fallow",
                             "double to single",
                             "single  to double")) +
  theme_light(base_size = 24)+
  theme(legend.position=c(.2, .9))

ss + 
  geom_hline(yintercept = mean(sand.df2$sand,na.rm=TRUE),
             linetype = 2) + 
  stat_compare_means(method = "kruskal.test", label.y = 63,
                     label = "p.signif")
#################################################
#########################################################
#Binomial Models
#########################################################
#################################################

vpd.df2 = vpd.df2 %>%
  mutate(resp = ifelse(var1 %in% 'abandon',0,1))

glimpse(vpd.df2)
ggplot(vpd.df2,aes(x=vpd,y=resp))+
  geom_point()+
  geom_smooth(glm(y~poly(x,2),family='binomial',na.rm=TRUE))+
  facet_wrap(~Yr,ncol=2)

df2 = cbind(chirps.df2)

# df3 = subset(df2,!is.na(double.single))
# nrow(df3)

#Regression analysis tidyverse
# library(lme4)
# library(dotwhisker)
# library(broom)
# 
# 
# 
# mod1.onset = glm(double.single~onset,data=df2,family='binomial')
# mod3 = glm(double.single~onset+Yr,data=df2,family='binomial')
# mod4 = glm(double.single~Year,data=df2,family='binomial')
# mod5 = glm(double.single~onset+Year,data=df2,family='binomial')
# dwplot(mod5)
# mod3_df = tidy(mod3);mod3_df
# 
# #sensitivity analysis
# range(df2$onset)
# xweight=seq(0,120,5)
# yweight=predict(mod1,list(onset=xweight),type="response")
# plot(df2$onset, df2$double.single, pch = 16, xlab = "onset (days after 28/8)", ylab = "double - single")
# lines(xweight, yweight)
# 
# range(df2$onset)
# xyear=as.factor(seq(2002,2016,1))
# yyear=predict(mod4,list(Year=xyear),type="response")
# plot(df2$Year, df2$double.single, pch = 16, xlab = "onset (days after 28/8)", ylab = "double - single")
# lines(xyear, yyear)
# 
# by_year_factor = df2%>%
#   #group_by(Yr) %>%                                         # group data by year
#   do(tidy(lm(double.single ~ onset+Year, data = .))) %>% # run model on each grp
#   rename(model=Year) %>%                                     # make model variable
#   relabel_predictors(c(Year = "Years"))
# 
# dwplot(by_year, 
#        vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
#   theme_bw() + xlab("Coefficient Estimate") + ylab("") +
#   ggtitle("Predicting Double to Single transition by rainfall onset") #+
#   # theme(plot.title = element_text(face="bold"),
#   #       #legend.position = c(-0.005,1),
#   #       legend.justification = c(0, 0),
#   #       legend.background = element_rect(colour="grey80"),
#   #       legend.title.align = .5) +
#   # scale_colour_grey(start = .3, end = .7,
#   #                   name = "Transmission",
#   #                   breaks = c(0, 1),
#   #                   labels = c("Automatic", "Manual"))
# 
# # df2$Year = as.factor(df2$Yr)
# # mod2 = glmer(double.single~onset+(1|Year),data=df2,family='binomial')
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# f1 = as_tibble(dl.sg.df) %>%#na.omit()%>%
#   df1$test =  rowSums(is.na(df1))
#   
# dl.sg.mod = apply(dl.sg.df, 1, FUN=function(x){glm(x[3:17] ~ x[18:32],family=binomial(link = "logit"))})
# lapply(dl.sg.mod,summary)
# 
# #lm
# #fun <- function(x) { glm(x[1:15] ~ x[16:30],family=binomial(link = "logit"))$coefficients}
# fun <- function(x,na.rm=TRUE) { 
#     glm(x[3:17] ~ x[18:32],family=binomial(link = "logit"))$coefficients}
# 
# slope <- calc(st1, fun)

# ########################################################################
# ########################################################################
# ##s? soja
# setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/3 - spera/spera_latlong")
# library(raster)
# spera = stack(list.files('.','.tif'));spera
# rcl=matrix(c(0,1,2,4,5,6,11,13,99,127,1,5,1,1,1,100,1,1,1,1),ncol=2);rcl
# spera.rec2 = reclassify(spera,rcl)
# 
# 
# rclX=matrix(c(0,1,2,4,5,6,11,13,99,127,0,1,1,1,1,1,0,0,0,0),ncol=2);rclX
# speraX = reclassify(spera,rclX)
# 
# 
# spera.dif2 = list();for(i in 1:15){spera.dif2[[i]] = spera.rec2[[i]]-spera.rec2[[i+1]]}
# spera.dif2 = stack(spera.dif2)
# 
# rcl2=matrix(c(NA,-95,0,95,4,99,-4,-99,NA,1,NA,-1,-2,-3,2,3),ncol=2);rcl2
# ss2.rec = reclassify(spera.dif2,rcl2)
# plot(ss2.rec[[5]])
# 
# setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/3 - spera/spera_latlong/spera_transition")
# writeRaster(ss2.rec,paste0("spera_transition_",2002:2016,".tif"),bylayer=TRUE,options=c("COMPRESS=LZW", "TFW=NO"),overwrite=TRUE)
# 
# single_to_double2 = freq(ss2.rec,value=1,progress='window',merge=TRUE);single_to_double
# double_to_single2 = freq(ss2.rec,value=-1,progress='window',merge=TRUE);double_to_single
# single_aband2 = freq(ss2.rec,value=-2,progress='window',merge=TRUE);single_aband
# double_aband2 = freq(ss2.rec,value=-3,progress='window',merge=TRUE);double_aband
# single_first2 = freq(ss2.rec,value=2,progress='window',merge=TRUE);single_first
# double_first2 = freq(ss2.rec,value=3,progress='window',merge=TRUE);double_first
# 
# spera_pat2 = as.data.frame(cbind(single_to_double2,double_to_single2,single_aband2,double_aband2,single_first2,double_first2));head(spera_pat2)
# #rownames(spera_pat)=paste0('crop_',2002:2015)
# spera_pat2$year = c(2002:2016)
# head(spera_pat2)
# library(tidyverse)
# 
# df = spera_pat2 %>%
#   gather(type_change,number_change,-year)%>%
#   mutate(area_km2 = number_change*0.09)
# 
# 
# library(ggplot2)
# ggplot(df,(aes(x=year,y=area_km2,col=type_change)))+
#   geom_line()+
#   geom_point()+
#   geom_smooth(method='lm')+
#   facet_wrap(~type_change,ncol=3)
# 
# 
# 
# library(ggplot2)
# ggplot(df,(aes(x=year,y=area_km2,col=type_change)))+
#   geom_line()+
#   geom_point()+
#   geom_smooth(method='lm')+
#   facet_wrap(~type_change,ncol=3)
# 
# 
# 
# 
# 
# 
# ########################################################################
# ########################################################################
# ##s? soja no MT
# setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/3 - spera/spera_latlong")
# library(raster)
# # spera = stack(list.files('.','.tif'));spera
# # rcl=matrix(c(0,1,2,4,5,6,11,13,99,127,1,5,1,1,1,100,1,1,1,1),ncol=2);rcl
# # spera.rec2 = reclassify(spera,rcl)
# # 
# # spera.dif2 = list();for(i in 1:15){spera.dif2[[i]] = spera.rec2[[i]]-spera.rec2[[i+1]]}
# # spera.dif2 = stack(spera.dif2)
# # 
# # rcl2=matrix(c(NA,-95,0,95,4,99,-4,-99,NA,1,NA,-1,-2,-3,2,3),ncol=2);rcl2
# # ss2.rec = reclassify(spera.dif2,rcl2)
# # plot(ss2.rec[[5]])
# 
# setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/3 - spera/spera_latlong/spera_transition")
# writeRaster(ss2.rec,paste0("spera_transition_mt_soy",2002:2016,".tif"),bylayer=TRUE,options=c("COMPRESS=LZW", "TFW=NO"),overwrite=TRUE)
# ss2.rec = stack(list.files()[16:30])
# 
# setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/produtos_resultados_mt_amazonico/xingu_sem_rebarba")
# setwd("D:/Ludmila-D/Dropbox/IPAM/produtos_resultados_mt_amazonico/xingu_sem_rebarba")
# library(rgdal)
# mt = readOGR("xingu_correto_sem_rebarba.shp")
# mt.up = subset(mt,Id==2);plot(mt.up)
# ss3.rec = mask(crop(ss2.rec,mt.up),mt.up);plot(ss3.rec[[5]])
# 
# single_to_double3 = freq(ss3.rec,value=1,progress='window',merge=TRUE);single_to_double3
# double_to_single3 = freq(ss3.rec,value=-1,progress='window',merge=TRUE);double_to_single3
# single_aband3 = freq(ss3.rec,value=-2,progress='window',merge=TRUE);single_aband3
# double_aband3 = freq(ss3.rec,value=-3,progress='window',merge=TRUE);double_aband3
# single_first3 = freq(ss3.rec,value=2,progress='window',merge=TRUE);single_first3
# double_first3 = freq(ss3.rec,value=3,progress='window',merge=TRUE);double_first3
# 
# spera_pat3 = as.data.frame(cbind(single_to_double3,double_to_single3,single_aband3,double_aband3,single_first3,double_first3));head(spera_pat3)
# #rownames(spera_pat)=paste0('crop_',2002:2015)
# spera_pat3$year = c(2002:2016)
# head(spera_pat3)
# library(tidyverse)
# df = spera_pat3 %>%
#   gather(type_change,number_change,-year)%>%
#   mutate(area_km2 = number_change*0.09)
# 
# 
# library(ggplot2)
# ggplot(df,(aes(x=year,y=area_km2,col=type_change)))+
#   geom_line()+
#   geom_point()+
#   geom_smooth(method='lm')+
#   facet_wrap(~type_change,ncol=3)
# 
# 
# 

setwd("~/Dropbox/IPAM/climate_agriculture/Manuscritos/figures_R")
proj = read_csv("Brazil_projections_ag.csv")


scaleFUN <- function(x) sprintf("%.0f", x)


proj %>%
  group_by(year) %>%
  mutate(prod_total = sum(production)) %>%
  ggplot(aes(year,prod_total)) + 
  geom_point(size = 2, shape = 23) +
  geom_line() +
  labs(x= "Year", y = "Soybean Production \n(million tonnes)") +
  lims(x=c(2019,2029)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(
    labels = scaleFUN,
    breaks = 2019:2029) #+
  theme(axis.text.x = element_text(angle=45,vjust = -0.001))
  
