######################
######################
#Ludmila Rattis
#Economic and climatic model to predict soybean prod and double cropping prob
#Sept 2018
#####################
######################

#Loading packages
x<-c("rgdal","rgeos","raster","maptools","sp","gdalUtils",
     "ggplot2","rasterVis","DescTools",'tidyverse','pracma');lapply(x, require, character.only=T)

P4S <- CRS("+proj=longlat +datum=WGS84")
P4 <- CRS("+proj=utm +datum=WGS84")

##########################################################


##PAM 

setwd("~/Dropbox/IPAM/climate_agriculture/analises/Produção Agrícola IBGE Soja e Safrinha - 2001_2016/PAM_1974-2019/shapes_pam")

lista = list.files('.','.shp')[28:43];lista
shapes = list()
for (i in 1:length(lista)) { shapes[[i]] = readOGR(lista[i])}

shapes = lapply(shapes,spTransform,P4S)


####
####
####

##Spera

setwd("C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/3 - spera")
limit = readOGR("limit_latlong.shp");plot(limit)

setwd("~/Dropbox/IPAM/climate_agriculture/analises/3 - spera/spera_latlong")

spera = stack(list.files('.','.tif')[1:16])

rcl=matrix(c(0,1,2 ,4 ,5,6,11,13,99,127,
            NA,1,NA,NA,1,1,10,NA,NA,NA),ncol=2);rcl

spera.soy = reclassify(spera,rcl)

rcl2=matrix(c(0,1 ,2,4 ,5 ,6,11,13,99,127,
             NA,NA,1,NA,NA,1,10,NA,NA,NA),ncol=2);rcl2

spera.corn = reclassify(spera,rcl2)

rcl2=matrix(c(0,1 ,2,4,5 ,6,11,13,99,127,
              0,0,0 ,0,0 ,0,1 ,0 ,NA,NA ),ncol=2);rcl2

spera.irri = reclassify(spera,rcl2)


shapes = lapply(shapes,crop,limit)
###
###
###

shapes2 = lapply(shapes,subset, muni != "Aldeias Altas")
#shapes2 = lapply(shapes2,subset, muni != "Afonso Cunha")

###
###
###

funExt = function (shape, raster) {
  
  n1 = nlayers(raster)
  n2 = length(shape[[1]])
  
  clipB = list ()
  resu = matrix(0,ncol=n1,nrow=n2)
  
  
  
  for (j in 1:n1) {
    print(paste0("year=",j))
    
  for (i in 1:n2) {
    print(paste0("city=",shape[[1]]$munici[i]))
    shape[[j]]$ID = 1: nrow(shape[[j]])
   ss = subset(shape[[j]],ID == i)
  clipB[[i]] <- mask(crop(raster[[j]], extent(ss)),ss)
  resu[i,j] <-unlist(raster::extract(clipB[[i]],
                                           ss,
                                           fun=mean,
                                           na.rm=T))
    }
  }
  resu = as.data.frame(cbind(shape[[1]]$munici[1:n2],resu))
  names(resu) = c('munic',names(raster)[n1])
  return(resu)
}


####
####
####

soya.cities = funExt(shapes2,spera.soy)
corn.cities = funExt(shapes2,spera.corn)
irri.cities = funExt(shapes2,spera.irri)

names(soya.cities) = c("municipalities",paste0('SoyAreaIrrig_',2001:2016))
names(corn.cities) = c("municipalities",paste0('CornAreaIrrig_',2001:2016))
names(irri.cities) = c("municipalities",paste0('IrriAreaIrrig_',2001:2016))


setwd("~/Dropbox/IPAM/climate_agriculture/analises/produtos_resultados_spera_florestada")

write.csv(soya.cities,"Area_Under_Soybean_and_Irrigated.csv")
write.csv(corn.cities,"Area_Under_Corn_and_Irrigated.csv")

(soya.cities = read.csv("Area_Under_Soybean_and_Irrigated.csv"))
(corn.cities = read.csv("Area_Under_Corn_and_Irrigated.csv"))
####
####
####

setwd("~/Dropbox/IPAM/climate_agriculture/analises/Produção Agrícola IBGE Soja e Safrinha - 2001_2016/PAM_1974-2019")

#remover linhas com NA



####
####

#To add irrigated area
for (i in 1:16){
shapes2[[i]]$soya_irrig_prop = soya.cities[,i+2]
shapes2[[i]]$corn_irrig_prop = corn.cities[,i+2]}


####
####
####




#####
#####
#####

##Irrigation data by ANA
library(rgdal)

setwd("~/Dropbox/IPAM/climate_agriculture/analises/14 - potential ag")
irrig <- readOGR("IrrigatedAreaIn2014_ANA.shp");irrig



irrig$NM_MUNICIP = str_replace_all(irrig$NM_MUNICIP, "Ã.", "O")
irrig$NM_MUNICIP = str_replace_all(irrig$NM_MUNICIP, "Ãs", "U")
irrig$NM_MUNICIP = str_replace_all(irrig$NM_MUNICIP, "ÃS", "E")
irrig$NM_MUNICIP = str_replace_all(irrig$NM_MUNICIP, "Ã???", "C")
irrig$NM_MUNICIP = str_replace_all(irrig$NM_MUNICIP, "Ã", "O")
irrig$NM_MUNICIP = str_replace_all(irrig$NM_MUNICIP, "D", "D")
irrig$NM_MUNICIP = str_replace_all(irrig$NM_MUNICIP, "Ã", "O")
irrig$NM_MUNICIP = str_replace_all(irrig$NM_MUNICIP, "Ã,", "A")
irrig$NM_MUNICIP = str_replace_all(irrig$NM_MUNICIP, "Ã???", "E")
irrig$NM_MUNICIP = str_replace_all(irrig$NM_MUNICIP, "Ã\u0081", "A")
irrig$NM_MUNICIP <-str_replace_all(irrig$NM_MUNICIP, "Ã\u008d", "I")
irrig$NM_MUNICIP <-str_replace_all(irrig$NM_MUNICIP, "Ãf", "A")
irrig$NM_MUNICIP <-str_replace_all(irrig$NM_MUNICIP, "Ã\u008d", "I")
irrig$NM_MUNICIP <-str_replace_all(irrig$NM_MUNICIP, "i\u008d", "A")
irrig$NM_MUNICIP <-str_replace_all(irrig$NM_MUNICIP, " ", "_")

(irrig$NM_ESTADO= paste(toupper(irrig$NM_ESTADO)))

irrig$NM_MUNICIP = paste(toupper(irrig$NM_MUNICIP))

(df.Irrig = irrig@data %>% as_tibble() %>%
  dplyr::select(-c(OBJECTID,CD_GEOCMU,Shape_Leng,Shape_Area)) %>%
  group_by(NM_MUNICIP) %>%
   # distinct(NM_MUNICIP) %>%
  mutate(IrrigArea = sum(HECTARES)) %>%
  distinct(NM_MUNICIP,.keep_all = TRUE) %>%
    ungroup())
  #select(-HECTARES)

names(df.Irrig) = c("OBJECTID_1",
                    "munici",
                    "name_uf",
                    "REGIAO_HID",
                    "HECTARES",
                    "IrrigArea")



df.Irrig$name_uf <-str_replace_all(df.Irrig$name_uf,
                                   "PIAUÃ\u008d","PIAUI")
df.Irrig$name_uf <-str_replace_all(df.Irrig$name_uf,
                                   "MARANHÃfO", "MARANHAO")
df.Irrig$name_uf <-str_replace_all(df.Irrig$name_uf,
                                   "GOIÃ\u0081S", "GOIAS")




irrig.final = list()
for(i in 1:16) {
  shapes2[[i]]$munici= paste(toupper(shapes2[[i]]$munici))
  shapes2[[i]]$name_uf= paste(toupper(shapes2[[i]]$name_uf))
  irrig.final[[i]] = merge(shapes2[[i]],
                           df.Irrig,by=c("munici","name_uf"))
}

Year = 2001:2016
irrig.selected = list()
for(i in 1:16){
  irrig.selected[[i]] = as_tibble(irrig.final[[i]]) %>%
  dplyr::select(munici,name_uf,
                soya_irrig_prop, corn_irrig_prop,
                HECTARES,IrrigArea)
  irrig.selected[[i]]$Year = Year[i]}

(irrig.rbind = do.call(rbind,irrig.selected))


#irrig.rbind = irrig.rbind %>%
 # rename_at(vars(name_uf), ~ "state")

irrig.rbind$name_uf = as.factor(irrig.rbind$name_uf)

levels(irrig.rbind$name_uf)[6] = "PIAUI"



(irrig.rbind2 = irrig.rbind %>%
  mutate(state = case_when(name_uf == "TOCANTINS" ~ "TO",
                           name_uf == "MARANHAO" ~ "MA",
                           name_uf == "PIAUI" ~ "PI", 
                           name_uf == "BAHIA" ~ "BA",
                           name_uf == "MATO GROSSO" ~ "MT",
                           name_uf == "GOIAS" ~ "GO",
                           name_uf == "DISTRITO FEDERAL" ~ "DF")))

#irrig.rbind2$soya_irrig_prop = as.numeric(irrig.rbind2$soya_irrig_prop)
#irrig.rbind2$corn_irrig_prop = as.numeric(irrig.rbind2$corn_irrig_prop)


#irrig.rbind$state[is.na(irrig.rbind$state)] <- "PI"
irrig.rbind2 %>%
  filter (soya_irrig_prop > 1) %>%
ggplot(aes(x = state, y = log(soya_irrig_prop))) +
  geom_boxplot() +
  facet_wrap(~Year)
  
irrig.rbind2 %>%
  filter (corn_irrig_prop > 1) %>%
  ggplot(aes(x = state, y = log(corn_irrig_prop))) +
  geom_boxplot() +
  facet_wrap(~Year)


###
###
###

irrig.rbind2 = irrig.rbind2 %>% 
  rename_at(vars(munici), ~ "municipalities")

irrig.soya = irrig.rbind2 %>%
  drop_na(soya_irrig_prop) 

irrig.corn = irrig.rbind2 %>%
  drop_na(corn_irrig_prop)



#PRECIPITATION
##---------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------##

#monthly precipitation
setwd("~/Dropbox/IPAM/climate_agriculture/analises/9 - dados climáticos/CHIRPS/extracted_by_polygon_GEE")

(chirps.monthly = read_csv("CHIRPS_monthly_ppt_1981-2019.csv"))

(chirps.monthly2 = chirps.monthly %>%
  gather('var','value',-c(munici,state)) %>%
  separate(var,c('month','Year')) %>%
  pivot_wider(values_from = value, names_from = month))


##---------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------##
#VPD
##---------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------##

#monthly vpd
setwd("~/Dropbox/IPAM/climate_agriculture/analises/9 - dados climáticos/TerraClimate/vpd_monthly_extracted_by_polygon")

t1 = read_csv("TERRA_monthly_vpd_1981-1988.csv")
t2 = read_csv("TERRA_monthly_vpd_1989-1996.csv")
t3 = read_csv("TERRA_monthly_vpd_1997-2004.csv")
t4 = read_csv("TERRA_monthly_vpd_2005-2012.csv")
t5 = read_csv("TERRA_monthly_vpd_2013-2019.csv")


####
####
####

mm = c('jan','feb','mar','apr',
       'may','jun','jul','aug',
       'sep','oct','nov','dec')

old.names1 = paste0('vpd_',0:95)
new.names1 = paste0(mm,'VPD_',rep(1981:1988,each=12))

t1 = t1 %>% 
  rename_at(vars(old.names1), ~ new.names1)

###
###

old.names2 = paste0('vpd_',0:95)
new.names2 = paste0(mm,'VPD_',rep(1989:1996,each=12))

t2= t2 %>% 
  rename_at(vars(all_of(old.names2)), ~ new.names2)

###
###

old.names3 = paste0('vpd_',0:95)
new.names3 = paste0(mm,'VPD_',rep(1997:2004,each=12))

t3= t3 %>% 
  rename_at(vars(all_of(old.names3)), ~ new.names3)


###
###

old.names4 = paste0('vpd_',0:95)
new.names4 = paste0(mm,'VPD_',rep(2005:2012,each=12))

t4= t4 %>% 
  rename_at(vars(all_of(old.names4)), ~ new.names4)

###
###

old.names5 = paste0('vpd_',0:83)
new.names5 = paste0(mm,'VPD_',rep(2013:2019,each=12))

t5= t5 %>% 
  rename_at(vars(all_of(old.names5)), ~ new.names5)

(vpd.monthly = full_join(t1,t2,by=c('munici','state')))
(vpd.monthly = full_join(vpd.monthly,t3,by=c('munici','state')))
(vpd.monthly = full_join(vpd.monthly,t4,by=c('munici','state')))
(vpd.monthly = full_join(vpd.monthly,t5,by=c('munici','state')))



(vpd.monthly2 = vpd.monthly %>%
    gather('var','value',-c(munici,state)) %>%
    separate(var,c('month','Year')) %>%
    pivot_wider(values_from = value, names_from = month))


(climate.monthly = full_join(chirps.monthly2,vpd.monthly2,
                            by=c("munici",'state','Year')))

climate.monthly = climate.monthly %>%
  rename_at(vars(munici), ~ "municipalities")

(climate.monthly$municipalities = paste(toupper(climate.monthly$municipalities)))

climate.monthly$Year = as.numeric(climate.monthly$Year)

climate.monthly.soy = subset(climate.monthly,
                             municipalities %in% irrig.soya$municipalities)


(climate.monthly.soy = full_join(climate.monthly.soy,
                                irrig.soya,
                                by=c('municipalities',
                                     'state',
                                     'Year')))
####
####

climate.monthly.corn = subset(climate.monthly,
                             municipalities %in% irrig.corn$municipalities)



(climate.monthly.corn = full_join(climate.monthly.corn,
                                 irrig.corn,
                                 by=c('municipalities',
                                      'state',
                                      'Year')))


  

####
####
####


####
####
####

#ler o arquivo de PAM completo
setwd("~/Dropbox/IPAM/climate_agriculture/analises/Produção Agrícola IBGE Soja e Safrinha - 2001_2016/PAM_1974-2019")

pam = read_delim("PAM.csv",delim =';');pam

(pam$municipalities = paste(toupper(pam$municipalities)))

#separate 

#tirar os anos antes de 2001? Fazer com e sem, dependendo do
#rowSum. Toda cidade que tenha aparecido pelo menos 3 vezes
#poderia entrar

# subset das cidades com soja

pam.soy = pam %>%
  select_at(vars(contains('soy')))

(pam.soy2 = as_tibble(cbind(municipalities = pam$municipalities,
                            pam.soy,state = pam$state,Year = pam$Year)))

#final.df3$Year = as.numeric(final.df3$Year)

#climate.monthly.soy$Year = as.numeric(climate.monthly.soy$Year)

(dfI.m.soya = right_join(pam.soy2,climate.monthly.soy,by=c('municipalities',
                                                    'Year','state')))

#climate.monthly$Year = as.numeric(climate.monthly$Year)

#(dfI.m = right_join(pam.soy2,climate.monthly,by=c('municipalities',
 #                                                     'Year','state')))


setwd("~/Dropbox/IPAM/climate_agriculture/analises/produtos_resultados_spera_florestada")
write.csv(dfI.m.soya,"ibge_monthly_climate_1981_2019.csv")


pam.corn = pam %>%
  select_at(vars(contains('corn')))

(pam.corn2 = as_tibble(cbind(municipalities = pam$municipalities,
                            pam.corn,state = pam$state,Year = pam$Year)))

#final.df3$Year = as.numeric(final.df3$Year)

#climate.monthly.corn$Year = as.numeric(climate.monthly.corn$Year)

(dfI.m.corn = right_join(pam.corn2,climate.monthly.corn,by=c('municipalities',
                                                     'Year','state')))


setwd("~/Dropbox/IPAM/climate_agriculture/analises/produtos_resultados_spera_florestada")
write.csv(dfI.m.corn,"ibge_monthly_climate_corn_1981_2019.csv")






#############GO TO SCRIPT: REGRESSION PROD VC WEATHER_CORN.R#################


library("tidyverse")
library("robustHD")

agr <- dfI.m.corn

agr2 <- agr %>%
  dplyr::select(c(municipalities,corn_acreage_2,
                  corn_harvestd_2,
                  corn_production_2,
                  corn_productivity_2,state:octVPD, IrrigArea))

View(as_tibble(agr2) %>%
       #filter (state == 'BA') %>%
       filter(state %in% c('GO','MA', 'TO','PI','BA')) %>%
       #dplyr::select(c(munici,state,soy_pro,mz_pro_2,ctn_pro)) %>%
       arrange(desc(corn_acreage_2)))

###
###

df <- agr2 %>%
  dplyr::mutate(IrrigArea = replace_na(IrrigArea, 0),
                Irrigation_Occur = ifelse(IrrigArea >=1,1,0)) %>%
  #select(municipalities:Year) %>%
  #filter(state %in% c('GO','MA', 'TO','PI','BA')) %>%
  drop_na() %>%
  mutate(m = as.integer(as.factor(municipalities)),
         Drought = ifelse(Year %in% c(1985,1991,2008,2016), 1, 0), ##colocar outros anos de acordo com a literatura
         year = Year,
         Year = standardize(Year),
         Y_fact = as.factor(Year),
         PPT_jas = standardize((julPPT + augPPT + sepPPT)/3),
         PPT_ond = standardize((octPPT + novPPT + decPPT)/3),
         PPT_ndj = standardize((novPPT + decPPT + janPPT)/3),
         PPT_jfm = standardize((janPPT + febPPT + marPPT)/3),
         PPT_fm = standardize((febPPT + marPPT)/2),
         PPT_jf = standardize((janPPT + febPPT)/2),
         PPT_amj = standardize((aprPPT + mayPPT + junPPT)/3),
         PPT_mam = standardize((marPPT+ aprPPT + mayPPT)/3),
         PPT_am = standardize((aprPPT + mayPPT)/2),
         VPD_jas = standardize((julVPD + augVPD + sepVPD)/3),
         VPD_ond = standardize((octVPD + novVPD + decVPD)/3),
         VPD_ndj = standardize((novVPD + decVPD + janVPD)/3),
         VPD_jfm = standardize((janVPD + febVPD + marVPD)/3),
         VPD_jf = standardize((janVPD + febVPD)/3),
         VPD_fm = standardize((febVPD + marVPD)/2),
         VPD_amj = standardize((aprVPD + mayVPD + junVPD)/3),
         VPD_mam = standardize((marVPD+ aprVPD + mayVPD)/3)) %>%
  group_by(municipalities) %>%
  mutate(Maize_Yield = corn_productivity_2- mean(corn_productivity_2),
         Maize_Production = corn_production_2 - mean(corn_production_2),
         Maize_Planted_Area = corn_acreage_2 - mean(corn_acreage_2)) %>% 
  ungroup()

df 

library(lme4)


df2.cerrado = df %>% 
  filter(Maize_Yield < 7000,
              Maize_Yield > -6000) %>% 
  filter(state %in% c("TO", "MA", "PI", "BA", "GO", "DF")) %>%
  drop_na()


df2.cerrado %>%
  # group_by(state) %>%
  filter(!year %in% c(1985,1991,2008,2016)) %>%
  filter(Irrigation_Occur == 1) %>%
  summarise(media = mean(corn_productivity_2,na.rm=TRUE),
            dp = sd(corn_productivity_2,na.rm=TRUE))


###
###
###
lmer0_corn_C_Prod <- lmer(Maize_Production ~ Year + 
                            #Irrigation_Occur +
                            # IrrigArea +
                            (Drought +   
                               PPT_jfm #+ #febPPT 
                             
                            ) +
                            (Drought +  
                               # VPD_ndj +
                               VPD_jfm #+ 
                            ) 
                          +(1+Year|m),
                          data = df2.cerrado)

sjPlot:: tab_model(lmer0_corn_C_Prod)
###
###
###

###
###
###
lmer0_corn_C_Area <- lmer(Maize_Planted_Area ~ Year + 
                            #Irrigation_Occur +
                            # IrrigArea +
                            (Drought +   
                               PPT_jfm #+ #febPPT 
                             
                            ) +
                            (Drought +  
                               # VPD_ndj +
                               VPD_jfm #+ 
                            ) 
                          +(1+Year|m),
                          data = df2.cerrado)

sjPlot:: tab_model(lmer0_corn_C_Area)
###
###
###

lmer0_corn_C <- lmer(Maize_Yield ~ Year + 
                       #Irrigation_Occur +
                       # IrrigArea +
                       (Drought +   
                          PPT_jfm #+ #febPPT 
                        # marPPT + 
                        #  aprPPT + mayPPT + junPPT +
                        #  PPT_amj
                       ) +
                       (Drought +  
                          # VPD_ndj +
                          VPD_jfm #+ 
                        # VPD_amj #+
                        #febVPD + 
                        #marVPD) #+ 
                        # aprVPD #+
                        # mayVPD #+
                        # VPD_mam
                       ) 
                     +(1+Year|m),
                     data = df2.cerrado)

print(summary(lmer0_corn_C), correlation=TRUE)

options(na.action = "na.fail")
mm0 <- MuMIn::dredge(lmer0)
options(na.action = "na.omit")

mm0


library(effects)
library(sjPlot)
effectsize::effectsize(lmer0_corn_C)
p = sjPlot::plot_model(lmer0_corn_C, colors = c("firebrick", "blue"),
                       show.values = T,
                       value.size = 5,
                       dot.size = 3,
                       line.size = 1.3,
                       vline.color = "black")

p + theme_sjplot2(20)

sjPlot:: tab_model(lmer0_corn_C)

df2.cerrado %>% 
  group_by(Year) %>% 
  summarise (mean(corn_productivity_2,na.rm=T))

###
####
###

library(ggeffects)
library("sjlabelled")

colfunc3<-colorRampPalette(c("orange","darkred"))
colfunc<-colorRampPalette(c("royalblue","black","darkred"))
plot(rep(1,16),col=(colfunc(16)), pch=19,cex=2)


dat_C_corn = ggpredict(lmer0_corn_C, 
                       terms = c("VPD_jfm","PPT_jfm", "Drought"))
#terms = c("VPD_amj","PPT_amj", "Drought"))
plot(dat_C_corn, facet = F, 
     jitter = 0.02,add.data = F) + 
  labs(tag="D",
       x = "VPD Jan-Mar (z-score)", 
       y = "Maize yield (kg/ha)", 
       title = "Predicted Maize yield",
       colour = "PPT Jan-Mar (z-score)"
  ) +
  ylim(-2500,750) +
  
  scale_fill_manual(values = rev(colfunc(3)),
                    name="PPT Jan-Mar (z-score)",
                    breaks=c("-1.34", "-0.53","0.27"),
                    labels=c("-1 SD", "Mean", "+1 SD")) +
  scale_color_manual(values = rev(colfunc(3)),
                     name="PPT Jan-Mar (z-score)",
                     breaks=c("-1.34", "-0.53","0.27"),
                     labels=c("-1 SD", "Mean", "+1 SD")) +
  theme_light(24) +
  theme(legend.position = c(0.8,0.8))


##esse resultado precisa ser interpretado assim: para cada aumento de
##0.13 em VPD, tem (o resultado acima). Pq o VPD ali tá em Z-score. Para
##voltar o VPD para o raw value, eu fiz:
4 * sd((df2.cerrado$janVPD +
          df2.cerrado$febVPD + 
          df2.cerrado$marVPD)/3,na.rm=TRUE) + mean((df2.cerrado$janVPD +
                                                      df2.cerrado$febVPD + 
                                                      df2.cerrado$marVPD)/3,na.rm=TRUE)
-0.53 * sd((df2.cerrado$janPPT +
              df2.cerrado$febPPT + 
              df2.cerrado$marPPT)/3,na.rm=TRUE) + mean((df2.cerrado$janPPT +
                                                          df2.cerrado$febPPT + 
                                                          df2.cerrado$marPPT)/3,na.rm=TRUE)
##que deu 0.13. Entao 0.01 em z-score dá 0.13


pred_corn.cerrado <- expand.grid(#Year = as.factor(c(1988,2019)),
  Year = median(df2.cerrado$Year),
  Drought = as.factor(c(0,1)),
  PPT_ndj = median(df2.cerrado$PPT_ndj),
  PPT_fm = median(df2.cerrado$PPT_fm),
  PPT_jfm = median(df2.cerrado$PPT_jfm),
  PPT_amj = median(df2.cerrado$PPT_amj),
  VPD_ndj =  median(df2.cerrado$VPD_ndj),
  VPD_fm =  median(df2.cerrado$VPD_fm),
  VPD_jfm =  seq(0.5,2.5,0.01),
  VPD_amj =  median(df2.cerrado$VPD_amj),
  states = unique(df2.cerrado$state),
  m = median(df2.cerrado$m))

pred_corn.cerrado2 <- predict(object = lmer0_corn_C,
                              newdata = pred_corn.cerrado,
                              #interval = "predict",
                              type='response')#, allow.new.levels = TRUE)

pred_df <- cbind(pred_corn.cerrado, pred_corn.cerrado2)
head(pred_df)

pred_df %>% 
  as_tibble() %>%
  group_by(Drought, states) %>%
  summarise(mean(pred_corn.cerrado2, na.rm=TRUE))

###
###
###

df2.cerrado %>%
  group_by(Irrigation_Occur) %>%
  do(w = wilcox.test(corn_productivity_2 ~ Drought, data=., paired=FALSE,
                     alternative = "greater")) %>% 
  summarise(Irrigation_Occur, Wilcox = w$p.value)

df2.cerrado %>%
  ggplot(aes(x = as.factor(Drought),
             y = corn_productivity_2,
             col = state)) +
  facet_wrap(~Irrigation_Occur) +
  geom_boxplot()

df2.cerrado %>%
  ggplot(aes(x = corn_productivity_2,
             col = Drought)) +
  facet_wrap(~Irrigation_Occur) +
  geom_density()

df2.cerrado %>%
  group_by(Drought) %>%
  summarise(media = mean(corn_productivity_2, na.rm = TRUE),
            sd = sd(corn_productivity_2, na.rm = TRUE))

###
###
###


p <- ggplot(data = pred_df,
            aes(x = VPD_amj,
                y = pred_corn.cerrado2,#, ymin = lwr, ymax = upr,
                #color = states,
                #fill = states,
                group = Drought))

p + geom_boxplot(aes(group = Drought,
                     col = Drought,
                     fill = Drought), alpha = 0.2) +
  scale_color_manual(values = c("#4169E1",
                                "#FFA500"),
                     name="Drought occurrence",
                     breaks=c("0", "1"),
                     labels=c("Normal", "Drought")) +
  scale_fill_manual(values = c("#4169E1",
                               "#FFA500"),
                    name="Drought occurrence",
                    breaks=c("0", "1"),
                    labels=c("Normal", "Drought")) +
  #geom_line(#data = pred_df,
  #                aes(x = VPD_jas,
  #                    y = pred_corn.cerrado2,
  #                    color = states),
  #                alpha = 0.3,
  #                inherit.aes = FALSE) + 
  #geom_jitter(alpha = 0.4) +
  #facet_wrap(~states)+
  theme_light(20) +
  labs(x='VPD (Apr-Jun)',y='Maize yield (kg/ha)')#+
#ylim(0.2,1)#+
geom_ribbon(alpha = 0.2, color = FALSE) #+
scale_x_log10(labels = scales::dollar)




####
####
####

##Amazon

###
###

df2 <- agr2 %>%
  dplyr::mutate(IrrigArea = replace_na(IrrigArea, 0),
                Irrigation_Occur = ifelse(IrrigArea >=1,1,0)) %>%
  #select(municipalities:Year) %>%
  drop_na() %>%
  mutate(m = as.integer(as.factor(municipalities)),
         Drought = ifelse(Year %in% c(1997,1998,2006,2011,2016), 1, 0), ##colocar outros anos de acordo com a literatura
         year = Year,
         Year = standardize(Year),
         Y_fact = as.factor(Year),
         PPT_jas = standardize((julPPT + augPPT + sepPPT)/3),
         PPT_ond = standardize((octPPT + novPPT + decPPT)/3),
         PPT_on = standardize((octPPT + novPPT)/2),
         PPT_jfm = standardize((janPPT + febPPT + marPPT)/3),
         PPT_amj = standardize((aprPPT + mayPPT + junPPT)/3),
         PPT_mj = standardize((mayPPT + junPPT)/2),
         PPT_mam = standardize((marPPT+ aprPPT + mayPPT)/3),
         PPT_am = standardize(( aprPPT + mayPPT)/2),
         VPD_jas = standardize((julVPD + augVPD + sepVPD)/3),
         VPD_ond = standardize((octVPD + novVPD + decVPD)/3),
         VPD_on = standardize((octVPD + novVPD)/2),
         VPD_nd = standardize((novVPD + decVPD)/2),
         VPD_jfm = standardize((janVPD + febVPD + marVPD)/3),
         VPD_mam = standardize((marVPD+aprVPD + mayVPD)/3),
         VPD_amj = standardize((aprVPD + mayVPD + junVPD)/3)) %>%
  group_by(municipalities) %>%
  mutate(Maize_Yield = corn_productivity_2- mean(corn_productivity_2),
         Maize_Production = corn_production_2 - mean(corn_production_2),
         Maize_Planted_Area = corn_acreage_2 - mean(corn_acreage_2)) %>% 
  ungroup()

df2 



df2.amazon = df2 %>% filter(Maize_Yield < 7000,
                            Maize_Yield > -6000) %>% 
  filter(state %in% c("MT")) %>%
  drop_na()


df2.amazon %>%
  filter(year %in% c(1997,1998,2006,2011,2016)) %>%
  filter(Irrigation_Occur == 0) %>%
  summarise(media = mean(corn_acreage_2,na.rm=TRUE),
            dp = sd(corn_acreage_2,na.rm=TRUE))

df2.amazon %>%
  filter(year %in% c(1997,1998,2006,2011,2016)) %>%
  filter(Irrigation_Occur == 0) %>%
  summarise(media = mean(corn_production_2,na.rm=TRUE),
            dp = sd(corn_production_2,na.rm=TRUE))

df2.amazon %>%
  filter(year %in% c(1997,1998,2006,2011,2016)) %>%
  filter(Irrigation_Occur == 0) %>%
  summarise(media = mean(corn_productivity_2,na.rm=TRUE),
            dp = sd(corn_productivity_2,na.rm=TRUE))
###
###
###

lmer0_corn_A_Prod <- lmer(Maize_Production ~ Year + 
                            #Irrigation_Occur +
                            # IrrigArea +
                            (Drought +  
                               #PPT_ond +
                               PPT_jfm #+ 
                             #PPT_amj #+
                             #julPPT
                             #PPT_am
                            ) +
                            (Drought +  
                               #VPD_ond + 
                               VPD_jfm #+ 
                             #VPD_amj +
                             #julVPD
                            ) +(1+Year|m),
                          data = df2.amazon)

sjPlot:: tab_model(lmer0_corn_A_Prod)

###
###
###

lmer0_corn_A_Area <- lmer(Maize_Planted_Area ~ Year + 
                            #Irrigation_Occur +
                            # IrrigArea +
                            (Drought +  
                               #PPT_ond +
                               PPT_jfm #+ 
                             #PPT_amj #+
                             #julPPT
                             #PPT_am
                            ) +
                            (Drought +  
                               #VPD_ond + 
                               VPD_jfm #+ 
                             #VPD_amj +
                             #julVPD
                            ) +(1+Year|m),
                          data = df2.amazon)

sjPlot:: tab_model(lmer0_corn_A_Area)
###
###
###

lmer0_corn_A <- lmer(Maize_Yield ~ Year + 
                       #Irrigation_Occur +
                       # IrrigArea +
                       (Drought +  
                          #PPT_ond +
                          PPT_jfm #+ 
                        #PPT_amj #+
                        #julPPT
                        #PPT_am
                       ) +
                       (Drought +  
                          #VPD_ond + 
                          VPD_jfm #+ 
                        #VPD_amj +
                        #julVPD
                       ) +(1+Year|m),
                     data = df2.amazon)



###
###
###


print(summary(lmer0_corn_A), correlation=TRUE)

options(na.action = "na.fail")
mm0 <- MuMIn::dredge(lmer0)
options(na.action = "na.omit")
mm0

#library(effects)
#library(sjPlot)
#library(ggthemes)
effectsize::effectsize(lmer0_corn_A)

p3 = sjPlot::plot_model(lmer0_corn_A , colors = c("firebrick", "blue"),
                        show.values = T,
                        value.size = 6,
                        dot.size = 3,
                        line.size = 1.5,
                        vline.color = "black")

p3 + theme_sjplot2(20)

sjPlot:: tab_model(lmer0_corn_A)
sjPlot:: tab_model(lmer0)

####
####
####

###
####
###

library(ggeffects)
library("sjlabelled")

dat_A_corn = ggpredict(lmer0_corn_A, 
                       terms = c("VPD_jfm","PPT_jfm", "Drought"))
plot(dat_A_corn, facet = F, 
     jitter = 0.02,add.data = F) + 
  labs(tag = "C",
       x = "VPD Jan-Mar (z-score)", 
       y = "Maize yield (kg/ha)", 
       title = "Predicted Maize yield",
       colour = "PPT Jan-Mar (z-score)"
  ) +
  ylim(-2500,750) +
  scale_color_manual(values = rev(colfunc(3)),
                     name="PPT Jan-Mar (z-score)",
                     breaks=c("-0.37", "0.52","1.41"),
                     labels=c("-1 SD", "Mean", "+1 SD")) +
  theme_light(24) +
  theme(legend.position = c(0.8,0.8))


##esse resultado precisa ser interpretado assim: para cada aumento de
##0.13 em VPD, tem (o resultado acima). Pq o VPD ali tá em Z-score. Para
##voltar o VPD para o raw value, eu fiz:
1 * sd((df2.amazon$janVPD +
          df2.amazon$febVPD + 
          df2.amazon$marVPD)/3,na.rm=TRUE) + mean((df2.amazon$janVPD +
                                                     df2.amazon$febVPD + 
                                                     df2.amazon$marVPD)/3,na.rm=TRUE)
0.52 * sd((df2.amazon$janPPT +
             df2.amazon$febPPT + 
             df2.amazon$marPPT)/3,na.rm=TRUE) + mean((df2.amazon$janPPT +
                                                        df2.amazon$febPPT + 
                                                        df2.amazon$marPPT)/3,na.rm=TRUE)
##que deu 0.13. Entao 0.01 em z-score dá 0.13

####
####



pred_corn.amazon <- expand.grid(#Year = as.factor(c(1988,2019)),
  Year = median(df2.amazon$Year),
  Drought = as.factor(c(0,1)),
  PPT_jfm = median(df2.amazon$PPT_jfm),
  PPT_amj = median(df2.amazon$PPT_amj),
  VPD_ond =  median(df2.amazon$VPD_ond),
  VPD_jfm =  median(df2.amazon$VPD_jfm),
  #VPD_jfm =  seq(0.5,2.5,0.01),
  julVPD =  median(df2.amazon$julVPD),
  states = unique(df2.amazon$state),
  m = median(df2.amazon$m))

pred_corn.amazon2 <- predict(object = lmer0,
                             newdata = pred_corn.amazon,
                             #interval = "predict",
                             type='response')#, allow.new.levels = TRUE)

pred_df <- cbind(pred_corn.amazon, pred_corn.amazon2)
head(pred_df)

pred_df %>% 
  as_tibble() %>%
  group_by(states) %>%
  do(w = wilcox.test(pred_corn.amazon2~Drought, data=., paired=FALSE,
                     alternative = "greater")) %>% 
  summarise(states, Wilcox = w$p.value)

wilcox.test(Maize_Yield ~ Drought, data = df2.amazon, 
            exact = FALSE, alternative = "less") 

df2.amazon %>%
  group_by(Irrigation_Occur) %>%
  do(w = wilcox.test(corn_productivity_2 ~ Drought, data=., paired=FALSE,
                     alternative = "greater")) %>% 
  summarise(Irrigation_Occur, Wilcox = w$p.value)

df2.amazon %>%
  ggplot(aes(x = as.factor(Drought),
             y = corn_productivity_2,
             col = state)) +
  facet_wrap(~Irrigation_Occur) +
  geom_boxplot()

df2.amazon %>%
  ggplot(aes(x = corn_productivity_2,
             col = Drought)) +
  facet_wrap(~Irrigation_Occur) +
  geom_density()

df2.amazon %>%
  group_by(Drought) %>%
  summarise(media = mean(corn_productivity_2, na.rm = TRUE),
            sd = sd(corn_productivity_2, na.rm = TRUE))


p <- ggplot(data = pred_df,
            aes(x = VPD_jfm,
                y = pred_corn.amazon2,#, ymin = lwr, ymax = upr,
                #color = states,
                #fill = states,
                group = Drought))

p + geom_boxplot(aes(group = Drought,
                     col = Drought,
                     fill = Drought), alpha = 0.2) +
  scale_color_manual(values = c("#4169E1",
                                "#FFA500"),
                     name="Drought occurrence",
                     breaks=c("0", "1"),
                     labels=c("Normal", "Drought")) +
  scale_fill_manual(values = c("#4169E1",
                               "#FFA500"),
                    name="Drought occurrence",
                    breaks=c("0", "1"),
                    labels=c("Normal", "Drought")) +
  #geom_line(#data = pred_df,
  #                aes(x = VPD_jas,
  #                    y = pred_corn.amazon2,
  #                    color = states),
  #                alpha = 0.3,
  #                inherit.aes = FALSE) + 
  #geom_jitter(alpha = 0.4) +
  #facet_wrap(~states)+
  theme_light(20) +
  labs(x='VPD (Jan-Mar)',y='Maize yield (kg/ha)')#+
#ylim(0.2,1)#+
geom_ribbon(alpha = 0.2, color = FALSE) #+
scale_x_log10(labels = scales::dollar)


###

###
###
###


###
###
###





###
###
setwd("~/Dropbox/IPAM/climate_agriculture/analises/produtos_resultados_spera_florestada")
dfI.m.soya = read_csv("ibge_monthly_climate_1981_2019.csv")

#agr <- read_csv(file.path(dir.lud, "ibge_monthly_climate_1981_2019.csv"))
agr <- dfI.m.soya
#agr <- read_csv(file.path(dir.lud, "ibge_monthly_climate_soy_1981_2019.csv"))


(agr2 <- agr %>%
    dplyr::select(c(municipalities,soy_acreage,
                    soy_harvested,
                    soy_production,
                    soy_productivity,state:octVPD, IrrigArea)))

View(as_tibble(agr2) %>%
       #filter (state == 'BA') %>%
       filter(state %in% c('GO','MA', 'TO','PI','BA')) %>%
       #dplyr::select(c(munici,state,soy_pro,mz_pro_2,ctn_pro)) %>%
       arrange(desc(soy_acreage)))

###
###

df <- agr2 %>%
  dplyr::mutate(IrrigArea = replace_na(IrrigArea, 0),
                Irrigation_Occur = ifelse(IrrigArea >=1,1,0)) %>%
  #select(municipalities:Year) %>%
  #filter(state %in% c('GO','MA', 'TO','PI','BA')) %>%
  drop_na() %>%
  mutate(m = as.integer(as.factor(municipalities)),
         Drought = ifelse(Year %in% c(1985,1991,2008,2016), 1, 0), ##colocar outros anos de acordo com a literatura
         year = Year,
         Year = standardize(Year),
         Y_fact = as.factor(Year),
         PPT_jas = standardize((julPPT + augPPT + sepPPT)/3),
         PPT_ond = standardize((octPPT + novPPT + decPPT)/3),
         PPT_ndj = standardize((novPPT + decPPT + janPPT)/3),
         PPT_jfm = standardize((janPPT + febPPT + marPPT)/3),
         PPT_fm = standardize((febPPT + marPPT)/2),
         PPT_jf = standardize((janPPT + febPPT)/2),
         PPT_amj = standardize((aprPPT + mayPPT + junPPT)/3),
         PPT_mam = standardize((marPPT+ aprPPT + mayPPT)/3),
         PPT_am = standardize((aprPPT + mayPPT)/2),
         VPD_jas = standardize((julVPD + augVPD + sepVPD)/3),
         VPD_ond = standardize((octVPD + novVPD + decVPD)/3),
         VPD_ndj = standardize((novVPD + decVPD + janVPD)/3),
         VPD_jfm = standardize((janVPD + febVPD + marVPD)/3),
         VPD_jf = standardize((janVPD + febVPD)/3),
         VPD_amj = standardize((aprVPD + mayVPD + junVPD)/3),
         VPD_mam = standardize((marVPD+ aprVPD + mayVPD)/3)) %>%
  group_by(municipalities) %>%
  mutate(Soy_Yield = soy_productivity- mean(soy_productivity),
         Soy_Production = soy_production - mean(soy_production),
         Soy_Planted_Area = soy_acreage - mean(soy_acreage)) %>% 
  ungroup()

df 




df2.cerrado = df %>% filter(Soy_Yield < 7000,
                            Soy_Yield > -6000) %>% 
  filter(state %in% c("TO", "MA", "PI", "BA", "GO", "DF")) %>%
  drop_na()


df2.cerrado %>%
  # group_by(state) %>%
  filter(year %in% c(1985,1991,2008,2016)) %>%
  filter(Irrigation_Occur == 1) %>%
  summarise(media = mean(soy_productivity,na.rm=TRUE),
            dp = sd(soy_productivity,na.rm=TRUE))

df2.cerrado %>%
  #filter(year %in% c(1985,1991,2008,2016)) %>%
  filter(Irrigation_Occur == 1) %>%
  summarise(media = mean(soy_production,na.rm=TRUE),
            dp = sd(soy_production,na.rm=TRUE))

df2.cerrado %>%
  #filter(year %in% c(1985,1991,2008,2016)) %>%
  filter(Irrigation_Occur == 1) %>%
  summarise(media = mean(soy_acreage,na.rm=TRUE),
            dp = sd(soy_acreage,na.rm=TRUE))

lmer0_soy_C_Prod <- lmer(Soy_Production ~ Year + 
                           #IrrigArea +
                           
                           (Drought + #*Irrigation_Occur +  
                              #PPT_jas +
                              PPT_ond #+
                            # PPT_jfm #+ 
                            #marPPT +
                            #aprPPT
                           ) +
                           (Drought +#*Irrigation_Occur +    
                              #VPD_jas +
                              VPD_ond #+
                            #  VPD_jfm + 
                            #marVPD +
                           )  +(1+Year|m),
                         data = df2.cerrado)

sjPlot:: tab_model(lmer0_soy_C_Prod)


lmer0_soy_C_Area <- lmer(Soy_Planted_Area ~ Year + 
                           #IrrigArea +
                           
                           (Drought + #*Irrigation_Occur +  
                              #PPT_jas +
                              PPT_ond #+
                            # PPT_jfm #+ 
                            #marPPT +
                            #aprPPT
                           ) +
                           (Drought +#*Irrigation_Occur +    
                              #VPD_jas +
                              VPD_ond #+
                            #  VPD_jfm + 
                            #marVPD +
                           )  +(1+Year|m),
                         data = df2.cerrado)

sjPlot:: tab_model(lmer0_soy_C_Area)



lmer0_soy_C <- lmer(Soy_Yield ~ Year + 
                      #IrrigArea +
                      
                      (Drought + #*Irrigation_Occur +  
                         #PPT_jas +
                         PPT_ond #+
                       # PPT_jfm #+ 
                       #marPPT +
                       #aprPPT
                      ) +
                      (Drought +#*Irrigation_Occur +    
                         #VPD_jas +
                         VPD_ond #+
                       #  VPD_jfm + 
                       #marVPD +
                      )  +(1+Year|m),
                    data = df2.cerrado)

print(summary(lmer0), correlation=TRUE)

options(na.action = "na.fail")
mm0 <- MuMIn::dredge(lmer0)
options(na.action = "na.omit")

mm0



effectsize::effectsize(lmer0)
p = sjPlot::plot_model(lmer0_soy_C, colors = c("firebrick", "blue"),
                       show.values = T,
                       value.size = 5,
                       dot.size = 3,
                       line.size = 1.3,
                       vline.color = "black")

p + theme_sjplot2(20)

sjPlot:: tab_model(lmer0_soy_C)

df2.cerrado %>% 
  group_by(year) %>% 
  summarise (mean(soy_productivity,na.rm=T))


pred_soy.cerrado <- expand.grid(#Year = as.factor(c(1988,2019)),
  Year = median(df2.cerrado$Year),
  Drought = as.factor(c(0,1)),
  PPT_ond = seq(-4,5,0.5),
  #PPT_ond = median(df2.cerrado$PPT_ond),
  #VPD_jas = seq(0.5,2.5,0.01),
  #VPD_jas = seq(-2,4,0.01), ##em z-score
  VPD_ond = median(df2.cerrado$VPD_ond),
  #VPD_jas = median(df2.cerrado$VPD_jas),
  #aprVPD =  median(df2.cerrado$aprVPD),
  states = unique(df2.cerrado$state),
  m = median(df2.cerrado$m))

pred_soy.cerrado2 <- predict(object = lmer0_soy_C,
                             newdata = pred_soy.cerrado,
                             #interval = "predict",
                             type='response')#, allow.new.levels = TRUE)


pred_df <- cbind(pred_soy.cerrado, pred_soy.cerrado2)
head(pred_df)

pred_df %>% 
  as_tibble() %>%
  group_by(Drought) %>%
  summarise(mean(pred_soy.cerrado2, na.rm=TRUE))

library(ggeffects)
library("sjlabelled")



dat_C_soy = ggpredict(lmer0_soy_C, 
                      terms = c("VPD_ond","PPT_ond", "Drought"))
plot(dat_C_soy, facet = F, 
     jitter = 0.02,add.data = F) + 
  labs(tag="B",
       x = "VPD Oct-Dec (z-score)", 
       y = "Soybean yield (kg/ha)", 
       title = "Predicted Soybean yield",
       colour = "PPT Oct-Dec (z-score)"
  ) +
  ylim(-530,330) +
  scale_color_manual(values = rev(colfunc(3)),
                     name="PPT Oct-Dec (z-score)",
                     breaks=c("-1.13", "-0.15","0.84"),
                     labels=c("-1 SD", "Mean", "+1 SD")) +
  theme_light(24) +
  theme(legend.position = c(0.8,0.8))


##esse resultado precisa ser interpretado assim: para cada aumento de
##0.13 em VPD, tem (o resultado acima). Pq o VPD ali tá em Z-score. Para
##voltar o VPD para o raw value, eu fiz:
2 * sd((df2.cerrado$octVPD +
          df2.cerrado$novVPD + 
          df2.cerrado$decVPD)/3,na.rm=TRUE) + mean((df2.cerrado$octVPD +
                                                      df2.cerrado$novVPD + 
                                                      df2.cerrado$decVPD)/3,na.rm=TRUE)
-0.15 * sd((df2.cerrado$octPPT +
              df2.cerrado$novPPT + 
              df2.cerrado$decPPT)/3,na.rm=TRUE) + mean((df2.cerrado$octPPT +
                                                          df2.cerrado$novPPT + 
                                                          df2.cerrado$decPPT)/3,na.rm=TRUE)
##que deu 0.13. Entao 0.01 em z-score dá 0.13
328.91-219.46
219.46-110.01



p <- ggplot(data = pred_df,
            aes(x = VPD_ond,
                y = pred_soy.cerrado2,#, ymin = lwr, ymax = upr,
                #color = states,
                #fill = states,
                group = Drought))

p + geom_boxplot(aes(group = Drought,
                     col = Drought,
                     fill = Drought), alpha = 0.2) +
  scale_color_manual(values = c("#4169E1",
                                "#FFA500"),
                     name="Drought occurrence",
                     breaks=c("0", "1"),
                     labels=c("Normal", "Drought")) +
  scale_fill_manual(values = c("#4169E1",
                               "#FFA500"),
                    name="Drought occurrence",
                    breaks=c("0", "1"),
                    labels=c("Normal", "Drought")) +
  #geom_line(#data = pred_df,
  #                aes(x = VPD_jas,
  #                    y = pred_soy.cerrado2,
  #                    color = states),
  #                alpha = 0.3,
  #                inherit.aes = FALSE) + 
  #geom_jitter(alpha = 0.4) +
  #facet_wrap(~states)+
  theme_light(20) +
  labs(x='VPD (Oct-Dec)',y='Soybean yield (kg/ha)')#+
#ylim(0.2,1)#+
geom_ribbon(alpha = 0.2, color = FALSE) #+
scale_x_log10(labels = scales::dollar)


###
###
###

df2.cerrado %>%
  group_by(Irrigation_Occur) %>%
  do(w = wilcox.test(soy_productivity ~ Drought, data=., paired=FALSE,
                     alternative = "greater")) %>% 
  summarise(Irrigation_Occur, Wilcox = w$p.value)

df2.cerrado %>%
  ggplot(aes(x = as.factor(Drought),
             y = soy_productivity,
             col = state)) +
  facet_wrap(~Irrigation_Occur) +
  geom_boxplot()

df2.cerrado %>%
  ggplot(aes(x = soy_productivity,
             col = Drought)) +
  facet_wrap(~Irrigation_Occur) +
  geom_density()

df2.cerrado %>%
  group_by(Drought) %>%
  summarise(media = mean(soy_productivity, na.rm = TRUE),
            sd = sd(soy_productivity, na.rm = TRUE))

###
###
###



##Amazon

###
###

df2 <- agr2 %>%
  dplyr::mutate(IrrigArea = replace_na(IrrigArea, 0),
                Irrigation_Occur = ifelse(IrrigArea >=1,1,0)) %>%
  #select(municipalities:Year) %>%
  #filter(state %in% c('GO','MA', 'TO','PI','BA')) %>%
  drop_na() %>%
  mutate(m = as.integer(as.factor(municipalities)),
         Drought = ifelse(Year %in% c(1997,1998,2006,2011,2016), 1, 0), ##colocar outros anos de acordo com a literatura
         year = Year,
         Year = standardize(Year),
         Y_fact = as.factor(Year),
         PPT_jas = standardize((julPPT + augPPT + sepPPT)/3),
         PPT_as = standardize(( augPPT + sepPPT)/2),
         PPT_js = standardize((julPPT  + sepPPT)/2),
         PPT_ja = standardize((julPPT + augPPT )/2),
         PPT_ond = standardize((octPPT + novPPT + decPPT)/3),
         PPT_jfm = standardize((janPPT + febPPT + marPPT)/3),
         PPT_amj = standardize((aprPPT + mayPPT + junPPT)/3),
         PPT_mj = standardize((mayPPT + junPPT)/2),
         PPT_mam = standardize((marPPT+ aprPPT + mayPPT)/3),
         PPT_am = standardize(( aprPPT + mayPPT)/2),
         VPD_jas = standardize((julVPD + augVPD + sepVPD)/3),
         VPD_ond = standardize((octVPD + novVPD + decVPD)/3),
         VPD_jfm = standardize((janVPD + febVPD + marVPD)/3),
         VPD_mam = standardize((marVPD+aprVPD + mayVPD)/3),
         VPD_amj = standardize((aprVPD + mayVPD + junVPD)/3)) %>%
  group_by(municipalities) %>%
  mutate(Soy_Yield = soy_productivity- mean(soy_productivity),
         Soy_Production = soy_production - mean(soy_production),
         Soy_Planted_Area = soy_acreage - mean(soy_acreage)) %>% 
  ungroup()

df2 



df2.amazon = df2 %>% filter(Soy_Yield < 7000,
                            Soy_Yield > -6000) %>% 
  filter(state %in% c("MT")) %>%
  drop_na()

df2.amazon %>%
  filter(Irrigation_Occur == 1) %>%
  filter(year %in% c(1997,1998,2006,2011,2016)) %>%
  summarise(media = mean(soy_productivity,na.rm=TRUE),
            dp = sd(soy_productivity,na.rm=TRUE))

df2.amazon %>%
  #  filter(Irrigation_Occur == 0) %>%
  #filter(year %in% c(1997,1998,2006,2011,2016)) %>%
  summarise(media = mean(soy_acreage,na.rm=TRUE),
            dp = sd(soy_acreage,na.rm=TRUE))

df2.amazon %>%
  filter(Irrigation_Occur == 0) %>%
  filter(year %in% c(1997,1998,2006,2011,2016)) %>%
  summarise(media = mean(soy_production,na.rm=TRUE),
            dp = sd(soy_production,na.rm=TRUE))



ggplot(df2.amazon,aes(Irrigation_Occur,Soy_Yield, 
                      group  = Irrigation_Occur)) +
  geom_point() #+ 
geom_boxplot()

ggplot(df2.amazon,aes(Drought,Irrigation_Occur, 
                      group  = Irrigation_Occur)) +
  geom_point() + 
  geom_boxplot()


###
###
###
lmer0_soy_A_Prod <- lmer(Soy_Production ~ Year + 
                           # Irrigation_Occur + 
                           (Drought +  
                              PPT_ond# +
                           ) +
                           (Drought +  
                              VPD_ond #+
                           )  +
                           (1+Year|m),
                         data = df2.amazon)

sjPlot:: tab_model(lmer0_soy_A_Prod)

###
###
###
lmer0_soy_A_Area <- lmer(Soy_Planted_Area ~ Year + 
                           # Irrigation_Occur + 
                           (Drought +  
                              PPT_ond# +
                           ) +
                           (Drought +  
                              VPD_ond #+
                           )  +
                           (1+Year|m),
                         data = df2.amazon)

sjPlot:: tab_model(lmer0_soy_A_Area)



lmer0_soy_A <- lmer(Soy_Yield ~ Year + 
                      # Irrigation_Occur + 
                      (Drought +  
                         # PPT_jas +
                         #   julPPT +
                         #   augPPT +
                         #   sepPPT +
                         #   octPPT +
                         #  novPPT +
                         #  decPPT +
                         #  janPPT +
                         # febPPT +
                         # marPPT #+
                         PPT_ond# +
                       #PPT_jfm #+ 
                       # aprPPT
                      ) +
                      (Drought +  
                         #VPD_jas +
                         #julVPD +
                         #augVPD +
                         #sepVPD +
                         # octVPD +
                         #novVPD +
                         #decVPD +
                         #janVPD + 
                         # febVPD +
                         #marVPD +
                         VPD_ond #+
                       #VPD_amj +
                       #VPD_jfm #+
                       #aprVPD
                      )  +
                      (1+Year|m),
                    data = df2.amazon)

print(summary(lmer0_soy_A), correlation=TRUE)

options(na.action = "na.fail")
mm0 <- MuMIn::dredge(lmer0)
options(na.action = "na.omit")
mm0

library(effects)
library(sjPlot)
library(ggthemes)
effectsize::effectsize(lmer0_soy_A)

p3 = sjPlot::plot_model(lmer0_soy_A , colors = c("firebrick", "blue"),
                        show.values = T,
                        value.size = 6,
                        dot.size = 3,
                        line.size = 1.5,
                        vline.color = "black")

p3 + theme_sjplot2(20)

sjPlot:: tab_model(lmer0_soy_A)
sjPlot:: tab_model(lmer0)

effectsize::effectsize(lmer0_soy_A)
p = sjPlot::plot_model(lmer0_soy_A, colors = c("firebrick", "blue"),
                       show.values = T,
                       value.size = 5,
                       dot.size = 3,
                       line.size = 1.3,
                       vline.color = "black")

p + theme_sjplot2(20)

sjPlot:: tab_model(lmer0)

df2.amazon %>% 
  group_by(year) %>% 
  summarise (mean(soy_productivity,na.rm=T))




dat_A_soy = ggpredict(lmer0_soy_A, 
                      terms = c("VPD_ond","PPT_ond", "Drought"))
plot(dat_A_soy, facet = F, 
     jitter = 0.02,add.data = F) + 
  labs(tag = "A",
       x = "VPD Oct-Dec (z-score)", 
       y = "Soybean yield (kg/ha)", 
       title = "Predicted Soybean yield",
       colour = "PPT Oct-Dec (z-score)"
  ) +
  ylim(-530,330) +
  scale_color_manual(values = rev(colfunc(3)),
                     name="PPT Oct-Dec (z-score)",
                     breaks=c("-0.5", "0.41","1.32"),
                     labels=c("-1 SD", "Mean", "+1 SD")) +
  theme_light(24) +
  theme(legend.position = c(0.80,0.8))


##esse resultado precisa ser interpretado assim: para cada aumento de
##0.13 em VPD, tem (o resultado acima). Pq o VPD ali tá em Z-score. Para
##voltar o VPD para o raw value, eu fiz:
1 * sd((df2.amazon$octVPD +
          df2.amazon$novVPD + 
          df2.amazon$decVPD)/3,na.rm=TRUE) + mean((df2.amazon$octVPD +
                                                     df2.amazon$novVPD + 
                                                     df2.amazon$decVPD)/3,na.rm=TRUE)
0.41 * sd((df2.amazon$octPPT +
             df2.amazon$novPPT + 
             df2.amazon$decPPT)/3,na.rm=TRUE) + mean((df2.amazon$octPPT +
                                                        df2.amazon$novPPT + 
                                                        df2.amazon$decPPT)/3,na.rm=TRUE)

pred_soy.amazon <- expand.grid(#Year = as.factor(c(1988,2019)),
  Year = median(df2.amazon$Year),
  Drought = as.factor(c(0,1)),
  marPPT = median(df2.amazon$marPPT),
  febVPD = median(df2.amazon$febVPD),
  VPD_ond = seq(0.5,2.5,0.01),
  marVPD =  median(df2.amazon$marVPD),
  states = unique(df2.amazon$state),
  m = median(df2.amazon$m))

pred_soy.amazon2 <- predict(object = lmer0,
                            newdata = pred_soy.amazon,
                            #interval = "predict",
                            type='response')#, allow.new.levels = TRUE)

pred_df <- cbind(pred_soy.amazon, pred_soy.amazon2)
head(pred_df)

pred_df %>% 
  as_tibble() %>%
  group_by(Drought, states) %>%
  summarise(mean(pred_soy.amazon2, na.rm=TRUE))


p <- ggplot(data = pred_df,
            aes(x = VPD_ond,
                y = pred_soy.amazon2,#, ymin = lwr, ymax = upr,
                #color = states,
                #fill = states,
                group = Drought))

p + geom_boxplot(aes(group = Drought,
                     col = Drought,
                     fill = Drought), alpha = 0.2) +
  scale_color_manual(values = c("#4169E1",
                                "#FFA500"),
                     name="Drought occurrence",
                     breaks=c("0", "1"),
                     labels=c("Normal", "Drought")) +
  scale_fill_manual(values = c("#4169E1",
                               "#FFA500"),
                    name="Drought occurrence",
                    breaks=c("0", "1"),
                    labels=c("Normal", "Drought")) +
  #geom_line(#data = pred_df,
  #                aes(x = VPD_jas,
  #                    y = pred_soy.amazon2,
  #                    color = states),
  #                alpha = 0.3,
  #                inherit.aes = FALSE) + 
  #geom_jitter(alpha = 0.4) +
  #facet_wrap(~states)+
  theme_light(20) +
  labs(x='VPD (Oct-Dec)',y='Soybean yield (kg/ha)')#+
#ylim(0.2,1)#+
geom_ribbon(alpha = 0.2, color = FALSE) #+
scale_x_log10(labels = scales::dollar)


###
###
###

###
###
###

df2.amazon %>%
  group_by(Irrigation_Occur) %>%
  do(w = wilcox.test(soy_productivity ~ Drought, data=., paired=FALSE,
                     alternative = "greater")) %>% 
  summarise(Irrigation_Occur, Wilcox = w$p.value)

df2.amazon %>%
  ggplot(aes(x = as.factor(Drought),
             y = soy_productivity,
             col = state)) +
  facet_wrap(~Irrigation_Occur) +
  geom_boxplot()

df2.amazon %>%
  ggplot(aes(x = soy_productivity,
             col = Drought)) +
  facet_wrap(~Irrigation_Occur) +
  geom_density()

df2.amazon %>%
  group_by(Drought) %>%
  summarise(media = mean(soy_productivity, na.rm = TRUE),
            sd = sd(soy_productivity, na.rm = TRUE))

###
###
###

###
###
###