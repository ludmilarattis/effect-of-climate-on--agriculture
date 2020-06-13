######################
######################
#Ludmila Rattis
#soybean and maize prod in extreme and not extreme years
#June 2020
#####################
######################

#Loading packages
sapply(c('tidyverse',"mFilter",'scales',
         'ggpubr',"DescTools","gridExtra"), require, character.only = TRUE)


P4S <- CRS("+proj=longlat +datum=WGS84")
P4 <- CRS("+proj=utm +datum=WGS84")


####
####
##Reading dataset
my.dir = "~/users/repository"
setwd(my.dir)
(dfC = read_csv('conab_data.csv'))
####
####

###
###
##Dividing VPD by 100 - TerraClimate raw data scale is 100
dfC$vpd = as.numeric(dfC$vpd)/100
###
###



## preparing for a nice plot
nomes = c(`1planted_area`= 'Planted Area',
          `3production` = 'Crop Production',
          `2yield` = 'Crop Yield',
          `2corn`='Maize',
          `1soy`="Soybean",
          `tocantins`='Tocantins',
          `maranhao`='Maranh?o',
          `piaui`='Piau?',
          `bahia`='Bahia',
          `m.grosso`='Mato Grosso',
          `goias`='Goi?s',
          `d.federal`='Distrito Federal',
          `Cerrado`='Cerrado',
          `Amazon`='Amazon')



##creating columns with subsetted variables and detrended ag data

prep = dfC %>%
  mutate(Drought = as.factor(ifelse(chirps < mean(chirps,na.rm=T),
                                    "Drought", "Not Extreme")),
         VPD_class = as.factor(ifelse(vpd > mean(vpd,na.rm=T),
                                      "High", "Not Extreme")),
         # VPD_PPT = as.factor(
         #   ifelse(vpd > mean(vpd) &
         #            chirps < mean(chirps),
         #          "Extreme", "Not Extreme"))
         VPD_PPT = as.factor(
           ifelse(vpd > mean(vpd,na.rm=T) &
                    chirps < mean(chirps,na.rm=T),
                  "Extreme", "Not Extreme"))) %>%
  drop_na(value) %>%
  mutate (var2 = case_when(variable == "production" ~ "3production",
                          variable == "yield"  ~ "2yield",
                          variable == "planted_area" ~ "1planted_area"),
         cul2 = case_when(culture == "corn" ~ "2corn",
                          culture == "soy"  ~ "1soy")) %>%
  group_by(culture, variable,state) %>%
  mutate(scaled = scale(value),
         lin.trend = (lm(scaled ~ year))$fitted.values,
         dif = value - lin.trend,
         hp = hpfilter(value, freq = 1, type = "lambda")$cycle) %>%
  drop_na(VPD_PPT)




####
####
##creating palette
colfunc<-colorRampPalette(c("royalblue","green4","orange","darkred"))
plot(rep(1,16),col=(colfunc(16)), pch=19,cex=2)
colfunc3<-colorRampPalette(c("orange","darkred"))
####
####


#Our transformation function
scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(prep,aes(#y=value,
    y=log(dif),
    x=VPD_PPT,
    fill=VPD_PPT)) +
  geom_violin(alpha=0.4,
             draw_quantiles = c(0.03,0.97)) +
  geom_jitter(height = 0, width = 0.1,alpha=0.2) +
  #geom_boxplot(notch = TRUE, alpha=.9) +
  #geom_smooth() +
  facet_grid(~cul2~var2,scales="free",
             labeller=as_labeller(nomes)) +
  scale_color_manual(values = colfunc3(2),
                     name="Condition",
                     breaks=c("Extreme", "Not Extreme"),
                     labels=c("Extreme", "Not Extreme"))+
  scale_fill_manual(values = colfunc3(2),
                    name="Condition",
                    breaks=c("Extreme", "Not Extreme"),
                    labels=c("Extreme", "Not Extreme")) +
  scale_y_continuous(labels=scaleFUN,
                     breaks = pretty_breaks(5)) +
  stat_compare_means(aes(#y=value,
    y=log(dif),
    x=VPD_PPT,paired=T,label =  ..p.signif..),
    label.y = 1.5,
    label.y.npc = "center",
    label.x=1.5,hide.ns=TRUE,
    method = "wilcox.test", bracket.size = 1) +
  labs(x=" ",y="log (Agriculture Outputs Standardized)") +
  theme_light(20) +
  theme(legend.position = c(0.5, 0.5),
                        legend.direction = "horizontal")


ggsave(filename = "agri_outputs.tiff",
                dpi = 600)


##summarising the results
View(prep %>%
       group_by(culture, variable,VPD_PPT) %>%
       summarise(min(dif,na.rm=TRUE),
                 max(dif,na.rm=TRUE),
                 mean(dif,na.rm=TRUE),
                 sd(dif,na.rm=TRUE),
                 median(dif,na.rm=TRUE)))


View(prep %>%
       group_by(culture, variable,VPD_PPT) %>%
       summarise(sd(value,na.rm=TRUE),
                 median(value,na.rm=TRUE)))

prod = prep %>%
       select(-c(X1,chirps,double.fallow,double.single,onset,
                 single.double,soil.lower,
                 vpd,Drought,VPD_class,var2,cul2)) %>%
       group_by(state) %>%
       mutate(ID2 = 1:n()) %>%
  ungroup() %>%
     spread(variable,value) %>%
  drop_na(production) %>%
  select(-c(planted_area,yield))

plan = prep %>%
  select(-c(X1,chirps,double.fallow,double.single,onset,
            single.double,soil.lower,
            vpd,Drought,VPD_class,var2,cul2)) %>%
  group_by(state) %>%
  mutate(ID2 = 1:n()) %>%
  ungroup() %>%
  spread(variable,value) %>%
  drop_na(planted_area) %>%
  select(-c(production,yield))


yiel = prep %>%
  select(-c(X1,chirps,double.fallow,double.single,onset,
            single.double,soil.lower,
            vpd,Drought,VPD_class,var2,cul2)) %>%
  group_by(state) %>%
  mutate(ID2 = 1:n()) %>%
  ungroup() %>%
  spread(variable,value) %>%
  drop_na(yield) %>%
  select(-c(production,planted_area))


all.df = full_join(yiel,plan,by=c("year","culture","state"))
all.df2= full_join(all.df,prod,by=c("year","culture","state"))

all.df2 %>%
  #select(-c(predicted.x,ID2.x,predicted.y,ID2.y,VPD_PPT.x,
   #         VPD_PPT.y)) %>%
  mutate(resu = planted_area * yield) %>%
  ggplot(aes(x=resu,y=production)) +
  geom_line() +
  geom_point() +
  geom_smooth ()

all.df2 %>%
#  select(-c(predicted.x,ID2.x,predicted.y,ID2.y,VPD_PPT.x,
 #           VPD_PPT.y)) %>%
  mutate(resu = planted_area * yield) %>%
  group_by(culture,VPD_PPT) %>%
  summarise(mean_prod = mean(production,na.rm=TRUE),
            mean_yiel = mean(yield,na.rm=TRUE),
            mean_plan = mean(planted_area,na.rm=TRUE))

###############################################
###############################################



ggplot(prep,aes(#y=value,
  x=log(dif),
  col=VPD_PPT,
  fill=VPD_PPT)) +
  geom_density(alpha=0.2) +
  facet_grid(~var2~cul2,scales = 'free',
             labeller=as_labeller(nomes)) +
  scale_color_manual(values = rev(colfunc(4)),
                     name="Condition",
                     breaks=c("Extreme", "Not Extreme"),
                     labels=c("Extreme", "Not Extreme"))+
  scale_fill_manual(values = rev(colfunc(4)),
                    name="Condition",
                    breaks=c("Extreme", "Not Extreme"),
                    labels=c("Extreme", "Not Extreme")) +
  #stat_compare_means(aes(#y=value,
   # y=log(dif),
  #  x=VPD_PPT,paired=T,label =  ..p.signif..),
  #  label.y = 1.5,
   # label.y.npc = "center",
  #  label.x=1.5,hide.ns=TRUE,
   # method = "wilcox.test", bracket.size = 1) +
  labs(x=" ",y="log (Agriculture Outputs Standardized)") +
  theme_bw(20) #+
theme(legend.position = c(0.65, 0.87),
      legend.direction = "vertical")


ggsave(filename = "agri_outputs.tiff",
       dpi = 600)


###############################################
###############################################


