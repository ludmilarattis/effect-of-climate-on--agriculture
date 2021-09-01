library(tidyverse)
require(RColorBrewer)
library(viridis)
library(wesanderson)
library(gridExtra)

rad2deg = function(r){
  r * 57.2957795130823
}

pal <- wes_palette("Zissou1", 100, type = "continuous")

plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 250,
                          dirres = 30,
                          spdmin = 124,
                          spdmax = 1680,
                          spdseq = NULL,
                          palette = "YlOrRd",
                          countmax = NA,
                          debug = 0,
                          labs = labs){
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  
  
  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    cat("Hadley broke my code\n")
    data$spd.binned = with(data,
                           factor(spd.binned,
                                  levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Climate change \nvelocity in mm", 
                      values = spd.colors,
                      drop = FALSE) +
    #facet_wrap(~Scenario) +
    theme_bw(15) +
    labs(x="",y="") +
    labs(tag = labs)+
    theme(axis.title.x = element_blank(),
         # panel.border = element_rect(colour = "blank"),
          panel.grid.major = element_line(colour="grey65"))
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose)  
  
  # return the handle to the wind rose
  #return(c(data,p.windrose))
  return(list(print(p.windrose), data))
}
###
###
###

wd <- "~/Google Drive File Stream/My Drive/Document/Manuscript/Lud_Agri/Data"
wd <- "C:/Users/Workshop/Dropbox/IPAM/climate_agriculture/analises/produtos_resultados_spera_florestada"

cen_45 <- read_csv(file.path(wd, "climate_ag_45_growing_season.csv"))
cen_85 <- read_csv(file.path(wd, "climate_ag_85_growing_season.csv"))

###
###
###

cen_45$Scenario <- "RCP4.5"
cen_85$Scenario <- "RCP8.5"


#names(cen_45) = gsub("45_", "", names(cen_45))
#names(cen_85) = gsub("85_", "", names(cen_85))

rcps = rbind(cen_45, cen_85)


###
###
###

org_dataset <- function(datasets = rcps, 
                          yr_i_ppt = quo(ppt_i),
                          yr_f_ppt = quo(ppt_f),
                          yr_i_cwd = quo(cwd_i),
                          yr_f_cwd = quo(cwd_f))
{
  
  temp = datasets %>%
    mutate(delta_ppt  =  !!yr_f_ppt - !!yr_i_ppt,
           delta_cwd = !!yr_f_cwd - !!yr_i_cwd,
           Hypotenuse = sqrt(delta_ppt^2 + delta_cwd^2),
           DD = 2*atan(delta_ppt/(delta_cwd + sqrt(delta_ppt^2 + delta_cwd^2))),
           DD2 = ifelse(rad2deg(DD) < 0, rad2deg(DD) + 360, rad2deg(DD)))
  
  
  
  rcps_avg = temp %>% 
    group_by(Scenario) %>%
    summarise(Mean_Hypo = mean(Hypotenuse, na.rm=T),
              Mean_cwd_i = mean(!!yr_i_cwd, na.rm=T),
              Mean_cwd_f = mean(!!yr_f_cwd, na.rm=T),
              Mean_ppt_i = mean(!!yr_i_ppt, na.rm=T),
              Mean_ppt_f = mean(!!yr_f_ppt, na.rm=T))
  
  min_ppt_vec = round(min(select(temp, !!yr_f_ppt)) - 200, 0) 
  max_ppt_vec = round(max(select(temp, !!yr_i_ppt)) + 200, 0)
  
  min_cwd_vec = round(min(select(temp, !!yr_f_cwd)) - 200, 0) 
  max_cwd_vec = round(max(select(temp, !!yr_i_cwd)) + 200, 0)
  
  df = expand.grid(X = seq(min_cwd_vec, max_cwd_vec, by = 10),
                   Y = seq(min_ppt_vec, max_ppt_vec, by = 10)) %>%
    mutate(Deficit = Y - abs(X))
  
  pt = temp %>%
    #filter(Scenario=="RCP8.5") %>%
    ggplot() +
    geom_segment(aes(x = !!yr_i_cwd,   ##setas dos pontos
                     y = !!yr_i_ppt, 
                     xend = !!yr_f_cwd, 
                     yend = !!yr_f_ppt,
                     color = Hypotenuse),
                 arrow=arrow(), alpha = 1) + 
    geom_point(aes(x=!!yr_i_cwd, 
                   y=!!yr_i_ppt), size = 1.5, shape = 16, alpha = .2) +
    #geom_point(aes(x=!!yr_f_cwd, 
    #               y=!!yr_f_ppt), shape = 2) +
    stat_ellipse(aes(x=!!yr_i_cwd, 
                     y=!!yr_i_ppt),type = "t", color = "blue",lwd=1.2) +
    stat_ellipse(aes(x=!!yr_f_cwd, 
                     y=!!yr_f_ppt),type = "t", color = "red",lwd=1.2) +
    #scale_color_gradientn(colours = pal) + 
    scale_colour_gradient(low = "gray90", 
                          high = "grey10") +
    geom_segment(data = rcps_avg,
                 aes(x = Mean_cwd_i, 
                     y = Mean_ppt_i, 
                     xend = Mean_cwd_f, 
                     yend = Mean_ppt_f),
                 lwd = 2,
                 arrow=arrow(), alpha = 1) +
    #guides(fill = guide_colorbar()) 
    # scale_fill_gradient(low = "chartreuse4", high = "coral3",
    #                      space = "Lab") + 
    scale_fill_gradient(high= c("cadetblue1","chartreuse4"),
                        low=c("coral3","khaki1"),
                        space = "Lab") +
    #scale_fill_distiller(palette="RdBu",direction=-1) +
    facet_wrap(~Scenario) + 
    geom_raster(data = df, 
                aes(x = X, 
                    y = Y,
                    fill = Deficit), alpha = .6) +
    ylab("Precipitation growing season (mm)") +
    xlab("Water deficit (mm)") +
    theme_minimal(20)
  #  pt
  
  return(list(print(pt), temp))
  #  return(temp)
}


###
###
###

org_dataset_1 <- function(datasets = rcps, 
                        yr_i_ppt = quo(ppt_i),
                        yr_f_ppt = quo(ppt_f),
                        yr_i_cwd = quo(cwd_i),
                        yr_f_cwd = quo(cwd_f))
  {

  temp = datasets %>%
         mutate(delta_ppt  =  !!yr_f_ppt - !!yr_i_ppt,
                delta_cwd = !!yr_f_cwd - !!yr_i_cwd,
                Hypotenuse = sqrt(delta_ppt^2 + delta_cwd^2),
                DD = 2*atan(delta_ppt/(delta_cwd + sqrt(delta_ppt^2 + delta_cwd^2))),
                DD2 = ifelse(rad2deg(DD) < 0, rad2deg(DD) + 360, rad2deg(DD)))
  
 
  
  rcps_avg = temp %>% 
    group_by(Scenario) %>%
    summarise(Mean_Hypo = mean(Hypotenuse, na.rm=T),
              Mean_cwd_i = mean(!!yr_i_cwd, na.rm=T),
              Mean_cwd_f = mean(!!yr_f_cwd, na.rm=T),
              Mean_ppt_i = mean(!!yr_i_ppt, na.rm=T),
              Mean_ppt_f = mean(!!yr_f_ppt, na.rm=T))
  
  min_ppt_vec = round(min(select(temp, !!yr_f_ppt)) - 200, 0) 
  max_ppt_vec = round(max(select(temp, !!yr_i_ppt)) + 200, 0)
  
  min_cwd_vec = round(min(select(temp, !!yr_f_cwd)) - 200, 0) 
  max_cwd_vec = round(max(select(temp, !!yr_i_cwd)) + 200, 0)
  
  df = expand.grid(X = seq(min_cwd_vec, max_cwd_vec, by = 10),
                   Y = seq(min_ppt_vec, max_ppt_vec, by = 10)) %>%
       mutate(Deficit = Y - abs(X))
  
  pt = temp %>%
    filter(Scenario=="RCP8.5") %>%
    ggplot() +
    geom_segment(aes(x = !!yr_i_cwd, 
                     y = !!yr_i_ppt, 
                     xend = !!yr_f_cwd, 
                     yend = !!yr_f_ppt,
                     color = Hypotenuse),
                 arrow=arrow(), alpha = 1) + 
    geom_point(aes(x=!!yr_i_cwd, 
                     y=!!yr_i_ppt), size = 1.5, shape = 16, alpha = .2) +
    #geom_point(aes(x=!!yr_f_cwd, 
    #               y=!!yr_f_ppt), shape = 2) +
    stat_ellipse(aes(x=!!yr_i_cwd, 
                     y=!!yr_i_ppt),type = "t", color = "blue",lwd=1.2) +
    stat_ellipse(aes(x=!!yr_f_cwd, 
                     y=!!yr_f_ppt),type = "t", color = "red",lwd=1.2) +
    #scale_color_gradientn(colours = pal) + 
    scale_colour_gradient(low = "gray90", 
                          high = "grey10") +
    geom_segment(data = rcps_avg,
                 aes(x = Mean_cwd_i, 
                     y = Mean_ppt_i, 
                     xend = Mean_cwd_f, 
                     yend = Mean_ppt_f),
                 lwd = 2,
                 arrow=arrow(), alpha = 1) +
    #guides(fill = guide_colorbar()) 
    # scale_fill_gradient(low = "chartreuse4", high = "coral3",
    #                      space = "Lab") + 
     scale_fill_gradient(high= c("cadetblue1","chartreuse4"),
                         low=c("coral3","khaki1"),
                         space = "Lab") +
    #scale_fill_distiller(palette="RdBu",direction=-1) +
  #  facet_wrap(~Scenario) + 
    geom_raster(data = df, 
              aes(x = X, 
                  y = Y,
                  fill = Deficit), alpha = .6) +
    ylab("Precipitation growing season (mm)") +
    xlab("Water deficit (mm; Sept-June)") +
    theme_minimal(20)
#  pt
  
  return(list(print(pt), temp))
#  return(temp)
}

###
###
###
rcps_past <- org_dataset_1(datasets = rcps,
                           yr_i_ppt = quo(ppt_1970),
                           yr_f_ppt = quo(ppt_2010),
                           yr_i_cwd = quo(cwd_1970),
                           yr_f_cwd = quo(cwd_2010))

rcps_future1 <- org_dataset(datasets = rcps,
                         yr_i_ppt = quo(ppt_1970),
                         yr_f_ppt = quo(ppt_2030),
                         yr_i_cwd = quo(cwd_1970),
                         yr_f_cwd = quo(cwd_2030))

rcps_future2 <- org_dataset(datasets = rcps,
                         yr_i_ppt = quo(ppt_1970),
                         yr_f_ppt = quo(ppt_2060),
                         yr_i_cwd = quo(cwd_1970),
                         yr_f_cwd = quo(cwd_2060))

rcps_future3 <- org_dataset(datasets = rcps,
                          yr_i_ppt = quo(ppt_1970),
                          yr_f_ppt = quo(ppt_2090),
                          yr_i_cwd = quo(cwd_1970),
                          yr_f_cwd = quo(cwd_2090))


grid.arrange(grobs=c(rcps_past[[1]],rcps_future1[[1]],
             rcps_future2[[1]],rcps_future3[[1]]),nrow=2)

###
###
###

p1 <- with(subset(rcps_past[[2]]),
           plot.windrose(spd = Hypotenuse,
                         dir = DD2,
                         labs="A"))

p2 <- with(subset(rcps_future1[[2]], Scenario == "RCP4.5"),
           plot.windrose(spd = Hypotenuse, dir = DD2,
                         labs="B"))

p3 <- with(subset(rcps_future1[[2]], Scenario == "RCP8.5"),
           plot.windrose(spd = Hypotenuse,
                         dir = DD2,
                         labs="C"))

p4 <- with(subset(rcps_future1[[2]], Scenario == "RCP4.5"),
           plot.windrose(spd = Hypotenuse, dir = DD2,
                         labs="D"))

p5 <- with(subset(rcps_future2[[2]], Scenario == "RCP8.5"),
           plot.windrose(spd = Hypotenuse, dir = DD2,
                         labs="E"))

grid.arrange(p1[[1]],p2[[1]],p3[[1]],p4[[1]],p5[[1]],ncol=2)
###
###
##
colfunc<-colorRampPalette(c("royalblue","green4","orange","darkred"))
plot(rep(1,16),col=(colfunc(16)), pch=19,cex=2)
colfunc3<-colorRampPalette(c("orange","darkred"))

## past

aa = rcps_past[[2]] %>%
  ggplot() +
  geom_density(aes(x=Hypotenuse),col="darkblue",
               fill='darkblue',alpha=0.2) + 
  labs(tag = "A")+
  xlim(50,400) +
  #scale_fill_manual(values = c("darkblue")) +
  #scale_color_manual(values = c("darkblue")) +
  xlab("Arrow length (hypothenuse) in mm from 1970 to 2010") +
  theme_minimal(20)


bb = rcps_past[[2]] %>%
  mutate(class.dry = ifelse(DD2 >= 180.0001 &
                              DD2 <= 269.9999,
                                  "warmer and drier",
                            ifelse(DD2 <= 179.9999 &
                                     DD2 >= 89.9999,"warmer and wetter",
                                   ifelse(DD2 >=0.0001 &
                                            DD2 <= 90.0001,
                                          "wetter and colder",
                                          "colder and drier")))) %>% 
  ggplot() +
  geom_density(aes(x=DD2,col=class.dry,
                   fill=class.dry),alpha=0.2) + 
  labs(tag = "B")+
  xlim(140,240) +
  scale_fill_manual(values = c("red","orange"),
                    name = "class of change") +
  scale_color_manual(values = c("red","orange"),
                     name = "class of change") +
  xlab("Angle of change in degrees from 1970 to 2010") +
  theme_minimal(20) +
  theme(legend.position = c(0.8, 0.88))




cc = rcps_future1[[2]] %>%
  ggplot() +
  geom_density(aes(x=Hypotenuse),col="darkblue",
               fill='darkblue',alpha=0.2) + 
  labs(tag = "C")+
  facet_wrap(~Scenario) +
  xlim(120,700) +
  #scale_fill_manual(values = c("darkblue")) +
  #scale_color_manual(values = c("darkblue")) +
  xlab("Arrow length (hypothenuse) in mm from 1970 to 2030") +
  theme_minimal(20)


dd = rcps_future1[[2]] %>%
  mutate(class.dry = ifelse(DD2 >= 180.0001 &
                              DD2 <= 269.9999,
                            "warmer and drier",
                            ifelse(DD2 <= 179.9999 &
                                     DD2 >= 89.9999,"warmer and wetter",
                                   ifelse(DD2 >=0.0001 &
                                            DD2 <= 90.0001,
                                          "wetter and colder",
                                          "colder and drier")))) %>% 
  ggplot() +
  geom_density(aes(x=DD2,col=class.dry,
                   fill=class.dry),alpha=0.2) + 
  labs(tag = "D")+
  #xlim(140,220) +
  facet_wrap(~Scenario) +
  scale_fill_manual(values = c("red","orange"),
                    name = "class of change") +
  scale_color_manual(values = c("red","orange"),
                     name = "class of change") +
  xlab("Angle of change in degrees from 1970 to 2030") +
  theme_minimal(20) +
  theme(legend.position = "none")



ee = rcps_future2[[2]] %>%
  ggplot() +
  geom_density(aes(x=Hypotenuse),col="darkblue",
               fill='darkblue',alpha=0.2) + 
  labs(tag = "E")+
  xlim(100,1500) +
  facet_wrap(~Scenario) +
  #scale_fill_manual(values = c("darkblue")) +
  #scale_color_manual(values = c("darkblue")) +
  xlab("Arrow length (hypothenuse) in mm from 1970 to 2060") +
  theme_minimal(20)


ff = rcps_future2[[2]] %>%
  mutate(class.dry = ifelse(DD2 >= 180.0001 &
                              DD2 <= 269.9999,
                            "warmer and drier",
                            ifelse(DD2 <= 179.9999 &
                                     DD2 >= 89.9999,"warmer and wetter",
                                   ifelse(DD2 >=0.0001 &
                                            DD2 <= 90.0001,
                                          "wetter and colder",
                                          "colder and drier")))) %>% 
  ggplot() +
  geom_density(aes(x=DD2,col=class.dry,
                   fill=class.dry),alpha=0.2) + 
  labs(tag = "F")+
 # xlim(140,240) +
  facet_wrap(~Scenario) +
  scale_fill_manual(values = c("red","orange"),
                    name = "class of change") +
  scale_color_manual(values = c("red","orange"),
                     name = "class of change") +
  xlab("Angle of change in degrees from 1970 to 2060") +
  theme_minimal(20) +
  theme(legend.position = 'none')

library(gridExtra)
grid.arrange(aa,bb,cc,dd,ee,ff, ncol = 2)




####
####
####
rcps_past2 = rcps_past[[2]]

rcps_past2$Scenario = 'past'

rcps_past2$time = "2010"
rcps_future1[[2]]$time = "2030"
rcps_future2[[2]]$time = "2060"
rcps_future3[[2]]$time = "2090"

rcps.all = as.data.frame(rbind(rcps_past2,
                               rcps_future1[[2]],
                               rcps_future2[[2]],
                               rcps_future3[[2]]))

library(ggjoy)

first = rcps.all %>% as.tbl() %>%
  #convert(dbl(l2:check)) %>%
  #mutate(signal = ifelse(check==0,l2,l2*-1)) %>%
  #filter(check == 0) %>%
  #filter(Scenario != "RCP85") %>%
  ggplot(aes(y = time, x = DD2,
             col=Scenario,fill=Scenario)) +
  geom_joy(alpha=0.2) +
  #scale_y_continuous(expand = c(0.01, 0)) +
  #scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() +
  labs (tag = "A", y= "Decade",x = "Direction of changes in climate space \nfor agricultural plots (in degrees)") +
  theme_light(20) +
  #facet_wrap(~check) +
  geom_vline(xintercept=180,linetype=2,col='gray55') +
  scale_color_manual(values = colfunc(3),
                     label = c("Observed",
                               "RCP 4.5 W/m2",
                               "RCP 8.5 W/m2")) +
  scale_fill_manual(values = colfunc(3),
                    label = c("Observed",
                              "RCP 4.5 W/m2",
                              "RCP 8.5 W/m2")) +
  theme(legend.position = c(0.85, 0.80)) +
  annotate(geom="text", x=170, y=5.3,
           label="wet and warm",
           color="red") +
  annotate(geom="text", x=198, y=5.3,
           label="dry and warm",
           color="red")


second = rcps.all %>% as.tbl() %>%
  #convert(dbl(l2:check)) %>%
  #mutate(signal = ifelse(check==0,l2,l2*-1)) %>%
  #filter(check == 0) %>%
  #filter(Scenario != "RCP85") %>%
  ggplot(aes(y = time, x = Hypotenuse,
             col=Scenario,fill=Scenario)) +
  geom_joy(alpha=0.2) +
  #scale_y_continuous(expand = c(0.01, 0)) +
  #scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() +
  labs (tag = "B", y= "Decade",
        x = "Speed of changes in climate space \nfor agricultural plots (in mm/30yr)") +
  theme_light(20) +
  #facet_wrap(~check) +
  geom_vline(xintercept=323,linetype=2,col='gray55') +
  scale_color_manual(values = colfunc(3),
                     label = c("Observed",
                               "RCP 4.5 W/m2",
                               "RCP 8.5 W/m2")) +
  scale_fill_manual(values = colfunc(3),
                    label = c("Observed",
                              "RCP 4.5 W/m2",
                              "RCP 8.5 W/m2")) +
  theme(legend.position = "none") +
  annotate(geom="text", x=335, y=4.5,
           label="Threshold (95%)",
             color="red")

# run the thris from the script "qunatify climate suitability"




rcps.all %>% as.tbl() %>%
  group_by(time,Scenario) %>%
  summarise(sd(DD2),median(DD2), sd(DD2)/sqrt(length(DD2)))

rcps.all %>% as.tbl() %>%
  group_by(time,Scenario) %>%
  summarise(med=median(Hypotenuse),
            mean1 = mean(Hypotenuse),
            sem=sd(Hypotenuse)/sqrt(length(Hypotenuse))) %>%
  mutate(lower = mean1-(2*sem),
         upper = mean1+(2*sem))

se = function(data){ sd(data)/sqrt(length(data))}

View(rcps.all %>% as.tbl() %>% 
  group_by(Scenario) %>%
  summarise_at(vars(ppt_2010:ppt_2090), funs(median,sd,se))) #%>% 
  #gather(key = "key", value = "value",-c(Scenario)) %>%
  #separate(key, c("variable", "stat"), sep = "_")) 

View(rcps.all %>% as.tbl() %>% 
       group_by(Scenario) %>%
       summarise_at(vars(cwd_2010:cwd_2090), funs(median,sd,se)))

rcps.all %>% as.tbl() %>%
  group_by(time,Scenario) %>%
  filter(DD2 < 179) %>%
  summarise(length(DD2))

###END

rcps_future1[[2]] %>%
  ggplot(aes(x = cwd_2000, 
             y = Hypotenuse)) +
  geom_point() +
  geom_smooth(se = F) +
  geom_smooth(se = F, method = "lm") +
  facet_wrap(~Scenario) +
  theme_minimal()

rcps_future1[[2]] %>%
  ggplot(aes(x = ppt_2000, 
             y = Hypotenuse)) +
  geom_point() +
  geom_smooth(se = F) +
  geom_smooth(se = F, method = "lm") +
  facet_wrap(~Scenario) +
  theme_minimal()

###
###
###


org_dataset2 <- function(datasets = rcps, 
                        yr_i_ppt = quo(ppt_2010),
                        yr_f_ppt = quo(ppt_2050),
                        yr_i_cwd = quo(cwd_2010),
                        yr_f_cwd = quo(cwd_2050))
{
  
  temp = datasets %>%
    mutate(delta_ppt  =  !!yr_f_ppt - !!yr_i_ppt,
           delta_cwd = !!yr_f_cwd - !!yr_i_cwd,
           Hypo = sqrt(delta_ppt^2 + delta_cwd^2),
           DD = 2*atan(delta_ppt/(delta_cwd + sqrt(delta_ppt^2 + delta_cwd^2))),
           DD2 = ifelse(rad2deg(DD) < 0, rad2deg(DD) + 360, rad2deg(DD)))
  
  
  
  rcps_avg = temp %>% 
    group_by(Scenario) %>%
    summarise(Mean_Hypo = mean(Hypo, na.rm=T),
              Mean_cwd_1970 = median(!!yr_i_cwd, na.rm=T),
              Mean_cwd_2090 = median(!!yr_f_cwd, na.rm=T),
              Mean_ppt_1970 = median(!!yr_i_ppt, na.rm=T),
              Mean_ppt_2090 = median(!!yr_f_ppt, na.rm=T))
  
  min_ppt_vec = round(min(select(temp, !!yr_f_ppt)) - 200, 0) 
  max_ppt_vec = round(max(select(temp, !!yr_i_ppt)) + 200, 0)
  
  min_cwd_vec = round(min(select(temp, !!yr_f_cwd)) - 200, 0) 
  max_cwd_vec = round(max(select(temp, !!yr_i_cwd)) + 200, 0)
  
  df = expand_grid(X = seq(min_cwd_vec, max_cwd_vec, by = 10),
                   Y = seq(min_ppt_vec, max_ppt_vec, by = 10)) %>%
    mutate(Deficit = Y - abs(X))
  
  pt = temp %>%
    ggplot() +
    geom_segment(aes(x = !!yr_i_cwd, 
                     y = !!yr_i_ppt, 
                     xend = !!yr_f_cwd, 
                     yend = !!yr_f_ppt,
#                     color = Hypo,
                     color = -1*(!!yr_f_ppt - abs(!!yr_f_cwd))),
                 arrow=arrow(), alpha = 0.2) + 
    geom_point(aes(x=!!yr_i_cwd, 
                   y=!!yr_i_ppt,
                   color = -1*(!!yr_f_ppt - abs(!!yr_f_cwd))), 
               size = 3.5, 
               shape = 1, alpha = .2) +
    #geom_point(aes(x=!!yr_f_cwd, 
    #               y=!!yr_f_ppt), shape = 2) +
    stat_ellipse(aes(x=!!yr_i_cwd, 
                     y=!!yr_i_ppt),type = "t", color = "blue") +
    stat_ellipse(aes(x=!!yr_f_cwd, 
                     y=!!yr_f_ppt),type = "t", color = "red") +
    scale_color_gradientn(colours = pal) + 
    #scale_colour_gradient(low = "white", 
    #                      high = "grey") +
    geom_segment(data = rcps_avg,
                 aes(x = Mean_cwd_1970, 
                     y = Mean_ppt_1970, 
                     xend = Mean_cwd_2090, 
                     yend = Mean_ppt_2090),
                 lwd = 2,
                 arrow=arrow(), alpha = 1) +
    #guides(fill = guide_colorbar()) 
    scale_fill_gradientn(colours = pal) + 
    facet_wrap(~Scenario) + 
    #geom_raster(data = df, 
    #            aes(x = X, 
    #                y = Y,
    #                fill = -1*Deficit), alpha = .6) +
    ylab("Annual Precipitation (mm)") +
    xlab("Dry-season Pntensity (mm)") +
    theme_minimal()
  #  pt
  
  return(list(print(pt), temp))
  #  return(temp)
}


org_dataset2(datasets = rcps,
             yr_i_ppt = quo(ppt_2000),
             yr_f_ppt = quo(ppt_2050),
             yr_i_cwd = quo(cwd_2000),
             yr_f_cwd = quo(cwd_2050))

