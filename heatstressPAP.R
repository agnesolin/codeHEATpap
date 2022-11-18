# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### script for Karlsö heat stress paper ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## author: Agnes Olin agnesolin@gmail.com
## 31 August 2022

library(boot) # version 1.3-28
library(DHARMa) # version 0.4.5
library(dplyr) # version 1.0.9
library(ggplot2) # version 3.3.6
library(ggpubr) # version 0.4.0
library(lubridate) # version 1.8.0
library(MASS) # version 7.3-57
library(MuMIn) # version 1.46.0
library(showtext) # version 0.9-5
library(rsq) # version 2.5
library(plyr) # version 1.8.7
library(reshape2) # version 1.4.4
library(gdata) # version 2.18.0.1
library(MetBrewer) # version 0.2.0
library(mgcv) # version 1.8-40
library(lme4) # version  1.1-29
library(sjPlot) # version 2.8.10
library(survival) # version 3.3-1
library(RcmdrPlugin.survival) # version 1.2-2
library(ggnewscale) # version 0.4.7
library(pracma) # version 2.3.8

# add custom font
font_add_google(name = "Raleway", family = "spec-font")
showtext_auto()

# add custom theme settings
theme_sets = theme(
  text = element_text(family = "spec-font"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title.x = element_text(size = 32),
  axis.title.y = element_text(size = 32),
  strip.background = element_rect(fill = "grey90"),
  legend.text = element_text(size = 30),
  legend.title = element_text(size = 30),
  
  strip.text.x = element_text(size = 38),
  axis.text.x = element_text(size = 28),
  axis.text.y = element_text(size = 28),
  legend.key.width = unit(0.5,"cm"),
  legend.key.height  = unit(0.5,"cm"),
  legend.spacing = unit(0, "cm")
)




#### load temperature data ####

# load local temperature data
temp_df = read.csv("data/Temperature_StoraKarlso.csv", sep=";")
names(temp_df) = c("date", "temp_sun", "temp_shade")
temp_df$time = as.POSIXlt(temp_df$date)
temp_df$date = as.Date(temp_df$time)

# load local temperature data from 2022
temp2022 = read.csv("data/temp_2022.csv", sep = ";", fileEncoding="UTF-8-BOM")
temp2022$time = as.POSIXlt(temp2022$time)
temp2022$time = as.POSIXlt(temp2022$time) -  hours(1) # mistake in time
temp2022$date = as.Date(temp2022$time)

# merge temperature datasets
temp_df = rbind(temp_df, temp2022)


#### behaviour analysis ####

# load data
behav = read.csv("data/thermal_behaviour.csv", sep=";", header=T, fileEncoding = 'UTF-8-BOM')
behav$time = as.POSIXlt(paste(behav$date, behav$time))
behav = merge(behav, temp_df, by = "time")

# look at size of data
range(table(paste0(behav$ledge, behav$time)))
mean(table(paste0(behav$ledge, behav$time)))
length(unique(paste0(behav$ledge, behav$time)))

# check consistency
behav$any = as.numeric(rowSums(behav[, c("panting", "raised_shoulders", "orientation_sun")]) > 0)
consistency = aggregate(any ~ as.factor(time) + temp_shade + sun_shade, data = behav, mean)
sum(consistency$any %in% c(0,1))/nrow(consistency)

# make sunniness column
behav$sunny = 1
behav$sunny[behav$sun_shade == "Shade"] = 0
behav$sunny[behav$sun_shade == "Both"] = 0.5


# calculate behaviour proportions
prop.data = behav %>% 
  group_by(ledge,time) %>% 
  dplyr::summarise(temp_shade = max(temp_shade), 
                   sunny = mean(sunny),
                   panting = mean(panting),
                   raised_shoulders = mean(raised_shoulders),
                   orientation_sun = mean(orientation_sun),
                   n = n()) 



# behaviour models
source("help_scripts/BehavModels.R")

### plot data

labels = c("panting"="a. Panting","raised_shoulders"="b. Spreading wings")
cols = rev(met.brewer(name="Demuth",n=3,type="discrete"))

ggplot2::ggplot()+
  
  # raw data
  ggplot2::geom_point(data = plot.behav[plot.behav$variable != "orientation_sun",], aes(x = temp_shade, y = value, colour = sunny),alpha = 0.4) +
  ggplot2::scale_color_gradient2(low = cols[1], mid = cols[2], high = cols[3], midpoint = 0.5, name = "Average exposure",
                                 breaks = c(0, 0.5, 1), labels = c("shade", "mix", "sun")) +
  
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5)) +
  
  ggnewscale::new_scale_color() +
  
  # line for predictions
  ggplot2::geom_line(data = pred.data, aes(x = temp_shade, y = pred, group = sun_shade, colour = sun_shade)) +
  
  # CI interval GAM predictions
  ggplot2::geom_ribbon(data = pred.data, aes(x = temp_shade, ymin = ymin, ymax = ymax, group = sun_shade, fill = sun_shade),  alpha = .15) +
  
  ggplot2::scale_color_manual(values = cols, name ="Individual exposure", labels = c("shade", "mix", "sun")) +
  ggplot2::scale_fill_manual(values = cols, name = "Individual exposure", labels = c("shade", "mix", "sun")) +
  
  ggplot2::facet_wrap(~variable,ncol=2,labeller=labeller(variable=labels))+
  
  ggplot2::labs(y = "Behaviour probability",x = "Temperature (\u00B0C)")+
  
  ggplot2::theme_bw(base_size = 10) + 
  
  theme_sets  +
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5)) +
  
  ggplot2::theme(strip.text.x=element_text(size = 40, hjust=0.01),
                 strip.background=element_blank(),
                 axis.title.x = element_text(size = 35),
                 axis.title.y = element_text(size = 35),
                 axis.line=element_line(colour="black"),
                 legend.position = "bottom",
                 legend.margin = margin(0,1.5,0,1.5, unit="cm"),
                 legend.spacing.y = unit(0.3, 'cm'),
                 legend.spacing.x = unit(0.3, 'cm'))



ggsave("figures/behaviour.png", width = 18.5, height = 11, units = "cm")



#### extract phenology and success data for each pair ####
source("help_scripts/PairNrMatch.R")


#### process data from feeding watches ####
source("help_scripts/dsPROCESSING.R")



#### create final data frame ####
presence_df$ledge[presence_df$ledge == "Röst3"] = "Rost3"
df = left_join(presence_df, pair_info_df, by = c("year", "ledge", "pairIDds"))
df$pairIDds = NULL; df$pairIDbreedingNo = NULL

df = left_join(df, temp_df, by = "time")
df$temp = df$temp_shade

df$day_since_egg = as.Date(df$time) - as.Date(as.POSIXlt(df$eggDate))

# check sample sizes #
table(df$pairIDbreeding, df$year)
sub = df[!duplicated(paste(df$ledge, df$date)),]
table(sub$year)
nrow(sub)*6


#### visualise data ####

# make violin plot

p1 = ggplot(df, aes(x = temp, y = as.factor(presence), fill = as.factor(presence), colour = as.factor(presence))) + 
  geom_violin() +
  scale_fill_manual(values = met.brewer(name="Hokusai1",n=7,type="discrete")[c(1,4,5)], labels = c("0", "1", "2"), name = "Parents present") +
  scale_colour_manual(values = met.brewer(name="Hokusai1",n=7,type="discrete")[c(1,4,5)], labels = c("0", "1", "2"), name = "Parents present") +
  labs(x = "Temperature (\u00B0C)", y = "Birds present in pair", fill = " ") +
  theme_bw() +
  theme_sets +
  theme(legend.spacing.y = unit(0.3, 'cm'),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.position = "bottom")
p1

# check min value for leaving ledge
min(df$temp[df$presence == 0], na.rm = T)


#### fit gam models ####

source("help_scripts/GAM_mods.R")

# figure of GAM predictions

plotDF$itF = factor(paste0(plotDF$it, plotDF$group))

p2 = ggplot(plotDF, aes(x = temp, y = prob, colour = group, group = itF)) +
  
  # model predictions
  geom_line(size = 0.5, alpha = 0.7) +
  scale_color_manual(values = met.brewer(name="Hokusai1",n=7,type="discrete")[c(1,4,5)], labels = c("0 birds", "1 bird", "2 birds")) +
  
  
  # labels for diff lines
  annotate("text", x = 17, y = 0.04, label = "no parent present", colour = met.brewer(name="Hokusai1",n=7,type="discrete")[c(1)],  size = 12, family = "spec-font") +
  #annotate("text", x = 26.5, y = 0.11, label = "present", colour = met.brewer(name="Hokusai1",n=7,type="discrete")[c(1)],  size = 12, family = "spec-font") +
  annotate("text", x = 22, y = 0.82 ,label = "1 parent", colour = met.brewer(name="Hokusai1",n=7,type="discrete")[c(4)],  size = 12, family = "spec-font") +
  annotate("text", x = 22, y = 0.76 ,label = "present", colour = met.brewer(name="Hokusai1",n=7,type="discrete")[c(4)],  size = 12, family = "spec-font") +
  annotate("text", x = 25, y = 0.22, label = "2 parents", colour = met.brewer(name="Hokusai1",n=7,type="discrete")[c(5)],  size = 12, family = "spec-font") +
  annotate("text", x = 25, y = 0.16, label = "present", colour = met.brewer(name="Hokusai1",n=7,type="discrete")[c(5)],  size = 12, family = "spec-font") +
  
  
  labs(x = "Temperature (\u00B0C)", y = "Attendance probability", colour = " ") +
  
  theme_bw() +
  theme_sets +
  theme(legend.position = "none")

p2


#### final plot ####


ggarrange(p1,p2,
          labels = c("a.", "b."),
          font.label = list(size = 50, face = "plain", family = "spec-font"), common.legend = T, legend = "bottom")

ggsave("figures/attendance.png", width = 18.5, height = 9.5, units = "cm")


# check probabilities for both partners being present
mean(plotDF$prob[plotDF$group == 2 & plotDF$temp == min(plotDF$temp)])
mean(plotDF$prob[plotDF$group == 2 & plotDF$temp == max(plotDF$temp)])
max(plotDF$temp)
min(plotDF$temp)





#### breeding analysis ####

## load Auklab breeding data ##
dat = read.csv("data/breedingAukLab_211114_clean.csv", na.strings = "", sep = ";")
dat2022 = read.csv("data/breeding2022.csv", na.strings = "", sep = ";")


# clean up date formats
dat$EggDate = as.POSIXlt(dat$EggDate)
dat$HatchDate = as.POSIXlt(dat$HatchDate)
dat$ChickGoneDate = as.POSIXlt(dat$ChickGoneDate)
dat$EggLossDate = as.POSIXlt(dat$EggLossDate)

dat2022$EggDate = as.POSIXlt(dat2022$EggDate, format = c("%Y-%m-%d"))
dat2022$HatchDate = as.POSIXlt(dat2022$HatchDate, format = c("%Y-%m-%d"))
dat2022$ChickLossDate = as.POSIXlt(dat2022$ChickLossDate, format = c("%Y-%m-%d"))
dat2022$FledgeDate = as.POSIXlt(dat2022$FledgeDate, format = c("%Y-%m-%d"))
dat2022$EggLossDate = as.POSIXlt(dat2022$EggLossDate, format = c("%Y-%m-%d"))

# combine older data with 2022 data

dat2022$ChickGoneDate = dat2022$FledgeDate
dat2022$ChickGoneDate[!is.na(dat2022$ChickLossDate)] = dat2022$ChickLossDate[!is.na(dat2022$ChickLossDate)]
dat2022$FledgeDate = NULL; dat2022$ChickLossDate = NULL
dat2022 = dat2022[,names(dat)]

dat = rbind(dat, dat2022)

# remove a couple of odd cases where incubation period is very short 
dat = dat[as.Date(dat$HatchDate)-as.Date(dat$EggDate) > 20 |  is.na(dat$HatchDate), ]

# fix pair id names
dat$PairID = gsub("\xf6", "o", dat$PairID) 
dat$PairID = gsub("\x9a", "o", dat$PairID) 


# add attempt no to data
pairs_attempts = paste(dat$PairID, dat$BrAttempt, sep = "-")
dat$pair_attempts = pairs_attempts

# sample size
nrow(dat)

# count no of failures
sum(dat$BreedSucc == 0, na.rm = T)



#### temperature plot ####

hoburg_temp = read.csv("data/hoburg_temp.csv", sep = ";", fileEncoding="UTF-8-BOM")
hoburg_cloud = read.csv("data/hoburg_cloud.csv", sep = ";", fileEncoding="UTF-8-BOM")

hoburg_temp_2022 = read.delim("data/hoburg_temp_2022.csv")
hoburg_cloud_2022 = read.csv("data/hoburg_cloud_2022.csv", sep = ";", fileEncoding="UTF-8-BOM")

hoburg_temp = rbind(hoburg_temp, hoburg_temp_2022)
hoburg_cloud = rbind(hoburg_cloud, hoburg_cloud_2022)



source("help_scripts/TemperaturePlot.R")


#### temperature comparisons Hoburg and Karlsö ####


# fix time formats
hoburg_temp$time = as.POSIXlt(paste(hoburg_temp$date, hoburg_temp$time), format = "%Y-%m-%d %H:%M:%S")
hoburg_cloud$time = as.POSIXlt(paste(hoburg_cloud$date, hoburg_cloud$time), format = "%Y-%m-%d %H:%M:%S")


# merge Hoburg data with local data
weather_comp = merge(hoburg_temp, temp_df, by = "time")
weather_comp = merge(hoburg_cloud, weather_comp, by = "time")


# make comparison plots
p1 = ggplot(data = weather_comp[hour(weather_comp$time) %in% 15:20,], aes(x = air_temp, y = temp_sun, col = cloudiness)) +
  geom_point() +
  scale_colour_gradient(low = "darkorange3", high = "white") +
  labs(x = "Hoburg temperature (\u00B0C)", y = "Sun-exposed probe temperature (\u00B0C)") +
  xlim(0, 45) +  ylim(0, 45) +
  geom_segment(aes(x = 0, y = 0, xend = 45, yend = 45), colour = "black", size = 0.5) +
  theme_sets


p2 = ggplot(data = weather_comp[hour(weather_comp$time) %in% 15:20,], aes(x = air_temp, y = temp_shade, col = cloudiness)) +
  geom_point() +
  scale_colour_gradient(low = "darkorange3", high = "white") +
  labs(x = "Hoburg temperature (\u00B0C)", y = "Shaded probe temperature (\u00B0C)") +
  xlim(0, 45) +  ylim(0, 45) +
  geom_segment(aes(x = 0, y = 0, xend = 45, yend = 45), colour = "black", size = 0.5) +
  theme_sets

ggarrange(p1, p2, ncol = 2, 
          common.legend = T, legend = "bottom", 
          labels = c("a.", "b."), font.label = list(size = 60, font = "spec-font"))

ggsave("figures/TempComp.png", width = 18.5, height = 12, units = "cm")



#### heat stress-induced mortality ####


# loop through breeding attempts to create histories for each attempt

df = data.frame() # empty df to store output

for (i in 1:length(pairs_attempts)) { # loop through breeding attempts
  
  d = subset(dat, pair_attempts == pair_attempts[i]) # subset to row for attempt 
  
  
  
  # identify last day with egg (hatch/loss) (count last day with egg as day before hatch day)
  eggend = ifelse(!(is.na(as.character(as.Date(d$HatchDate)-1))), as.character(as.Date(d$HatchDate)-1), ifelse(!is.na(as.character(d$EggLossDate)), as.character(d$EggLossDate), as.character(d$EggDate))) 
  
  # make sequence of dates with egg
  eggdays = seq(d$EggDate, as.POSIXlt(eggend), "days") 
  
  # add chick days if egg hatch
  if(!is.na(as.character(d$HatchDate))){
    chickdays = seq(d$HatchDate, d$ChickGoneDate, "days")
    chickdays = data.frame(Class = "chick", Date = chickdays)
  }
  
  if(is.na(as.character(d$HatchDate)))  chickdays = data.frame()
  
  
  df_out = data.frame(Pair_attempt = pairs_attempts[i], rbind(data.frame(Class = "egg", Date = eggdays), 
                                                              chickdays))
  df_out$breedingDay = 1:nrow(df_out)
  
  df = rbind(df, df_out)
  
  
  
}

## create survival variable ##

# add in end of attempt
enddate = aggregate(Date ~ Pair_attempt, data = df, function(x) max(x, na.rm = TRUE)) # calculate final dates
names(enddate) = c("Pair_attempt", "enddate")
df = merge(df, enddate, by = "Pair_attempt")


df$End = 0
df$End[df$Date == df$enddate] = 1

# identify whether end was loss or fledge
success = dat[, c("pair_attempts", "BreedSucc")]
names(success) = c("Pair_attempt", "Success")
df = merge(df, success, by = "Pair_attempt", all.x = T)
df$Death = ifelse(df$Success == 0 & df$End == 1, 1, 0)

df$End = NULL
df$Success = NULL
df$enddate = NULL


#### look at temp effect ####

# add time variables for survival analysis
df$time1 = df$breedingDay-1
df$time2 = df$breedingDay

## add weather data (assume that weather the day before is the most likely determinant of failure)

# temp
hoburg_temp = hoburg_temp[hoburg_temp$quality == "G",]
hoburg_temp$time = as.POSIXct(hoburg_temp$time, format = "%Y-%m-%d %H:%M:%S")
hoburg_temp$h = hour(hoburg_temp$time)
hoburg_temp_max = aggregate(air_temp ~ date, data = hoburg_temp[hoburg_temp$h %in% 15:21,], max)
hoburg_temp_max$date[year(hoburg_temp_max$date)!= 2022] = as.Date(hoburg_temp_max$date[year(hoburg_temp_max$date)!= 2022])+1 # this is so that weather variables from previous day matches up with the day that the failure is recorded (in 2022 we know exact date)

# clouds
hoburg_cloud = hoburg_cloud[hoburg_cloud$quality == "G",]
hoburg_cloud$time = as.POSIXct(hoburg_cloud$time, format = "%Y-%m-%d %H:%M:%S")
hoburg_cloud$h = hour(hoburg_cloud$time)
hoburg_cloud_min = aggregate(cloudiness ~ date, data = hoburg_cloud[hoburg_cloud$h %in% 15:21,], min)
hoburg_cloud_min$date[year(hoburg_temp_max$date)!= 2022] = as.Date(hoburg_cloud_min$date[year(hoburg_temp_max$date)!= 2022])+1 # this is so that weather variables from previous day matches up with the day that the failure is recorded (in 2022 we know exact date)

# merge together
df$date = as.Date(df$Date)
df = merge(df, hoburg_temp_max, by = "date", all.x = T, all.y = F)
df = merge(df, hoburg_cloud_min, by = "date", all.x = T, all.y = F)


#### make plots checking how variables breeding day and temp relate ####
df$Class = factor(df$Class, levels = c("egg", "chick"))

ggplot(data = df, aes(x = breedingDay, y = air_temp, colour = as.factor(Death), fill = as.factor(Death))) +
  scale_colour_manual(values =  rev(met.brewer("Veronese", 2)) , name = "Status") +
  scale_fill_manual(values =  rev(met.brewer("Veronese", 2)) , name = "Status") +
  facet_wrap(~Class,
             labeller = labeller(Class = 
                                   c("egg" = "a. Egg",
                                     "chick" = "b. Chick"))) +
  labs(x = "Breeding day", y = "Hoburg temperature (\u00B0C)") +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_smooth(size = 1.2) +
  theme_bw() +
  theme_sets 

ggsave("figures/TempSeason.jpg", width = 18.5, height = 9, units = "cm")



ggplot(data = df, aes(x = breedingDay, y = cloudiness, colour = as.factor(Death), fill = as.factor(Death))) +
  scale_colour_manual(values =  rev(met.brewer("Veronese", 2)) , name = "Status") +
  scale_fill_manual(values =  rev(met.brewer("Veronese", 2)) , name = "Status") +
  facet_wrap(~Class,
             labeller = labeller(Class = 
                                   c("egg" = "a. Egg",
                                     "chick" = "b. Chick"))) +
  labs(x = "Breeding day", y = "Cloudiness") +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_smooth(size = 1.2) +
  theme_bw() +
  theme_sets 

ggsave("figures/CloudSeason.jpg", width = 18.5, height = 9, units = "cm")


# add status variable (response)

df$status = as.numeric(abs(df$Death))

# remove rows of missing data
df = df[!is.na(df$air_temp) & !is.na(df$cloudiness),]


# create side variable
df$ledgeSide = sapply(df$Pair_attempt,  function(x) unlist(strsplit(x, "-"))[1])
df$ledgeSun = "sunny"
df$ledgeSun[df$ledgeSide == "Bonden"] = "mid"
df$ledgeSun[df$ledgeSide == "Rost"] = "shady" 
df$ledgeSun = as.factor(df$ledgeSun)
df$ledgeSun = relevel(df$ledgeSun, ref = "shady")


# create year factor
df$yearF = as.factor(year(df$date))

# create cloudiness factor
df$cloudF = "sunny"
df$cloudF[df$cloudiness > 25] = "cloudy"
df$cloudF = as.factor(df$cloudF)

## data exploration according to Landes et al. 2020 ##

# distribution of durations to events over the observed time
sort(df$breedingDay[df$Death == 1])
hist(df$breedingDay[df$Death == 1], breaks = seq(0, 70, 1))

# the number of failures (i.e., change of state)
sum(df$Death) # 170 failures

# the sample size
length(unique(df$Pair_attempt))
table(year(df$date))
nrow(df)
plot(
  table(year(df$Date)[df$Death == T])/
    table(year(df$Date)[!duplicated(df$Pair_attempt)])
)

# look at relationship between covariates
boxplot(air_temp ~ yearF, df) # possibly small increase but looks fine
boxplot(air_temp ~ cloudF, df) # fine (slightly warmer when sunny)
boxplot(air_temp ~ ledgeSun, df) # fine

plot(table( df$yearF, df$cloudF)) # fine
plot(table( df$yearF, df$ledgeSun)) # slight change as auk lab fills up
plot(table( df$ledgeSun, df$cloudF)) # fine 




### egg models ###

# subset data
df_egg = df[df$Class == "egg",]


mod_egg_full = coxph(Surv(time1, time2, status) ~ yearF + ledgeSun + cloudF + pspline(air_temp),
                     data = df_egg, cluster = Pair_attempt, na.action = "na.fail")

summary(mod_egg_full)

# test for proportional hazards
cox.zph(mod_egg_full) # suggests non-proportional hazards for ledgeSun


mod_egg_full = coxph(Surv(time1, time2, status) ~ yearF + tt(ledgeSun) + cloudF + pspline(air_temp),
                     data = df_egg, cluster = Pair_attempt, na.action = "na.fail")

summary(mod_egg_full)

dredge_res = dredge(mod_egg_full)

tab_df(dredge_res,
       file="dredge_egg.doc")


mod_egg_final = coxph(Surv(time1, time2, status) ~ pspline(air_temp),
                      data = df_egg, cluster = Pair_attempt, na.action = "na.fail")


### plot effect ###

mean_temp = mean(df$air_temp)
plotDF = termplot(mod_egg_final, se = TRUE, plot = FALSE)
plotDF = plotDF$air_temp
center = with(plotDF, y[x == x[which.min(abs( x - mean_temp ))]])
ytemp = plotDF$y + outer(plotDF$se, c(0, -1.96, 1.96), '*')
plotDF_new = data.frame(cbind(plotDF$x, exp(ytemp - center)))
names(plotDF_new) = c("x", "y", "ymin", "ymax")


# interpolate data for smoother lines in plot
xi = plotDF_new$x
yi = plotDF_new$y
yimin = plotDF_new$ymin
yimax = plotDF_new$ymax
x = seq(min(plotDF_new$x), max(plotDF_new$x), 0.01)

plotDF_int = data.frame(x = x, 
                        y = pchip(xi, yi, x),
                        ymin = pchip(xi, yimin, x),
                        ymax = pchip(xi, yimax, x)
)


ggplot(plotDF_int, aes(x, y, ymin = ymin, ymax = ymax)) +
  geom_line(size = 0.3) +
  geom_ribbon(alpha = 0.2) +
  labs(x = "Temperature (\u00B0C)", y = "Relative failure rate at the egg stage") +
  theme_bw() +
  theme_sets

ggsave("figures/survival.jpg", width = 8.5, height = 8.5, units = "cm")




### chick models ###

# subset data
df_chick = df[df$Class == "chick",]

# fix time variables
first_chick_day = aggregate(time1 ~ Pair_attempt, df_chick, min); names(first_chick_day) = c("Pair_attempt", "day0")
df_chick = merge(df_chick, first_chick_day)
df_chick$time1 = df_chick$time1 - df_chick$day0
df_chick$time2 = df_chick$time2 - df_chick$day0


mod_chick_full = coxph(Surv(time1, time2, status) ~ yearF + ledgeSun + cloudF + pspline(air_temp), 
                       data = df_chick, cluster = Pair_attempt, na.action = "na.fail")
summary(mod_chick_full)

# test for proportional hazards
cox.zph(mod_chick_full) # suggests non-proportional hazards for year

mod_chick_full = coxph(Surv(time1, time2, status) ~ tt(yearF) + ledgeSun + cloudF + pspline(air_temp), 
                       data = df_chick, cluster = Pair_attempt, na.action = "na.fail")

dredge_res = dredge(mod_chick_full)


tab_df(dredge_res,
       file="dredge_chick.doc")

#### plot of timing of failures in 2020 and 2022


sub2020 = df[year(df$date) == 2020,]
deaths2020 = aggregate(Death ~ date, data = sub2020, sum)

sub2022 = df[year(df$date) == 2022,]
deaths2022 = aggregate(Death ~ date, data = sub2022, sum)

deaths_heatwave = rbind(deaths2020, deaths2022)

temp_max = aggregate(temp_sun ~ date, data = temp_df, max)
deaths_heatwave = merge(deaths_heatwave , temp_max, by = "date")

deaths_heatwave$year = year(deaths_heatwave$date)


ggplot(deaths_heatwave, aes(date, Death)) +
  
  geom_rect( 
    aes(xmin = date-0.5, xmax = date+0.5, ymin = 0, ymax = 6.2, fill = temp_sun), alpha = 0.6) +
  geom_line() +
  
  scale_fill_gradientn(colours = rev(met.brewer("Greek", 10)),
                       name = "Temperature (\u00B0C)") +
  
  
  labs(x = "Day of year", y = "Recorded failures") +
  
  facet_wrap(~ year
             ,
             labeller = labeller(year = 
                                   c(
                                     "2020" = "a. 2020",
                                     
                                     "2022" = "b. 2022")),
             scale = "free_x",
             ncol = 1) +
  
  theme_bw() +
  theme_sets

ggsave("figures/plot2020.jpg", width = 18.5, height = 12, units = "cm")





table(sub2020$yday)
9/53 # losses during heatwave



