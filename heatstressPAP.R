# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### attendance ~ temperature heat stress paper ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(boot)
library(DHARMa)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(MASS)
library(MuMIn)
library(showtext) # version 0.9-4
library(rsq)
library(plyr)
library(reshape2)
library(gdata)
library(MetBrewer)
library(mgcv)
library(lme4)
library(sjPlot)

# add custom font
font_add_google(name = "Raleway", family = "spec-font")
showtext_auto()


theme_sets = theme(
  text = element_text(family = "spec-font"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title.x = element_text(size = 32),
  axis.title.y = element_text(size = 32),
  strip.background = element_rect(fill = "grey90"),
  legend.text = element_text(size = 30),
  legend.title = element_text(size = 30),
  legend.key.width = unit(1,"cm"),
  strip.text.x = element_text(size = 38),
  axis.text.x = element_text(size = 28),
  axis.text.y = element_text(size = 28))



#### behaviour analysis ####

behav = read.csv("data/Temp_study_30sec.csv", sep=";", header=T, fileEncoding = 'UTF-8-BOM')

# subset dataframe to columns of interest
behav = behav[, c(1:2,6:10)] 

#change decimal separator in temperature values so Temp turns into a numerical variable
behav$Temp = as.numeric(gsub(",",".",behav$Temp))

# calculate behaviour proportions
prop.data = behav %>% 
  group_by(Date,Video.time) %>% 
  dplyr::summarise(across(2:5, mean),n=n(),Temp=max(Temp)) 

# behaviour models
source("help_scripts/BehavModels.R")

### plot data

labels = c("Panting"="a. Panting","Raised.shoulders"="b. Spreading/drooping wings","Orientation.towards.sun"="c. Orientation towards sun")


beh.plot = ggplot(plot.behav, aes(Temp, value))+
  
  geom_point(alpha = 0.4)+
  
  # line for GAM predictions
  geom_line(aes(y = pred)) +
  
  # CI interval GAM predictions
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) +
  
  
  facet_wrap(~variable,ncol=1,labeller=labeller(variable=labels))+
  
  labs(y = "Behaviour probability",x = "Temperature ((\u00B0C))")+
  
  theme_bw() + 
  
  theme_sets +
  
  theme(strip.text.x=element_text(size = 25, hjust=0.01),
        strip.background=element_blank(),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.line=element_line(colour="black"))


png("figures/behaviour.png", 
    width = 8.5, height = 17, units = 'cm', res = 200, pointsize = 9, family = "sans")

beh.plot

dev.off()





#### extract phenology and success data for each pair ####
source("help_scripts/PairNrMatch.R")


#### process dygnsstudie-data ####
source("help_scripts/dsPROCESSING.R")


#### load and plot temperature data ####

temp_df = read.csv("data/Temperature_StoraKarlso.csv", sep=";")
names(temp_df) = c("date", "temp_sun", "temp_shade")

temp_df$time = as.POSIXlt(temp_df$date)
temp_df$date = as.Date(temp_df$time)




#### create final data frame ####
pair_info_df$ledge[pair_info_df$ledge == "Rost3"] = "R?st3"
df = left_join(presence_df, pair_info_df, by = c("year", "ledge", "pairIDds"))
df$pairIDds = NULL; df$pairIDbreedingNo = NULL

df = left_join(df, temp_df, by = "time")
df$temp = df$temp_shade
df$temp[df$ledge == "Farallon3"] = df$temp_sun[df$ledge == "Farallon3"]

df$day_since_egg = as.Date(df$time) - as.Date(as.POSIXlt(df$eggDate))



#### visualise data ####


# make violin plot

p1 = ggplot(df, aes(x = temp, y = as.factor(presence), fill = as.factor(presence))) + 
  geom_violin() +
  scale_fill_manual(values = met.brewer(name="Hokusai1",n=7,type="discrete")[c(1,4,5)]) +
  labs(x = "Temperature (\u00B0C)", y = "Birds present in pair", fill = "Birds present") +
  theme_bw() +
  theme_sets 
p1

# check min value for leaving ledge
min(df$temp[df$presence == 0])



### ordinal logistic regression ###

# prepare variables
df$hour = hour(df$time) # hour of day
df$day_since_egg = as.integer(df$day_since_egg) # no of day since laying
df$presence_mod = as.integer(df$presence + 1)
df$yearF = as.factor(df$year)
df$ledge = as.factor(df$ledge)

# remove cases with missing data
df = df[!is.na(df$day_since_egg) & !is.na(df$temp),]

# look at sample sizes
sort(unique(df$pairIDbreeding))
samp = unique(data.frame(ledge =df$ledge, date = as.Date(df$time)))
table(year(samp$date))
length(unique(paste(df$ledge, as.Date(df$time))))*6


# fit initial gam mod
gam.mod = gam(presence_mod ~ s(temp) + 
                s(day_since_egg, k = 10) + 
                s(hour, k = 6) + # k adjusted based on possible values 
                s(yearF, bs = "re") + 
                s(ledge, bs = "re"), 
              family = ocat(R = 3),
              data = df,
              method = "REML",
              na.action = na.fail)

# gam.check
gam.check(gam.mod)

# indicates k is too low for day since egg
# successively increase k until gam.check results look better

gam.mod = gam(presence_mod ~ s(temp, k = 3) + 
                s(day_since_egg, k = 4) + 
                s(hour, k = 3) + # k adjusted based on possible values 
                s(yearF, bs = "re") + 
                s(ledge, bs = "re"), 
              family = ocat(R = 3),
              data = df,
              method = "REML",
              na.action = na.fail)
gam.check(gam.mod)

# check smooths
plot(gam.mod, pages = 1, rug = T)
# temp and day since eggs quite wiggly

gam.mod$sp
# temp 0.10862644
# days since egg = 0.01432075
# increase until they look reasonable

gam.mod = gam(presence_mod ~ 
                s(temp) + 
                s(day_since_egg, k = 17) + 
                s(hour, k = 6) + # k adjusted based on possible values 
                s(yearF, bs = "re") + 
                s(ledge, bs = "re"), 
              family = ocat(R = 3),
              data = df,
              method = "REML",
              na.action = na.fail,
              select = T)
plot(gam.mod, pages = 1, rug = T)



# https://www.maths.ed.ac.uk/~swood34/mgcv/tampere/mgcv-advanced.pdf




# checking concurvity
concurvity(gam.mod, full = TRUE)
concurvity(gam.mod, full = FALSE)
# looks fine


scatter.smooth(df$hour, df$temp)
cor.test(df$temp, df$hour)
scatter.smooth( df$day_since_egg, df$temp)
cor.test(df$temp, df$day_since_egg)


# how to find right level of smoothness?
# model selection etc?
# model assumptions incl multicollinearity
# predictions for random effects?

gam.mod = gam(presence_mod ~ s(temp, sp = 5) + 
                s(day_since_egg, sp = 5) + 
                s(hour, k = 6) + 
                s(yearF, bs = "re") + 
                s(ledge, bs = "re"), 
              family = ocat(R = 3),
              data = df,
              method = "REML",
              na.action = na.fail)


summary(gam.mod)

res = dredge(gam.mod,
             subset = {s(yearF, bs = "re")} & {s(ledge, bs = "re")})

gam.check(gam.mod)
plot(gam.mod)


new.data = data.frame(temp = seq(min(df$temp), max(df$temp), by = 0.1),
                      day_since_egg = 32, # at hatching
                      hour = 17.5, # halfway through study period
                      ledge = "Farallon3",
                      yearF = 2021)

preds = predict(gam.mod, type = "response", newdata = new.data)

preds = as.data.frame(preds)
names(preds) = c("prob0", "prob1", "prob2")

plotDF = data.frame(
  temp = rep(new.data$temp, 3),
  prob = c(preds$prob0, preds$prob1, preds$prob2),
  group = as.factor(rep(c(0,1,2), each = nrow(new.data)))
  
)


p2 = ggplot(plotDF, aes(x = temp, y = prob, colour = group)) +
  geom_line(size = 1) +
  scale_color_manual(values = met.brewer(name="Hokusai1",n=7,type="discrete")[c(1,4,5)]) +
  labs(x = "Temperature (\u00B0C)", y = "Probability", colour = "Birds present") +
  theme_bw() +
  theme_sets

p2



#### final plot ####
png("figures/attendance.png", 
    width = 30, height = 15, units = 'cm', res = 200, pointsize = 9, family = "sans")


ggarrange(p1,p2,
          labels = c("a.", "b."),
          font.label = list(size = 50, face = "plain", family = "spec-font"),
          common.legend = TRUE, legend = "bottom")

dev.off()




#### temperature plot ####

hoburg_temp = read.csv("data/hoburg_temp.csv", sep = ";", fileEncoding="UTF-8-BOM")
hoburg_cloud = read.csv("data/hoburg_cloud.csv", sep = ";", fileEncoding="UTF-8-BOM")


source("help_scripts/TemperaturePlot.R")




#### extract temperatures for heat stress-induced mortality ####


# Farallon 3, pair 4, 06-06-2019, 19:12:16
hoburg_temp$air_temp[hoburg_temp$date == "2019-06-06" & hoburg_temp$time == "19:00:00"]
hoburg_cloud$cloudiness[hoburg_cloud$date == "2019-06-06" & hoburg_cloud$time == "19:00:00"]


# Farallon 3, pair 4, 27-06-2020, 17:44:34
temp_df$temp_sun[temp_df$time == as.POSIXlt("2020-06-27 17:44:00")]
hoburg_temp$air_temp[hoburg_temp$date == "2020-06-27" & hoburg_temp$time == "18:00:00"]
hoburg_cloud$cloudiness[hoburg_cloud$date == "2020-06-27" & hoburg_cloud$time == "18:00:00"]


# Farallon 3, pair 7, 27-06-2020, 18:49:43
temp_df$temp_sun[temp_df$time == as.POSIXlt("2020-06-27 18:50:00")]
hoburg_temp$air_temp[hoburg_temp$date == "2020-06-27" & hoburg_temp$time == "19:00:00"]
hoburg_cloud$cloudiness[hoburg_cloud$date == "2020-06-27" & hoburg_cloud$time == "19:00:00"]

# Triangle 3, pair 2, 26-06-2020, 15:57:35
temp_df$temp_shade[temp_df$time == as.POSIXlt("2020-06-26 15:58:00")]
hoburg_temp$air_temp[hoburg_temp$date == "2020-06-26" & hoburg_temp$time == "16:00:00"]
hoburg_cloud$cloudiness[hoburg_cloud$date == "2020-06-26" & hoburg_cloud$time == "16:00:00"]


# Triangle 3, pair 3, 27-06-2020, 17:32:58
temp_df$temp_shade[temp_df$time == as.POSIXlt("2020-06-27 17:32:00")]
hoburg_temp$air_temp[hoburg_temp$date == "2020-06-27" & hoburg_temp$time == "18:00:00"]
hoburg_cloud$cloudiness[hoburg_cloud$date == "2020-06-27" & hoburg_cloud$time == "18:00:00"]






#### heat stress-induced mortality ####


hoburg_temp$time = paste(hoburg_temp$date, hoburg_temp$time)
hoburg_cloud$time = paste(hoburg_cloud$date, hoburg_cloud$time)


weather_comp = merge(hoburg_temp, temp_df, by = "time")
weather_comp = merge(hoburg_cloud, weather_comp, by = "time")


ggplot(data = weather_comp, aes(x = air_temp, y = temp_shade)) +
  geom_point()

ggplot(data = weather_comp, aes(x = air_temp, y = temp_sun, col = cloudiness)) +
  geom_point(size = 3)

ggplot(data = weather_comp[hour(weather_comp$time) %in% 15:20,], aes(x = air_temp, y = temp_sun, col = cloudiness)) +
  geom_point(size = 3)


ggplot(data = weather_comp, aes(x = temp_shade, y = temp_sun, col = cloudiness)) +
  geom_point(size = 3)

ggplot(data = weather_comp[hour(weather_comp$time) %in% 15:20,], aes(x = temp_shade, y = temp_sun, col = cloudiness)) +
  geom_point(size = 3)

# add wind too?

# add Jonas's temp predictions

predTempMax = read.csv("data/~/nonSU/Karlso/heat_stress/heatstressATTENDANCE/PredictedTempsDaily2009-2021.csv")
names(predTempMax)[1] = "date"
predTempMax$date = as.Date(predTempMax$date)

# compare with ledge data
maxLedgeTemp = aggregate(temp_sun  ~ date, data = temp_df, FUN = max)
maxLedgeTemp$temp_shade = aggregate(temp_shade  ~ date, data = temp_df, FUN = max)$temp_shade
#maxLedgeTemp$Yr = year(maxLedgeTemp$date)
#maxLedgeTemp$Day = yday(maxLedgeTemp$date)

maxComp = merge(maxLedgeTemp, predTempMax, by = c("date"))

ggplot(data = maxComp, aes(x = temp_sun, y = Predicted)) +
  geom_point()+
  geom_segment(x = 0, y = 0, xend = 50, yend = 50) +
  theme_classic() +
  facet_wrap(~ year(date))


## Load Auklab breeding data ##
dat = read.csv("data/breedingAukLab_211114_clean.csv", na.strings = "", sep = ";")
dat = read.csv("data/breedingAukLab_211114_original.csv", na.strings = "", sep = ";")


# remove some odd cases where incubation period is very short 
dat = dat[as.Date(dat$HatchDate)-as.Date(dat$EggDate) > 20 |  is.na(dat$HatchDate), ]


# clean up date formats
dat$EggDate = as.POSIXlt(dat$EggDate)
dat$HatchDate = as.POSIXlt(dat$HatchDate)
dat$ChickGoneDate = as.POSIXlt(dat$ChickGoneDate)
dat$EggLossDate = as.POSIXlt(dat$EggLossDate)


# fix pair id names
dat$PairID = gsub("\xf6", "o", dat$PairID) 
dat$PairID = gsub("\x9a", "o", dat$PairID) 


# add attempt no to data
pairs_attempts = paste(dat$PairID, dat$BrAttempt, sep = "-")
dat$pair_attempts = pairs_attempts

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




# add in end of attempt
enddate = aggregate(Date ~ Pair_attempt, data = df, function(x) max(x, na.rm = TRUE)) # calculate final dates
enddate$End = 0
df$End = enddate[match(paste(df$Pair_attempt, df$Date), paste(enddate$Pair_attempt, enddate$Dat)), "End"]


# identify whether end was loss or fledge
success = dat[, c("pair_attempts", "BreedSucc")]
names(success) = c("Pair_attempt", "Success")
df = merge(df, success, by = "Pair_attempt", all.x = T)
df$Death = ifelse(df$Success == 0 & df$End == 0, 1, 0)
df$Death[is.na(df$Death)] = 0

df$End = NULL
df$Success = NULL


# some initial failure stats
sum(df$Death)

plot(
  table(year(df$Date)[df$Death == T])/
    table(year(df$Date)[!duplicated(df$Pair_attempt)])
)

#### look at temp effect ####

# add time variables for survival analysis
df$time1 = df$breedingDay-1
df$time2 = df$breedingDay


# add temp
df$date = as.Date(df$Date)
df = merge(df, predTempMax, by = "date", all.x = T, all.y = F)

# make some initial plots
ggplot(data = df, aes(x = Predicted, y = Death)) +
  facet_wrap(~Class) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  theme_bw()

ggplot(df[df$Class == "chick",], aes(x = Predicted, y=as.factor(Death))) + 
  geom_violin() +
  theme_bw()

ggplot(df[df$Class == "egg",], aes(x = Predicted, y=as.factor(Death))) + 
  geom_violin() +
  theme_bw()


ggplot(data = df, aes(x = breedingDay, y = Death)) +
  facet_wrap(~Class) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  theme_bw()


ggplot(data = df, aes(x = breedingDay, y = Predicted, colour = as.factor(Death))) +
  facet_wrap(~Class) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  theme_bw()


# possible effect of temperature on egg loss, but could be due to corr with time in breeding season

# need to be explicit with assumptions re when losses occur

library(survival)
library(RcmdrPlugin.survival)

df$status = as.factor(df$Death)
df = df[!is.na(df$Predicted),]

mod = coxph(Surv(time1, time2, status) ~ Predicted + Class,
            data = df, id = Pair_attempt)
summary(mod)

mod2 = coxph(Surv(time1, time2, status) ~ Predicted + Class + Predicted*Class,
             data = df, id = Pair_attempt)
summary(mod2)

mod3 = coxph(Surv(time1, time2, status) ~ Predicted,
             data = df[df$Class == "egg",], id = Pair_attempt)
summary(mod3)

mod4 = coxph(Surv(time1, time2, status) ~ Predicted,
             data = df[df$Class == "chick",], id = Pair_attempt)
summary(mod4)

plot(df[df$Class == "egg", "Predicted"], predict(mod3))
