### ordinal logistic regression ###

# prepare variables
df$hour = hour(df$time) # hour of day
df$day_since_egg = as.integer(df$day_since_egg) # no of day since laying
df$presence_mod = as.integer(df$presence + 1) # create response variable
df$id = as.factor(df$pairIDbreeding) # factor to be used as a random variable

# remove cases with missing data
df = df[!is.na(df$day_since_egg) & !is.na(df$temp),]

# look at sample sizes
sort(unique(df$pairIDbreeding))
samp = unique(data.frame(ledge =df$ledge, date = as.Date(df$time)))
table(year(samp$date))
length(unique(paste(df$ledge, as.Date(df$time))))*6


#### fit gam model subsampling to one interval per hour ####

# unique hour-pair combinations
subsamp = unique(data.frame(id = df$pairIDbreeding,
                            date = df$date, 
                            hour = df$hour))

# empty data frame to store dredge results
dredge_res = data.frame()

# empty data frame to store concurvity results
conc_res = data.frame()

ggplot(df, aes(day_since_egg, temp)) +
  geom_point(alpha = 0.3) +
  geom_smooth()

ggplot(df, aes(hour, temp)) +
  geom_point(alpha = 0.3) +
  geom_smooth(formula = y ~ s(x, bs = "cs", k = 5))


# empty data for qq data
qq_data = data.frame()


for(it in 1:100){ # 100 iterations
  
  print(it) # print iteration
  set.seed(it) # set seed so that results can be reproduced
  
  new_df = data.frame() # empty data frame to store generated data
  
  for(i in 1:nrow(subsamp)){ # loop through unique combinations, sample data and tie together
    
    sub = df[df$pairIDbreeding == subsamp$id[i] & df$date == subsamp$date[i] & df$hour == subsamp$hour[i], ]
    
    ranRow = sub[sample(1:nrow(sub), 1),]
    
    new_df = rbind(new_df, ranRow)
  }
  
  
  # fit gam mod
  gam.mod = gam(presence_mod ~ 
                  s(temp, k = 3) + # temp effect, restrict to cubic
                  s(day_since_egg, k = 5) + # effect of day since egg - allow more flexible shape
                  s(hour, k = 3) +  # effect of hour, restrict to cubic
                  s(id, bs = "re"), # random effect of pair id
                family = ocat(R = 3),
                data = new_df,
                method = "REML",
                na.action = na.fail)
  
  
  # save model comparison data
  res = dredge(gam.mod,
               subset = {s(id, bs = "re")})
  
  dredge_res = rbind(dredge_res, 
                     
                     data.frame(days  = res$`s(day_since_egg, k = 5)`,
                                hour = res$`s(hour, k = 3)`,
                                temp = res$`s(temp, k = 3)`,
                                delta = res$delta)
                     
                     
                     )
  
  # save concurvity data
  conc_res = rbind(conc_res,
  concurvity(gam.mod, full = TRUE))
  
  # save qq-data
  qq_data = rbind(qq_data, 
                  data.frame(
                    theo_quant = qq.gam(gam.mod),
                    resid =  sort(residuals.gam(gam.mod, type = "deviance"))
                  ))
  
}


# summarise model comparison results
sum(!is.na(dredge_res[dredge_res$delta <= 4,]$days))/nrow( dredge_res[dredge_res$delta <= 4,])
sum(!is.na(dredge_res[dredge_res$delta <= 4,]$hour))/nrow(dredge_res[dredge_res$delta <= 4,])
sum(!is.na(dredge_res[dredge_res$delta <= 4,]$temp))/nrow( dredge_res[dredge_res$delta <= 4,])

mean(dredge_res$delta[!is.na(dredge_res$temp)])
mean(dredge_res$delta[!is.na(dredge_res$days)])
mean(dredge_res$delta[!is.na(dredge_res$hour)])


# checking concurvity
colMeans(conc_res[substr(rownames(conc_res), 1,1) == "w", ])


# make qq-plot
ggplot(data = qq_data, aes(x = theo_quant, y = resid)) +
  geom_segment(aes(x = min(qq_data), y = min(qq_data), xend = max(qq_data), yend = max(qq_data)), colour = "red") +
  geom_point(alpha = 0.3) +
  labs(x = "Theoretical quantiles", y = "Deviance residuals", title = "QQ plot of residuals", subtitle = "Method: uniform") +
  theme_bw() +
  theme_sets +
  theme(plot.title = element_text(size = 38),
        plot.subtitle = element_text(size = 35))

ggsave("figures/qq_GAM.png", width = 10, height = 10, units = "cm")





#### repeat fitting with "best model" + make visualisation ####

# new prediction data
new.data = data.frame(temp = seq(min(df$temp), max(df$temp), by = 0.1),
                      id = "Farallon-3-2021-1") # random choice, does not impact predictions

# empty df for plotting data
plotDF = data.frame()



for(it in 1:100){ # 100 iterations
  
  print(it) # print iteration
  set.seed(it) # set seed so that results can be reproduced
  
  new_df = data.frame() # empty data frame to store generated data
  
  for(i in 1:nrow(subsamp)){ # loop through unique combinations, sample data and tie together
    
    sub = df[df$pairIDbreeding == subsamp$id[i] & df$date == subsamp$date[i] & df$hour == subsamp$hour[i], ]
    
    ranRow = sub[sample(1:nrow(sub), 1),]
    
    new_df = rbind(new_df, ranRow)
  }
  
  
  # fit gam mod
  gam.mod = gam(presence_mod ~ 
                  s(temp, k = 3) + # temp effect, restrict to cubic
                  s(id, bs = "re"), # random effect of pair id
                family = ocat(R = 3),
                data = new_df,
                method = "REML",
                na.action = na.fail)
  
  # make and store predictions
  preds = as.data.frame(predict(gam.mod, type = "response", newdata = new.data, exclude = "s(id)"))
  names(preds) = c("prob0", "prob1", "prob2")
  
  plotDF = rbind(plotDF,
                 
                 data.frame(
                   temp = rep(new.data$temp, 3),
                   prob = c(preds$prob0, preds$prob1, preds$prob2),
                   group = as.factor(rep(c(0,1,2), each = nrow(new.data))),
                   it = it
                 ))
  

  
  
}

set.seed(seed = NULL)


