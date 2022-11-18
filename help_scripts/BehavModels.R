#### BEHAVIOUR MODELS ####


# create interval variable to be used as random effect
behav$ledge_date_time = paste(behav$ledge, behav$time)


### run glm models PANTING
model1 = glmer(panting ~ temp_shade + sun_shade + temp_shade*sun_shade + (1 | ledge_date_time),
               data = behav, 
               family = binomial, 
               na.action = "na.fail")
summary(model1)
simres1 = simulateResiduals(model1) # simulate residuals
dredge(model1, extra = c("BIC", "rsq"))
model1 = glmer(panting ~ temp_shade + sun_shade + (1 | ledge_date_time),
               data = behav, 
               family = binomial, 
               na.action = "na.fail")

plot_model(model1, type = "pred", terms = c("temp_shade", "sun_shade"), show.data = T)



### run glm models SHOULDERS
model2 = glmer(raised_shoulders ~ temp_shade + sun_shade + temp_shade*sun_shade +  (1 | ledge_date_time),
               data = behav, 
               family = binomial, 
               na.action = "na.fail")
summary(model2)
dredge(model2, extra = c("BIC", "rsq"))
model2 = glmer(raised_shoulders ~ temp_shade + sun_shade + (1 | ledge_date_time),
               data = behav, 
               family = binomial, 
               na.action = "na.fail")
simres2 = simulateResiduals(model2) # simulate residuals
plot_model(model2, type = "pred", terms = c("temp_shade", "sun_shade"), show.data = T)





### run glm models ORIENTATION
orientation_data = behav[behav$sun_shade != "Shade",]

model3 = glmer(orientation_sun ~ temp_shade + sun_shade +  temp_shade*sun_shade + (1 | ledge_date_time),
               data = orientation_data, 
               family = binomial, 
               na.action = "na.fail")
summary(model3)
dredge(model3, extra = c("BIC", "rsq"))
simres3 = simulateResiduals(model3) # simulate residuals
plot_model(model3, type = "pred",terms = c("temp_shade", "sun_shade"), show.data = T)

# how many orientation towards sun?
mean(orientation_data$orientation_sun)


# diagnostic plots -look at residuals/ appropriateness of model structure
png("figures/behaviour_residuals.png", 
    width = 18.5, height = 18.5, units = 'cm', res = 300, family = "sans")
par(mfrow = c(2,2))
plotQQunif(simres1, cex.main = 3, cex.axis = 2, cex.lab = 3, main = "a. QQ panting", testUniformity = F, testOutliers = F, testDispersion = F) # look at residuals/ appropriateness of model structure
plotQQunif(simres2, cex.main = 3, cex.axis = 2, cex.lab = 3, main = "b. QQ wing spreading", testUniformity = F, testOutliers = F, testDispersion = F) # look at residuals/ appropriateness of model structure
plotQQunif(simres3, cex.main = 3, cex.axis = 2, cex.lab = 3, main = "c. QQ facing sun", testUniformity = F, testOutliers = F, testDispersion = F) # look at residuals/ appropriateness of model structure
dev.off()


### restructure data for plotting

# raw data
prop.data$time = as.POSIXct(prop.data$time)
plot.data = melt(prop.data,id.vars=c("ledge", "time", "temp_shade", "sunny"))

plot.behav = plot.data %>%
  filter(!variable %in% c("n"))


## mod 1 ##

# model prediction data 
pred.data = expand.grid(temp_shade = seq(floor(min(behav$temp_shade)), ceiling(max(behav$temp_shade)), 0.1), sun_shade = c("Shade", "Both", "Sun"))

# model prediction model 1
p = predict(model1, 
            type = "link",
            #se.fit = TRUE,
            re.form = NA,
            newdata = pred.data)


# make prediction df
pant.pred = cbind(pred.data, pred = inv.logit(p))
pant.pred$variable = "panting"

# 95% CI
bootRES = bootMer(model1,
                  FUN=function(x)predict(x, re.form=NA, newdata = pred.data),
                  nsim=500, .progress = "txt")

pant.pred$ymin = inv.logit(apply(bootRES$t, 2, quantile, 0.025))
pant.pred$ymax = inv.logit(apply(bootRES$t, 2, quantile, 0.975))


## mod 2 ##

# model prediction model 2
p = predict(model2, 
            type = "link",
            #se.fit = TRUE,
            re.form = NA,
            newdata = pred.data)


# make prediction df
shoulder.pred = cbind(pred.data, pred = inv.logit(p))
shoulder.pred$variable = "raised_shoulders"

# 95% CI
bootRES = bootMer(model2,
                  FUN=function(x)predict(x, re.form=NA, newdata = pred.data),
                  nsim=500, .progress = "txt")

shoulder.pred$ymin = inv.logit(apply(bootRES$t, 2, quantile, 0.025))
shoulder.pred$ymax = inv.logit(apply(bootRES$t, 2, quantile, 0.975))


# bind all together
pred.data = rbind(pant.pred, shoulder.pred)




# check corrs between variables
cor.test(prop.data$panting, prop.data$raised_shoulders)
cor.test(prop.data$panting, prop.data$orientation_sun)
cor.test(prop.data$raised_shoulders, prop.data$orientation_sun)
