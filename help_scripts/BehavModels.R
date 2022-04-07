#### BEHAVIOUR MODELS ####


# sample sizes
range(prop.data$n)
mean(prop.data$n)
nrow(prop.data)
length(unique(prop.data$Date))
range(unique(as.Date(prop.data$Date, format = "%d.%m.%y")))
mean(prop.data$Temp)
range(prop.data$Temp)
range(prop.data$Video.time)


### run glm models PANTING
model1 = glmer(Panting ~ Temp + (1| Video.time),behav, family = binomial, na.action = "na.fail")
dredge(model1, extra = c("BIC", "rsq"))
simres = simulateResiduals(model1) # simulate residuals
plot(simres) # look at residuals/ appropriateness of model structure
plot_model(model1, type = "pred", show.data = T)

### run glm models SHOULDERS
model2 = glmer(Raised.shoulders ~ Temp + (1| Video.time),behav, family = binomial, na.action = "na.fail")
summary(model2)
dredge(model2, extra = c("BIC", "rsq"))
simres = simulateResiduals(model2) # simulate residuals
plot(simres) # look at residuals/ appropriateness of model structure
plot_model(model2, type = "pred", show.data = T)


### run glm models ORIENTATION
model3 = glmer(Orientation.towards.sun ~ Temp + (1| Video.time),behav, family = binomial, na.action = "na.fail")
summary(model3)
dredge(model3, extra = c("BIC", "rsq"))
simres = simulateResiduals(model3) # simulate residuals
plot(simres) # look at residuals/ appropriateness of model structure
plot_model(model3, type = "pred", show.data = T)



### restructure data for plotting
plot.data = melt(prop.data,id.vars=c("Temp","Date","Video.time"))

plot.behav = plot.data %>%
  filter(!variable %in% c("All","n"))

# add model predictions
p = predict(model1, 
            type = "link",
            #se.fit = TRUE,
            re.form = NA,
            newdata = data.frame(Temp = plot.behav$Temp[plot.behav$variable == "Panting"]))

# add predictions to df, including 95% CI
plot.behav$pred[plot.behav$variable == "Panting"] = inv.logit(p)

bootRES = bootMer(model1,
                  FUN=function(x)predict(x, re.form=NA, newdata = data.frame(Temp = plot.behav$Temp[plot.behav$variable == "Panting"])),
                  nsim=500, .progress = "txt")

plot.behav$ymin[plot.behav$variable == "Panting"] = inv.logit(apply(bootRES$t, 2, quantile, 0.025))
plot.behav$ymax[plot.behav$variable == "Panting"] = inv.logit(apply(bootRES$t, 2, quantile, 0.975))


p = predict(model2, 
            type = "link",
            #se.fit = TRUE,
            re.form = NA,
            newdata = data.frame(Temp = plot.behav$Temp[plot.behav$variable == "Raised.shoulders"]))

# add predictions to df, including 95% CI
plot.behav$pred[plot.behav$variable == "Raised.shoulders"] = inv.logit(p)

bootRES = bootMer(model2,
                  FUN=function(x)predict(x, re.form=NA, newdata = data.frame(Temp = plot.behav$Temp[plot.behav$variable == "Raised.shoulders"])),
                  nsim=500, .progress = "txt")

plot.behav$ymin[plot.behav$variable == "Raised.shoulders"] = inv.logit(apply(bootRES$t, 2, quantile, 0.025))
plot.behav$ymax[plot.behav$variable == "Raised.shoulders"] = inv.logit(apply(bootRES$t, 2, quantile, 0.975))


p = predict(model3, 
            type = "link",
            #se.fit = TRUE,
            re.form = NA,
            newdata = data.frame(Temp = plot.behav$Temp[plot.behav$variable == "Orientation.towards.sun"]))

# add predictions to df, including 95% CI
plot.behav$pred[plot.behav$variable == "Orientation.towards.sun"] = inv.logit(p)

bootRES = bootMer(model3,
                  FUN=function(x)predict(x, re.form=NA, newdata = data.frame(Temp = plot.behav$Temp[plot.behav$variable == "Orientation.towards.sun"])),
                  nsim=500, .progress = "txt")

plot.behav$ymin[plot.behav$variable == "Orientation.towards.sun"] = inv.logit(apply(bootRES$t, 2, quantile, 0.025))
plot.behav$ymax[plot.behav$variable == "Orientation.towards.sun"] = inv.logit(apply(bootRES$t, 2, quantile, 0.975))


# check corrs between variables
cor.test(prop.data$Panting, prop.data$Raised.shoulders)
cor.test(prop.data$Panting, prop.data$Orientation.towards.sun)
cor.test(prop.data$Raised.shoulders, prop.data$Orientation.towards.sun)
