# ~~~~~~~~~~~~~~~~~~~~~~ #
#### temperature plot ####
# ~~~~~~~~~~~~~~~~~~~~~~ #


### breeding data ###

egg_date = aggregate(yday(EggDate) ~ year(EggDate), data = dat, FUN = mean) 
names(egg_date) = c("year", "egg_date")

hatch_date = aggregate(yday(HatchDate) ~ year(HatchDate), data = dat, FUN = mean) 
names(hatch_date) = c("year", "hatch_date")

breeding_dates = merge(egg_date, hatch_date)
breeding_dates[breeding_dates$year %in% 2019:2022,]



### ledge temp data ###
max_sun = aggregate(temp_sun ~ date, data = temp_df, FUN = max)
max_sun$loc = "Sun probe"
names(max_sun) = c("date", "temp", "loc")

max_shade = aggregate(temp_shade ~ date, data = temp_df, FUN = max)
max_shade$loc = "Shade probe"
names(max_shade) = c("date", "temp", "loc")

max_temp = rbind(max_sun, max_shade)
max_temp$year = year(max_temp$date)

max_temp$yday = yday(max_temp$date)



### hoburg data ###
hoburg_temp$date = as.Date(hoburg_temp$date) 
hoburg_temp$yday = yday(hoburg_temp$date)
hoburg_temp$year = year(hoburg_temp$date)
temp_agg = aggregate(air_temp ~  date, 
                     data = hoburg_temp,
                     FUN = max)

max_temp = rbind(max_temp,
                 data.frame(
                   date = temp_agg$date,
                   temp = temp_agg$air_temp,
                   loc = "Hoburg",
                   year = year(temp_agg$date),
                   yday = yday(temp_agg$date)
                 )
)


hoburg_cloud$date = as.Date(hoburg_cloud$date) 
hoburg_cloud$yday = yday(hoburg_cloud$date)
hoburg_cloud$year = year(hoburg_cloud$date)
cloud_agg = aggregate(cloudiness ~ year + yday, 
                      data = hoburg_cloud[hoburg_cloud$time %in% c("15:00:00", 
                                                                   "16:00:00",
                                                                   "17:00:00", 
                                                                   "18:00:00",
                                                                   "19:00:00", 
                                                                   "20:00:00"),], 
                      FUN = mean)
cloud_agg$cloudiness = cloud_agg$cloudiness/max(cloud_agg$cloudiness)*100


#### heat-related failures ####

deaths_ledge = data.frame(
  year = c(2019, 2020, 2020, 2022, 2022),
  day = c(157, 177, 178, 176, 177),
  ypos = c(24.5 , 50, 49.1, 47.8, 47.2),
  loc = c(NA, NA, NA, NA, NA)
  
)


### final plot ###
  ggplot() +
  
  geom_rect(data = cloud_agg[cloud_agg$year >= 2019,], 
            aes(xmin = yday-0.5, xmax = yday+0.5, ymin = 7, ymax = 50, fill = cloudiness), alpha = 0.6) +
  
  
  scale_fill_gradient(low = "white", high = "darkgrey",
                      name = NULL,
                      breaks = c(0, 100),
                      labels = c("0% cloud", "100% cloud")) +
  
  facet_wrap(~ year
             ,
             labeller = labeller(year = 
                                   c("2019" = "a. 2019",
                                     "2020" = "b. 2020",
                                     "2021" = "c. 2021",
                                     "2022" = "d. 2022")),
             ncol = 1) +
  
  
  geom_vline(data = breeding_dates[breeding_dates$year >= 2019,], aes(xintercept = egg_date), linetype = 2) +
  geom_vline(data = breeding_dates[breeding_dates$year >= 2019,], aes(xintercept = hatch_date), linetype = 2) +
  
  
  geom_line(data = max_temp[max_temp$year >= 2019,], aes(x = yday, y = temp, group = loc, colour = loc), size = 0.5) +
  
  scale_color_manual(values =  rev(met.brewer("Nattier", 3)) , name = "") +
  
  geom_point(data = deaths_ledge, aes(x = day, y = ypos), pch = 25, fill = "black", col = "black") +
  
  xlim(130.5, 190.5 ) +
  ylim(7, 50) +
  
  labs(x = "Day of year", y = "Temperature (\u00B0C)") +
  
  theme_bw() +
  
  theme_sets +
    theme(strip.text = element_text(size=12))




ggsave("figures/TempPlot.pdf", width = 18.5, height = 19, units = "cm")
  

rm(max_sun, max_shade, max_temp, egg_date, hatch_date, breeding_dates)

