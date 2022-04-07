# ~~~~~~~~~~~~~~~~~~~~~~ #
#### temperature plot ####
# ~~~~~~~~~~~~~~~~~~~~~~ #


### breeding data ###
egg_date = aggregate(yday(eggDate) ~ year, data = pair_info_df, FUN = mean); names(egg_date) = c("year", "date")
hatch_date = aggregate(yday(as.Date(hatchDate)) ~ year, data = pair_info_df, FUN = mean); names(hatch_date) = c("year", "date")
breeding_dates = rbind(egg_date, hatch_date)

df = read.delim("BreedingDataAuklabUntil2021.txt")
PairIds = strsplit(df$PairID, "-")

df$keep = FALSE
for(i in 1:nrow(df)){
  if(
    PairIds[[i]][3] %in% c("2019") &
    PairIds[[i]][2] %in% c("3") &
    PairIds[[i]][1] != c("Bonden")
  ) df$keep[i] = TRUE
}

df = df[df$keep == 1 & df$BrAttempt == 1, ]

breeding_dates = rbind(
  data.frame(year = 2019,
             date = c(mean(yday(as.Date(df$EggDate))),
                      mean(yday(as.Date(df$HatchDate)), na.rm = T)
                      )
             ),
  breeding_dates
)


### ledge temp data ###
max_sun = aggregate(temp_sun ~ date, data = temp_df, FUN = max)
max_sun$loc = "Sun"
names(max_sun) = c("date", "temp", "loc")

max_shade = aggregate(temp_shade ~ date, data = temp_df, FUN = max)
max_shade$loc = "Shade"
names(max_shade) = c("date", "temp", "loc")

max_temp = rbind(max_sun, max_shade)
max_temp$year = year(max_temp$date)

max_temp$yday = yday(max_temp$date)

deaths_ledge = data.frame(
  year = c(2020, 2020),
  day = c(177, 178),
  ypos = c(50, 49.1),
  loc = c(NA, NA)
  
)


### ledge plot ###
ledge_plot = 
  ggplot(data = max_temp, aes(x = yday, y = temp, group = loc, colour = loc)) +
  
  geom_vline(data = breeding_dates[breeding_dates$year > 2019,], aes(xintercept = date), linetype = 2) +


  geom_line() +
  
  geom_point(data = deaths_ledge, aes(x = day, y = ypos), pch = 25, fill = "black", col = "black") +
  
  
  facet_wrap(~ year,
             labeller = labeller(year = 
                                   c("2020" = "a. 2020",
                                     "2021" = "b. 2021"))) +
  
  xlim(135, max(yday(presence_df$time)) ) +
  ylim(10, 50) +
  
  scale_color_manual(values =c (met.brewer(name="Hokusai1",n=7,type="discrete")[6],
                               met.brewer(name="Hokusai1",n=7,type="discrete")[2]), name = "") +
  #scale_linetype_manual(values = c(1,2), name = "") +
  
  labs(x = "Day of year", y = "Temperature (\u00B0C)") +
  
  theme_bw() +
  
  theme_sets +
  
  theme(legend.position = "left",
        strip.text.x = element_text(margin = margin()),
        strip.text.y = element_text(margin = margin()),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.key.width = unit(3, 'cm'),
        legend.spacing.x = unit(0, "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))



### hoburg data ###

hoburg_temp$date = as.Date(hoburg_temp$date) 
hoburg_temp$yday = yday(hoburg_temp$date)
hoburg_temp$year = year(hoburg_temp$date)
temp_agg = aggregate(air_temp ~  year + yday, 
                     data = hoburg_temp,
                     FUN = max)


hoburg_cloud$date = as.Date(hoburg_cloud$date) 
hoburg_cloud$yday = yday(hoburg_cloud$date)
hoburg_cloud$year = year(hoburg_cloud$date)
cloud_agg = aggregate(cloudiness ~  year + yday, 
                      data = hoburg_cloud[hoburg_cloud$time %in% c("15:00:00", 
                                                                   "16:00:00",
                                                                   "17:00:00", 
                                                                   "18:00:00",
                                                                   "19:00:00", 
                                                                   "20:00:00"),], 
                      FUN = mean)

hoburg_dat = merge(temp_agg, cloud_agg, all = T)

deaths_hoburg = data.frame(
  year = c(2019, 2020, 2020),
  day = c(157, 177, 178),
  ypos = c(23.5, 26, 27),
  loc = NA
  
)

### hoburg plot ###
hoburg_plot =
  ggplot(data = hoburg_dat[hoburg_dat$year %in% c(2019, 2020, 2021),], aes(x = yday, y = air_temp)) +
  

  geom_vline(aes(xintercept = yday, colour = cloudiness), size = 1.5) +
  
  geom_vline(data = breeding_dates, aes(xintercept = date), linetype = 2) +
  
  
  scale_colour_gradient(low = "orange2", high = "white",
                        name = NULL,
                        breaks = c(0, 100),
                        labels = c("0% cloud", "100% cloud")) +
  
  geom_line() +
  
  geom_point(data = deaths_hoburg, aes(x = day, y = ypos), pch = 25, fill = "black", col = "black") +
  
  facet_wrap(~ year,
             labeller = labeller(year = 
                                   c("2019" = "c. 2019",
                                     "2020" = "d. 2020",
                                     "2021" = "e. 2021"
                                   ))) +
  
  xlim(135, max(yday(presence_df$time)) ) +
  ylim(11, 27) +
  
  labs(x = "Day of year", y = "Temperature (\u00B0C)") +
  
  theme_bw() +
  
  theme_sets +
  
  theme(legend.position = "bottom",
        strip.text.x = element_text(margin = margin()),
        strip.text.y = element_text(margin = margin()),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.spacing.y = unit(0, "cm"),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))






png("figures/temp_plot.png", 
    width = 17, height = 17, units = 'cm', res = 300, pointsize = 9, family = "sans")


print(
  plot_grid(ledge_plot, hoburg_plot, nrow = 2, rel_heights = c(1, 1.4)),
  greedy = FALSE
  
)


dev.off()


rm(max_sun, max_shade, max_temp, egg_date, hatch_date, breeding_dates)