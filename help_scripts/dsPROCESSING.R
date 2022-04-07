#######################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  DYGNSSTUDIE  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#######################################################################################################



#### initial setup ####

inputpath = "~/nonSU/Karlso/heat_stress/heatstressATTENDANCE/input"

#### main loop ####

# set start and time time of observation period
start_time = "15:00:00" # start time of obs in hours:minutes
end_time = "20:59:59" # one second before end time of obs in hours:minutes:seconds

# List all input files in directory
files = list.files(inputpath); files = files[substr(files, 1, 1) != "~"]


# Loop for each file (one file per Date and Ledge)

mega_df = data.frame()

for(f in 1:length(files)) {
  
  # Read file
  df = read.csv(paste0(inputpath, "/", files[f]), sep = ";");  df = droplevels(df)
  df = df[!is.na(df$PairNr) & df$Event %in% c("Start", "Departure", "Arrival"),]
  
  no_pairs = max(df$PairNr, na.rm= T)
  
  time = seq.POSIXt(from =    as.POSIXlt(paste(df$Date[1], start_time)),
                    to = as.POSIXlt(paste(df$Date[1], end_time)),
                    by = "min")
  
  
  
  df$Time = as.POSIXlt(paste(df$Date, df$Time))
  
  main_df = data.frame()
  
  for(p in 1:no_pairs){
    
    presence_df = data.frame(
      ledge = strsplit(files[f], "_")[[1]][2],
      pairIDds = p,
      time = time,
      presence = NA
    )
    
    df_sub = df[df$PairNr == p,]
    
    if(nrow(df_sub) == 0) presence_df = data.frame() # if birds are absent throughout study period, we just ignore this entry
    
    
    if(nrow(df_sub) == 1){ 
      presence_df$presence = 1
      if(df_sub$Event != "Start") print("Absence at start - check!") # just to catch the case were both are absent at start and then one shows up
    }
    
    
    if(nrow(df_sub) > 1){
      
      presence_df$presence = sum(df_sub$Event == "Start")
      
      for(t in 2:nrow(df_sub)){
        
        if(df_sub[t, "Event"] == "Arrival") presence_df$presence[ presence_df$time >=  df_sub[t, "Time"] ] = presence_df$presence[ presence_df$time >=  df_sub[t, "Time"] ] + 1
        if(df_sub[t, "Event"] == "Departure") presence_df$presence[ presence_df$time >=  df_sub[t, "Time"] ] = presence_df$presence[ presence_df$time >=  df_sub[t, "Time"] ] - 1
      }
      
      
      
      
    }
    
    
    
    
    main_df = rbind(main_df, presence_df)
    
    
    
    
  }
  main_df = main_df[order(main_df$time),]
  
  mega_df = rbind(mega_df, main_df)
}


presence_df = mega_df
presence_df$year = year(presence_df$time)
presence_df$pairIDds = as.numeric(presence_df$pairIDds)


rm(df, df_sub, main_df, mega_df,  end_time, f, files, inputpath, no_pairs, p, start_time, t, time)


# need to make sure it contains info so that it can be matched up with breeding data



