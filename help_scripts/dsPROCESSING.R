#######################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  DYGNSSTUDIE  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#######################################################################################################



#### initial setup ####

inputpath = "data/input"

#### main loop ####

# set start and time time of observation period
start_time = "15:00:00" # start time of obs in hours:minutes
end_time = "20:59:59" # one second before end time of obs in hours:minutes:seconds

# list all input files in directory
files = list.files(inputpath); files = files[substr(files, 1, 1) != "~"]



# check for comments that may be important
comm_df = data.frame()

for(f in 1:length(files)) {
  df = read.csv(paste0(inputpath, "/", files[f]), sep = ";");  df = droplevels(df)
  
  comms = unique(df$Comments)
  comms = comms[comms != "" & !is.na(comms)]
  
  if(length(comms) != 0) comm_df = rbind(comm_df, data.frame(file = files[f],  comm = comms))
  
}




# Loop for each file (one file per Date and Ledge)





mega_df = data.frame()

for(f in 1:length(files)) {
  
  # Read file
  df = read.csv(paste0(inputpath, "/", files[f]), sep = ";");  df = droplevels(df)
  df = df[!is.na(df$PairNr) & df$PairNr != "<NA>" & df$Event %in% c("Start", "Departure", "Arrival"),]
  
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
  
  
  # fix case were pair loses egg in middle
  if(files[f] == "Dygnsstudie_Farallon3_20200627.csv"){
    
    main_df = main_df[!(main_df$pairIDds == 7 & 
                        main_df$time > "2020-06-27 18:49:00 CEST"),]
  }
  
  # fix cases were video is not complete
  if(files[f] == "Dygnsstudie_Farallon3_20210615.csv"){
    main_df = main_df[!(main_df$time > "2021-06-15 20:47:00 CEST"),]}
  if(files[f] == "Dygnsstudie_Triangle3_20200702.csv"){
    main_df = main_df[!(main_df$time < "2020-07-02 15:12:00 CEST"),]}
  if(files[f] == "Dygnsstudie_Triangle3_20210615.csv"){
    main_df = main_df[!(main_df$time > "2021-06-15 20:47:00 CEST"),]}
  
  # remove pair where chick is being ringed
  if(files[f] == "Dygnsstudie_Triangle3_20210627.csv"){
    main_df = main_df[!(main_df$pairIDds == 3),]}
  
  
  
  
  
  mega_df = rbind(mega_df, main_df)
}


presence_df = mega_df
presence_df$year = year(presence_df$time)
presence_df$pairIDds = as.numeric(presence_df$pairIDds)


rm(df, df_sub, main_df, mega_df,  end_time, f, files, inputpath, no_pairs, p, start_time, t, time)





