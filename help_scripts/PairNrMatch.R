df = read.delim("data/BreedingDataAuklabUntil2021.txt")
df = df[, 1:18]

PairIds = strsplit(df$PairID, "-")

df$keep = FALSE

for(i in 1:nrow(df)){
  if(
    PairIds[[i]][3] %in% c("2020", "2021") &
    PairIds[[i]][2] %in% c("3") &
    PairIds[[i]][1] != c("Bonden")
  ) df$keep[i] = TRUE
}

df_sub = df[df$keep == TRUE, ]


IDs =  matrix(unlist(strsplit(df_sub$PairID, "-")), ncol = 4, byrow = T)
transl_df = data.frame(
  pairIDbreeding = df_sub$PairID,
  shelf1 = IDs[,1],
  shelf2 = IDs[,2],
  year = as.numeric(IDs[,3]),
  pairIDbreedingNo = as.numeric(IDs[,4]),
  pos = df_sub$EggPosX,
  eggDate = df_sub$EggDate,
  hatchDate = df_sub$HatchDate,
  chickGoneDate = df_sub$ChickGoneDate,
  eggLossDate = df_sub$EggLossDate
)

transl_df$pairIDds = NA
transl_df$pairIDds[transl_df$shelf1 == "Farallon" & transl_df$year == 2020] =
  transl_df$pairIDbreedingNo[transl_df$shelf1 == "Farallon" & transl_df$year == 2020]

transl_df$pairIDds[transl_df$shelf1 == "Rost" & transl_df$year == 2020] =
  rank(as.numeric(transl_df$pos[transl_df$shelf1 == "Rost" & transl_df$year == 2020]))

transl_df$pairIDds[transl_df$shelf1 == "Triangle" & transl_df$year == 2020] =
  rank(as.numeric(transl_df$pos[transl_df$shelf1 == "Triangle" & transl_df$year == 2020]))

pos2021 = 
  data.frame(
    pairIDbreeding = 
      c("Farallon-3-2021-1", "Farallon-3-2021-2","Farallon-3-2021-3","Farallon-3-2021-4","Farallon-3-2021-5","Farallon-3-2021-6", "Farallon-3-2021-7",
        "Rost-3-2021-1", "Rost-3-2021-4", "Rost-3-2021-2", "Rost-3-2021-3", "Rost-3-2021-5", 
        "Triangle-3-2021-4", "Triangle-3-2021-3", "Triangle-3-2021-2", "Triangle-3-2021-1"),	 	
    
    pos2021 = 
      c(104, 133, 119, 222.5, 177, 246, 197,
        90, 117, 133, 141, 167, 
        106, 184, 222, 250) 
  )

transl_df = merge(transl_df, pos2021, by = "pairIDbreeding", all = T)

transl_df$pairIDds[transl_df$shelf1 == "Rost" & transl_df$year == 2021] =
  rank(as.numeric(transl_df$pos2021[transl_df$shelf1 == "Rost" & transl_df$year == 2021]))

transl_df$pairIDds[transl_df$shelf1 == "Triangle" & transl_df$year == 2021] =
  rank(as.numeric(transl_df$pos2021[transl_df$shelf1 == "Triangle" & transl_df$year == 2021]))

transl_df$pairIDds[transl_df$shelf1 == "Farallon" & transl_df$year == 2021] =
  rank(as.numeric(transl_df$pos2021[transl_df$shelf1 == "Farallon" & transl_df$year == 2021]))

pair_info_df = transl_df[, !(names(transl_df) %in% c("pos", "pos2021"))]
pair_info_df$ledge = paste0(pair_info_df$shelf1, pair_info_df$shelf2)
pair_info_df$shelf1 = NULL; pair_info_df$shelf2 = NULL

transl_df$pos2021[is.na(transl_df$pos2021)] =
  transl_df$pos[is.na(transl_df$pos2021)] 

transl_df = transl_df[,c("pairIDbreedingNo", "pairIDbreeding", "pairIDds", "pos2021")]
names(transl_df)[3] = "xPOS"



# fix Triangle 2021 where they are not in order (see comments in dygnsstudier)
transl_df$xPOS[29:32] = c(3, 2, 4, 1)


write.csv(transl_df, "translateID.csv")


rm(transl_df, IDs, df, df_sub, i, pos2021, PairIds)



