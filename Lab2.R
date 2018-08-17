input_xdf <- 'mht_lab2.xdf'
Lab2_xdf <- RxXdfData(input_xdf)

rxSummary( ~ payment_type_desc, Lab2_xdf)

#data frame to test on
Lab2Sample <- rxGetInfo(Lab2_xdf, getVarInfo = FALSE, numRows = 1000)
Lab2Sample_df <- Lab2Sample$data

rxCrossTabs(~ payment_type_desc:Ratecode_type_desc, Lab2_xdf)

##NOTE: trip_duration in the data is in SECONDS not MINUTES
rxCube(~ trip_dist:trip_dur, Lab2_xdf, 
            transforms = list(trip_dist = as.factor(ifelse(trip_distance <=5, "short", "long")),
                              trip_dur = as.factor(ifelse(trip_duration <=10*60, "short", "long"))
                              ))

Lab2Sample_df$trip_dist = as.factor(ifelse(Lab2Sample_df$trip_distance <=5, "short", "long"))
Lab2Sample_df$trip_dur = as.factor(ifelse(Lab2Sample_df$trip_duration <=10*60, "short", "long"))

q2 <- rxDataStep(Lab2_xdf, rowSelection = (trip_distance > 5 & trip_duration <= 10*60))
dim(q2)

rxCube (~ payment_type_desc, Lab2_xdf, rowSelection = (trip_distance > 5 & trip_duration <= 10*60))

rxCube(~ trip_dist:trip_dur, Lab2_xdf, 
       transforms = list(trip_dist = factor(ifelse(trip_distance <=5, "short", "long")),
                         trip_dur = factor(ifelse(trip_duration <=10*60, "short", "long"))),
       rowSelection = (payment_type_desc == "card")
       )

rxCrossTabs(~ trip_dist:trip_dur, Lab2_xdf, 
       transforms = list(trip_dist = factor(ifelse(trip_distance <=5, "short", "long")),
                         trip_dur = factor(ifelse(trip_duration <=10*60, "short", "long"))),
       rowSelection = (payment_type_desc == "card")
)

####Part 2####
rxHistogram( ~ tip_percent, Lab2_xdf, startval = -5, endval = 35, histType = "Percent", numBreaks = 20)

rxHistogram( ~ tip_percent | payment_type_desc, Lab2_xdf, 
             histType = "Percent", numBreaks = 20)

cut_levels <- c(5, 10, 15, 20, 25, 100)
#cut_levels <- addNA(cut_levels)
cut_level_names <- c("Up to 5", ">5-10", ">10-15", ">15-20", ">20-25", ">25")

rxHistogram( ~ tip_percent | payment_type_desc, Lab2_xdf,
             histType = "Percent",
             transforms = list(tip_percent = factor(tip_percent, 
                                                       levels = cut_levs,
                                                       labels = cut_lev_names)),
             transformObjects = list(cut_levs = unique(cut_levels),
                                     cut_lev_names = unique(cut_level_names))
)

rxHistogram( ~ tip_percent | Ratecode_type_desc, Lab2_xdf,
             histType = "Percent",
             transforms = list(tip_percent = factor(tip_percent, 
                                                    levels = cut_levs,
                                                    labels = cut_lev_names)),
             transformObjects = list(cut_levs = unique(cut_levels),
                                     cut_lev_names = unique(cut_level_names)),
             rowSelection = (payment_type_desc == "card")
)

rxSummary(~ tip_percent, Lab2_xdf)