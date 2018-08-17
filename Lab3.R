input_xdf <- 'mht_lab2/mht_lab2.xdf'
Lab3_xdf <- RxXdfData(input_xdf)

rxSummary( ~ pickup_dow:pickup_hour, Lab3_xdf)

Lab3LM <- rxLinMod( tip_percent ~ trip_duration + pickup_dow:pickup_hour, Lab3_xdf,
          dropFirst = TRUE, covCoef = TRUE)

#Lab3LM$adj.r.squared

Lab3LM.pd <- rxLinMod( 
  tip_percent ~ trip_duration + payment_type_desc + pickup_dow:pickup_hour, 
  data = Lab3_xdf,
  dropFirst = TRUE, covCoef = TRUE)

#Lab3LM.pd$adj.r.squared

rxPredict(Lab3LM, data = Lab3_xdf, outData = Lab3_xdf,
          predVarNames = "tip_pred_1", overwrite = TRUE)

rxPredict(Lab3LM.pd, data = Lab3_xdf, outData = Lab3_xdf,
          predVarNames = "tip_pred_2", overwrite = TRUE)

rxHistogram( ~ tip_percent, Lab3_xdf, startval = 0,
             endval = 50, histType = "Counts",
             rowSelection = (tip_percent <= 50))

rxHistogram( ~ tip_pred_1, Lab3_xdf, startval = 0,
             endval = 50, histType = "Counts",
             rowSelection = (tip_percent <= 50))

rxHistogram( ~ tip_pred_2, Lab3_xdf, startval = 0,
             endval = 50, histType = "Counts",
             rowSelection = (tip_percent <= 50))
