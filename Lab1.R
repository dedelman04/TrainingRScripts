input_xdf <- 'nyc_lab1.xdf'
Lab1_xdf <- RxXdfData(input_xdf)

#rxSummary( ~ RatecodeID, Lab1_xdf)

#rxGetInfo(Lab1_xdf, getVarInfo = TRUE, numRows = 10)

#data frame to test on
Lab1Sample <- rxGetInfo(Lab1_xdf, getVarInfo = FALSE, numRows = 5000)
Lab1Sample_df <- Lab1Sample$data
#

Lab1xform <- function(data) {
  #decode values
  RateCodes <- c("Standard Rate",
                 "JFK",
                 "Newark",
                 "Nassau or Westchester",
                 "Negotiated fare",
                 "Group ride",
                 "MV")
  
  PaymentCodes <- c("Cash",
                    "Credit Card",
                    "MV")
  
  #addNA - used to reassign values into missing values
  Rates <- addNA(data$RatecodeID)
  data$Ratecode <- factor(Rates, levels = 1:7)
  Rates <- factor(Rates, levels = 1:7, labels=RateCodes)
  data$Ratecode_type_desc <- Rates
  
  Payment <- ifelse(data$payment_type == 1 | data$payment_type == 2, data$payment_type, NA)
  data$payment_type_desc <- factor(Payment, levels = 1:3, labels = PaymentCodes)#, ordered = TRUE)
  
  data
}

#Testing code
#str(Lab1xform(Lab1Sample_df))
#rxDataStep(Lab1Sample_df, transformFunc = Lab1xform)

#rxDataStep - the basic of R Server "chunk"-wise operation
rxDataStep(Lab1_xdf, Lab1_xdf, overwrite = TRUE, transformFunc = Lab1xform) 

rxSummary( ~ Ratecode_type_desc, Lab1_xdf)
rxSummary( ~ payment_type_desc, rxDataStep(Lab1_xdf, Lab1_xdf, overwrite = TRUE, transformFunc = Lab1xform))
rxSummary( ~ payment_type, Lab1_xdf)
