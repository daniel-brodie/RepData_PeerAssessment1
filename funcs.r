getMean <- function(whatInterval) {
  as.integer(mean((filter(rawdata.nona,interval==whatInterval) %>% select(steps))$steps))
}

addMissingData <- function() {
  
  for (i in 1:nrow(onlyNA)) {
    onlyNA[[i,"newSteps"]] <- getMean(onlyNA[[i,"interval"]])
  }
}

