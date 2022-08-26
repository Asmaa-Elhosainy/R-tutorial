hourly_slp <- function(x){ #folder or path
  files <- list.files(path = x, pattern = ".csv",
                   full.names = T, recursive = T)
  id <- str_split_fixed(basename(files), "_", 4)[,c(1,3)]
  combinecsv <- ldply(files,read.csv,row.names = NULL)
  names(combinecsv)[1:(ncol(combinecsv)-1)] <- names(combinecsv)[2:ncol(combinecsv)]
  combinecsv[, ncol(combinecsv)] <- NULL
  combinecsv$TT_WAKE_24 <- as.numeric(combinecsv$TT_WAKE_24)
  n <- nrow(combinecsv)
  columnames <- c("Date","ID",
               "TT_24","TT_LIGHT","TT_DARK",
               paste("ZT",seq(0,23),sep = ""), "Stage")
  wake1 <- combinecsv %>% select(starts_with("TT_WAKE")) %>%
    mutate(Stage = "Wake")
  wake2 <- data.frame(id,wake1[,1:28])
  colnames(wake2) <- columnames
  nrem1 <- combinecsv %>% select(starts_with("TT_NREM"))%>%
    mutate(Stage = "NREM")
  nrem2 <- data.frame(id,nrem1[,1:28])
  colnames(nrem2) <- columnames
  rem1 <- combinecsv %>% select(starts_with("TT_REM"))%>%
    mutate(Stage = "REM")
  rem2 <- data.frame(id,rem1[,1:28])
  colnames(rem2) <- columnames
  all <- combine(wake2,nrem2,rem2)
  all[,c(1,2,30,3:29)]
}
#any change