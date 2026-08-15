cleanDataset <- function(rawDDAdataset){

cleaned <- data.table::copy(rawDDAdataset)
  cleaned[, ROUTENCODE := tolower(ROUTENCODE)]
  cleaned <- cleaned[Reviere != -1]
  cleaned[, year := as.integer(format(as.Date(Countdate), "%Y"))]

return(cleaned)

}