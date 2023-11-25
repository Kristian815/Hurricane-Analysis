convert_latitude <- function(lat) {
  # Extract the numeric value and the direction from the latitude string
  numeric_value <- as.numeric(substr(lat, 1, nchar(lat) - 1))
  direction <- substr(lat, nchar(lat), nchar(lat))
  
  # Convert the latitude to degrees north or south
  if (direction == "N") {
    return(numeric_value)
  } else if (direction == "S") {
    return(-numeric_value)
  } else {
    return(NA)
  }
}

convert_longitude <- function(lon) {
  # Extract the numeric value and the direction from the longitude string
  numeric_value <- as.numeric(substr(lon, 1, nchar(lon) - 1))
  direction <- substr(lon, nchar(lon), nchar(lon))
  
  # Convert the longitude to degrees west or east
  if (direction == "W") {
    return(-numeric_value)
  } else  {
    return(numeric_value)
  }
}

dat <-read.csv("hurdat2-1851-2022-040723.txt", header=FALSE)
code_rows <- grep('AL', dat$V1)
whether_code <- grepl("AL", dat$V1)
hurdat_list <- dat$V1[code_rows]
name_list <- dat$V2[code_rows]
df <- dat[!whether_code, ]
hur_id <- rep(hurdat_list, diff(c(code_rows, nrow(dat) + 1)) - 1)
hur_name <- rep(name_list, diff(c(code_rows, nrow(dat) + 1)) - 1)
hurdat <- data.frame(hur_id, hur_name, df)
colnames(hurdat) <- c("Cyclone number", "Name", "date", "time", "Record identifier", "Status of system", "Latitude", "Longitude", "Maximum sustained wind", "Minimum Pressure", "34 kt NE", "34 kt SE", "34 kt SW", "34 kt NW", "50 kt NE", "50 kt SE", "50 kt SW", "50 kt NW", "64 kt NE", "64 kt SE", "64 kt SW", "64 kt NW", "Radius of Maximum Wind")
hurdat[hurdat == -999] <- NA
hurdat$Latitude <- sapply(hurdat$Latitude, convert_latitude)
hurdat$Longitude <- sapply(hurdat$Longitude, convert_longitude)

hurdat
