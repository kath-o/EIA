rm(list=ls())
gc()
graphics.off()

#install.packages("rgbif")
library("rgbif")  


# table <- data.frame(order=as.character(unique(North_stream_inverts$Order)))

# write.csv(table, 'North_stream_inverts.csv', row.names = F)

install.packages("readxl")
library(readxl)

dat.messy <-
  read_xlsx("~/Desktop/MSc EEB/WD/EIA/Arran_fieldcourse.xlsx",sheet="North side invert transects")

spp <- unique(dat.messy$order)

n_spp <- length(spp)

# Initialize the data frames for a species that is sure to be in the taxonomy
spp.check.ok <- name_backbone("Hymenoptera", verbose = TRUE, strict = TRUE)
spp.check.ok <- spp.check.ok[-c(1:nrow(spp.check.ok)), ]  # Remove all the rows
spp.check.ok <- spp.check.ok[,-which(names(spp.check.ok) %in% c("acceptedUsageKey","family","genus","familyKey","genusKey","class","classKey"))]

# Initialize a data frame for a bad word (not a species name)
spp.check.bad <- name_backbone("xxx", verbose = TRUE, strict = TRUE)
spp.check.bad <- spp.check.bad[-1, ]  # Remove this row


#pb <- txtProgressBar(min = 0, max = n_spp, initial = 0, style = 3)
# Loop over species
for (i in 1:n_spp) {
  # Try-catch block to handle potential errors during each loop
  
  # i <- 0
  # 
  # i <- i+1
  # print(i)
  
  try({
    # i <- i+1
    toto <- name_backbone(spp[i], verbose = TRUE, strict = TRUE)  # Check species against GBIF backbone taxonomy
    # print(toto)
    if(nrow(toto)>1)
      toto <- toto[which(toto$rank=="ORDER"),]
    # print(toto)
    if(ncol(toto) > ncol(spp.check.ok))
      toto <- toto[,-which(names(toto) %in% c("acceptedUsageKey","class","classKey","family","genus","familyKey","genusKey"))]
    
    # Ensure the result has the same number of columns as spp.check.ok
    if (ncol(toto) == ncol(spp.check.ok)) {
      toto <- toto[,names(spp.check.ok)]
      # Check the status field
      if ("ACCEPTED" %in% toto$status) {
        spp.check.ok <- rbind(spp.check.ok, toto[toto$status == "ACCEPTED", ])
      } else if ("SYNONYM" %in% toto$status) {
        warning(paste("Order", spp[i], "is a synonym"))
        spp.check.ok <- rbind(spp.check.ok, toto[toto$status == "SYNONYM", ][1, ])
      } else if ("DOUBTFUL" %in% toto$status) {
        warning(paste("Order", spp[i], "is doubtful"))
        spp.check.ok <- rbind(spp.check.ok, toto[toto$status == "DOUBTFUL", ][1, ])
      } else {
        stop("Status unknown for species:", spp[i])
      }
    } else {
      spp.check.bad <- rbind(spp.check.bad, toto)
    } 
    
    # print(spp.check.ok)
  }, silent = TRUE)  # Continue the loop even if an error occurs
  #setTxtProgressBar(pb, i)  # Update the progress bar
}






length(which(spp.check.ok$status=="SYNONYM"))
length(which(spp.check.ok$status=="DOUBTFUL")) 
length(which(spp.check.ok$status=="BAD")) 

#for north side invert transects, no doubtful or synonyms


