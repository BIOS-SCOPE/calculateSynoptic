# synoptic.func <- function(data,useDepthLine,oneVariable) 
# 
# This script does the calculations and can be called from within any other R
# script/markdown file. The goal here is to pull out the calculations so the 
# other R scripts/markdown files can focus on analyzing the results. 
# This is quite specific to BATS data, but you can modify it if the cruise 
# information is not a five digit number

# first written by Paul Matson on 11 Nov 2016
# revised by Craig Carlson on 6 - 12 Dec 2016
# revised by Krista Longnecker June to August, 2024
# script setup by Krista Longnecker June 11, 2026
#
# This script does the following:
# 1. Imports a set of CTD from a series of cruises with multiple casts and depths sampled
# 2. Sets up a series of regularly-spaced depth bins
# 3. Splits the existing data into the depth bins
# 4. Calculates the average values for the each parameter in the each depth bin
# 5. For ONE variable, the code will determine how the final value was determined.
# This is used if there is one variable you are particularly interested in.
## For example - was there only one value in the depth bin? Multiple values? 
## Duplicate values at one depth?
## Were the data from a single CTD cast during a cruise? multiple casts? 
## Were the data grouped into a shallow cast and a deep cast based on some 
## depth value set by the user?
## This step is done because it may be useful to know if discrete values are 
## from one cast or spread across multiple casts. If you do not care, you can
## ignore this value.
# 
# The input require is as follows:
# --> one or more cruises (see syntax below for examples)
# --> oneVariable (a variable you are particularly interested in)
# --> useDepthLine is a depth in the water column is you want details on the 
#number of casts used for a given variable
#
# The output is as follows:
## a matrix with the information for one cruise, with one row per nominal depth
## a series of columns with the averaged data within the depth bin
## There are some new columns added to the matrix:
### meanMLD : mean MLD depth across the cruise 
# 
# The syntax to call this function is as follows, for one cruise (e.g., '10065'):
# temp.result = synoptic.func(listOfCruises[['1']],useDepthLine,oneVariable)
# or for a list of cruises:
# result.list <- lapply(split(data, data$cruise), synoptic.func, useDepthLine, oneVariable)

#library(dplyr)

# #need the variables that come in when the function is called
args <- commandArgs(trailingOnly = TRUE)
useDepthLine <- as.numeric(args[1])
oneVariable <- as.character(args[2])

# #now setup the function itself

synoptic.func <- function(data,useDepthLine,oneVariable) { 
  # Extract cruise, cast, and bottle IDs
  data <- data %>%
    mutate(
      cruise = substr(Id, 1, 5),
      cast = as.integer(substr(Id, 6, 8)),
      bottle = as.integer(substr(Id, 9, 10))
    ) %>%
    #filter(yyyymmdd > 19890131) %>%  # Can limit cruise years from 1989 onwards
    mutate(
      target.depth = cut(Depth, breaks = c(0, 7, seq(13, 103, 10), seq(110, 230, 20), 240, 260,
                                           seq(275, 4225, 50), 4550), 
                         labels = c(1, seq(10, 120, 10), seq(140, 220, 20), 230, 250, 275,
                                    seq(300, 4220, 50), 4530))
    ) %>%
    relocate(target.depth, .after = Depth)

  # Create synoptic dataframe of nominal depths
  nom.depth <- data.frame(
    nom.depth = c(1,10,20,40,60,80,100,120,140,160,200,250,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1600,1800,2000,2200,2400,2600,3000,3400,3800,4000,4200,4530),
    target.depth = c(1,10,20,40,60,80,100,120,140,160,200,250,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1600,1800,2000,2200,2400,2600,3000,3400,3800,4000,4200,4530)
  )
  
  # Merge casts with synoptic dataframe
  synoptic.BATS <- merge(nom.depth, data, all.x = TRUE)
  oc <- as.numeric(unique(na.omit(synoptic.BATS$cruise)))
  uc <- as.numeric(unique(na.omit(synoptic.BATS$cast)))
  
  #setup the groupings before doing the math
  synoptic <- synoptic.BATS %>%
    select(-c(Id)) %>%
    group_by(nom.depth) %>%
    mutate(cast = list(unique(cast))) %>%
    summarise(
      across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
      across(where(is.list), ~ list(first(.))),
      cruise = first(cruise)
    )
  
  #browser()
  
  #calculate the mean MLD - since we are averaging across casts, can end up with 
  #different MLD values for a given cruise and that is confusing
  synoptic$meanMLD <- mean(synoptic$MLD, na.rm = TRUE)
  
  # #now, which variable do you want to track to get specific details on? 
  trackOneVariable <- synoptic.BATS[!(is.na(synoptic.BATS[[oneVariable]])),]


  if (nrow(trackOneVariable) == 0) {
    trackOneVariable[1, ] <- NA
  }

  
  #WAS setLine <- 250; use this to set an border if you want to know if the 
  #variable you are tracking is collected in a shallow versus deep cast. For
  #example, the TOC samples are often collected in two casts - a shallow
  #cast to 250 m and a deep cast for depths below that.
  setLine <- useDepthLine
  
  #this next bit figures out the different options to describe how the 
  #oneVariable was collected. See the description in the readme for the
  #BIOS-SCOPE/calculateSynoptic repository
  subset <- trackOneVariable[trackOneVariable$nom.depth <= setLine,]
  test <- as.numeric(subset$cast)
  dtm <- diff(test)
  
  if (any(duplicated(trackOneVariable$target.depth))) {
    synoptic$case <- rep("duplicates", nrow(synoptic))
    w <- which(duplicated(trackOneVariable$target.depth))
    if (length(w) > 1) {
      #synoptic$var1_mld <- rep('NA', nrow(synoptic))
    } else if (length(w) == 1 && trackOneVariable$target.depth[w] > setLine) {
      #synoptic$var1_mld <- rep(unique(subset$MLD), nrow(synoptic))
    } else {
      #synoptic$var1_mld <- rep('NA', nrow(synoptic))
    }
  } else if (length(test) == 0 && nrow(subset) == 0) {
    synoptic$case <- rep("no TOC/TN", nrow(synoptic))
    #synoptic$var1_mld <- rep('NA', nrow(synoptic))
  } else if (length(test) == 0) {
    synoptic$case <- rep("no TOC/TN", nrow(synoptic))
    #synoptic$var1_mld <- rep('NA', nrow(synoptic))
  } else if (all(is.na(unique(test)))) {
    synoptic$case <- rep("no TOC/TN", nrow(synoptic))
    #synoptic$var1_mld <- rep('NA', nrow(synoptic))
  } else if (length(unique(test)) == 1 && !is.na(unique(test))) {
    synoptic$case <- rep("one cast", nrow(synoptic))
    #synoptic$var1_mld <- rep(unique(subset$MLD), nrow(synoptic))
  } else if (sum(abs(dtm) > 0) == 1) {
    synoptic$case <- rep("grouped", nrow(synoptic))
    #synoptic$var1_mld <- rep('NA', nrow(synoptic))
  } else if (length(unique(test)) > 2) {
    synoptic$case <- rep("threePlus", nrow(synoptic))
    #synoptic$var1_mld <- rep('NA', nrow(synoptic))
  } else if (sum(abs(dtm) > 0) > 1) {
    synoptic$case <- rep("mixed", nrow(synoptic))
    #synoptic$var1_mld <- rep('NA', nrow(synoptic))
  } else {
    stop("Unexpected case encountered")
  }
  
  synoptic <- synoptic %>% relocate(year, month, .after = yyyymmdd)
  synoptic <- synoptic %>% relocate(cruise, .after = month)
  synoptic <- synoptic %>% relocate(case, .after = cast)
  
  ## end calculating the different cases
  
  #now end by returning synoptic...which is a data.frame with the result
  return(synoptic)
}



