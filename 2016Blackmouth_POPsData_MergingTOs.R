# First step, clear workspace to make sure every thing will work, rm means remove
rm(list=ls(all=TRUE))


#load required packages/libraries
library(readxl)
library(doBy)
library(reshape2)
library(base)
library(plyr)

#set paths, make a list of text for the files to be used
paths = list("C:\\data\\GitHub\\Blackmouth-2016\\PS16BlackmouthMuscle_POPsA.xlsx",
             "C:\\data\\GitHub\\Blackmouth-2016\\PS16BlackmouthMuscle_POPsB.xlsx",
             "C:\\data\\GitHub\\Blackmouth-2016\\PS16BlackmouthMuscle_StableIsotopes.xlsx",
             "C:\\data\\GitHub\\Blackmouth-2016\\2016Blackmouth_UWStableIsotopes.xlsx",
             "C:\\data\\GitHub\\Blackmouth-2016\\Outputs\\")

# set outfile
outfile = paths[[5]]

#read in data
POPsA <- read_excel(paths[[1]],"POPs_A")
POPsB <- read_excel(paths[[2]],"POPs_B")
SI_NOAA <- read_excel(paths[[3]],"BlackmouthSI")
SI_UW <- read_excel(paths[[4]],"UWSIs_Blckmth")

#Check data in Row 1, Column 1 
POPsA [1,1]
POPsB [1,1]
SI_NOAA[1,1]
SI_UW [1,1]


#Now that all dataframes have the proper columns, they can be merged into one large dataset
POPs_2016BM <- rbind(POPsA,POPsB) #concatenates the two POPs results files


colnames(SI_NOAA) <- paste("NOAA", colnames(SI_NOAA), sep = "_") # add SI prefix to all NOAA SI data so that SI data can be identified next to POPs data
names(SI_NOAA)[names(SI_NOAA)=="NOAA_Field ID"] <- "Animal ID" # rename Field ID in SI file to Animal ID to match POPs files

colnames(SI_UW) <- paste("UW", colnames(SI_UW), sep = "_") # add UW prefix to all UW SI data
names(SI_UW)[names(SI_UW)=="UW_Sample"] <- "Animal ID" # rename Field ID in SI file to Animal ID to match POPs files


POPsSI_2016BM <- merge(POPs_2016BM, SI_NOAA, by = c("Animal ID"), all = T)
POPsSI_2016BM <- merge(POPsSI_2016BM, SI_UW, by = c("Animal ID"), all = T)

#re order data by SetName and then SampleNumber
POPsSI_2016BM <-POPsSI_2016BM[order(POPsSI_2016BM$SetName, POPsSI_2016BM$SampleNumber),]



# export to a csv file
write.csv(POPsSI_2016BM, paste(outfile,"BlackmouthPOPsAndSI_2016.csv", sep=""))


