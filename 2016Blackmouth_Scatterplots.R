rm(list=ls(all=TRUE))

library(readxl)
library(ggplot2)

#set paths, make a list of text for the files to be used
paths = list("C:\\Users\\careyajc\\PSEMP\\PSEMP\\GitHub\\Blackmouth-2016\\Blackmouth-2016\\2016Blackmouth_DataForR_6.27.17.xlsx",
             "C:\\Users\\careyajc\\PSEMP\\PSEMP\\GitHub\\Blackmouth-2016\\Blackmouth-2016\\Outputs\\")

# set outfile
outfile = paths[[2]]

#read in data
BM <- read_excel(paths[[1]],"BlackmouthPOPsSIs")

#create dataframe of data only needed for scatterplots, subset out the Matrix = muscle
BMdat <- as.data.frame(BM[ ,c(2:4,6:8)])
BMmuscle <- subset(BMdat, Matrix=="muscle")
BMmuscle$ForkLength <- as.numeric(BMmuscle$ForkLength)

#add MA to Marine Area numbers in order to change data from a continuous variable to a categorical variable
BMmuscle$MA[BMmuscle$MarineArea == 6] <- "MA6"
BMmuscle$MA[BMmuscle$MarineArea == 7] <- "MA7"
BMmuscle$MA[BMmuscle$MarineArea == 8.1] <- "MA8.1"
BMmuscle$MA[BMmuscle$MarineArea == 8.2] <- "MA8.2"
BMmuscle$MA[BMmuscle$MarineArea == 9] <- "MA9"
BMmuscle$MA[BMmuscle$MarineArea == 10] <- "MA10"
BMmuscle$MA[BMmuscle$MarineArea == 12] <- "MA12"
BMmuscle$MA <-factor(BMmuscle$MA) #assign MA column as a factor or categorical variable
levels(BMmuscle$MA) #examine levels of MA = alphabetical, need to be from lowest to highest
BMmuscle$MA <- factor(BMmuscle$MA, levels = c('MA6', 'MA7', 'MA8.1', 'MA8.2', 'MA9', 'MA10', 'MA12')) #redefine the order of levels

###
# 1. Scatterplot of individual lengths (y) and scale ages - Gilbert Rich (x)

a <- ggplot(BMmuscle, aes(x = ScaleAge, y = ForkLength)) + geom_point()
print(a) #simple scatterplot, removed NAs (13)
print({a + aes(colour = MA)}) #color codes fish based on marine area where they were collected, removed NAs (13) 

a <- a + aes(color = MA) #color codes fish based on marine area where they were collected, removed NAs (13) 

# Save the plot to outfile
ggsave(paste(outfile,"16BM_ScaleAge and Length.jpg",sep=""),a,height=5,width=7.5)

# 2. Scatterplot of individual lengths (y) and saltwater ages (x)
b <- ggplot(BMmuscle, aes(x = SWAge, y = ForkLength)) + geom_point()
b <- b + aes(colour = MA) 

ggsave(paste(outfile,"16BM_SWage and Length.jpg",sep=""),b,height=5,width=7.5)

###
# Setting all data up for plotting

#create new dataframe from master data file - BM, keep only muscle samples, remove any rows with unknowns for ScaleAge n = 13
colnames(BM)
BMdata <- as.data.frame(BM[ ,c(2:4,6:11,13:18,20:26)])
BMdata <- subset(BMdata, Matrix=="muscle") #keep only muscle samples, don't need whole body samples since we only have them from MA 10
BMmus.all <- subset(BMdata, Fwage>="1") #removes fish that did not get analyzed for POPs (priority 3 fish which were unable to be aged)

#add MA to each Marine Area # so that R reads it as a categorical variable and not a continuous variable
BMmus.all$MA[BMmus.all$MarineArea == 6] <- "MA6"
BMmus.all$MA[BMmus.all$MarineArea == 7] <- "MA7"
BMmus.all$MA[BMmus.all$MarineArea == 8.1] <- "MA8.1"
BMmus.all$MA[BMmus.all$MarineArea == 8.2] <- "MA8.2"
BMmus.all$MA[BMmus.all$MarineArea == 9] <- "MA9"
BMmus.all$MA[BMmus.all$MarineArea == 10] <- "MA10"
BMmus.all$MA[BMmus.all$MarineArea == 12] <- "MA12"
BMmus.all$MA <-factor(BMmus.all$MA) #assign MA column as a factor or categorical variable
levels(BMmus.all$MA) #examine levels of MA = alphabetical, need to be from lowest to highest
BMmus.all$MA <- factor(BMmus.all$MA, levels = c('MA6', 'MA7', 'MA8.1', 'MA8.2', 'MA9', 'MA10', 'MA12')) #redefine the order of levels

#create new dataframe, remove MarineArea column since it's a continuous variable and remove Matrix since it's useless

BMpops <- as.data.frame(BMmus.all[ ,c(1,4:23)])

# transpose data from wide format to long format
BMpops.long <- reshape(BMpops,varying = c("ForkLength", "ScaleAge", "SWAge", "Fwage", "OutmigrationLH", "Origin", "Lipids", 
                                            "HCB", "SumHCHs", "SumCHLDs", "dieldrin", "SumDDTs", "TPCBs", "SumBDE", "NOAA_CNratio", 
                                            "NOAA_delta13C_LipExt", "NOAA_delta15N_LipExt", "UW_DeltaN_NotExt","UW_DeltaC_NotExt"), 
                        times = c("ForkLength", "ScaleAge", "SWAge", "Fwage", "OutmigrationLH", "Origin", "Lipids", 
                                  "HCB", "SumHCHs", "SumCHLDs", "dieldrin", "SumDDTs", "TPCBs", "SumBDE", "NOAA_CNratio", 
                                  "NOAA_delta13C_LipExt", "NOAA_delta15N_LipExt", "UW_DeltaN_NotExt","UW_DeltaC_NotExt"), 
                        v.names = "Variables", idvar = c("FishID", "MA"), direction = "long")

# scatterplot of length vs. TPCBs colored by Marine Area
PCBslen <- ggplot(BMpops, aes(x = ForkLength, y = TPCBs)) + geom_point()
PCBslen <- PCBslen + aes(colour = MA)
print(PCBslen)

PCBslreg <- ggplot(BMpops, aes(x = ForkLength, y = TPCBs, color = MA)) + 
    geom_point() +
    scale_colour_hue(1=50) +
    geom_smooth(method = lm,
                se = FALSE)
print(PCBslreg)


