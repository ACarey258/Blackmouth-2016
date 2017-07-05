#clear workspace
rm(list=ls(all=TRUE))

#load packages
library(readxl)
library(ggplot2)

#set paths, make a list of text for the files to be used
paths = list("C:\\Users\\careyajc\\PSEMP\\PSEMP\\GitHub\\Blackmouth-2016\\Blackmouth-2016\\2016Blackmouth_DataForR_6.27.17.xlsx",
             "C:\\Users\\careyajc\\PSEMP\\PSEMP\\GitHub\\Blackmouth-2016\\Blackmouth-2016\\Outputs\\")
# paths = list("C:\\data\\GitHub\\Blackmouth-2016\\2016Blackmouth_DataForR_6.27.17.xlsx",
#              "C:\\data\\GitHub\\Blackmouth-2016\\Outputs\\")



# set outfile
outfile = paths[[2]]

#read in data
BM <- read_excel(paths[[1]],"BlackmouthPOPsSIs")

#create dataframe of data only needed for scatterplots, subset out the Matrix = muscle
BMdat <- as.data.frame(BM[ ,c(2:4,6:27)])
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
print(b)
ggsave(paste(outfile,"16BM_SWage and Length.jpg",sep=""),b,height=5,width=7.5)

# 3. Scatterplot of individual lengths (x) and whole fish weights (y), gutted fish weights not included in data file 
BMmuscle$SWageCat[BMmuscle$SWAge == 0] <- "Zero"
BMmuscle$SWageCat[BMmuscle$SWAge == 1] <- "One"
BMmuscle$SWageCat[BMmuscle$SWAge == 2] <- "Two"
BMmuscle$SWageCat[BMmuscle$SWAge == 3] <- "Three"
BMmuscle$SWageCat[BMmuscle$SWAge == 4] <- "Four"
BMmuscle$SWageCat <-factor(BMmuscle$SWageCat) #assign SWAge column as a factor/categorical variable
levels(BMmuscle$SWageCat) #examine levels of SWageCat = alphabetical, need to be from lowest to highest
BMmuscle$SWageCat <- factor(BMmuscle$SWageCat, levels = c('Zero', 'One', 'Two', 'Three', 'Four'))

lw_eqn <- function(BMmuscle){
    m <- lm(WholeFishWt ~ ForkLength, BMmuscle);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = format(coef(m)[1], digits = 2), 
                          b = format(coef(m)[2], digits = 2), 
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));                 
} #function to add regression equation and r squared to a scatterplot

lw <- ggplot(BMmuscle, aes(x = ForkLength, y = WholeFishWt)) + 
    geom_point(aes(color = SWageCat), shape = 1, size = 3) + #allows for only one regression line for all points instead of by category
    geom_smooth(method = "lm",
                se = FALSE,
                color = "black") + #add regression line, need to add r^2 for each line
    theme_classic() +
    geom_text(x = 45, y = 10, label = lw_eqn(BMmuscle), parse = TRUE) + #adds regression equation and r^2 value
    labs(x = "Fork Length (cm)", y = "Whole Body Fish Weight (lbs)", color = "SW Age")

print(lw)
ggsave(paste(outfile,"16BM_LengthWtRegression.jpg",sep=""),lw,height = 5,width = 7.5)

## START HERE
##BMmuscle$WholeFishWtCat <- categorize(BMmuscle$WholeFishWt, breaks = 4, quantile = FALSE,
                                      #labels = c("one", "two", "three", "four"))
##levels(BMmuscle$WholeFishWtCat)

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

# scatterplot of length vs. TPCBs colored by Marine Area, all fish
PCBslen <- ggplot(BMpops, aes(x = ForkLength, y = TPCBs)) + geom_point()
PCBslen <- PCBslen + aes(colour = MA)

print(PCBslen)

PCBslreg <- ggplot(BMpops, aes(x = ForkLength, y = TPCBs, color = MA)) + 
    geom_point() +
    geom_smooth(method = "lm", #adds regression lines to each Marine Area 
                se = FALSE) #removes shaded standard error? area around regression lines
print(PCBslreg)
ggsave(paste(outfile,"16BM_LengthTPCBs.jpg",sep=""),PCBslreg,height=5,width=7.5)

## only plot legal sized fish > 56cm Total Length, 52.5 cm FL = 22 inch fish, Graphed fish that were greater than ~51.5cm Fork Length
BMlegal <- subset(BMpops, ForkLength>="51.5") #excludes 2 fish from MA12, 1 from MA7 and 6 from MA10
PCBsleg <- ggplot(BMlegal, aes(x = ForkLength, y = TPCBs, color = MA)) + 
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE)
print(PCBsleg)
ggsave(paste(outfile,"16BM_LengthTPCBs_LegalSize.jpg",sep=""),PCBsleg,height=5,width=7.5)

## messing around with ggplot...
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #palette of 8 colorblind-friendly colors
#excludes 2 fish from MA12, 1 from MA7 and 6 from MA10
PCBsleg2 <- ggplot(BMlegal, aes(x = ForkLength, y = TPCBs, color = MA)) + 
    geom_point(size = 3) +
    scale_colour_manual(values = cbPalette) + #colorblind-friendly colors
    geom_smooth(method = "lm",
                se = FALSE) + #adds regression lines, need to add r^2 for each line
    theme_classic() + #black and white with no gridlines
    xlab("Fork Length (cm)") + #x-axis label
    ylab("TPCBs (ng/g ww) in muscle tissue") + #y-axis label
    scale_y_continuous(breaks = seq(0,175,25)) + #set y-axis scale, start=0, end=175, tick every 25
    scale_x_continuous(breaks = seq(50,85,5)) #set x-axis scale, start=50, end=85, tick every 5
print(PCBsleg2)

ggsave(paste(outfile,"16BM_LengthTPCBs_LegalSize_Ver2.jpg",sep=""),PCBsleg2,height=5,width=7.5)

## PBDEs vs length

PBDEsleg <- ggplot(BMlegal, aes(x = ForkLength, y = SumBDE, color = MA)) + 
    geom_point(size = 3) +
    scale_colour_manual(values = cbPalette) + #colorblind-friendly colors
    geom_smooth(method = "lm",
                se = FALSE) + #adds regression lines, need to add r^2 for each line
    theme_classic() + #black and white with no gridlines
    xlab("Fork Length (cm)") + #x-axis label
    ylab("SumBDEs (ng/g ww) in muscle tissue") + #y-axis label
    scale_y_continuous(breaks = seq(0,35,5)) + #set y-axis scale, start=0, end=35, tick every 25
    scale_x_continuous(breaks = seq(50,85,5)) #set x-axis scale, start=50, end=85, tick every 5
print(PBDEsleg)
ggsave(paste(outfile,"16BM_LengthPBDEs_LegalSize_Ver2.jpg",sep=""),PBDEsleg,height=5,width=7.5)

## !! TO DO - add an r-squared value for each regression line/Marine Area

## Comparison of contaminants and various biometrics (ForkLength, ScaleAge, SWage, FWage, OutmigrationLH, Lipids)
# loop
varlist <- unique(BMpops.long$time) #19 variables

i=1
for(i in 1:length(varlist)) {
    var <- varlist[i]
    
    vardat <- BMpops.long[BMpops.long$time == var, ]
    
    }