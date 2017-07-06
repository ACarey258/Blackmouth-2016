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
BMdat <- as.data.frame(BM[ ,c(2:4,6:30)])
BMmuscle <- subset(BMdat, Matrix =="muscle") #includes 13 priority 3 fish that weren not analyzed for POPs
BMmuscle$FL_cm <- as.numeric(BMmuscle$FL_cm)  #make lengths and weights a numeric variable since there's one NA in there
BMmuscle$TL_cm <- as.numeric(BMmuscle$TL_cm)
BMmuscle$TL_inch <- as.numeric(BMmuscle$TL_inch)
BMmuscle$WBWt_lbs <- as.numeric(BMmuscle$WBWt_lbs)
BMmuscle$Wt_kg <- as.numeric(BMmuscle$Wt_kg)

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

a <- ggplot(BMmuscle, aes(x = ScaleAge, y = FL_cm)) + geom_point()
print(a) #simple scatterplot, removed NAs (13)
print({a + aes(colour = MA)}) #color codes fish based on marine area where they were collected, removed NAs (13) 

a <- a + aes(color = MA) #color codes fish based on marine area where they were collected, removed NAs (13) 

# Save the plot to outfile
ggsave(paste(outfile,"16BM_ScaleAge and Length.jpg",sep=""),a,height=5,width=7.5)

# 2. Scatterplot of individual lengths (y) and saltwater ages (x)
b <- ggplot(BMmuscle, aes(x = SWAge, y = FL_cm)) + geom_point()
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


#Log transform length and weight data 
lwdat <- as.data.frame(BMmuscle[ ,c(1:2,4:15,29:30)])
lwdat <- transform(lwdat, log_FLcm = log10(FL_cm))
lwdat <- transform(lwdat, log_TLcm = log10(TL_cm))
lwdat <- transform(lwdat, log_TLinch = log10(TL_inch))
lwdat <- transform(lwdat, log_WTlbs = log10(WBWt_lbs))
lwdat <- transform(lwdat, log_WTkg = log10(Wt_kg))

# try simple plotting to see how data looks before getting fancy with ggplot
LenWt <- plot(lwdat$TL_inch, lwdat$WBWt_lbs,
              xlab = "Total Length (inches)",
              ylab = "Whole Body Weight (pounds)",
              log = "x") #log x=axis scale

# ggplot scatterplot 
lw <- ggplot(lwdat, aes(x = log_TLinch, y = WBWt_lbs)) + 
    geom_point(aes(color = SWageCat), size = 3) +
    geom_point(shape = 1, size = 3, color = "black") + #allows for only one regression line for all points instead of by category
    theme_classic() +
    labs(x = "(Log10) Total Length (in)", y = "Whole Body Fish Weight (lbs)", color = "SW Age")
print(lw)

#explore which line is best for data
lw.all <- lw + stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE, colour = "black") + #linear fit
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE, colour = "blue") + #quadratic, includes squared x term
    stat_smooth(method = "loess", formula = y ~ x, size = 1, se = FALSE, colour = "red") + #locally weighted regression
    stat_smooth(method = "gam", formula = y ~ s(x), size = 1, se = FALSE, colour = "green") + # a GAM w/a smoother for x
    stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = FALSE, colour = "violet") # a GAM w/an x smoother and a dimension of 3 
print(lw.all)

# scatter with best looking line - gam = general additive model, smoother on the x predictor variable
lw2 <- lw + stat_smooth(method = "gam", formula = y ~ s(x), size = 1, colour = "black")
print(lw2) #gray shading is standard error

ggsave(paste(outfile,"16BM_LengthWt_.jpg",sep=""),lw2,height = 5,width = 7.5)

# code for calculating regression equation and r squared values - not used since length vs. weight relationship isn't linear
#lw_eqn <- function(BMmuscle){
#m <- lm(WholeFishWt ~ ForkLength, BMmuscle);
#eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                list(a = format(coef(m)[1], digits = 2), 
#                    b = format(coef(m)[2], digits = 2), 
#                   r2 = format(summary(m)$r.squared, digits = 3)))
#    as.character(as.expression(eq));                 
#} #function to add regression equation and r squared to a scatterplot

# text for adding regression eqtn and r^2 value to plot: geom_text(x = 45, y = 10, label = lw_eqn(BMmuscle), parse = TRUE) 



##############################################################################################################################################
# SETTING UP ALL DATA FOR PLOTTING IN A LOOP
# DATA NEEDS TO BE IN SKINNY/LONG FORMAT

#create new dataframe from master data file - BM, keep only muscle samples, remove any rows with unknowns for ScaleAge n = 13
colnames(BM)
BMdata <- as.data.frame(BM[ ,c(2:4,6:15,17:22,24:30)])
BMdata <- subset(BMdata, Matrix=="muscle") #keep only muscle samples, don't need whole body samples since we only have them from MA 10
BMmus.chem <- subset(BMdata, FWAge>="1") #removes fish that did not get analyzed for POPs (priority 3 fish which were unable to be aged)

#add MA to each Marine Area # so that R reads it as a categorical variable and not a continuous variable
BMmus.chem$MA[BMmus.chem$MarineArea == 6] <- "MA6"
BMmus.chem$MA[BMmus.chem$MarineArea == 7] <- "MA7"
BMmus.chem$MA[BMmus.chem$MarineArea == 8.1] <- "MA8.1"
BMmus.chem$MA[BMmus.chem$MarineArea == 8.2] <- "MA8.2"
BMmus.chem$MA[BMmus.chem$MarineArea == 9] <- "MA9"
BMmus.chem$MA[BMmus.chem$MarineArea == 10] <- "MA10"
BMmus.chem$MA[BMmus.chem$MarineArea == 12] <- "MA12"
BMmus.chem$MA <-factor(BMmus.chem$MA) #assign MA column as a factor or categorical variable
levels(BMmus.chem$MA) #examine levels of MA = alphabetical, need to be from lowest to highest
BMmus.chem$MA <- factor(BMmus.chem$MA, levels = c('MA6', 'MA7', 'MA8.1', 'MA8.2', 'MA9', 'MA10', 'MA12')) #redefine the order of levels

#create new dataframe, remove MarineArea column since it's a continuous variable and remove Matrix since it's useless
colnames(BMmus.chem)
BMpops <- as.data.frame(BMmus.chem[ ,c(1,4:27)])

# transpose data from wide format to long format
BMpops.long <- reshape(BMpops,varying = c("FL_cm", "TL_cm", "TL_inch", "WBWt_lbs", "Wt_kg", "ScaleAge", "SWAge", "FWAge", "Lipids", 
                                            "HCB", "SumHCHs", "SumCHLDs", "dieldrin", "SumDDTs", "TPCBs", "SumBDE", "NOAA_CNratio", 
                                            "NOAA_delta13C_LipExt", "NOAA_delta15N_LipExt", "UW_DeltaN_NotExt","UW_DeltaC_NotExt"), 
                        times = c("FL_cm", "TL_cm", "TL_inch", "WBWt_lbs", "Wt_kg", "ScaleAge", "SWAge", "FWAge", "Lipids", 
                                  "HCB", "SumHCHs", "SumCHLDs", "dieldrin", "SumDDTs", "TPCBs", "SumBDE", "NOAA_CNratio", 
                                  "NOAA_delta13C_LipExt", "NOAA_delta15N_LipExt", "UW_DeltaN_NotExt","UW_DeltaC_NotExt"), 
                        v.names = "Variables", idvar = c("FishID", "OutmigrationLH", "Origin", "MA"), direction = "long")

## Using the wide format data set - scatterplot of length vs. TPCBs colored by Marine Area, all fish
PCBslen <- ggplot(BMpops, aes(x = FL_cm, y = TPCBs)) + geom_point()
PCBslen <- PCBslen + aes(colour = MA)
print(PCBslen)

PCBslreg <- ggplot(BMpops, aes(x = FL_cm, y = TPCBs, color = MA)) + 
    geom_point() +
    geom_smooth(method = "lm", #adds regression lines to each Marine Area 
                se = FALSE) #removes shaded standard error area around regression lines
print(PCBslreg)
ggsave(paste(outfile,"16BM_LengthTPCBs.jpg",sep=""),PCBslreg,height=5,width=7.5)

## only plot legal sized fish > 56cm Total Length, 52.5 cm FL = 22 inch fish, Graphed fish that were greater than ~51.5cm Fork Length
BMlegal <- subset(BMpops, TL_inch>="21.5") #excludes 1 fish from MA12, 1 from MA7 and 6 from MA10
PCBsleg <- ggplot(BMlegal, aes(x = FL_cm, y = TPCBs, color = MA)) + 
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE)
print(PCBsleg)
ggsave(paste(outfile,"16BM_LengthTPCBs_LegalSize.jpg",sep=""),PCBsleg,height=5,width=7.5)

## messing around with ggplot...
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #palette of 8 colorblind-friendly colors
#excludes 1 fish from MA12, 1 from MA7 and 6 from MA10
PCBsleg2 <- ggplot(BMlegal, aes(x = FL_cm, y = TPCBs, color = MA)) + 
    geom_point(aes(color = MA), size = 3) +
    geom_point(shape = 1, size = 3, color = "black") +
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
PBDEsleg <- ggplot(BMlegal, aes(x = FL_cm, y = SumBDE, color = MA)) + 
    geom_point(aes(color = MA), size = 3) +
    geom_point(shape = 1, size = 3, color = "black") +
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
varlist <- unique(BMpops.long$time) #21 variables

i=1
for(i in 1:length(varlist)) {
    var <- varlist[i]
    
    vardat <- BMpops.long[BMpops.long$time == var, ]
    
    }