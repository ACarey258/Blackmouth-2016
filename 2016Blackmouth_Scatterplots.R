rm(list=ls(all=TRUE))

library(readxl)
library(ggplot2)

#set paths, make a list of text for the files to be used
paths = list("C:\\data\\GitHub\\Blackmouth-2016\\2016Blackmouth_DataForR_6.27.17.xlsx",
             "C:\\data\\GitHub\\Blackmouth-2016\\Outputs\\")

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
# 1. Scatterplot of individual lengths (y) and ages (x)

a <- ggplot(BMmuscle, aes(x = ScaleAge, y = ForkLength)) + geom_point()
print(a) #simple scatterplot, removed NAs (13)
print({a + aes(colour = MA)}) #color codes fish based on marine area where they were collected, removed NAs (13) 

# Save the plot to outfile
ggsave(paste(outfile,"scaleplot.jpg",sep=""),a,height=5,width=7.5)

## need to export graphic...

# default dimensions in pixels, copied from website
jpeg(file = "BM_length and age.jpg",
     width = 10,
     height = 6.67, 
     units = "mm",
     pointsize = 12, 
     quality = 75)
dev.off()
