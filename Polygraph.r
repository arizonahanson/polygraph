# Script to take data output from "Tartini" as plain text
# and draw graphs of this in a different form than Tartini
# Written by Graeme Roxburgh April 2008 groxburgh@infoscience.otago.ac.nz
# version 1.0.2 (17-4-08)

# if doing vioplot then
# Uses packages sm, vioplot so these need to be installed
# Only need to do this once
# May need to change working directory if not in the correct one
# if using this interactively 

#############################
# read in command line arguments
# arguments are       1                         2                    3  4     5     6  7    8
# eg "C:\Program Files\R\R-2.6.2\bin\R.exe" --no-save < Polygraph.r 441 20 boxplot 10 1.5 justD
# first set some defaults
args <- 0
args[3] <- 440
args[4] <- 5
args[5] <- "boxplot"
args[6] <- 10
args[7] <- 1.5
args[8] <- "equal"
# test to see if we've got the right number of command line Arguments and if we have use them
# we just hope they're all in the correct order
testArgs <- commandArgs()
if (length(testArgs) == 8){args  <- commandArgs()}
length (commandArgs)
# [3] is pitch shift (real), 
# [4] filtering diff width (real), 
# [5] type of plot,
# [6] is min number of data points for note to be included in graph (integer)
# [7] is h in vioplot which determines the smoothing - around 1.5 looks good (real)
# [8] is temperament either equal or justD
#############################
# What temperament are we plotting against
# set equal temperament as the default
temperament <- c(0,0,0,0,0,0,0,0)
if (args[8] == "justD") {temperament <- c(0,+4,-14,-2,+2,-16,-4,-12)}

#############################
# read data from file "rtest.txt" in current working directory 
rawdata <- read.table("export.txt", header=TRUE) 

#############################
# Trying some Filtering using value of cents from command line argument
# remove data where cosectutive values of Pitch are > diff semitones different 
# and put into only pitch data a new array "z" ------ 0.20 is 20 cents
# difference from command line is 4th item as string in cents 0 = no filtering
diff <- (as.double(args[4]) / 100)
if(diff == 0) 
	{z <- rawdata$Pitch}
if(diff > 0)
{
	z <- NULL
	i <- 1
	for (i in 2:(length(rawdata$Pitch)-1)) {   
               if (((rawdata$Pitch[i]-rawdata$Pitch[i-1]) > diff) | ((rawdata$Pitch[i-1]-rawdata$Pitch[i]) > diff) | ((rawdata$Pitch[i]-rawdata$Pitch[i+1]) > diff) | ((rawdata$Pitch[i]-rawdata$Pitch[i+1]) > diff)) 
		{ z <- c(z,0)  } else { z <- c(z,rawdata$Pitch[i])  }
      }
}

#############################
# Not at A = 440? convert pitch in Hz to semitone and shift all values
# Pitch entered from command line is 3rd item as string
pitch <- as.double(args[3])
if (pitch != 440) {z <- (z - (log2(pitch/440)*12.00))}
# or if you want to hard code a shift in this file do this
# z <- z - 12.00 # plots high D whistle
# z <- z + 2.00 # plots C flute
# z <- z - 0.15 # shifts data 15 cents down if instrument is sharp


#############################
# assign notes all data from "Tartini" in their range then change to cents above or below equal temperament
D4 <- (z[z>=61.5 & z<=62.5] - 62) * 100
E4 <- (z[z>=63.5 & z<=64.5] - 64) * 100
Fs4 <- (z[z>=65.5 & z<=66.5] - 66) * 100
G4 <- (z[z>=66.5 & z<=67.5] - 67) * 100
A4 <- (z[z>=68.5 & z<=69.5] - 69) * 100
B4 <- (z[z>=70.5 & z<=71.5] - 71) * 100
C5 <- (z[z>=71.5 & z<=72.5] - 72) * 100
Cs5 <- (z[z>=72.5 & z<=73.5] - 73) * 100
D5 <- (z[z>=73.5 & z<=74.5] - 74) * 100
E5 <- (z[z>=75.5 & z<=76.5] - 76) * 100
Fs5 <- (z[z>=77.5 & z<=78.5] - 78) * 100
G5 <- (z[z>=78.5 & z<=79.5] - 79) * 100
A5 <- (z[z>=80.5 & z<=81.5] - 81) * 100
B5 <- (z[z>=82.5 & z<=83.5] - 83) * 100

#############################
# adjust for temperament
D4 <- D4 - temperament[1]
E4 <- E4 - temperament[2]
Fs4 <- Fs4 - temperament[3]
G4 <- G4 - temperament[4]
A4 <- A4 - temperament[5]
B4 <- B4 - temperament[6]
C5 <- C5 - temperament[7]
Cs5 <- Cs5 - temperament[8]
D5 <- D5 - temperament[1]
E5 <- E5 - temperament[2]
Fs5 <- Fs5 - temperament[3]
G5 <- G5 - temperament[4]
A5 <- A5 - temperament[5]
B5 <- B5 - temperament[6]

#############################

# discard notes where there's < minsize data points but
# minsize entered from command line is 6th item as string
# also ensures theres one value for each note at 0 cents or graph may fail to print
minsize <- as.integer(args[6])
if (length(D4) < minsize)	{ D4 <- 0  } 
if (length(E4) < minsize)	{ E4 <- 0  } 
if (length(Fs4) < minsize)	{ Fs4 <- 0  } 
if (length(G4) < minsize)	{ G4 <- 0  } 
if (length(A4) < minsize)	{ A4 <- 0  } 
if (length(B4) < minsize)	{ B4 <- 0  } 
if (length(C5) < minsize)	{ C5 <- 0  } 
if (length(Cs5) < minsize)	{ Cs5 <- 0  } 
if (length(D5) < minsize)	{ D5 <- 0  } 
if (length(E5) < minsize)	{ E5 <- 0  } 
if (length(Fs5) < minsize)	{ Fs5 <- 0  } 
if (length(G5) < minsize)	{ G5 <- 0  } 
if (length(A5) < minsize)	{ A5 <- 0  } 
if (length(B5) < minsize)	{ B5 <- 0  } 


#############################
# output graph to JPEG file rather than within R console
jpeg("Polygraph.jpg", width = 1024, height = 768, units = "px",)

# 8 HCL colors
mycolors <- c("#80C990", "#50CACD", "#A3B8EF", "#CCACED", "#E6A3DC", "#EFA6A2", "#E0AF85", "#C8C874")
#############################
# draw a "violin" plot of each note 
# Load packages sm, vioplot
if (args[5] == "vioplot") 
{
H <- as.double(args[7])
library(sm)
library(vioplot)
vioplot(D4, E4, Fs4, G4, A4, B4, C5, Cs5, D5, E5, Fs5, G5, A5, B5, names=c("D4", "E4", "F#4", "G4", "A4", "B4", "C5", "C#5", "D5", "E5", "F#5", "G5", "A5", "B5"), col=mycolors, h=H)
}
#############################
# draw a boxplot
if (args[5] == "boxplot") 
{
boxplot(D4, E4, Fs4, G4, A4, B4, C5, Cs5, D5, E5, Fs5, G5, A5, B5, varwidth = TRUE, outline = FALSE, names=c("D4", "E4", "F#4", "G4", "A4", "B4", "C5", "C#5", "D5", "E5", "F#5", "G5", "A5", "B5"), col=mycolors, range = 0)
}

#############################
# label the graph
title(xlab="Pitch", ylab="Cents deviation from equal temperament", main="Tartini-R Polygraph", sub="Starlight flute")
# add horizontal gridlines
axis(2, tck = 1, col = "grey", lty = "dotted")

#############################
# write median values of notes to a file for import to MSExcel or similar
meds <- c(median(D4), median(E4), median(Fs4), median(G4), median(A4), median(B4), median(C5), median(Cs5), median(D5), median(E5), median(Fs5), median(G5), median(A5), median(B5))
write(round(meds, digits = 0), file = "medians.txt", ncolumns = 1, append = FALSE, sep = ",")

#############################
# close jpeg file  
dev.off()

#############################
# quit R
q(save = "no")




