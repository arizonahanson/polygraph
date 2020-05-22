# Script to take data output from "Tartini" as plain text
# and draw graphs of this in a different form than Tartini
# Written by Graeme Roxburgh April 2008 groxburgh@infoscience.otago.ac.nz
# version 1.0.1(chromatic) (12-4-08)

# May need to change working directory if not in the correct one
# if using this interactively 

#############################
# read in command line arguments
# arguments are       1                         2      3  4     5     6  7    8
# eg "C:\Program Files\R\R-2.6.2\bin\R.exe" --no-save 441 20 boxplot 10 1.5 justD < Polygraph.r
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
# [3] is pitch shift (integer), 
# [4] filtering diff witdth (integer), 
# [5] type of plot,
# [6] is min number of data points for note to be included in graph (integer)
# [7] is h in vioplot which determines the smoothing - around 1.5 looks good (real)
# [8] is temperament either equal or justD
#############################
# What temperament are we plotting against
# set equal temperament as the default
# temperament starts with C, C#....
temperament <- c(0,0,0,0,0,0,0,0,0,0,0,0)
if (args[8] == "justD") {temperament <- c(-4,-12,0,+12,+4,+16,-14,-2,-10,+2,+14,-16)}
# JustD
#  C  C# D  Eb  E  F   F#  G  G#  A  Bb  B   
# -4,-12,0,+12,+4,+16,-14,-2,-10,+2,+14,-16  



#############################
# read data from file "rtest.txt" in current working directory 
rawdata <- read.table("rtest.txt", header=TRUE) 

#############################
# Trying some Filtering using value of cents from command line argument
# remove data where cosectutive values of Pitch are > diff semitones different 
# and put into only pitch data a new array "z" ------ 0.20 is 20 cents
# difference from command line is 4th item as string in cents 0 = no filtering
diff <- (as.integer(args[4]) / 100)
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
pitch <- as.integer(args[3])
if (pitch != 440) {z <- (z - (log2(pitch/440)*12.00))}
# or if you want to hard code a shift in this file do this
# z <- z - 12.00 # plots high D whistle
# z <- z + 2.00 # plots C flute
# z <- z - 0.15 # shifts data 15 cents down if instrument is sharp


#############################
# assign notes all data from "Tartini" in their range then change to cents above or below equal temperament
G3 <- (z[z>=54.5 & z<=55.5] - 55) * 100
Gs3 <- (z[z>=55.5 & z<=56.5] - 56) * 100
A3 <- (z[z>=56.5 & z<=57.5] - 57) * 100
Bb3 <- (z[z>=57.5 & z<=58.5] - 58) * 100
B3 <- (z[z>=58.5 & z<=59.5] - 59) * 100
C4 <- (z[z>=59.5 & z<=60.5] - 60) * 100
Cs4 <- (z[z>=60.5 & z<=61.5] - 61) * 100
D4 <- (z[z>=61.5 & z<=62.5] - 62) * 100
Eb4 <- (z[z>=62.5 & z<=63.5] - 63) * 100
E4 <- (z[z>=63.5 & z<=64.5] - 64) * 100
F4 <- (z[z>=64.5 & z<=65.5] - 65) * 100
Fs4 <- (z[z>=65.5 & z<=66.5] - 66) * 100
G4 <- (z[z>=66.5 & z<=67.5] - 67) * 100
Gs4 <- (z[z>=67.5 & z<=68.5] - 68) * 100
A4 <- (z[z>=68.5 & z<=69.5] - 69) * 100
Bb4 <- (z[z>=69.5 & z<=70.5] - 70) * 100
B4 <- (z[z>=70.5 & z<=71.5] - 71) * 100
C5 <- (z[z>=71.5 & z<=72.5] - 72) * 100
Cs5 <- (z[z>=72.5 & z<=73.5] - 73) * 100
D5 <- (z[z>=73.5 & z<=74.5] - 74) * 100
Eb5 <- (z[z>=74.5 & z<=75.5] - 75) * 100
E5 <- (z[z>=75.5 & z<=76.5] - 76) * 100
F5 <- (z[z>=76.5 & z<=77.5] - 77) * 100
Fs5 <- (z[z>=77.5 & z<=78.5] - 78) * 100
G5 <- (z[z>=78.5 & z<=79.5] - 79) * 100
Gs5 <- (z[z>=79.5 & z<=80.5] - 80) * 100
A5 <- (z[z>=80.5 & z<=81.5] - 81) * 100
Bb5 <- (z[z>=81.5 & z<=82.5] - 82) * 100
B5 <- (z[z>=82.5 & z<=83.5] - 83) * 100
C6 <- (z[z>=83.5 & z<=84.5] - 84) * 100
Cs6 <- (z[z>=84.5 & z<=85.5] - 85) * 100
D6 <- (z[z>=85.5 & z<=86.5] - 86) * 100
Eb6 <- (z[z>=86.5 & z<=87.5] - 87) * 100
E6 <- (z[z>=87.5 & z<=88.5] - 88) * 100
F6 <- (z[z>=88.5 & z<=89.5] - 89) * 100
Fs6 <- (z[z>=89.5 & z<=90.5] - 90) * 100
G6 <- (z[z>=90.5 & z<=91.5] - 91) * 100
Gs6 <- (z[z>=91.5 & z<=92.5] - 92) * 100
A6 <- (z[z>=92.5 & z<=93.5] - 93) * 100
Bb6 <- (z[z>=93.5 & z<=94.5] - 94) * 100
B6 <- (z[z>=94.5 & z<=95.5] - 95) * 100
C7 <- (z[z>=95.5 & z<=96.5] - 96) * 100

#############################
# adjust for temperament
G3 <- G3 - temperament[8]
Gs3 <- Gs3 - temperament[9]
A3 <- A3 - temperament[10]
Bb3 <- Bb3 - temperament[11]
B3 <- B3 - temperament[12]
C4 <- C4 - temperament[1]
Cs4 <- Cs4 - temperament[2]
D4 <- D4 - temperament[3]
Eb4 <- Eb4 - temperament[4]
E4 <- E4 - temperament[5]
F4 <- F4 - temperament[6]
Fs4 <- Fs4 - temperament[7]
G4 <- G4 - temperament[8]
Gs4 <- Gs4 - temperament[9]
A4 <- A4 - temperament[10]
Bb4 <- Bb4 - temperament[11]
B4 <- B4 - temperament[12]
C5 <- C5 - temperament[1]
Cs5 <- Cs5 - temperament[2]
D5 <- D5 - temperament[3]
Eb5 <- Eb5 - temperament[4]
E5 <- E5 - temperament[5]
F5 <- F5 - temperament[6]
Fs5 <- Fs5 - temperament[7]
G5 <- G5 - temperament[8]
Gs5 <- Gs5 - temperament[9]
A5 <- A5 - temperament[10]
Bb5 <- Bb5 - temperament[11]
B5 <- B5 - temperament[12]
C6 <- C6 - temperament[1]
Cs6 <- Cs6 - temperament[2]
D6 <- D6 - temperament[3]
Eb6 <- Eb6 - temperament[4]
E6 <- E6 - temperament[5]
F6 <- F6 - temperament[6]
Fs6 <- Fs6 - temperament[7]
G6 <- G6 - temperament[8]
Gs6 <- Gs6 - temperament[9]
A6 <- A6 - temperament[10]
Bb6 <- Bb6 - temperament[11]
B6 <- B6 - temperament[12]
C7 <- C7 - temperament[1]
#############################

# discard notes where there's < minsize data points but
# minsize entered from command line is 6th item as string
# also ensures theres one value for each note at 0 cents or graph may fail to print
minsize <- as.integer(args[6])
if (length(G3) < minsize)	{ G3 <- 0  } 
if (length(Gs3) < minsize)	{ Gs3 <- 0  } 
if (length(A3) < minsize)	{ A3 <- 0  } 
if (length(Bb3) < minsize)	{ Bb3 <- 0  } 
if (length(B3) < minsize)	{ B3 <- 0  } 
if (length(C4) < minsize)	{ C4 <- 0  } 
if (length(Cs4) < minsize)	{ Cs4 <- 0  } 
if (length(D4) < minsize)	{ D4 <- 0  } 
if (length(Eb4) < minsize)	{ Eb4 <- 0  } 
if (length(E4) < minsize)	{ E4 <- 0  } 
if (length(F4) < minsize)	{ F4 <- 0  } 
if (length(Fs4) < minsize)	{ Fs4 <- 0  } 
if (length(G4) < minsize)	{ G4 <- 0  } 
if (length(Gs4) < minsize)	{ Gs4 <- 0  } 
if (length(A4) < minsize)	{ A4 <- 0  } 
if (length(Bb4) < minsize)	{ Bb4 <- 0  } 
if (length(B4) < minsize)	{ B4 <- 0  } 
if (length(C5) < minsize)	{ C5 <- 0  } 
if (length(Cs5) < minsize)	{ Cs5 <- 0  } 
if (length(D5) < minsize)	{ D5 <- 0  } 
if (length(E5) < minsize)	{ E5 <- 0  } 
if (length(F5) < minsize)	{ F5 <- 0  } 
if (length(Fs5) < minsize)	{ Fs5 <- 0  } 
if (length(G5) < minsize)	{ G5 <- 0  } 
if (length(Gs5) < minsize)	{ Gs5 <- 0  } 
if (length(A5) < minsize)	{ A5 <- 0  } 
if (length(Bb5) < minsize)	{ Bb5 <- 0  } 
if (length(B5) < minsize)	{ B5 <- 0  } 
if (length(C6) < minsize)	{ C6 <- 0  } 
if (length(Cs6) < minsize)	{ Cs6 <- 0  } 
if (length(D6) < minsize)	{ D6 <- 0  } 
if (length(E6) < minsize)	{ E6 <- 0  } 
if (length(F6) < minsize)	{ F6 <- 0  } 
if (length(Fs6) < minsize)	{ Fs6 <- 0  } 
if (length(G6) < minsize)	{ G6 <- 0  } 
if (length(Gs6) < minsize)	{ Gs6 <- 0  } 
if (length(A6) < minsize)	{ A6 <- 0  } 
if (length(Bb6) < minsize)	{ Bb6 <- 0  } 
if (length(B6) < minsize)	{ B6 <- 0  } 
if (length(C7) < minsize)	{ C7 <- 0  } 


#############################
# output graph to JPEG file rather than within R console
jpeg("Polygraph-chromatic.jpg", width = 1600, height = 600, units = "px",)


#############################
# draw a boxplot of each note
boxplot(G3, Gs3, A3, Bb3, B3, C4, Cs4, D4, Eb4, E4, F4, Fs4, G4, Gs4, A4, Bb4, B4, C5, Cs5, D5, Eb5, E5, F5, Fs5, G5, Gs5, A5, Bb5, B5, C6, Cs6, D6, Eb6, E6, F6, Fs6, G6, Gs6, A6, Bb6, B6, C7, varwidth = TRUE, outline = FALSE, names=c("G", "", "A", "", "B", "C", "", "D", "", "E", "F", "", "G", "", "A", "", "B", "C", "", "D", "", "E", "F", "", "G", "", "A", "", "B", "C", "", "D", "", "E", "F", "", "G", "", "A", "", "B", "C" ), col="gold", range = 0)


#############################
# label the graph
title(xlab="Note", ylab="Cents difference from Chosen Temperament", main="Pitch of Irish Flute
")
# add horizontal gridlines
axis(2, tck = 1, col = "grey", lty = "dotted")

#############################
# close jpeg file  
dev.off()

#############################
# quit R
q(save = "no")




