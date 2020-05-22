# Script to take text output from Tartini and draw a boxplot of pitch variation
# around ideal 12TET tuning.
# Written by Martin Sandiford ms@mcdev.com.au
#
# Usage goes something like:
# Start R (I use the GUI app under Windows or Mac).
# Set R's idea of the current directory to where this script is.
# At the R prompt:
# > source("PolyGraph2.R")           # Only need once
# > x <- read.tartini("Sample1.txt") # Or other file as required
# > pitch.plot(x)
#
# Based on code from Graeme Roxburgh groxburgh@infoscience.otago.ac.nz
# Please feel free to use all or part of this as you see fit.

# Convert MIDI/Tartini pitch numbers to names
as.pitchname <- function(x) {
  pitch.names <- c("C", "C#", "D", "Eb", "E", "F", "F#", "G", "G#", "A", "Bb", "B")
  octave <- as.integer(x / 12)
  note   <- x - (octave * 12)
  note.names <- pitch.names[note + 1]
  paste(note.names, as.character(octave - 1), sep="")
}

# Read in a Tartini text file
# Returns a data frame with Time, Pitch, Volume, Length
read.tartini <- function(x,
                         count.threshold  = 20.0,  # In samples
                         length.threshold = 0.08,  # In seconds
                         volume.threshold = -80.0, # In Tartini dB(?)
                         pitch.threshold  = 60) {  # In MIDI pitch numbers
  # Read in the nominated file
  raw.data <- read.table(x, header=TRUE, col.names=c("Time", "Pitch", "Volume"))

  # Calculate length and pitch class
  raw.data$Length     <- c(diff(raw.data$Time), NA)

  # Here would be the appropriate spot to adjust for different
  # temperaments or reference pitches.  We assume 12TET for now.
  raw.data$PitchClass <- round(raw.data$Pitch)
  raw.data$PitchError <- (raw.data$Pitch - as.numeric(raw.data$PitchClass)) * 100.0

  # Remove last sample, as we don't know how long this is
  raw.data <- raw.data[-length(raw.data$Pitch),]

  # Work out the length of each note, and the ones we are going to keep
  notes.number <- cumsum(c(TRUE, as.logical(diff(raw.data$PitchClass))))
  notes.length <- tapply(raw.data$Length, notes.number, sum)
  notes.in     <- names(notes.length[notes.length >= length.threshold])
  inc.len      <- notes.number %in% notes.in

  # Work out number of samples for each note, and count inclusions
  raw.freqtab <- table(raw.data$PitchClass)
  raw.count   <- as.numeric(raw.freqtab[as.character(raw.data$PitchClass)])
  inc.count   <- raw.count >= count.threshold

  # Work out volume inclusions
  inc.vol <- raw.data$Volume >= volume.threshold

  # Work out pitch inclusions
  inc.pitch <- raw.data$PitchClass >= pitch.threshold

  # Keep the ones we want, and calculate pitch error
  data            <- raw.data[(raw.data$Pitch != 0) & inc.count & inc.len & inc.vol & inc.pitch,]

  data
}

pitch.plot <- function(data) {
  vol.calc   <- tapply(data$Volume, data$PitchClass, mean)
  pitchclass <- as.numeric(names(vol.calc))
  o          <- order(pitchclass)
  volumes    <- round(vol.calc - min(vol.calc))
  volumes    <- max(volumes) - volumes
  print(volumes)
  colours    <- heat.colors(max(volumes)+1)
  boxplot(PitchError ~ PitchClass, data=data, names=as.pitchname(pitchclass[o]),
          varwidth=TRUE, outline=FALSE, col=colours[volumes[o]+1], range=0)
}

