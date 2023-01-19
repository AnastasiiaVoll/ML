# ML

### Upload the data 
# Set the path to the directory where the MIT-BIH arrhythmia database is located
path <- "/Users/macbook.12/Desktop/+ReMa+/1b -> Machine learning/archive" 

# Create an empty list to store the ecg signals data frames
ecg_signals <- list()

# Loop through all of the files in the directory
for (i in 100:234) {
  # Try reading the ecg signal data and storing it in the ecg_signals list
  tryCatch({
    ecg_signals[[i]] <- read.csv(paste0(path, "/", i, ".csv"))
  }, error = function(e) {
    # If an error occurs, print a message and move on to the next file
    print(paste0("Could not read file ", i, ".csv: ", e))
  })
}

# Create an empty list to store the annotations data frames
annotations <- list()

# Loop through all of the files in the directory
for (i in 100:234) {
  # Try reading the annotations file
  tryCatch({
    # Read in the annotations file as a character vector
    annotations_file <- readLines(paste0(path, "/", i, "annotations.txt"), skipNul = TRUE)
    
    # Remove the first line of the file (header)
    annotations_file <- annotations_file[-1]
    
    # Split the file on whitespace to separate the columns
    annotations_file <- strsplit(annotations_file, "\\s+")
    
    # Convert the data to a data frame
    annotations_df <- data.frame(matrix(unlist(annotations_file), ncol = 7, byrow = TRUE))
    
    # Assign column names to the data frame
    colnames(annotations_df) <- c("Time", "Sample", "Type", "Sub", "Chan", "Num", "Aux")
    
    # Convert the 'Time' column to numeric
    annotations_df$Time <- as.numeric(gsub(":", ".", annotations_df$Time))
    
    # Store the data frame in the annotations list
    annotations[[i]] <- annotations_df
  }, error = function(e) {
    # If an error occurs, print a message and move on to the next file
    print(paste0("Could not read file ", i, "annotations.txt: ", e))
  })
}


###Preprocessing stage 1 - frequency bandpass filter
# Load the signal package
library(signal)

# Specify the sampling rate of the data (in Hz)
sampling_rate <- 360

# Set the lower and higher cutoff frequencies
low_cutoff <- 0.4
high_cutoff <- 45

# Convert the cutoff frequencies to the range of 0 to 1
low_cutoff_normalized <- low_cutoff / (sampling_rate / 2)
high_cutoff_normalized <- high_cutoff / (sampling_rate / 2)

# Create the Butterworth filter
filter_coefficients <- butter(4, c(low_cutoff_normalized, high_cutoff_normalized), "pass")

# Loop through all of the files in the ecg_signals list
for (i in 1:length(ecg_signals)) {
  # Extract the ECG signal from the current file
  ecg_signal <- ecg_signals[[i]]$signal
  
  # Filter the ECG signal using the Butterworth filter
  filtered_ecg_signal <- filter(ecg_signal, filter_coefficients)
  
  # Replace the original signal column with the filtered signal
  ecg_signals[[i]]$signal <- filtered_ecg_signal
}
