require(dplyr)
require(readr)
require(gdata)

# The read.dataset function reads and assembles the data subset defined by the
# name argument (which corresponds to the data subset folder and file names) and
# returns the loaded data.
#   Arguments:  - name: character variable defining the name of the data subset
#               - activity.labels: character vector defining the names of the
#                   activities
#               - features: character vector defining the names of the columns
#   Output: dplyr data table containing the data subset properly annotated
#               with the following columns:
#               - subject: test subject number
#               - subset: subset name (= name argument)
#               - activity: associated activity as defined by the
#                    activity.labels variable
#               - parameter variables
read.dataset <- function(name, activity.labels, features) {
    
    data <- read_fwf(paste0("raw/", name, "/X_", name, ".txt"),
                     col_positions = fwf_widths(rep(16, 561), col_names = features))
    subject <- read.csv(paste0("raw/", name, "/subject_", name, ".txt"),
                        as.is = T, header = F, col.names = "Subject")
    subset <- sub(pattern = "^(.)", replacement = "\\U\\1", tolower(name), perl = T)
    subject$Subset <- rep(subset, length(subject$Subject))
    activity <- tbl_df(read.csv(paste0("raw/", name, "/y_", name, ".txt"), 
                                header = F, col.names = c("ID")))
    activity <- select(left_join(activity, activity.labels), Activity)

    data <- tbl_df(cbind(cbind(subject, activity), data))
    data
}

# Setting the working directory to the dataset path (unquote and adapt as needed)
#setwd("Coursera\\GACDw4\\")

# Loading and cleaning / capitalizing activity labels
activity.labels <- read.csv("raw/activity_labels.txt", sep = " ", as.is = T,
                            header = F, col.names = c("ID", "Activity"))
activity.labels$Activity <- gsub("_", " ", activity.labels$Activity) %>%
    tolower() %>%
    sub(pattern = "^(.)", replacement = "\\U\\1", perl = T) %>%
    factor()

# Loading and cleaning / capitalizing feature names
features <- read.csv("raw/features.txt", sep = " ", as.is = T, header = F)
features <- features$V2 %>%
    sub(pattern = "\\bt", replacement = "Time") %>%
    sub(pattern = "\\bf", replacement = "Frequency") %>%
    sub(pattern = "\\b([a-z])", replacement = "\\U\\1", perl = T) %>%
    sub(pattern = "[,-]([a-z])", replacement = "\\U\\1", perl = T) %>%
    sub(pattern = "Gyro", replacement = "Gyroscope") %>%
    sub(pattern = "BodyBody", replacement = "Body") %>%
    sub(pattern = "Mag", replacement = "Magnitude") %>%
    sub(pattern = "Mad", replacement = "MeanAbsoluteDeviation") %>%
    sub(pattern = "Acc", replacement = "Acceleration") %>%
    sub(pattern = "Std", replacement = "StandardDeviation") %>%
    sub(pattern = "Max", replacement = "Maximum") %>%
    sub(pattern = "Min", replacement = "Minimum") %>%
    sub(pattern = "MeanFreq", replacement = "MeanFrequency") %>%
    sub(pattern = "Sma", replacement = "SignalMagnitudeArea") %>%
    sub(pattern = "MaxInds", replacement = "MaximalIndex") %>%
    sub(pattern = "Iqr", replacement = "InterquartileRange") %>%
    sub(pattern = "\\(\\)", replacement = "") %>%
    sub(pattern = "([[:digit:]]+),([[:digit:]]+)", replacement = "\\1To\\2") %>%
    gsub(pattern = "[(),-]", replacement = "")

# Loading, naming and assembling the train dataset with the read.dataset function
train <- read.dataset(name = "train", activity.labels = activity.labels, features = features)
# Loading, naming and assembling the test dataset with the read.dataset function
test <- read.dataset(name = "test", activity.labels = activity.labels, features = features)

# Fusing test and train datasets and conversion to dplyr data table
d <- tbl_df(rbind(train, test))
# Converting the Subject and Subset (train/test) columns to factors
d$Subject <- factor(d$Subject)
d$Subset <- factor(d$Subset)

# Selecting only means and standard deviations in addition to factor variables
d <- d[c(names(d)[1:3], names(d)[grepl("(Mean|StandardDeviation)[X-Z]?$", names(d))])]

write.csv(d, file = "HumanActivityRecognition.csv")

# Calculating means of each variable by Subject, Subset and Activity
#   grouping by Subset does not break the data in more groups since subjects
#   do not belong to more than 1 subset.
means <- group_by(d, Subject, Subset, Activity) %>%
    summarise_all(mean)

write.csv(means, file = "HumanActivityRecognitionSummary.csv")

# Assembling the code book
# Starting with the header
cb <- c("#Code book",
            "Please find below the names, types and descriptions of all variables used in the 'HumanActivityRecognition.csv' and 'HumanActivityRecognitionSummary.csv' files",
            "##Variables")
# Constructing the table of name, type and description for each variable
v <- data.frame(names(d), stringsAsFactors = F)
names(v) <- c("Name")
v$Type <- sapply(d, class) %>%
    sub(pattern = "^(.)", replacement = "\\U\\1", perl = T)
v$Description <- sub(pattern = "([X-Z])$", replacement = " in the \\1 axis", v$Name) %>%
    sub(pattern = "^([^[:blank:]]+)(Mean|StandardDeviation)(.*)$", replacement = "\\2 of \\1\\3") %>%
    sub(pattern = "^StandardDeviation", replacement = "Standard Deviation") %>%
    sub(pattern = "Angle(.+)Gravity", replacement = "angle between \\1 and gravity") %>%
    sub(pattern = " Time", replacement = " time-wise ") %>%
    sub(pattern = " Frequency", replacement = " the fast Fourier transform of the ") %>%
    sub(pattern = "( [A-Z][a-z]+)([A-Z])", replacement = "\\L\\1 \\2", perl = T) %>%
    sub(pattern = "( [a-z]+)([A-Z])", replacement = "\\L\\1 \\2", perl = T) %>%
    sub(pattern = "( [a-z]+)([A-Z])", replacement = "\\L\\1 \\2", perl = T)
v[1, "Description"] <- "Subject number"
v[2, "Description"] <- paste0("Raw data subset (", paste(levels(d$Subset), collapse = "/"), ")")
v[3, "Description"] <- paste0("Physical activity (", paste(levels(d$Activity), collapse = "/"), ")")

# Adding the variable description as table to the code book
cb <- c(cb, paste(names(v), collapse = " | "),
        "--- | --- | --- ",
        paste(v$Name, v$Type, v$Description, sep = " | "))

# Writing the code book to CodeBook.md
write(cb, file = "CodeBook.md")
