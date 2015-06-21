run_analysis <- function() {
  
  #assume working directory is UCI HAR Dataset
      #library(plyr)
      #library(dplyr)
      options(warn = -1) # remove odd warnings after upgrade
  # getting file paths for table assembly
  # test_lst <- list.files(path = "test", full.names = TRUE, recursive = TRUE, include.dirs = FALSE)
  # get test data
      test_txt <- read.table("test/subject_test.txt")
      test_X <- read.table("test/X_test.txt")
      test_Y <- read.table("test/y_test.txt")
  #make one test data table
      test_set <- cbind(test_txt, test_Y,test_X)
  #get train data
      train_txt <- read.table("train/subject_train.txt")
      train_X <- read.table("train/X_train.txt")
      train_Y <- read.table("train/y_train.txt")
  #make one train data table
      train_set <- cbind(train_txt, train_Y,train_X)
  # Combine sets
      one_set <- rbind(test_set,train_set)
  # get column names for variables
      features <- "features.txt"
      Features <- read.table(features)
      F <- as.character(Features[,2])
  # set column names in one_set
      colnames(one_set) <- c("subj_ID","activity_code", F)
 
  # The features.txt file describes duplicate column names - these must be removed for tidy
  # data remove columns with duplicate names, keep all rows
      DataSetNames <-names(one_set)
      unique_names <- unique(DataSetNames)
  # Make data frame with only unique column names     
      unique_set <- subset(one_set,TRUE,unique_names)

  # get activity_code factors
      activity <- read.table("activity_labels.txt")
      
  # make character vector of factors
      act <- as.character(activity[,2])

  # make column vector of factors     
      ACTIVITY <- cut(as.numeric(unique_set$activity_code), breaks = 6,labels = act)
   
  # Add column with activity factors
      withCODE <- cbind(ACTIVITY, unique_set)
      
  # sort df by subject ID the activity
      OrderedSet <- arrange(withCODE, subj_ID, activity_code)
      
  # create character vector with names of ALL columns to be subsetted
      measures <-grep("mean\\(|std\\(",names(OrderedSet), value = TRUE)
  
  # subset data frame to give only the columns with "mean(" OR "std("
      m <- subset(OrderedSet,TRUE,select = measures)
      
  # Get subject ID and activity info
      ID <- subset(OrderedSet,TRUE,2)
      activ <- subset(OrderedSet,TRUE,1)
  # Append subject ID and activity info to m
      one_set <- cbind(ID,activ,m)
  # Fix column names to be lower case and more descriptive   
      colN <- names(one_set)

  # No upper case variables
      lowerCol<-tolower(colN)
  # More descriptive names & correct name error
      lowerCol[1] <-"subjectid"
      newNames <- sub("^t", "timed",lowerCol,TRUE)
      newNames <- sub("^f", "frequency",newNames,TRUE)
      newNames <- sub("bodybody", "body",newNames,TRUE)
  # Remove "(" & ")" from column names
      newNames <- gsub("\\(|\\)", "",newNames,TRUE)
  # make syntactically valid names
      newNames <- make.names(newNames)
   # reset column names in one_set
      colnames(one_set) <- newNames  
  # summerize one_set
      aggdata <- aggregate(oneset, by=list(oneset$activity,oneset$subjectid), FUN = mean)
      tidydata= subset(aggdata,TRUE,-2)
      tidydata= subset(tidydata,TRUE,-3)
      tidynames = colnames(tidydata)
      tidynames[1] <- "activity"
      colnames(tidydata) <- tidynames
      tidydata
}
