pollutantmean <- function (directory, pollutant, id=1:332) {
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files.
    if (!dir.exists(directory)) {
        print ("ERROR in finding directory")
        return (FALSE)
    }
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    if (!pollutant %in% c("sulfate", "nitrate")) {
        print ("ERROR in pollutant, must be sulfate or nitrate")
        return (FALSE)
    }
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used.  Create a vector of the filenames which are 0ID.csv
    idfiles <- seq(length(id))
    j <- 1
    for (i in id) {
        filename <- paste(formatC(i, width=3, format="d", flag="0"), ".csv", sep="")
        testfile <- file.path(directory,filename)
        if (!file.exists(testfile)) {
            print (c("ERROR in finding file ", testfile))
            return (FALSE)
        }
        idfiles[j] <- testfile
        j <- j+1
    }
    
    ## Return the mean of the pollutant across all monitors
    ## listed in the 'id' vector (ignoring NA values).
    values <- numeric(0)
    for (monitorFile in idfiles) {
        monitorData <- read.csv(monitorFile)
        values <- c(values, monitorData[[pollutant]])
    }
    print (mean(values, na.rm=TRUE))
    
}