complete <- function (directory, id=1:332) {
    
    ## 'directory' is a character vector of length 1 indicating the lcoation
    ## of the CSV files.
    if (!dir.exists(directory)) {
        print ("ERROR in finding directory")
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
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## Where 'id' is the monitor ID number and 'nobs' is the number
    ## of complete cases
    
    for (monitorFile in idfiles) {
        monitorData <- read.csv(monitorFile)
        print (monitorData)
    }
    
}