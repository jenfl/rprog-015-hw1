corr <- function (directory, threshold = 0) {
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    if (!dir.exists(directory)) {
        print ("ERROR in finding directory")
        return (FALSE)
    }
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    idfiles <- list.files(path=directory)
    correlations <- numeric(0)
    
    for (thisfile in idfiles) {
        monitorData <- read.csv(file.path(directory, thisfile))
        if (sum(complete.cases(monitorData)) > threshold) {
            thiscor <- cor(monitorData[,"sulfate"], monitorData[,"nitrate"], use="complete.obs")
            correlations <- c(correlations, thiscor)
        }
    }
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    correlations
}