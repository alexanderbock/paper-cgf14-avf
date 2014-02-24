## read in png images and compute a grayscale histogram
## save result in CSV file
##
## $Id$
##


library(png)


computeDCH <- function(imgFileName, outputFile) {
    csvData <- data.frame(matrix(vector(),0,257))
    
    print(paste("Computing DCH for:    ", imgFileName))
    flush.console()
    data <- readPNG(imgFileName);
    ## compute histogram from first channel
    h <- hist(data[,,1]*256, breaks=0:256, right=T, plot=F)
    csvData <- rbind(csvData,  h$counts)
    ## set column/bin names
    colnames(csvData) <- c(0:(ncol(csvData)-1))
    
    ## save data as csv file
    print(paste("  writing CSV data to:", outputFile))
    flush.console()
    write.table(csvData, file=outputFile, append=FALSE, 
                quote = TRUE, sep =",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = T, qmethod = c("escape", "double"),
                fileEncoding = "")
    remove(csvData, data)
}


batchComputeDCH <- function (path='', imgPath='', imgPrefix='',
                             outputFile='viewdep-dch.csv',
                             numFiles=1500) {

    library("parallel")
    library("foreach")
    library("doParallel")

    csvData <- data.frame(matrix(vector(),0,257))

    cl <- makeCluster(detectCores() - 1)
    print(paste("cores detected:", detectCores()))
    flush.console()
    registerDoParallel(cl, cores = detectCores() - 1)

    csvData = foreach(i = seq(0,numFiles-1), .packages = c("ncdf","chron","stats","png"),
                   .combine = rbind) %dopar% {
        try({
            # your operations; line 1...
            # your operations; line 2...
            file <- paste(imgPrefix, formatC(i, width=4, format="d", flag="0"), ".png", sep="")
            
            fileName <- paste(path, imgPath, file, sep="/");
            data <- readPNG(fileName);

            ## compute histogram from first channel
            h <- hist(data[,,1]*256, breaks=0:256, right=T, plot=F)
            # your output
            c(i, h$counts)
        })
    }

    stopCluster(cl)


    if (0) {
    ## single threaded version with progress bar
    pb <- txtProgressBar(min=0, max=numFiles-1, style=3)
    for (i in seq(0,numFiles-1)) {
        ## read in files
        
        file <- paste(imgPrefix, formatC(i, width=4, format="d", flag="0"), ".png", sep="")
        
        fileName <- paste(path, imgPath, file, sep="/");
        data <- readPNG(fileName);

        ## compute histogram from first channel
        h <- hist(data[,,1]*256, breaks=0:256, right=T, plot=F)
        csvData <- rbind(csvData, c(i, h$counts))
        
        # update progress bar
        setTxtProgressBar(pb, i)
    }
    close(pb)
    }
    
    colnames(csvData) <- c("frame", c(0:255))

    ## save data as csv file
    write.table(csvData, file=paste(path,outputFile, sep="/"), append=FALSE, 
                quote = TRUE, sep =",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = T, qmethod = c("escape", "double"),
                fileEncoding = "")

    remove(csvData)
}

# batchComputeDCH(path='e:/dci', imgPath='', imgPrefix='', outputFile='viewdep-dch.csv', numFiles=1500)

# batchComputeDCH(path='G:/pavis14-avf/benchmark_graph', imgPath='dci', imgPrefix='', outputFile='viewdep-dch.csv', numFiles=1500)

# batchComputeDCH(path='G:/pavis14-avf/Viewdep/Protein',imgPath='dci', imgPrefix='', outputFile='viewdep-protein-dch.csv', numFiles=500)

# batchComputeDCH(path='G:/pavis14-avf/Viewdep/Quads',imgPath='dci', imgPrefix='', outputFile='viewdep-quads-dch.csv', numFiles=150)

if (TRUE) {
    computeDCH('../snapshots/mol/cgf/dci.png', '../snapshots/dch/dch-mol.csv')
    computeDCH('../snapshots/dti/cgf/dci_Neuro.png', '../snapshots/dch/dch-neuro.csv')
    computeDCH('../snapshots/space/space_dci.png', '../snapshots/dch/dch-space.csv')
    computeDCH('../snapshots/flow/cgf/dci_Flow.png', '../snapshots/dch/dch-flow.csv')
}
