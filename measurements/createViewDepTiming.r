## combine timing data from view-dependent benchmark to a single CSV file
##
## $Id$
##

readFpsData <- function(fileName, num=5) {
    for (i in c(0:num)) {
        if (num > 0) {
            tmpName <- paste(fileName, 
                formatC(i, width=2, flag='0'),'.txt', sep='')
        } else {
            tmpName <- paste(fileName, '.txt', sep='')
        }
            
        tmp <- read.csv(tmpName, comment.char='#', header=F)
        # drop first column
        if (i == 0)
            fps <- tmp[2]
        else
            fps <- cbind(fps, tmp[2])
    }
    if (num == 0) {
        fps <- cbind(fps, fps);
    }
    
    colnames(fps) <- c(1:num+1)
    fps
}

if (FALSE){
    ## regular view dependent benchmark
    outputFile <- 'viewdep-timings.csv'

    basePath <- 'G:/pavis14-avf/benchmark_graph/frametimes-GTX799-'

    fps.base <- readFpsData(paste(basePath, 'base-', sep=''))
    fps.ppdp <- readFpsData(paste(basePath, 'ppDP-', sep=''))
    fps.ppao <- readFpsData(paste(basePath, 'ppAO-', sep=''))
    fps.comb <- readFpsData(paste(basePath, 'both-', sep=''))

    numRows = nrow(fps.base);
    numCols = ncol(fps.base);
}

if (FALSE) {
    ## protein benchmark
    outputFile <- 'G:/pavis14-avf/Viewdep/Protein/viewdep-protein-timings.csv'

    basePath <- 'G:/pavis14-avf/Viewdep/Protein/frametimes-GTX799_PPLL_'

    fps.base <- readFpsData(paste(basePath, '64-0-0-', sep=''), 2)
    fps.ppao <- readFpsData(paste(basePath, '64-0-1-', sep=''), 2)
    fps.ppdp <- readFpsData(paste(basePath, '8-1-0-', sep=''), 2)
    fps.comb <- readFpsData(paste(basePath, '8-1-1-', sep=''), 2)

    numRows = nrow(fps.base);
    numCols = ncol(fps.base);
}

if (TRUE) {
    ## quad benchmark
    outputFile <- 'G:/pavis14-avf/Viewdep/Quads/viewdep-quads-timings.csv'

    basePath <- 'G:/pavis14-avf/Viewdep/Quads/frametimes-GTX799_PPLL_'

    fps.base <- readFpsData(paste(basePath, '64-0-0-', sep=''), 5)
    fps.ppao <- readFpsData(paste(basePath, '64-0-1-', sep=''), 5)
    fps.ppdp <- readFpsData(paste(basePath, '8-1-0-', sep=''), 5)
    fps.comb <- readFpsData(paste(basePath, '8-1-1-', sep=''), 5)

    numRows = nrow(fps.base);
    numCols = ncol(fps.base);
}


## save accumulated data in single csv file

## create single data frame first

if (numCols > 2) {
csvData <- data.frame(c(0:(numRows-1)),
                      rowMeans(fps.base[,2:numCols]), 
                      rowMeans(fps.ppao[,2:numCols]),
                      rowMeans(fps.ppdp[,2:numCols]),
                      rowMeans(fps.comb[,2:numCols]),
                      fps.base[,2:numCols],
                      fps.ppao[,2:numCols],
                      fps.ppdp[,2:numCols],
                      fps.comb[,2:numCols])
} else {
csvData <- data.frame(c(0:(numRows-1)),
                      fps.base[,2:numCols],
                      fps.ppao[,2:numCols],
                      fps.ppdp[,2:numCols],
                      fps.comb[,2:numCols],
                      fps.base[,2:numCols],
                      fps.ppao[,2:numCols],
                      fps.ppdp[,2:numCols],
                      fps.comb[,2:numCols])
}
colnames(csvData) <- c("frame",
                       paste("mean", c("Base", "PPAO", "PPDP", "Comb"), sep=''),
                       paste("rawBase", c(2:numCols), sep=''),
                       paste("rawPPAO", c(2:numCols), sep=''),
                       paste("rawPPDP", c(2:numCols), sep=''),
                       paste("rawComb", c(2:numCols), sep=''))

write.table(csvData, outputFile, append=FALSE, 
            quote = TRUE, sep =",",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = T, qmethod = c("escape", "double"),
            fileEncoding = "")



## plot the data

plot(1, xlim=c(0,numRows), ylim=c(0,500), type='n')

#lines(c(1:length(fps[,1])),fps[,1], col='blue')
for (i in c(1:numCols)) {
    lines(c(1:numRows),fps.base[,i], col='black')
}
lines(c(1:numRows), rowMeans(fps.base), col='red', lwd=2)

for (i in c(1:numCols)) {
    lines(c(1:numRows),fps.ppdp[,i], col='black')
}
lines(c(1:numRows), rowMeans(fps.ppdp), col='blue', lwd=2)
for (i in c(1:numCols)) {
    lines(c(1:numRows),fps.ppao[,i], col='black')
}
lines(c(1:numRows), rowMeans(fps.ppao), col='orange', lwd=2)
for (i in c(1:numCols)) {
    lines(c(1:numRows),fps.comb[,i], col='black')
}
lines(c(1:numRows), rowMeans(fps.comb), col='green', lwd=2)
