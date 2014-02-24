# R functions for view dependent data
#
# $Id$
#


## omit scientific numbers with exponents
options("scipen" = 10)

def.par <- par(no.readonly = TRUE) # save default, for resetting...

source("plot.r");

layout(1)

plotMode <- "protein"
plotMode <- "quads"
#plotMode <- ''


tallPlot <- FALSE
tallPlot <- TRUE

setOutputMode(TRUE, 'tex')

outfile <- switch(plotMode,
    'protein' = "plot-viewdep",
    'quads'   = "plot-viewdep-quad",
    ...       = "plot-viewdep-regular")

plt.width <- 7.6; ## cm
plt.height <- 6; ## cm

if (tallPlot) {
    plt.height <- 8; ## cm
    outfile <- paste(outfile, "-tall", sep='')
}

if (plotMode == 'quads') {
    plt.height <- 6; ## cm
}

timeColors <- c(colorVec[1], colorVecThesis[3], colorVec[2], colorVecThesis[4])



#colorVec <- c("#6d81ffff","#ff9f62ff","#42e58bff","#ffef6aff",
#    "#c9d0ff60","#ffccab60","#ceffe460","#fff9c660",
    ## colors used in other figures
#    "#7cedaeff", "#ede17cff", "#9cabffff", "#fec39dff"
#    );
#palette(colorVec);

palette(c(colorVec, greenish));




###------------------------------------------

if (plotMode == 'protein') {

    dataFileName <- "../snapshots/performance/viewdep-protein-dch.csv";
    fpsFileName <- "../snapshots/performance/viewdep-protein-timings.csv";
    
    imgRes <- 1024
    ## bin data over time
    timeBinSize <- 10
    
    maxYtime <- 0.25
    
} else if (plotMode == 'quads') {

    dataFileName <- "../snapshots/performance/viewdep-quads-dch.csv";
    fpsFileName <- "../snapshots/performance/viewdep-quads-timings.csv";
    
    imgRes <- 512
    ## bin data over time
    timeBinSize <- 3
    
    maxYtime <- .4
    
} else {
    dataFileName <- "../snapshots/performance/viewdep-dch.csv";
    fpsFileName <- "../snapshots/performance/viewdep-timings.csv";

    imgRes <- 1024
    ## bin data over time
    timeBinSize <- 20

    maxYtime <- 0.5
}

maxPercentage <- 60 / 100

if (tallPlot) {
    maxPercentage <- 100/100
    if (plotMode == 'protein') {
        maxYtime <- 0.4
    }
}

## read in the data

csvData <- read.csv(dataFileName, header=T, comment.char="#");
# read in time/frame [ms]
fpsData <- read.csv(fpsFileName, header=T, comment.char="#")
## convert to seconds
fpsData[,2:ncol(fpsData)] <-  fpsData[,2:ncol(fpsData)] / 1000
## convert to fps
#fpsData[,2:ncol(fpsData)] <-  1000/fpsData[,2:ncol(fpsData)]

## order the data according to the first column
csvData <- csvData[order(csvData[,1]),]
## use first column as row names and remove it
csvData <- data.frame(csvData[,-1], row.names=csvData[,1])
#rowNames(csvData) <- csvData[,1]
#csvData[,1] <- NULL

numRows <- nrow(csvData)

##  binning
## 0-4, 5-8, 9-16, 17-32, 33-64

binNames <- c("1-4", "5-8", '9-16', '17-32', '33-64', '65-');

binnedData <- data.frame(rowSums(csvData[,c(2:5)]), # first bin
    rowSums(csvData[,c(6:9)]), # second bin
    rowSums(csvData[,c(10:17)]), # ...
    rowSums(csvData[,c(18:33)]),
    rowSums(csvData[,c(34:65)]),
    rowSums(csvData[,c(65:256)]));
colnames(binnedData) <- binNames;

binNames <- c("1-4", "5-8", '9-16', '17-64');

## normalize bin data to 1
binnedData <- binnedData / (imgRes^2)

## assign bin size as column names
colnames(csvData) <- c(0:255)

time <- seq(0, numRows, 1)

## bin data of complexity
timeBinSamples <- numRows / timeBinSize
time <- c(0, rep(c(1:(timeBinSamples-1)) * timeBinSize, each=2), numRows)

#[1:(2*timeBinSamples+0)];
#binXVals <- rep(c(0:binSamples) * 30, each=2)[2:(2*binSamples+1)];
#bin1Y <- rep(bin1, each=2, length.out=2*binSamples);


if (timeBinSize > 1) {
    ## aggregate n lines together and drop first column
    binnedData <- aggregate(binnedData, list(rep(seq(0, numRows/timeBinSize - 1), each=timeBinSize)), mean)[,-1]
}

## duplicate binnedData
binnedData <- binnedData[rep(1:nrow(binnedData), each=2),]


###------------------------------------------



## define default margins
## default
mar.default.mgp=c(2.5, 0.5, 0);
mar.default.mgpx=c(2.5, 0.6, 0);
mar.default.mar=c(2.5, 4, 0.5, 6);

## adjusted margins for latex
mar.latex.mgp=c(2, 0.6, 0);
mar.latex.mgpx=c(2.5, 0.3, 0);
mar.latex.mar=c(2.2, 2.5, 0.45, 4.64);


if (latexMode)
{
    plotmgp <- mar.latex.mgp;
    plotmgpX <- mar.latex.mgpx;
    plotmar <- mar.latex.mar;
} else {
    plotmgp <- mar.default.mgp;
    plotmgpX <- mar.default.mgpx;
    plotmar <- mar.default.mar;
}


plotRange <- c(0, numRows)



beginWriteFile(plotWidth=plt.width, plotHeight=plt.height, writeFiles,
    fileName=paste(outfile, fileExt, sep="."),
    format=fileExt);


plotStuff <- function(axis.leftMax, axis.rightMax, subPlot=FALSE) {

    par(mar=plotmar);
    par(bty="n") # deleting the box
    plot(1, type="n",main=NULL, xlab="", ylab="",
        xlim=plotRange, ylim=c(0,axis.rightMax), yaxt="n", xaxt="n",
        mgp=plotmgp, las=1);

    ## draw color markers on time axis
    if (plotMode == 'protein') {
        #rect(100, -0.02, 200, -0.005, col=timeColors[1], border=NA, xpd=TRUE)
        #rect(300, -0.02, 400, -0.005, col=timeColors[2], border=NA, xpd=TRUE)
        rect(100, -0.025, 200, -0.04, col=timeColors[3], border=NA, xpd=TRUE)
        rect(300, -0.025, 400, -0.04, col=timeColors[4], border=NA, xpd=TRUE)
    }
    
    #axis(2, padj=2.5, tcl=-0.2, cex.axis=0.7)
    #mtext(side=2, "No.\\ pixels (log)", line=0.8, adj=0.5, cex=0.7)


    ## main x axis
    axis(side=1, mgp=plotmgpX, las=1, tcl=-0.4, padj=0)#, tcl=-0.2, cex.axis=0.7);
    if (!subPlot) {
        mtext(side=1, text="Time", line=1.25)#, adj=0.5, cex=0.7) #, line=1.25);
    }

    ## secondary y axis
    ticks <- axTicks(4)
    axis(side=4, at=ticks, labels=ticks*100, las=0, tcl=-0.4, padj=-1);
    if (!subPlot) {
        mtext(side=4, text="Pixel cov.\\ by complexity (\\%)", 
            line=1.7, adj=1)
    }

    ## draw Bins
    stackPlot(time, #c(0:(numRows/timeBinSize - 1))*timeBinSize, 
        rev(list(binnedData[,1], binnedData[,2], binnedData[,3], 
            binnedData[,4] + binnedData[,5])), 
        color=c(greenColorRamp(4)), border=NA) #greenColorRamp(5)

    stackPlot(time, 
        rev(list(binnedData[,1], binnedData[,2], binnedData[,3], 
            binnedData[,4] + binnedData[,5])), 
        color=rep(gray(0.6), 4), lwd=0.5, filled=FALSE)


    ## draw cut-out images first
    #w <- par("pin")[1]/diff(par("usr")[1:2])
    #h <- par("pin")[2]/diff(par("usr")[3:4])
    #asp <- w/h

    ## first cut-out on the left (fps)
    #drawCutOut(c(220,180), c(60,220))
    ## second cut-out in the middle (complexity 1)
    #drawCutOut(c(550,110), c(670,170))

    if (!subPlot) {
        par(new=TRUE)
        plot(1, type="n",main=NULL, xlab="", ylab="",
            xlim=plotRange, ylim=c(0,axis.leftMax), yaxt="n", xaxt="n",
            mgp=plotmgp, las=1);

        ## primary y axis
        axis(side=2, las=0, tcl=-0.4, hadj=0.5, padj=0.8);
        mtext("Time [s]", side=2, line=1.7)
        #mtext("Frames per second", side=2, line=3)
        ## draw timing data
        plotData <- function(x, y, range, color) {
            lines(x[range[1]:range[2]], y[range[1]:range[2]], col=color, lwd=2.5);
        }

        ## draw plot segments
        #r <- c(0,100,200,300,400,500)
        #for (i in 1:5) {
        #    plotData(c(1:numRows), fpsData[,2], c(r[i]+1, r[i+1]), timeColors[1])
        #    plotData(c(1:numRows), fpsData[,5], c(r[i]+1, r[i+1]), timeColors[2])
        #}
        
        if (plotMode == 'quads') {
            lines(c(1:numRows), fpsData[,3], col=timeColors[3], lwd=2)
            lines(c(1:numRows), fpsData[,4], col=timeColors[4], lwd=2)
        }
        lines(c(1:numRows), fpsData[,2], col=timeColors[1], lwd=2)
        lines(c(1:numRows), fpsData[,5], col=timeColors[2], lwd=2)
    }
}

plotStuff(axis.leftMax=maxYtime, axis.rightMax=maxPercentage)


## full legend (lines and classes)
#legend("topleft", xpd=TRUE, 
#    legend=c("no opt", "ppAO", "ppDP", "ppAO + ppDP", binNames[1:5]), 
#    inset=c(0,0), bty="o", horiz=FALSE, 
#    col=c(blueColorRamp(4),rev(greenColorRamp(5))), 
#    lwd=c(rep(2,4), rep(-1,5)),
#    pch=c(rep(0, 4), rep(15,5)), 
#    pt.cex=c(rep(0,4), rep(2.5, 5))) #, pch=rep(15,4), pt.cex=2.5);


if (plotMode == 'protein') {

    legendPos1 <- c(-20, 0.25)    
    legendPos2 <- c(580, -0.07)


    if (tallPlot) {
        legendPos1 <- legendPos1 + c(0, 0.13)
    }

} else if (plotMode == 'quads') {

    ## GTX 560
    legendPos1 <- c(1, 0.17)    
    legendPos2 <- c(173, -0.055)
    
    legendPos1 <- c(1, 0.34)    
    legendPos2 <- c(173, -0.11)

} else {

    legendPos1 <- c(-220, 0.45)
    legendPos2 <- c(1700, -0.015)
}

#legend(x=legendPos1[1], y=legendPos1[2], xpd=TRUE, 
#    yjust=.5, #x.intersp=0.8,
#    legend=c("no opt", "ppAO", "ppDP", "ppAO + ppDP"), 
#    inset=c(0,0), bty="n", horiz=FALSE, 
#    col=timeColors, 
#    lwd=c(rep(2.5,4)),
#    pch=c(rep(0, 4)), 
#    pt.cex=c(rep(0,4))) 

if (plotMode == 'quads') {

    legend(x=legendPos1[1], y=legendPos1[2], xpd=TRUE, 
        yjust=.5, #x.intersp=0.8,
        legend=c("base", "ppAO", "ppDP", "ppAO + ppDP"), 
        inset=c(0,0), bty="n", horiz=FALSE, 
        col=timeColors[c(1,3,4,2)], 
        lwd=c(rep(2.5,4)),
        pch=c(rep(0, 4)), 
        pt.cex=c(rep(0,4)),
        seg.len=1, x.intersp=0.4) 
} else {
    legend(x=legendPos1[1], y=legendPos1[2], xpd=TRUE, 
        yjust=.5, #x.intersp=0.8,
        legend=c("base", "ours"), 
        inset=c(0,0), bty="n", horiz=FALSE, 
        col=timeColors, 
        lwd=c(rep(2.5,2)),
        pch=c(rep(0, 2)), 
        pt.cex=c(rep(0,2)),
        seg.len=1, x.intersp=0.4) 
}
    
legend(x=legendPos2[1], y=legendPos2[2], xpd=TRUE,
    yjust=0, x.intersp=0.1,
    legend=c("bins", binNames[1:4]), 
    inset=c(0,0), bty="n", horiz=FALSE, 
    col=c(0, rev(greenColorRamp(4))),  #greenColorRamp(5)
    lwd=c(rep(-1,5)),
    pch=c(-1, rep(15,4)), 
    pt.cex=c(rep(2.5, 4)))


endPlot();

par(def.par)  #- reset to default
