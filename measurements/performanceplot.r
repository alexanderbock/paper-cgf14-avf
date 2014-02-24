# R functions for performance analysis
#
# $Id$
#


## omit scientific numbers with exponents
options("scipen" = 10)

source("plot.r");

scenes <- c("Molecule", "Space", "Flow", "Neuro");
techniqueNames <- c("ab_linkedlist");


techniques <- c("FFB", "DFB", "PPLL");
opts <- c("no optimization", "ppAO", "ppDP", "ppAO + ppDP");


generateData <- function() {
    return (data.frame(ffb=runif(4, 0.0, 1.0), dfb=runif(4, 0.0, 1.0), 
        ppll=runif(4, 0.0, 1.0), row.names=opts))
}

# data in data frames
sceneData.protein <- generateData();
sceneData.medical <- generateData();
sceneData.space <- generateData();
sceneData.flow <- generateData();

# data in matrices, columns "FFB", "DFB", "PPLL", rows optimizations
sceneData.protein <- cbind(runif(4, 0.0, 1.0), runif(4, 0.0, 1.0), runif(4, 0.0, 1.0));
sceneData.medical <- cbind(runif(4, 0.0, 1.0), runif(4, 0.0, 1.0), runif(4, 0.0, 1.0));
sceneData.space <- cbind(runif(4, 0.0, 1.0), runif(4, 0.0, 1.0), runif(4, 0.0, 1.0));
sceneData.flow <- cbind(runif(4, 0.0, 1.0), runif(4, 0.0, 1.0), runif(4, 0.0, 1.0));


benchmarkFileName <- "GTX560_2014-01-07_13-38-35.csv";
benchmarkFileName <- "GTX799_2014-01-07_16-07-19.csv";

csvData <- read.csv(benchmarkFileName, header=T, comment.char="#");
## convert data frame into (transposed) matrices and [fps] to [s]

# molecule data set
sceneData.protein <- 1/base::t(data.matrix(csvData[1:3,1:4]))
# space data set
sceneData.space <- 1/base::t(data.matrix(csvData[4:6,1:4]))
# flow data set
sceneData.flow <- 1/base::t(data.matrix(csvData[7:9,1:4]))
# neuro data set 
sceneData.medical <- 1/base::t(data.matrix(csvData[10:12,1:4]))


generateBarPlot <- function(data, maxHeight=1, ...) {
    par(mar=plotmar);
    ## names.arg=rep(opts, 3)    
    barplot(data, names.arg=, col=c(1:4), ylim=c(0,maxHeight), beside=TRUE, ylab="Time [s]", space=c(0,1.5), xaxt='n', ...);
    abline(0,0);
}

addImagePlot <- function(imgFileName) {
    ## add the image on the righthand side
    par(mar=mar.image.mar);
    plot.new();
    if (latexMode) {
        text(x=0.5, y=0.15, paste("\\tikz{\\node at (-1,-1) {\\includegraphics[width=2cm]{", imgFileName, "}};}", sep=""));
    }
    else
    {
    text(x=0.5, y=0.5, paste("insert image here\n<", imgFileName, ">", sep=""));
    }
}


def.par <- par(no.readonly = TRUE) # save default, for resetting...



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

beginWriteFile(plotWidth=8, plotHeight=12, writeFiles, 
    fileName=paste("plot-GTX560", fileExt, sep="."), 
    format=fileExt);

layout(matrix(c(1:8), 4, 2, byrow=TRUE), w=c(3,1), h=rep.int(1, 4))
##layout(matrix(c(1:9,9), 5, 2, byrow=TRUE), w=c(3,1), h=c(rep.int(1, 4), 0.2)

# with matrix data
generateBarPlot(sceneData.protein, maxHeight=1.4);
mtext(techniques[1], side=3, adj=0.15);
mtext(techniques[2], side=3);
mtext(techniques[3], side=3, adj=0.85);

plotmar <- plotmar + c(0, 0, -1, 0);

addImagePlot("../snapshots/combined-molecule-zoom");

generateBarPlot(sceneData.medical, maxHeight=2.5);
addImagePlot("../snapshots/combined-dti-fibers");

generateBarPlot(sceneData.space);
addImagePlot("../snapshots/combined-space");

generateBarPlot(sceneData.flow, maxHeight=0.8);
## file Name (for debugging)
mtext(benchmarkFileName, side=3);
plotmar <- plotmar + c(3, 0, 0, 0);
legend("bottom", xpd=TRUE, legend=opts, inset=c(0,-0.3), col=c(1:4), bty="o", horiz=TRUE, fill=c(1:4));
addImagePlot("../snapshots/combined-flow");

endPlot();

par(def.par)  #- reset to default



if (FALSE) {

#techniqueLookup <- function(type) {
#    switch(type,
#        ab_linkedlist = 1;
#}

    ## read data file
    fileName <- "GTX880_2013-12-20_15-47-54.txt"

    fHandle <- file(fileName, "r");
    lineData <- readLines(fHandle);
    close(fHandle);

    ## read data set description
    description <- lineData[1];

    for (i in c(2:length(lineData))) {
        ## skip empty lines
        if (lineData[i] != "")
        {
            tmp <- unlist(strsplit(lineData[i], " "));
            if (length(tmp) == 1) {
                ## read data set name
                dataSetName <- tmp[1];
                dataSet <- matrix(c(0), nrow=4, ncol=3, dimnames=list(c("no", "ppAO", "ppDP", "combined"), c("ffb", "ll", "dfb")))
            }
            else if ((length(tmp) > 1) && (tmp[2] == ':')) {
                ## performance result
                tech <- tmp[1];
                ppDPflag <- 2*type.convert(tmp[4]);
                ppAOflag <- type.convert(tmp[6]);
                framerate <- tmp[12];
                
                ## fill row in data set
                dataSet[ppDPflag+ppAOflag, 2] <- 1.0/framerate;
            }
        }
    }
}

