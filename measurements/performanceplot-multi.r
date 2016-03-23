# R functions for performance analysis
#
# $Id: performanceplot-multi.r 5015 2014-01-23 17:31:02Z marfa45 $
#


## omit scientific numbers with exponents
options("scipen" = 10)

source("plot.r");

setOutputMode(TRUE, 'tex')


## define default margins
## default
mar.default.mgp=c(2.5, 0.5, 0);
mar.default.mgpx=c(2.5, 0.6, 0);
mar.default.mar=c(2, 4, 0.5, .5);


## adjusted margins for latex
mar.latex.mgp=c(2, 0.6, 0);
mar.latex.mgpx=c(2.5, 0.3, 0);
mar.latex.mar=c(1.7, 3.2, 0.45, 0.1);


scenes <- c("protein", "space", "medical", "cfd");
techniqueNames <- c("ab_linkedlist");


techniques <- c("FFB", "DFB", "PPLL");
opts <- c("no optimization", "ppAO", "ppDP", "ppAO + ppDP");

# data in matrices, columns "FFB", "DFB", "PPLL", rows optimizations
sceneData.protein <- cbind(runif(4, 0.0, 1.0), runif(4, 0.0, 1.0), runif(4, 0.0, 1.0));
sceneData.medical <- cbind(runif(4, 0.0, 1.0), runif(4, 0.0, 1.0), runif(4, 0.0, 1.0));
sceneData.space <- cbind(runif(4, 0.0, 1.0), runif(4, 0.0, 1.0), runif(4, 0.0, 1.0));
sceneData.flow <- cbind(runif(4, 0.0, 1.0), runif(4, 0.0, 1.0), runif(4, 0.0, 1.0));


###------------------------------------------
basePath <- '../snapshots/performance'

gpuNames <- c("GTX670", "Titan")
gpuNames <- c("GTX580", "Titan")

file1 <- "GTX670.csv";
if (gpuNames[1] == "GTX580") {
    file1 <- "GTX580.csv";
}

file2 <- "GTX799.csv";

csvData <- read.csv(paste(basePath, file1, sep='/'), header=T, comment.char="#");
## convert data frame into (transposed) matrices and [fps] to [s]

# molecule data set
sceneData1.protein <- 1.0/base::t(data.matrix(csvData[1:3,1:4]))
# space data set
sceneData1.space <- 1.0/base::t(data.matrix(csvData[4:6,1:4]))
# flow data set
sceneData1.flow <- 1.0/base::t(data.matrix(csvData[7:9,1:4]))
# neuro data set
sceneData1.medical <- 1.0/base::t(data.matrix(csvData[10:12,1:4]))

###------------------------------------------
csvData <- read.csv(paste(basePath, file2, sep='/'), header=T, comment.char="#");
## convert data frame into (transposed) matrices and [fps] to [s]

# molecule data set
sceneData2.protein <- 1.0/base::t(data.matrix(csvData[1:3,1:4]))
# space data set
sceneData2.space <- 1.0/base::t(data.matrix(csvData[4:6,1:4]))
# flow data set
sceneData2.flow <- 1.0/base::t(data.matrix(csvData[7:9,1:4]))
# neuro data set
sceneData2.medical <- 1.0/base::t(data.matrix(csvData[10:12,1:4]))

###------------------------------------------

## combine GPU data with empty data lines in between
sceneData.protein <- rbind(sceneData1.protein, c(0),
                           sceneData2.protein)
sceneData.space <- rbind(sceneData1.space, c(0),
                         sceneData2.space)
sceneData.flow <- rbind(sceneData1.flow, c(0),
                        sceneData2.flow)
sceneData.medical <- rbind(sceneData1.medical, c(0),
                           sceneData2.medical)

## print max data values
print("Max. values")
print(paste("  protein:", max(sceneData.protein)))
print(paste("  space:  ", max(sceneData.space)))
print(paste("  medical:", max(sceneData.medical)))
print(paste("  flow:   ", max(sceneData.flow)))


## define heights of y axes

maxData.protein <- 0.8;
maxData.space <- .8;
maxData.medical <- 3;
maxData.flow <- 2;

if (gpuNames[1] == "GTX580") {
    ## GTX 580
    maxData.protein <- 1.5;
    maxData.space <- 1.5;
}

## older data (rev <4961)
#maxData.protein <- 1.4;
#maxData.medical <- 2.5;
#maxData.space <- 1;
#maxData.flow <- 0.8;


generateBarPlot <- function(data, maxHeight=1, datasetDesc='', ...) {
    par(mar=plotmar);
    
    #barplot(data, names.arg=,
    #    col=c(1:4,0,5:8),
    #    width=c(rep(1,4),0.5,rep(1,4)), ylim=c(0,maxHeight),
    #    beside=TRUE, ylab="", space=c(0,1.5), xaxt='n', ...);
    #mtext("Time [s]", side=2, line=2, cex=.7);
    
    barplot(data, names.arg=,
        col=c(1:4,0,5:8),
        width=c(rep(1,4),0.5,rep(1,4)), ylim=c(0,maxHeight),
        beside=TRUE, ylab="", space=c(0,1.5), xaxt='n', yaxt='n', ...);
    axis(2, padj=0.5, tcl=-0.4, cex.axis=1, line=-0.5)
    mtext("Time [s]", side=2, line=1.3, cex=0.7);
    if (datasetDesc != '') {
        mtext(datasetDesc, side=2, line=2.3, cex=.85);
    }
    axis(1, labels=FALSE, lwd.ticks=0)#, tick=FALSE, labels=FALSE)
#    abline(0,0, lwd=2);
}

addImagePlot <- function(imgFileName, height=0.5, caption="") {
    ## add the image on the righthand side
#    par(mar=mar.image.mar);
#    def.parTmp <- par(xpd=TRUE);        
#    plot.new();
    if (latexMode) {
        text(x=31, y=height, paste("\\tikz{\\node[anchor=south] at (-1,-1) {\\includegraphics[width=2cm]{", imgFileName, "}};\\node[below=5pt] at (-1,-1){", caption, "};}", sep=""), xpd=TRUE);
                
        #mtext(paste("\\tikz{\\node at (0,0) {\\includegraphics[width=1.4cm]{", imgFileName, "}};}", sep=""), side=3, line=-6.5, adj=1.17);
    }
    else
    {
    text(x=31, y=height, paste("insert image here\n<", imgFileName, ">", sep=""));
    }
#    par(def.parTmp)
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

beginWriteFile(plotWidth=7.6, plotHeight=10, writeFiles,
    fileName=paste("plot-performance", fileExt, sep="."),
    format=fileExt);

##layout(matrix(c(1:8), 4, 2, byrow=TRUE), w=c(3,0.3), h=rep.int(1, 4))
#layout(matrix(c(1:9,9), 5, 2, byrow=TRUE), w=c(3,.5), h=c(rep.int(1, 4), 0.3))

marginFactor <- 0.15
defHeight <- 2
#layout(matrix(c(1:9,9), 5, 2, byrow=TRUE), w=c(3,.5), 
#    h=c(c(maxData.protein, maxData.medical, maxData.space, maxData.flow)/defHeight + marginFactor, 0.18))

layout(matrix(c(1:6), 6, 1, byrow=TRUE), w=c(1), 
    h=c(0.18, c(maxData.protein, maxData.space, maxData.medical, maxData.flow)/defHeight + marginFactor, 0.18))

drawGPUCaption <- function(posX, posY, caption, scale=1, ...) {
#    curly(xpos=posX, ypos=posY,length=4,height=0.05/scale,theta=-pi/2,
#          N=11,roundness=0.2,col="black",lwd=1, xpd=T)

    text(posX, posY, caption, pos=1, offset=.7, xpd=T, ...)
}
printGPUTypes <- function(names, scale=1){
    gpuOffset <- 3.425
    gpuDelta <- 9.9
    gpuYOffset <- -0.01/scale
    drawGPUCaption(gpuOffset, gpuYOffset, names[1], scale);
    drawGPUCaption(gpuOffset+gpuDelta, gpuYOffset, names[1], scale);
    drawGPUCaption(gpuOffset+2*gpuDelta, gpuYOffset, names[1], scale);

    gpuOffset <- gpuOffset + 4.5
    drawGPUCaption(gpuOffset, gpuYOffset, names[2], scale);
    drawGPUCaption(gpuOffset+gpuDelta, gpuYOffset, names[2], scale);
    drawGPUCaption(gpuOffset+2*gpuDelta, gpuYOffset, names[2], scale);
}


## start plotting...

## show technique names
par(mar=mar.image.mar+c(0,0,0.3,0));
plot.new()
mtext(techniques[1], side=1, line=-1.3, adj=0.23, cex=.85);
mtext(techniques[2], side=1, line=-1.3, adj=0.53, cex=.85);
mtext(techniques[3], side=1, line=-1.3, adj=0.85, cex=.85);


generateBarPlot(sceneData.protein, maxHeight=maxData.protein, scenes[1])
printGPUTypes(gpuNames, 1/maxData.protein)
#addImagePlot("../snapshots/combined-molecule-zoom", .4, scenes[1]);

generateBarPlot(sceneData.space, maxHeight=maxData.space, scenes[2]);
printGPUTypes(gpuNames, 1/maxData.space)
#addImagePlot("../snapshots/combined-space", 0.2, scenes[3]);

generateBarPlot(sceneData.medical, maxHeight=maxData.medical, scenes[3]);
printGPUTypes(gpuNames, 1/maxData.medical)
#addImagePlot("../snapshots/combined-dti-fibers", 1, scenes[2]);

generateBarPlot(sceneData.flow, maxHeight=maxData.flow, scenes[4]);
printGPUTypes(gpuNames, 1/maxData.flow)
## file Name (for debugging)
#mtext(benchmarkFileName, side=3);
#addImagePlot("../snapshots/combined-flow", .1, scenes[4]);

par(mar=mar.image.mar+c(0,0,0.3,0));
plot.new()
## box around legend: bty='o', no box: bty='n'
legend("bottom", xpd=TRUE, legend=opts, inset=c(0,0.05), col=c(9:12), bty="n", horiz=TRUE, pch=rep(15,4), pt.cex=2.5);


endPlot();

par(def.par)  #- reset to default


