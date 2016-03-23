# R script for plotting all depth complexity histograms
#
# $Id: plothistograms.r 5007 2014-01-21 15:02:06Z marfa45 $
#


## omit scientific numbers with exponents
options("scipen" = 10)

source("plot.r");

setOutputMode(TRUE, 'tex')

def.par <- par(no.readonly = TRUE) # save default, for resetting...

basePath <- '../snapshots/dch'
outPath <- '../figures/plot-dch-'

hData.Mol <- read.csv(paste(basePath, 'dch-mol.csv', sep='/'), header=T, comment.char="#");

hData.Neuro <- read.csv(paste(basePath, 'dch-neuro.csv', sep='/'), header=T, comment.char="#");

hData.Space <- read.csv(paste(basePath, 'dch-space.csv', sep='/'), header=T, comment.char="#");

hData.Flow <- read.csv(paste(basePath, 'dch-flow.csv', sep='/'), header=T, comment.char="#");


## define default margins
## default
mar.default.mgp=c(2.5, 0.5, 0);
mar.default.mgpx=c(2.5, 0.6, 0);
mar.default.mar=c(2, 4, 0.5, .5);


## adjusted margins for latex
mar.latex.mgp=c(2, 0.6, 0);
mar.latex.mgpx=c(2.5, 0.3, 0);
mar.latex.mar=c(1, 1.4, 0.1, 0.25);

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


par(mar=plotmar)

color <- colorVec[1]

pltWidth <- 4.0 # 7.6
pltHeight <- 4.0 # 5.7


## molecule
beginWriteFile(plotWidth=pltWidth, plotHeight=pltHeight, writeFiles, 
    fileName=paste(outPath, "mol.", fileExt, sep=""), format=fileExt);
myPlotoldpar <- par(mar=plotmar)
plotHistogram(hData.Mol, binRange=c(1,48), col=color, border=NA)
endPlot();

## enable interactive mode
#par(ask=TRUE)


## neuro
beginWriteFile(plotWidth=pltWidth, plotHeight=pltHeight, writeFiles, 
    fileName=paste(outPath, "neuro.", fileExt, sep=""), format=fileExt);
myPlotoldpar <- par(mar=plotmar)
plotHistogram(hData.Neuro, tickInterval=32, col=color, border=NA)
endPlot();

## space
beginWriteFile(plotWidth=pltWidth, plotHeight=pltHeight, writeFiles, 
    fileName=paste(outPath, "space.", fileExt, sep=""), format=fileExt);
myPlotoldpar <- par(mar=plotmar)
plotHistogram(hData.Space, binRange=c(1,32), tickInterval=4, col=color, border=NA)
endPlot();

## flow
beginWriteFile(plotWidth=pltWidth, plotHeight=pltHeight, writeFiles, 
    fileName=paste(outPath, "flow.", fileExt, sep=""), format=fileExt);
myPlotoldpar <- par(mar=plotmar)
plotHistogram(hData.Flow, binRange=c(1,96), col=color, border=NA)
endPlot();

par(def.par)  #- reset to default

