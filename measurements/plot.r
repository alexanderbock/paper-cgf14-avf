# R script for plotting performance data
#
# $Id$
#

## omit scientific numbers with exponents
options("scipen" = 10)


## color defs from LaTeX
#\definecolor{niceblue}{rgb}{0.122,0.396,0.651}   %% 31, 101, 166 or #1F65A6
#\definecolor{niceorange}{RGB}{255,205,86}        %% #FFCD56
#\colorlet{nicered}{Crimson} %% rgb(220, 20, 60)

#  \colorlet{gp lt color 0}{niceblue}
#  \colorlet{gp lt color 1}{niceorange}
#  \colorlet{gp lt color 2}{nicered}
#  \definecolor{gp lt color 3}{RGB}{0,169,171}% cyan
#  \definecolor{gp lt color 4}{RGB}{130,0,128}% magenta
#  \definecolor{gp lt color 5}{RGB}{222,222,0}% yellow
#  \definecolor{gp lt color 6}{gray}{0.5}% gray
#  \definecolor{gp lt color 7}{RGB}{80,155,0}% orange
#  \definecolor{gp lt color 8}{gray}{0}% black

## custom color palette
colorVecThesis <- c(rgb(31, 101, 166, 255, maxColorValue=255),   # niceblue
    rgb(255, 205, 86, 255, maxColorValue=255), # niceorange
    rgb(220, 20, 60, 255, maxColorValue=255),  # nicered
    rgb(0,169,171, 255, maxColorValue=255),  # cyan
    rgb(130,0,128, 255, maxColorValue=255),  # magenta
    rgb(222,222,0, 255, maxColorValue=255),  # yellow
    gray(0.5),                               # gray
    rgb(80,155,0, 255, maxColorValue=255),   # orange
    gray(0) # black
    );
    
colorVec <- c("#6d81ffff","#ff9f62ff","#42e58bff","#ffef6aff",
    "#c9d0ffff","#ffccabff","#ceffe4ff","#fff9c6ff",
    ## colors used in other figures
    "#7cedaeff", "#ede17cff", "#9cabffff", "#fec39dff"
    );
    
greenish <- c("#11c35fff", "#51e793ff", "#b5f5d2ff", "#dff9ecff",
    "#ff7016ff", "#ff9f62ff", "#ffcaa8ff", "#ffebdeff",
    gray(0.41), gray(0.61), gray(0.78), gray(0.92),
    "#ffe713", "#f4e56f", "#f6eda7", "#fcf8e2", ## yellow
    "#6a7eff", "#8e9cf3", "#bcc2f1", "#e5e7f9", ## blue
    "#ea2828ff", "#ea6c6cff", "#f2b5b5ff", "#fae9e9ff"
    );
    
palette(colorVec);
palette(greenish);

## color ramp from mid green to bright green
greenColorRamp <- colorRampPalette(c("#11c35f", "#dff9ecff"))
greenColorRamp <- colorRampPalette(c("#60E097ff", "#DDFFEBff"))

greenColorRamp <- colorRampPalette(c("#ABF2CA", "#E8FFF1ff")) #"#DDFFEBff"))

blueColorRamp <- colorRampPalette(c("#5972ffff", "#A3B2FFff"))

grayRamp <- colorRampPalette(c(gray(0.7), gray(0.9)))

orangeColorRamp <- colorRampPalette(c("#FFAE7C", "#fff0eaff"))

## show the ramp colors
#plot(rep(1,10),col=greenColorRamp(10),pch=19,cex=3)

#palette("default")


## define default margins
## default
mar.default.mgp=c(2.5, 0.5, 0);
mar.default.mgpx=c(2.5, 0.6, 0);
mar.default.mar=c(3.1, 1.5, 0.5, 0.5);

## adjusted margins for latex
mar.latex.mgp=c(2, 0.6, 0);
mar.latex.mgpx=c(2.5, 0.3, 0);
mar.latex.mar=c(2.25, 2.8, 0.2, 0.2);


## define default margins
## default
mar.default.mgp=c(2.5, 0.5, 0);
mar.default.mgpx=c(2.5, 0.6, 0);
mar.default.mar=c(2.5, 4, 2.0, 0.5);

## adjusted margins for latex
mar.latex.mgp=c(2, 0.6, 0);
mar.latex.mgpx=c(2.5, 0.3, 0);
mar.latex.mar=c(.4, 3, 1.2, 0.2);

## margin for images
mar.image.mar = c(0, 0, 0, 0);


writeFiles <- FALSE;
#writeFiles <- TRUE;

fileExt <- "png";
#fileExt <- "tex";

latexMode <- FALSE

## enable/disable file output
## output device is chosen depending on the file extension
## supported extensions: 'tex', 'png', 'jpg'
setOutputMode <- function(fileoutput=TRUE, ext='tex') {
    writeFiles <<- fileoutput;
    fileExt <<- ext;

    latexMode <<- ((fileExt == "tex") && writeFiles);
    
    if (latexMode) {
        library(tikzDevice);
    }

    options(tikzLatexPackages=c(
        "\\usepackage{fixltx2e}",
        "\\usepackage[T1]{fontenc}",
        "\\usepackage{tikz}",
        "\\usepackage{relsize}",
        "\\usepackage{graphicx}",
        "\\usepackage[active,tightpage]{preview}",
        "\\PreviewEnvironment{pgfpicture}",
        "\\setlength\\PreviewBorder{0pt}",
        "\\tikzset{font=\\sffamily\\footnotesize}",  # \\normalsize / \\small / \\footnotesize
        ""
        ))
    #options ( tikzLatexPackages = c(
    #getOption ( " tikzLatexPackages " ),
    #" \\ usepackage { mathpazo }"
    #))
    
}

#setOutputMode(TRUE, 'png');
#setOutputMode(TRUE, 'tex');


plt.marginFactor <- 0.166044 * 2.54; # 1 line corresponds to 0.166044 inches
plt.width <- 7.2; # cm
plt.height <- 6; ## 6cm




myPlotwriteToFile <- FALSE;

beginWriteFile <- function(plotWidth=8, ## in cm
    plotHeight=6, ## in cm
    fileOutput=FALSE, fileName="", format="tex",
    asp=NA)
{
    #print("begin plotting...");

    myPlotwriteToFile <<- FALSE;
    if (fileOutput && (fileName != ""))
    {
        myPlotwriteToFile <<- TRUE;
        pltheight <- ifelse(is.null(plotHeight),
            ifelse(is.null(asp), plotWidth, plotWidth*asp), plotHeight);
        print(paste("plotting to file: ", fileName, sep=""));
        switch (format, 
            ## export plot to Tikz, width/height in inch
            tex = tikz(fileName, width=plotWidth/cm(1), height=pltheight/cm(1),
                pointsize=12, standAlone=TRUE),
            ## export to PNG
            png = png(fileName, units="cm", res=72, width=plotWidth*3, height=pltheight*3),
            jpg = jpg(fileName, units="cm", res=72, width=plotWidth*3, height=pltheight*3))
    }
}


beginPlot <- function(yRange=NULL, xRange=NULL, xLabels=NULL, 
    xlab="", ylab="", title=NULL,
    logScale="", customAxis=TRUE,
    plotWidth=8, ## in cm
    plotHeight=6, ## in cm
    fileOutput=FALSE, fileName="", format="tex",
    plotmgp=c(2.5, 0.5, 0), plotmgpX=NULL, plotmar=c(3.1, 3.6, 0.5, 0.5),
    ...)
{
    if (fileOutput)
    {
        beginWriteFile(plotWidth, plotHeight, fileOutput, fileName, format, ...);
    }
    
    #oldpar <- par(oma=c(0,0,0,0))
    #if (is.null(title))
    {
        myPlotoldpar <<- par(mar=plotmar) ## default: 5.1 4.1 4.1 2.1
    }

    if (!is.null(xLabels) && customAxis)
    {
        xRange <- range(xLabels);
    }

    plot(1, type="n",main=NULL, xlab="", ylab=ylab,
        xlim=xRange, ylim=yRange, log=logScale, xaxt="n",
        mgp=plotmgp, las=1, ...);
    # add custom title
    if (!is.null(title))
    {
        title(main=title, line=-1);
    }
    
    if (customAxis)
    {
        if (is.null(xLabels))
        {
            axis(side=1,mgp=ifelse(rep(is.null(plotmgpX), 3), plotmgp, plotmgpX), las=1);
        }
        else
        {
            axis(side=1,mgp=ifelse(rep(is.null(plotmgpX), 3), plotmgp, plotmgpX),
                at=xLabels, labels=as.character(xLabels), las=1);
        }
        ## for output with Tikz:
        #axis(side=1,at=xLabels,labels=paste("\\num{", as.character(xLabels), "}", sep=""), las=1);
        mtext(side = 1, text = xlab, line = 1.25)
    }
    
}


endPlot <- function()
{
    #print("end plotting...");
    if (myPlotwriteToFile)    
    {
        dev.off();
    }
        
    if (exists("myPlotoldpar"))
    {
        par(myPlotoldpar);
    }
}


## generate a stacked plot, where the area inbetween is filled
##   xValues   vector of x values
##   yValList  list of vectors of y values, i.e. list(c(1, 2, 3), c(2, 1, 0.1))
##   norm      factor used for normalization
##   colors
stackPlot <- function(xValues, yValuesList, norm=1, 
    color=NULL, filled=TRUE, ...)
{
    if (is.null(color))
    {
        col <- c(1:length(xValues));
    } else {
        col <- color;
    }

    xrange <- c(xValues,rev(xValues));

    sum <- rep(0, length(xValues));
    if (filled)
    {
        for (i in 1:length(yValuesList))
        {
            tmp <- sum + yValuesList[[i]] * norm;
            polygon(xrange, c(sum,rev(tmp)),
                col=col[i], ...); #border=NA, 
            sum <- tmp;
        }
    } else {
        for (i in 1:length(yValuesList))
        {
            tmp <- sum + yValuesList[[i]] * norm;
            lines(xValues, tmp, col=col[i], ...);
            sum <- tmp;
        }
    }
    
    remove(sum, tmp, col, xrange);
}


## plot thread data with complete, mono, and bimo reactions 
##
plotCompMonoBimo <- function(dataX, dataY, yRange, xRange, xLabels,
    xlab="", ylab="", title=NULL,
    logScale="", legendPos="left",
    fileOutput=FALSE, fileName="", format="tex",
    plotmgp=c(2.5, 0.5, 0), plotmar=c(3.1, 3.6, 0.5, 0.5), ...) 
{
    latexModeP <- ((format == "tex") && fileOutput);


    beginPlot(yRange, xLabels, "Threads", yaxislabel, title, logScale, fileOutput, fileName, format, plotmgp=plotmgp, plotmar=plotmar, ...);

    numEntries <- length(dataX) / 3;
    
    points(dataX[1:numEntries*3-2], dataY[1:numEntries*3-2], col=1, pch=22);
    points(dataX[1:numEntries*3-1], dataY[1:numEntries*3-1], col=2, pch=22);
    points(dataX[1:numEntries*3], dataY[1:numEntries*3], col=3, pch=22);

    # add a legend    
    legend(legendPos,legend=passname,col=c(1:3),pch=c(22),bty="n");
    #,lw=1,pch=c(22),fill=c(1:3),);
    #border=c(1:3),fill="white"\sffamily\footnotesize

    endPlot();
}

# Function to create curly braces
# x, y position where to put the braces
# range is the width
# position: 1 vertical, 2 horizontal
# direction: 1 left/down, 2 right/up
CurlyBraces <- function(x, y, range, pos = 1, direction = 1 ) {
    a=c(0,1,1.5,49,50)    # set flexion point for spline
    b=c(0,.2,.28,.7,.8)*10 # set depth for spline flexion point
    curve = spline(a, b, n = 100, method = "natural")$y / 2 
    curve = c(curve,rev(curve))
    a_sequence = rep(x,200)
    b_sequence = seq(y-range/2,y+range/2,length=200)  
    # direction
    if(direction==1)
        a_sequence = a_sequence+curve
    if(direction==2)
        a_sequence = a_sequence-curve
    # pos
    if(pos==1)
        lines(a_sequence,b_sequence) # vertical
    if(pos==2)
        lines(b_sequence,a_sequence) # horizontal
}

curly <- function(xpos = 0.5, ypos = 0.5, 
                  length = 2, height = 0.1, theta = 0, 
                  roundness = 1, N = 100,
                  col = 1, lwd = 1, ...){
    ## draw a curly brace (vertical with given length and height, 
    ## centered at pos)
    ##  xpos,ypos central position
    ##  length    total length of brace
    ##  height    height of brace
    ##  theta     brace orientation (radians), default=0 -> vertical
    ##  roundness 
    ##  N   smoothness, higher = smoother
    ## col and lwd are passed to points/grid.lines

    N <- max(N, 3)

    ## clamp roundness to fit within total length
    roundness <- min(roundness, 0.25 * length)
    
    ymin <- height
    y2 <- length/2.0 - 2.0*roundness
    i <- seq(0, pi/2, length.out = N)

    x <- c(ymin *  (sin(i)-1)*0.5,
        seq(0,0, length.out = 2),
        ymin * ((1 - sin(rev(i))))*0.5,
        ymin * ((1 - sin(i)))*0.5,
        seq(0,0, length.out = 2),
        ymin * (sin(rev(i)) - 1)*0.5)
        
    y <- c(-cos(i) * roundness,
        c(0,y2),
        y2 + (cos(rev(i))) * roundness,
        y2 + (2 - cos(i)) * roundness,
        c(y2 + 2*roundness, 2 * y2 + 2 * roundness),
        2 * y2 + 2 * roundness + cos(rev(i)) * roundness)
        
    x <- x + xpos
    y <- y + ypos - length/2.0 + roundness
        x1 <- cos(theta) * (x - xpos) - sin(theta) * (y - ypos) + xpos
    y1 <- cos(theta) * (y - ypos) + sin(theta) * (x - xpos) + ypos

    points(x1,y1,type='l',col=col,lwd=lwd,...)
}


## create a histogram plot
##   histogramData  histogram given as vector
##   binRange       show bins between binRange[1] and binRange[2]        
##   tickInterval   show ticks with given interval, if negative ticks are no shown
plotHistogram <- function(histogramData, binRange=c(1,256), tickInterval=8, ...) {

    ## prepare data (mark zero values as NA because of log plot)
    hData <- sapply(histogramData[binRange[1]:min(binRange[2],length(histogramData))], function(x) if (x > 0) return (x) else return(NA))

    mids <- barplot(hData, log="y", ylab="", xlab="", names.arg='', space=c(0,1), axes=FALSE, ...)
    axis(2, padj=2.5, tcl=-0.2, cex.axis=0.7)
    mtext(side=2, "No.\\ pixels (log)", line=0.8, adj=0.5, cex=0.7)
    
    if (tickInterval > 0) {
        tickLocation <- mids[c(1,seq(tickInterval, length(mids), tickInterval))]
        axis(1, at=tickLocation, labels=c(1,seq(1, length(tickLocation)-1, 1)*tickInterval), padj=-3, tcl=-0.2, cex.axis=0.7)
        remove(tickLocation)
    } else {
        axis(1, labels=FALSE, lwd.ticks=0)
    }
    remove(hData, mids)
}


## draw a cut-out 
## one frame at dataCenter, the second at imgCenter
## both rectangles are connected by lines
drawCutOut <- function(dataCenter, imgCenter) {
    boxWidth <- 80
    imgBoxWidth <- 180
    
    boxX <- c(dataCenter[1], dataCenter[1] + boxWidth) - boxWidth/2
    boxY <- c(dataCenter[2], dataCenter[2]+asp*boxWidth) - boxWidth * asp/2
    rect(boxX[1], boxY[1], boxX[2], boxY[2], border="black");

    imgBoxX <- c(imgCenter[1], imgCenter[1] + imgBoxWidth) - imgBoxWidth/2
    imgBoxY <- c(imgCenter[2], imgCenter[2] + asp*imgBoxWidth) - imgBoxWidth * asp/2
    rect(imgBoxX[1], imgBoxY[1], imgBoxX[2], imgBoxY[2], border="black");

    if (((dataCenter[1] < imgCenter[1]) && dataCenter[2] < imgCenter[2])
        || ((dataCenter[1] > imgCenter[1]) && dataCenter[2] > imgCenter[2])) 
    {
        segments(boxX[1], boxY[2], imgBoxX[1], imgBoxY[2], col='black');
        segments(boxX[2], boxY[1], imgBoxX[2], imgBoxY[1], col='black');
    } else {
        segments(boxX[1], boxY[1], imgBoxX[1], imgBoxY[1], col='black');
        segments(boxX[2], boxY[2], imgBoxX[2], imgBoxY[2], col='black');
    }
}
