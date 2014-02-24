## generate synthetic data for R scripts
## 
## $Id$
##

if (FALSE) {
    ## generate randomized data which is normal distributed

    numSamples <- 1200
    time <- seq(0, numSamples, 1)

    ## fps data
    fpsData <- rnorm(numSamples+1, 70, 10) ## runif(numSamples, 35, 80);
    fpsData <- fpsData * dnorm(time, 600.0, 600) * 1500

    ## bin data of complexity
    binSamples <- numSamples / 30

    bin1 <- as.integer(runif(binSamples + 1, 10, 50) * dnorm(seq(0, binSamples, 1), 40.0, 30) * 250)

    bin2 <- as.integer(runif(binSamples+1, 10, 30) * dnorm(seq(0, binSamples, 1), 50.0, 30) * 200)

    bin3 <- as.integer(max(runif(binSamples+1, 0, 40), 0) * dnorm(seq(0, binSamples, 1), 20.0, 30) * 200)

    #stepBin1 <- stepfun(c(0:39)*30, bin1)
    #stepBin2 <- stepfun(c(0:39)*30, bin2)
    #stepBin3 <- stepfun(c(0:39)*30, bin3)

    binXVals <- rep(c(0:binSamples) * 30, each=2)[2:(2*binSamples+1)];
    bin1Y <- rep(bin1, each=2, length.out=2*binSamples);
    bin2Y <- rep(bin2, each=2, length.out=2*binSamples);
    bin3Y <- rep(bin3, each=2, length.out=2*binSamples);

    ## smoothing of fps data
    lo <- loess(fpsData~time, span=0.05)
    smoothedFpsData <- predict(lo);
}