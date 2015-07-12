#2015-07-12
#R learning scripts
# specify the portfolio to analyze
portfolioName<-"port1"

#load the RMetrics dependencies
#https://www.rmetrics.org/files/Meielisalp2008/Presentations/Wuertz2.pdf
library(fPortfolio);

#get the equity list in the portfolio
equities<-read.csv(paste(portfolioName,"/EQ.csv", sep=""), header = TRUE, sep = ",")

#fetch data from yahoo (more sources to be supported)
eqData<-yahooSeries(equities[,2],from=NULL,to=Sys.timeDate(), nDaysBack=366)

#use close MV by default
eqDataClose<-eqData[,paste(equities[,2], ".Close", sep="")]

#calculate returns of the equities
eqReturns<-100*returns(eqDataClose)

#setup minimum risk mean-variance portfolio
ewSpec <- portfolioSpec()
nAssets <- ncol(eqReturns)
setWeights(ewSpec) <- rep(1/nAssets, times = nAssets)

#get feasible portfolio
ewPortfolio <- feasiblePortfolio(
  data = eqReturns,
  spec = ewSpec,
  constraints = "LongOnly")

#plot cov risk budget pie
col = divPalette(ncol(eqReturns), "RdBu")
covRiskBudgetsPie(ewPortfolio, radius = 0.7, col = col)
mtext(text = "Equally Weighted MV Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)

#long only frontier plot
f=portfolioFrontier(data=eqReturns, spec= ewSpec, constraints="LongOnly")
frontierPlot(f)
minvariancePoints(f)
tangencyPoints(f)
tangencyLines(f)
equalWeightsPoints(f)
twoAssetsLines(f)
singleAssetPoints(f)
sharpeRatioLines(f)

#allow short now
#otherwise, the target return might not be archivable.
shortSpec<-ewSpec
setNFrontierPoints(shortSpec) <- 5
setTargetReturn(shortSpec) = 0.2
setSolver(shortSpec) <- "solveRshortExact"
ef = efficientPortfolio(data=eqReturns, spec= shortSpec, constraints="LongOnly")
weightsPie(ef)