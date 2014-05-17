plot2 <- function(nei.data = NULL){
        
        NEI <- if (is.null(nei.data)) {
                readRDS(paste("data/summarySCC_PM25.rds"))
        } else {
                nei.data
        }
        baltData <- NEI[NEI$fips=="24510",]
        baltData$year <- as.factor(baltData$year)
        p2Data <- aggregate(Emissions ~ year, data= baltData, FUN=sum)
        plot( p2Data$Emissions  ~ levels(p1Data$year)
              , xlab="year", ylab="Emissions"
              , type = "o"
              , main=expression('Baltimore: Total PM'[2.5]))
}