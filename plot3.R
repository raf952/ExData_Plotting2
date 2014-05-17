plot3 <- function(nei.data = NULL){
        
        NEI <- if (is.null(nei.data)) {
                readRDS(paste("data/summarySCC_PM25.rds"))
        } else {
                nei.data
        }
        baltData <- NEI[NEI$fips=="24510",]
        baltData$year <- factor(baltData$year
                                , levels=c(1999, 2002, 2005, 2008)
                                , labels=c("1999", "2002", "2005", "2008"))
        baltData$type <- factor(baltData$type
                                , levels=c(1:4)
                                , labels=c("NONPOINT", "NON-ROAD", "ON-ROAD", "POINT"))
        p3Data <- aggregate(Emissions ~ type+year, data=baltData, FUN=sum)
        
        qplot(year, data=p3Data, Emissions, geom="density", fill=type, alpha=I(0.5)
              , main="foo", xlab="bar", ylab="zog")
}