plot1 <- function(nei.data = NULL){
        
        NEI <- if (is.null(nei.data)) {
                readRDS(paste("data/summarySCC_PM25.rds"))
        } else {
                nei.data
        }
        NEI$year <- as.factor(NEI$year)
        p1Data <- aggregate(Emissions ~ year, data= NEI, FUN=sum)
        plot( p1Data$Emissions  ~ levels(p1Data$year)
              , xlab="year", ylab="Emissions"
              , type = "o" )
}