plot5 <- function(nei.data = NULL){
        
        NEI <- if (is.null(nei.data)) {
                readRDS(paste("data/summarySCC_PM25.rds"))
        } else {
                nei.data
        }
        sccVehicleCodes <- levels(SCC[grep("Vehicle" , SCC$SCC.Level.Two),]$SCC)
        baltVehicleData <- NEI[NEI$fips=="24510" & NEI$SCC %in% sccVehicleCodes,]
        baltVehicleData$year <- factor(baltVehicleData$year
                                , levels=c(1999, 2002, 2005, 2008)
                                , labels=c("1999", "2002", "2005", "2008"))
#         baltVehicleData$type <- factor(baltVehicleData$type
#                                 , levels=c(1:4)
#                                 , labels=c("NONPOINT", "NON-ROAD", "ON-ROAD", "POINT"))
        p5Data <- aggregate(Emissions ~ year, data=baltVehicleData, FUN=sum)
        
        ggplot(aes(year, Emissions), data=p5Data) + geom_point() +
           geom_smooth(method = "lm", se=FALSE, color="blue", aes(group=1))


}