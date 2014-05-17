plot6 <- function(nei.data = NULL){
        
        NEI <- if (is.null(nei.data)) {
                readRDS(paste("data/summarySCC_PM25.rds"))
        } else {
                nei.data
        }
        sccVehicleCodes <- levels(SCC[grep("Vehicle" , SCC$SCC.Level.Two),]$SCC)
        locationFips <- factor(c("24510", "06037")) #, labels=c("Baltimore City", "Los Angeles County"))
        vehicleData <- NEI[NEI$fips %in% locationFips & NEI$SCC %in% sccVehicleCodes, c("fips","Emissions", "year")]
        baltVehicleData$year <- factor(baltVehicleData$year
                                       , levels=c(1999, 2002, 2005, 2008)
                                       , labels=c("1999", "2002", "2005", "2008"))
        #         baltVehicleData$type <- factor(baltVehicleData$type
        #                                 , levels=c(1:4)
        #                                 , labels=c("NONPOINT", "NON-ROAD", "ON-ROAD", "POINT"))
        p6Data <- aggregate(Emissions ~ year + fips, data=baltVehicleData, FUN=sum)
        
        ggplot(aes(year, Emissions), data=p6Data) + facet_grid(. ~ fips)  + geom_point() +
                geom_smooth(method = "lm", se=FALSE, color="blue", aes(group=1))
        
        
}