plot4 <- function(nei.data = NULL){
        
        NEI <- if (is.null(nei.data)) {
                readRDS(paste("data/summarySCC_PM25.rds"))
        } else {
                nei.data
        }
        
        coalComb <- function(field) {
                
                intersect(grep("coal", SCC[[field]], ignore.case=TRUE)
                          , grep("comb", SCC[[field]], ignore.case=TRUE))
        }
        
        coalCombustion <- as.vector(SCC[coalComb("EI.Sector"),"SCC"])
        p4Data <- aggregate(Emissions ~ year
                      
                            , data=NEI[NEI$SCC %in% coalCombustion, c("Emissions", "year")]
                            , FUN = sum)
        p4Data$year <-as.factor(p4Data$year)
        qplot( data = p4Data
              , year, Emissions
              , geom=c("point")
              , main="Emissions from Coal Combustion-related sources")
}