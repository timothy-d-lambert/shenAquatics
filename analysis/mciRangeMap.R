library(mapShen)
library(plotHacks)
bedrock<-readOGR(dsn="C:/Users/echildress/Documents/mapShen/data/gis",layer="Bdrck_2009",verbose=F)
boundary<-readOGR(dsn="C:/Users/echildress/Documents/mapShen/data/gis",layer="PkBndryPly",verbose=F)

bedrockClip<-suppressWarnings(raster::intersect(bedrock,boundary))


plot(boundary)
for(g in c("Ct","Cwa","Ce","OCc","Ob")){
  plot(bedrockClip[bedrockClip$MapUnit==g,],
       col=palette()[which(g==c("Ct","Cwa","Ce","OCc","Ob"))],
       add=T)
}
plot(bedrockClip[bedrockClip$MapUnit %in% c("Om","Oeln","Ob","Os","OCc","Ce","Cwa","Ct"),],
     col='blue',add=T)

sinkholes<-readOGR(dsn="C:/Users/echildress/Documents/mapShen/data/gis",layer="SINK~6$3",verbose=F)

mci<-readOGR(dsn="C:/Users/echildress/Documents/mapShen/data/gis/a_lira_potential_habitat",
             layer="a_lira_potential_habitat",verbose=F) %>%
  sp::spTransform(CRS("+proj=utm +zone=17 +datum=NAD83 +units=m
                             +no_defs +ellps=GRS80 +towgs84=0,0,0"))

parkMci<-sf::st_intersection(boundary,mci)

tiff.par(file="figures/Madison cave isopod.tif",height=8)
plot(boundary)
plot(mci,col='gray',add=T,border=NA)
plot(boundary,add=T)
plot(parkMci,add=T,col='red')

legend(722000,4240000,c("SHEN Boundary","MCI Potential Habitat","MCI Habitat in Park"),
       col=c("black","gray","red"),pch=c(NA,22,22),pt.bg=c(NA,"gray","red"),pt.cex=2,
       lty=c(1,NA,NA),bty='n')
dev.off()
