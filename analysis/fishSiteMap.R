library(plotHacks)
library(shenAquatics)

sites <- aqData("sites")


tmap_mode(c("plot", "view")[1]) # define tmap mode

# Map of Shenandoah streams and park boundary
streams_map <- mapShenStreams(streamCol = 'cornflowerblue',
                              border="darkgreen",
                              bg.color = NA)
print(streams_map)

# Map of primary fish sites
site_map <- mapAqSites(siteId = sites[FISH_SiteType=="Primary",SiteID],
                       tmap_args_list = list(
                         tm_dots = list(col = c("darkblue", "Elev_m", "MAJ_GEOL")[1],
                                        size = 0.2,
                                        title = c("", "Elevation (m)", "Geology")[1]),
                         tm_borders = list(col = "black",
                                           lwd = 2)
                         )
                       )
print(site_map)

# Map of secondary
site_map <- mapAqSites(siteId = sites[FISH_SiteType=="Primary",SiteID],
                       add = TRUE, existing_map = streams_map)
print(site_map)

site_map2 <- mapAqSites(siteId=sites[FISH_SiteType=="Secondary",SiteID])
print(site_map2)

legend("left",legend=c("Primary","Secondary"),
       col=c("cornflowerblue","darkseagreen"),pch=19,bg='black',text.col='gray90',
       text.font=2,pt.cex=1,cex=1.5)

dev.off()

tiff.par("C:/Users/echildress/Documents/presentations/aqinSites.tiff",width=5,height=8,mar=c(0,0,0,0))
mapShenStreams(streamCol = 'gray20',bg='black',border="darkgreen")

mapAqSites(siteId=sites[AQIN_SiteType=="Primary",SiteID],pch=19,col="cornflowerblue")
mapAqSites(siteId=sites[AQIN_SiteType=="Secondary",SiteID],pch=19,col="darkseagreen")

legend("left",legend=c("Primary","Secondary"),
       col=c("cornflowerblue","darkseagreen"),pch=19,bg='black',text.col='gray90',
       text.font=2,pt.cex=1,cex=1.5)

dev.off()
