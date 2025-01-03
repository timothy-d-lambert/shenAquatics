library(plotHacks)
library(shenAquatics)
library(here)
here::i_am("analysis/fishSiteMap.R")


#### MAPPING ####

sites <- aqData("sites")

tmap_mode(c("plot", "view")[1]) # define tmap mode

# Map of Shenandoah streams and park boundary
streams_map <- mapShenStreams()
print(streams_map)


# Map of primary fish sites
png(filename = here("figures", "fishSiteMap_primary.png"),
    width = 3.75, height = 6, units = "in", res = 250)
site_map <- mapAqSites(siteId = sites[FISH_SiteType=="Primary",SiteID])
print(site_map)
dev.off()

# Map of primary and secondary fish sites
png(filename = here("figures", "fishSiteMap_primary-secondary.png"),
    width = 3.75, height = 6, units = "in", res = 250)
site_map <- mapAqSites(
  siteId = sites[FISH_SiteType %in% c("Primary","Secondary"), SiteID],
  tmap_args_list = list(
    tm_lines = list(col = "lightblue",
                    lwd = 1),
    tm_dots = list(col = c("FISH_SiteType"),
                   size = 0.2,
                   title = c("Site type"),
                   palette = "Set1")
  )
)
print(site_map)
dev.off()


# Map sites by elevation
# # a. continuous version
# site_map <- mapAqSites(
#   siteId = sites[FISH_SiteType %in% c("Primary","Secondary"), SiteID],
#   tmap_args_list = list(
#     tm_lines = list(col = "lightblue",
#                     lwd = 1),
#     tm_dots = list(col = c("Elev_m"),
#                    size = 0.2,
#                    title = c("Elevation (m)"),
#                    style = c("cont","order")[2],
#                    palette = "viridis",
#                    legend.col.reverse = TRUE),
#     tm_borders = list(col = "burlywood3",
#                       lwd = 2)
#   )
# )
# print(site_map)

# b. categorical version, with histogram
png(filename = here("figures", "fishSiteMap_elevation-categorical.png"),
    width = 4.6875, height = 6, units = "in", res = 250)
site_map <- mapAqSites(
  siteId = sites[FISH_SiteType %in% c("Primary","Secondary"), SiteID],
  tmap_args_list = list(
    tm_lines = list(col = "lightblue",
                    lwd = 1),
    tm_dots = list(col = c("Elev_m"),
                   size = 0.2,
                   title = c("Elevation (m)"),
                   style = "pretty",
                   palette = "viridis",
                   legend.col.reverse = TRUE,
                   legend.hist = TRUE),
    tm_borders = list(col = "burlywood3",
                      lwd = 2)
  ),
  xlim_bbox = c(-0.25,1), ylim_bbox = c(0,1)
)
print(site_map)
dev.off()


#### OTHER MAPS TO MAKE: ####
# Map sites by geology
# Map sites by stream temperature sensitivity (this will be done in TSS.stats.SHEN package instead)


#### Sample use of site_vars argument (in mapAqSites()) ####
# The site_vars argument adds additional variables to the sites data table, which can then be used as the variable by which site colors are mapped. The example below shows how this is done for latitude (pretending for the moment that it is not in the sites data table to begin with):

rm(list = ls())
library(plotHacks)
library(shenAquatics)
library(here)
here::i_am("analysis/fishSiteMap.R")
tmap_mode(c("plot", "view")[1]) # define tmap mode


# restrict to two columns: siteID and Lat_n83
sites <- aqData("sites") # BUG TO FIX: Doesn't seem like this should be needed, but ERROR IN mapAqSites() without this global variable. Why?
site_vars <- aqData("sites") %>%
  # select columns by name
  select(SiteID, Lat_n83) %>%
  # rename column Lat_n83 to be color_var
  rename(color_var = Lat_n83)

# map using the new color_var
site_map <- mapAqSites(
  siteId = sites[FISH_SiteType %in% c("Primary","Secondary"), SiteID],
  site_vars = site_vars,
  tmap_args_list = list(
    tm_lines = list(col = "lightblue",
                    lwd = 1),
    tm_dots = list(col = c("color_var"),
                   size = 0.2,
                   title = c("Latitude"),
                   style = c("cont","order")[2],
                   palette = "viridis",
                   legend.col.reverse = TRUE),
    tm_borders = list(col = "burlywood3",
                      lwd = 2)
  )
)
print(site_map)

