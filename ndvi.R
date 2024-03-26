library(rgee)
library(rgeeExtra)
library(googledrive)
library(dplyr)
library(sf)
library(lattice)
library(latticeExtra)


# AOI

pred <- st_read(path_base, layer = "Predios_BR") %>%
  # filter(grepl(c("MULTIPOLYGON", "POLYGON"), st_geometry_type(Shape))) %>%
  filter(NOMBRE == 'ERVA DOCE')

aoi <- pred %>%
  st_transform(31982) %>% st_buffer(5000) %>% st_bbox() %>%
  st_as_sfc() %>% st_as_sf() %>% st_transform(4326)

# sf::st_write(ed, "erva_doce.shp")
# mapview::mapview(ed)

region <- aoi %>%
  st_bbox() %>%
  st_as_sfc() %>%
  sf_as_ee()

roi_bound <- region
#-------------------------------------------------------------------------------

startDate_consult = rdate_to_eedate(as.Date("2023-10-01"))
endDate_consult = rdate_to_eedate(as.Date(Sys.Date()))


#
getQABits <- function(image, qa) {
  qa <-
    sum(2 ^ (which(rev(
      unlist(strsplit(as.character(qa), "")) == 1
    )) - 1))
  image$bitwiseAnd(qa)$lt(1)
}

# Máscara de qualidade de pixel Sentinel
s2_clean_qa60 <- function(img) {
  ndvi = img$normalizedDifference(c("B4", "B8"))$rename('NDVI')
  # qa <- img$select('QA60')
  # quality_mask <- getQABits(qa, "100000000000")
  img$addBands(ndvi)#$updateMask(quality_mask)
}


col =  ee$ImageCollection("COPERNICUS/S2_SR")$
  filterBounds(roi_bound)$
  filterDate("2023-10-01", "2024-03-25")$
  filter(ee$Filter$lte("CLOUDY_PIXEL_PERCENTAGE", 30))$
  sort('system:time_start')$
  map(s2_clean_qa60)
#ee_print(col)
# ee_get_date_ic(col)

# mylist = ee$List$sequence(0,10)

#-------------------------------------------------------------------------------
library(tidyrgee)

col_tidy <- as_tidyee(col)
median_ndvi <-  col_tidy |>
  select("NDVI") |>
  filter(year %in% 2023:2024) |>
  group_by(year, month) |>
  summarise(stat= "median")

ee_ndvi <- as_ee(median_ndvi)

library(cptcity)
# Display results
# Map$setCenter(lon = -51.610,lat = -19.873,zoom = 9)
Map$addLayers(
  eeObject = ee_ndvi,
  visParams = list(
    min = -1 ,
    max = 0.95 ,
    # gamma=0.1, # gamma
    # palette = cpt("grass_ndvi", 10)
    palette = cpt("jjg_neo10_elem_rain", 20)
    # palette = cptcity::lucky()
  )
)

ic_date <- ee_get_date_ic(as_ee(median_ndvi))

nm_dates =  ic_date[ , 2] %>% as.Date() %>% as.character() %>% as.vector()

#-------------------------------------------------------------------------------
# # Approach #1: use an option.
# # Either specify the user:
# options(gargle_oauth_email = "viniciovcl@gmail.com")
# # Or, if you only use one Google identity, you can be more vague:
# options(gargle_oauth_email = TRUE)
# # Or, you can specify the identity to use at the domain level:
# options(gargle_oauth_email = "viniciovcl@gmail.com")
#
# # Approach #2: call PACKAGE_auth() proactively.
# library(googledrive)
# # Either specify the user:
# drive_auth(email = "viniciovcl@gmail.com")
# # Or, if you only use one Google identity, you can be more vague:
# drive_auth(email = TRUE)
# # Or, you can specify the identity to use at the domain level:
# drive_auth(email = "*@example.com")
#
# options(gargle_oath_cache = "~/.cache/gargle",
#         gargle_oauth_email = "viniciovcl@gmail.com",
#         use_oob = TRUE)
# ------------------------------------------------------------------------------

# Download

# Download tif images

stack_list =  ee_imagecollection_to_local(
  ic = ee_ndvi,
  via = "drive",
  region = roi_bound,
  dsn = paste0("Sentinel_", nm_dates),
  scale = 10)

# read raster stack
library(raster)
# install.packages("spDataLarge")
# library(spDataLarge)
path_imgs=  list.files(file.path(getwd()), pattern = '.tif')
r_stack = stack(path_imgs[1:6])
object.size(r_stack)
raster::crs(r_stack)

#Manipulação Raster
# rasterOptions (maxmemory = 1e + 09)

# plot(r_stack[[1]])
# hist(r_stack)
# plot(r_stack)

ndvi_ed <- r_stack * -1

names = r_stack[[i]]@file@name

names= c("2023-10", "2023-11", "2023-12", "2024-01", "2024-02", "2024-03")
names(ndvi_ed) = names

# plot(ndvi_ed)

my.theme <- rasterTheme(region=brewer.pal(11,'RdYlGn'))

p_ervN <- levelplot(
  ndvi_ed,
  par.settings = my.theme,
  margin = FALSE,
  main = "NDVI",
  contour = F,
  names.attr = c(
    "2023-10",
    "2023-11",
    "2023-12",
    "2024-01",
    "2024-02",
    "2024-03"
  ),
  #,
  #"Airbus WorldDEM DTM")
  colorkey = list(title = list(
    "NDVI",
    cex = .55,
    fontface = "bold",
    col = "grey5"
  ))
) +
  layer(sp.polygons(ervD,  fill = "transparent"))


# DIF

nov_out <- ndvi_ed[[2]] - ndvi_ed[[1]]
dez_nov <- ndvi_ed[[3]] - ndvi_ed[[2]]
jan_dez <- ndvi_ed[[4]] - ndvi_ed[[3]]
fev_jan <- ndvi_ed[[5]] - ndvi_ed[[4]]
mar_fev <- ndvi_ed[[6]] - ndvi_ed[[5]]

r <- stack(nov_set, dez_nov, jan_dez, fev_jan, mar_fev)

plot(r)

ervD <- as(pred_ed, "Spatial")

p_ervD <- levelplot(
  r,
  par.settings = RdBuTheme,
  margin = FALSE,
  main = "Diferença mensal",
  contour = F,
  names.attr = c("Nov - Out",
                 "Dez - Nov",
                 "Jan - Dez",
                 "Fev - Jan",
                 "Mar - Fev"),
  #,
  #"Airbus WorldDEM DTM")
  colorkey = list(title = list(
    "T - T(t-1) NDVI ",
    cex = .55,
    fontface = "bold",
    col = "grey5"
  ))
) +
  layer(sp.polygons(ervD,  fill = "transparent"))


#

ggsave(filename = "ndvi_dif_erva_doce.png", r,
       width = 180, height = 135, units = "mm")



