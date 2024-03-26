# ------------------------------------------------------------------------------
# Seca de ponteiro no eucalipto


# Evento de seca de ponterios na Fazenda Erva Doce.
# A principal hipótese é de que houve deriva de herbicidas de aplicações do vizinho.
# Detecção ocorrida em dez de 2023.

# Verificar se a seca do eucalipto coincide com a dessecação da área do vizinho.

# Fitossanidade, seca-de-ponteiro, Erva-Doce

# vinicio.lima@arauco.com

# https://github.com/topics/rgee
# ------------------------------------------------------------------------------

library(rgee)
ee_Initialize(user= "viniciovcl@gmail.com", drive = TRUE)

# > ee_check()
# ◉  Python version
# ✔ [Ok] /home/vinicio/.virtualenvs/rgee/bin/python v3.8
# ◉  Python packages:
#   ✔ [Ok] numpy
# ✔ [Ok] earthengine-api



library(reticulate)
py_config() # see the name of your conda (python) environment, in my case "r-reticulate". From: "pythonhome: C:/Users/user_name/AppData/Local/r-miniconda/envs/r-reticulate"
reticulate::py_install('earthengine-api==0.1.370', envname='r-reticulate')

# Check the installation of "earthengine-api" with
py_list_packages()
pyl <- py_list_packages()
pyl[pyl$package == "earthengine-api", ]

# check python version with
py_run_string("import sys; print(sys.version)")

library(rgee)
ee_Initialize(user= "viniciovcl@gmail.com", drive = TRUE)

library(googledrive)

# Install geemap in the same Python ENV that use rgee
# py_install("geemap")
# gm <- import("geemap")

# Atualize a API do EarthEngine
library(rgee)
# ee_Authenticate()
# ee_Initialize(user= "viniciovcl@gmail.com", drive = TRUE)
# # ee_install_upgrade()

ee_Initialize()
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Teste RGEE

# createTimeBand <-function(img) {
#   year <- ee$Date(img$get('system:time_start'))$get('year')$subtract(1991L)
#   ee$Image(year)$byte()$addBands(img)
# }
#
# collection <- ee$
#   ImageCollection('NOAA/DMSP-OLS/NIGHTTIME_LIGHTS')$
#   select('stable_lights')$
#   map(createTimeBand)
#
# col_reduce <- collection$reduce(ee$Reducer$linearFit())
# col_reduce <- col_reduce$addBands(
#   col_reduce$select('scale'))
# ee_print(col_reduce)
#
#
# Map$setCenter(9.08203, 47.39835, 3)
# Map$addLayer(
#   eeObject = col_reduce,
#   visParams = list(
#     bands = c("scale", "offset", "scale"),
#     min = 0,
#     max = c(0.18, 20, -0.18)
#   ),
#   name = "stable lights trend"
# )

# ------------------------------------------------------------------------------
# AOI

library(dplyr)
library(sf)

path_base <-  "/media/vinicio/Vinicio Lima/Arauco_MS/BASE_MS/BKP_ARAUCARIA/2024/1/Movimentos_janeiro2024.gdb"

ed <- st_read(path_base, layer = "Predios_BR") %>%
  # filter(grepl(c("MULTIPOLYGON", "POLYGON"), st_geometry_type(Shape))) %>%
  filter(NOMBRE == 'ERVA DOCE') %>%
  st_transform(31982) %>% st_buffer(5000) %>% st_bbox() %>%
  st_as_sfc() %>% st_as_sf() %>% st_transform(4326)

# sf::st_write(ed, "erva_doce.shp")
# mapview::mapview(ed)

region <- ed %>%
st_bbox() %>%
  st_as_sfc() %>%
  sf_as_ee()
#-------------------------------------------------------------------------------

#### Arquivo reduce_biweekly

roi_bound <- region

# Periodo de interesse - Dados a partir de 2018-12-14 13:38:00
start_s2 = ee$ImageCollection("COPERNICUS/S2_SR")$filterBounds(roi_bound)$first()
start_s2_first = start_s2$get('system:time_start')
startDate_consult = eedate_to_rdate(start_s2_first) %>% as.Date("%y/%m/%d") %>% rdate_to_eedate()
#eedate_to_rdate(start_s2_first)

startDate_consult = rdate_to_eedate(as.Date("2023-10-01"))
endDate_consult = rdate_to_eedate(as.Date(Sys.Date()))

# Cenas Sentinel
# cenas_tiles = ee$List("") # Criar uma lista de tiles de interesse.

#-------------------------------------------------------------------------------
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
  qa <- img$select("QA60")
  quality_mask <- getQABits(qa, "100000000000")
  img$addBands(ndvi)$updateMask(quality_mask)
}
# ------------------------------------------------------------------------------
# Cola - csaybar
# #Function for remove cloud and shadows ----------------------------------------
# getQABits <- function(image, qa) {
#   # Convert decimal (character) to decimal (little endian)
#   qa <- sum(2^(which(rev(unlist(strsplit(as.character(qa), "")) == 1))-1))
#   # Return a single band image of the extracted QA bits, giving the qa value.
#   image$bitwiseAnd(qa)$lt(1)
# }
#
# s2_clean <- function(img) {
#   # Select only band of interest, for instance, B2,B3,B4,B8
#   img_band_selected <- img$select("B[2-4|8]")
#
#   # quality band
#   ndvi_qa <- img$select("QA60")
#
#   # Select pixels to mask
#   quality_mask <- getQABits(ndvi_qa, "110000000000")
#
#   # Mask pixels with value zero.
#   img_band_selected$updateMask(quality_mask)
# }
# ------------------------------------------------------------------------------
#
col =  ee$ImageCollection("COPERNICUS/S2_SR")$
  filterBounds(roi_bound)$
  filterDate(startDate_consult, endDate_consult)$
  filter(ee$Filter$lte("CLOUDY_PIXEL_PERCENTAGE", 1))$
  sort('system:time_start')$
  map(s2_clean_qa60)
# map(s2_clean)
# ee_print(col)

# ------------------------------------------------------------------------------
# Função Agreggate temporally reduce
# Quinzenal: Quebrado

# biweekly_img = function(collection){
#   # Data da Imagem inicial da colecao
#   init_img = collection$first()
#   startDate_col_first = init_img$get('system:time_start')
#   startDate_col = eedate_to_rdate(startDate_col_first) %>% as.Date("%y/%m/%d") %>% rdate_to_eedate()
#
#   # Data da Imagem final da colecao
#   last_img= collection$limit(1, 'system:time_start', FALSE)$first() # ultima imagem de uma coleção
#   endDate_col_last = last_img$get('system:time_start')
#   endDate_col = eedate_to_rdate(endDate_col_last) %>% as.Date("%y/%m/%d") %>% rdate_to_eedate()
#
#   # Numero de composições de 15 dias (3 passagens Sentinel)
#   n_2weekly = ee$Number(endDate_col$difference(startDate_col, "day"))$divide(15L)$round()
#
#   # Lista para mapear o total de composições de 15 dias
#   seq_biweekly = ee$List$sequence(0,n_2weekly$subtract(1))
#
#   return(ee$ImageCollection(seq_biweekly$map(
#     ee_utils_pyfunc(
#       function(n){
#         ini = startDate_col$advance(ee$Number(15L)$multiply(n), 'day')
#         end = ini$advance(15,'day')
#
#         return(
#           collection$filterDate(ini,end)$median()
#           $set('system:time_start', ini$millis())
#           $set('system:time_end', end$millis())
#         )}
#
#     ))))
# }

# col_biweekly= biweekly_img(col)
# ee_print(col_biweekly)

#-------------------------------------------------------------------------------
# Mensal
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Teste Cobertura de núvens

library(rgeeExtra)
library(rgee)

# 1. Define your ROI
ROI <- ee$Geometry$Rectangle(
  coords = c(440030, 4568820, 445120, 4573910),
  proj = "EPSG:32719",
  geodesic = FALSE
)

# 2. load S2 collection
ic <- ee$ImageCollection("COPERNICUS/S2_SR") %>%
  ee$ImageCollection$filterBounds(ROI) %>%
  ee$ImageCollection$filterDate("2021-09-01", "2022-01-01")

add_new_property <- function(img) {

  # 3. Use sen2cor cloud mask
  sen2cor_cloudmask <- img[["SCL"]]
  bmask01 <- sen2cor_cloudmask == 8 | sen2cor_cloudmask == 9 | sen2cor_cloudmask == 10

  # 4. Use sen2cloudless cloud mask
  # sen2cloudless recommends 0.4 (as I remenber), but it varies according the ROI.
  bmask02 <- img[["MSK_CLDPRB"]] > 0.5

  # 5. Merge both cloud masks
  bmask <- bmask01 | bmask02

  # 6. Estimate the new CLOUDY_PIXEL_PERCENTAGE
  newproperty <- ee$Image$reduceRegion(
    image = bmask,
    geometry = ROI, # PUT HERE YOUR ROI!
    reducer = ee$Reducer$mean(),
    bestEffort = TRUE
  ) %>%
    # The rgeeExtra results have the band name "layer".
    # It works as well the raster R package!
    ee$Dictionary$get("layer") %>%
    ee$Number()

  # 7. Set the property
  img$set("newCLOUDY_PIXEL_PERCENTAGE", newproperty)
}

new_ic <- ic$map(add_new_property)

ncloud_coverage <- unlist(new_ic$aggregate_array("newCLOUDY_PIXEL_PERCENTAGE")$getInfo())
nold_coverage <- unlist(new_ic$aggregate_array("CLOUDY_PIXEL_PERCENTAGE")$getInfo())
