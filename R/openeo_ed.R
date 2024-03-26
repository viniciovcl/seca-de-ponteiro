# ------------------------------------------------------------------------------
# openEO

# O conceito central de “openEO” está relacionado às estratégias de processamento
# de big data. “openEO”” define uma API unificada para software back-end e cliente,
# bem como uma série de processos comuns para manipulação de cubos de dados
# espaço-temporais. A ideia básica é distinguir entre computação (servidor back-end)
# e definição de fluxo de trabalho (software cliente). Embora alguns back-ends
# tenham sido desenvolvidos no projeto principal openEO , outros foram atualmente
# melhorados ou desenvolvidos dentro do projeto openEO Platform da ESA .
# Esses back-ends oferecem acesso às suas coletas de dados e plataforma de
# processamento, enquanto o software cliente (por exemplo, R, Python, JavaScript, QGIS)
# ajuda a criar fluxos de trabalho de processamento em um ambiente de programação
# familiar ao usuário.
# ------------------------------------------------------------------------------


library(openeo)

con = connect(host = "https://openeo.cloud")
collections = list_collections()
print(collections)
print(collections$SENTINEL2_L2A)

s2 = describe_collection("SENTINEL2_L2A") # or use the collection entry from the list, e.g. collections$SENTINEL1_GRD
print(s2)

collection_viewer(x="SENTINEL2_L2A")

# List of available openEO processes with full metadata
processes = list_processes()

# List of available openEO processes by identifiers (string)
print(names(processes))

# print metadata of the process with ID "load_collection"
print(processes$load_collection)

process_viewer(processes)


### Atutenticação

login()

### Criando um processo (definido pelo usuário)

p = processes()

datacube = p$load_collection(
  id = "SENTINEL2_L2A",
  spatial_extent = list(
    west = -52.48659,
    south = -19.21732,
    east = -52.34063,
    north = -19.07570
  ),
  temporal_extent = c("2020-10-01", "2024-12-01"),
  bands = c("B02", "B03", "B04", "B08", "SCL")
)

# spectral_reduce = p$reduce_dimension(data = datacube, dimension = "bands", reducer = function(data,context) {
#   B08 = data[1]
#   B04 = data[2]
#   return((B08-B04)/(B08+B04))
# })
#
# temporal_reduce = p$reduce_dimension(data=spectral_reduce,dimension = "t", reducer = function(x,y){
#   p$min(x)
# })
#
# apply_linear_transform = p$apply(data=temporal_reduce,process = function(value,...) {
#   p$linear_scale_range(x = value,
#                        inputMin = -1,
#                        inputMax = 1,
#                        outputMin = 0,
#                        outputMax = 255)
# })
#
# result = p$save_result(data=apply_linear_transform,format="PNG")


min_reducer = function(data,context) {
  return(p$min(data = data))
}

reduced = p$reduce_dimension(data = datacube, reducer = min_reducer, dimension="t")


# job_id = create_job(
#   graph = result,
#   title = "Example graph",
#   description = "This graph is just a general example",
#   format = "png"
# )

# openeo::login(user="viniciovcl",password="RcFN_00682507")
# start_job(job_id)
# result_obj = list_results(job_id)
# formats = list_file_formats()

result = p$save_result(data = reduced, format = formats$output$GTIFF)

job = create_job(graph=result, title = "Example Title")
start_job(job = job)

#results
temp = tempfile()
file = compute_result(graph = result, output_file = temp)

r = raster::raster(file)
raster::spplot(r)
#-------------------------------------------------------------------------------
