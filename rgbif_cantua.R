
# install.packages("rgbif") # CRAN version
library(rgbif)
library(raster)

taxonKey <- name_backbone("Cantua buxifolia")$usageKey
cantua <- occ_search(taxonKey = taxonKey)$data
cantua_coords <- cantua[!is.na(cantua$decimalLongitude),
                        c('decimalLongitude', 'decimalLatitude')]

# para sacar datos de GBIF, hay dos funciones:
# occ_download(): registros ilimitados (para investigacion)
# occ_search(): limitada a 100K registros
# occ_download(pred("taxonKey", taxonKey)) 

bol <- geodata::worldclim_country('Bolivia', 'tavg', 'data/')
tavg <- geodata::worldclim_tile('tavg', lat = cantua_coords[3, 'decimalLatitude'], 
                                lon = cantua_coords[3, 'decimalLongitude'],
                                path = 'data/')[[1]]
plot(tavg)
points(cantua_coords, pch = 20)

tavg_extract <- extract(tavg, cantua_coords)

