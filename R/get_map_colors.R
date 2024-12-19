#' @title get_map_colors
#' @author Weinan Li
#' @param lon_min A number. Default is 100
#' @param lon_max A number. Default is 125
#' @param lat_min A number. Default is 0
#' @param lat_max A number. Default is 25
#' @param nbreaks one number. Default is 100
#' @param oceancolor A color vector for ocean. Default is rev(RColorBrewer::brewer.pal(9, "Blues"))
#' @param landcolor A color vector for land. Default is RColorBrewer::brewer.pal(11, "PiYG")
#'
#' @return A data.table data.frame
#' @export
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom terra rast crop ext
#' @importFrom data.table as.data.table setnames
#' @examples mapcolors <- get_map_colors()

#example
# mapcolors <- get_map_colors()
#
# ggplot(mapcolors, aes(Lon, Lat)) +
#   geom_raster(aes(fill = fill_color)) +
#   scale_fill_identity()+
#   coord_cartesian(expand = FALSE)

get_map_colors <- function(lon_min = 100,
                           lon_max = 125,
                           lat_min = 0,
                           lat_max = 25,
                           nbreaks = 100,
                           oceancolor = rev(RColorBrewer::brewer.pal(9, "Blues")),
                           landcolor = RColorBrewer::brewer.pal(11, "PiYG")){
  oceanpalette <- grDevices::colorRampPalette(oceancolor)
  landpalette <- grDevices::colorRampPalette(landcolor)
  ncfile <- system.file('data/ETOPO1_15second.nc', package='CEGrplot')
  datafile <- system.file('data', package='CEGrplot')
  if(ncfile == ""){
    message("Error: Please download the ETOPO1_15second.nc dataset from the following DOI link: https://doi.org/10.5281/zenodo.14223594, and then place the data under: ", datafile)
  } else {
    bathy <- terra::rast(ncfile) %>%
        terra::crop(., ext(lon_min, lon_max, lat_min, lat_max)) %>%
        data.table::as.data.table(., xy = TRUE)

    data.table::setnames(bathy, old = c("x", "y", "ETOPO1_15second"), new = c("Lon", "Lat", "Depth"))

    bathy[, cuts := base::cut(Depth, breaks = nbreaks)]

    bathy[, fill_color := ifelse(Depth < 0, oceanpalette(nbreaks)[as.numeric(cuts)], landpalette(nbreaks)[as.numeric(cuts)])]

    mapcolors <- bathy[, cuts := NULL]
  }

  return(mapcolors)
}


