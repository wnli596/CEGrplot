#' @title ceg_station_plot
#' @author Weinan Li
#' @param data A data.frame. including Lon, Lat, Station
#' @param lon_min A number. Default is 100
#' @param lon_max A number. Default is 125
#' @param lat_min A number. Default is 0
#' @param lat_max A number. Default is 25
#' @param brief logical. If TRUE/FALSE plotting a brief/colorful map, Default is FALSE
#' @param color character. Color for Station points, Default is "black"
#' @param shape A number. Shape for Station points, Default is 16
#' @param point_size A number. Size for Station points, Default is 6
#' @param text logical. If TRUE adding Station names, Default is TRUE
#' @param text_size A number. Size for Station names, Default is 6
#' @param base_size A number. Base size, default is 16
#' @param family character. "sans"/ Arial, "serif"/Time new roman and any other fronts, Default is "sans"
#' @param save_width A number. width for save figures, Default is 10
#' @param save_height A number. height for save figures, Default is 8
#' @param format character. file type for save figures, Default is "pdf"
#'
#' @return A figure
#' @export
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 scale_fill_identity
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 coord_sf
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom rnaturalearth ne_countries
#' @importFrom ggrepel geom_text_repel
#' @examples NULL
#example
# df <- data.frame(Station = c("A", "B", "C"),
#                  Lon = c(120, 130, 140),
#                  Lat = c(10, 20, 30))
# ceg_station_plot(df)
ceg_station_plot <- function(data,
                             lon_min = 100,
                             lon_max = 170,
                             lat_min = 0,
                             lat_max = 60,
                             brief = FALSE,
                             color = "black",#点颜色，默认为black
                             shape = 16,#点形状，默认为16
                             point_size = 6,#点大小，默认为6
                             text = TRUE,# 添加站位名，默认为TRUE
                             text_size = 6,#站位字体大小，默认为6
                             base_size = 16,#theme_bw字体大小
                             family = "sans",#默认字体，"serif"Time new roman
                             save_width = 10,#图片保存宽度设置
                             save_height = 8,#图片保存高度设置
                             format = "pdf")#默认保存格式为pdf
{ library(tidyverse)
  path <- getwd()
  suppressWarnings(dir.create(paste0(path, "/Station/")))  # 忽略已存在目录的警告
  if(brief == FALSE){
    # 检查是否已经有 mapcolors 数据框存在，如果不存在则获取新的颜色数据
    if (!exists("mapcolors") || is.null(mapcolors)) {
      message("Fetching map colors...")
      mapcolors <- get_map_colors(lon_min, lon_max, lat_min, lat_max)
      message("If you want to customize colors, please manually (mapcolors) by get_map_colors()")
    }
    #绘图
    p <- ggplot() +
      geom_raster(data = mapcolors, aes(Lon, Lat, fill = fill_color)) +
      scale_fill_identity() +
      coord_cartesian(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
      geom_point(data = data %>%
                   dplyr::select(Station, Lon, Lat) %>%
                   group_by(Station, Lon, Lat) %>%
                   distinct(),
                 aes(x = Lon, y = Lat), color = color, shape = shape, size = point_size)+
      theme_bw(base_size = base_size)+
      labs(x = "Longitude", y = "Latitude") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(family = family))
  } else if(brief == TRUE){
    #加载地图数据
    world_shp <-rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
    p <- ggplot() +
      geom_sf(data = world_shp, fill = 'grey', col = 'black', size = 0.1) +
      coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
      geom_point(data = data %>%
                   dplyr::select(Station, Lon, Lat) %>%
                   group_by(Station, Lon, Lat) %>%
                   distinct(),
                 aes(x = Lon, y = Lat), color = color, shape = shape, size = point_size)+
      theme_bw(base_size = base_size)+
      labs(x = "Longitude", y = "Latitude") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(family = family))
  }

  if(text) { # 如果text为TRUE，则添加文本标签
    p <- p + ggrepel::geom_text_repel(data = data %>%
                                        dplyr::select(Station, Lon, Lat) %>%
                                        group_by(Station, Lon, Lat) %>%
                                        distinct(),
                                      aes(x = Lon, y = Lat, label = Station),
                                      size = text_size, family = family)
  }
  ggsave(paste0(path, "/Station/", cruise, "站位图.", format), width = save_width, height = save_height, units = "in", dpi = 300)
  return(p)
}
