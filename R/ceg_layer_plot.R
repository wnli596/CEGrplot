#' @title ceg_layer_plot
#' @author Weinan Li
#' @param data A data.frame. including Lon, Lat, Station, Variable, Value, unit, Layer
#' @param lon_min A number. Default is 100
#' @param lon_max A number. Default is 125
#' @param lat_min A number. Default is 0
#' @param lat_max A number. Default is 25
#' @param brief logical. If TRUE/FALSE plotting a brief/colorful map, Default is FALSE
#' @param variable character. unique(data$Variable). Default is "TChla"
#' @param layer character. unique(data$Layer). Default is "Surface"
#' @param colorbar character."auto" or "manual"
#' @param palette A color vector for data$Value. Default is colorRampPalette(c("#00008F","#0000FF","#004FFF", "#00FFFF", "#80FF80", "#FFFF00", "#FFAF00","#FF0000","#800000"))
#' @param mincolor A number. If colorbar = "manual", need a mincolor number
#' @param maxcolor A number. If colorbar = "manual", need a maxcolor number
#' @param shape A number. Shape for Station points, Default is 16
#' @param point_size A number. Size for Station points, Default is 6
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
#' @importFrom ggplot2 scale_color_gradientn
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
#' @examples NULL
ceg_layer_plot <- function(data,
                           lon_min = 100,
                           lon_max = 170,
                           lat_min = 0,
                           lat_max = 60,
                           brief = FALSE,
                           variable = "TChla",#需要手动设置展示变量列名，默认为TChla
                           layer = "Surface",
                           colorbar = "auto",#c("auto", "manual")自动和手动设置colorbar
                           palette = colorRampPalette(c("#00008F","#0000FF","#004FFF", "#00FFFF", "#80FF80", "#FFFF00", "#FFAF00","#FF0000","#800000")),
                           mincolor,#需手动设置colorbar范围
                           maxcolor,
                           shape = 16,#点形状，默认为21
                           point_size = 6,#点大小，默认为6
                           base_size = 16,
                           family = "sans",#默认字体，"serif"Time new roman
                           save_width = 10,
                           save_height = 8,
                           format = "pdf") {
  path <- getwd()
  suppressWarnings(dir.create(paste0(path, "/Layer/")))  # 忽略已存在目录的警告
  #绘图数据筛选
  plotdata <- data %>%
    filter(Variable == {{variable}} & Layer == {{layer}})

  if(brief == FALSE){
    # 检查是否已经有 mapcolors 数据框存在，如果不存在则获取新的颜色数据
    if (!exists("mapcolors") || is.null(mapcolors)) {
      message("Fetching map colors...")
      assign("mapcolors", get_map_colors(lon_min, lon_max, lat_min, lat_max), envir = .GlobalEnv)
      message("If you want to customize colors, please manually (mapcolors) by get_map_colors()")
    }
    #绘图
    p <- ggplot() +
      geom_raster(data = mapcolors, aes(Lon, Lat, fill = fill_color)) +
      scale_fill_identity() +
      coord_cartesian(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
      geom_point(data = plotdata,
                 aes(x = Lon, y = Lat, color = Value), shape = shape, size = point_size)+
      theme_bw(base_size = base_size)+
      labs(x = "Longitude", y = "Latitude", color = NULL, title = paste0(variable, " ", unique(plotdata$Unit), " ", "Layer = ", layer), family = family)+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(family = family),
            plot.title = element_text(hjust = 1, margin = margin(t = 10, r = 0, b = 0, l = 0)))  # 调整标题位置到右上方

  } else if(brief == TRUE){
    #加载地图数据
    world_shp <-rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
    #绘图
    p <- ggplot() +
      geom_sf(data = world_shp, fill = 'grey', col = 'black', size = 0.1) +
      coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
      geom_point(data = plotdata,
                 aes(x = Lon, y = Lat, color = Value), shape = shape, size = point_size)+
      theme_bw(base_size = base_size)+
      labs(x = "Longitude", y = "Latitude", color = NULL, title = paste0(variable, " ", unique(plotdata$Unit), " ", "Layer = ", layer), family = family)+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(family = family),
            plot.title = element_text(hjust = 1, margin = margin(t = 10, r = 0, b = 0, l = 0)))  # 调整标题位置到右上方
  }
  #自动调整colorbar高度函数
  make_colorbar_full <- function() structure("", class = "fullsizebar")

  ggplot_add.fullsizebar <- function(obj, g, name = "fullsizebar") {
    h <- ggplotGrob(g)$heights
    panel <- which(grid::unitType(h) == "null")
    panel_height <- unit(1, "npc") - sum(h[-panel])

    g +
      guides(color = guide_colorbar(barheight = panel_height,
                                    title.position = "right"))
  }
  # 在全局环境中保存自动调整colorbar高度函数
  assign("make_colorbar_full", make_colorbar_full, envir = .GlobalEnv)
  assign("ggplot_add.fullsizebar", ggplot_add.fullsizebar, envir = .GlobalEnv)
  # 根据 colorbar 参数选择相应的颜色条设置
  if (colorbar == "auto") {
    p <-  p +
      scale_color_gradientn(colours = palette(100)) +
      make_colorbar_full()
  } else if (colorbar == "manual") {
    if (is.null(mincolor) || is.null(maxcolor)) {
      stop("Please provide both 'mincolor' and 'maxcolor' when using 'manual' colorbar.")
    }
    p <-  p +
      scale_color_gradientn(colours = palette(100),
                            limits = c(mincolor, maxcolor),
                            breaks = seq(mincolor, maxcolor, length.out = 5),
                            oob = scales::oob_squish)+
      make_colorbar_full()
  } else {
    stop("Invalid value for 'colorbar'. Must be 'auto' or 'manual'.")
  }
  #保存图片
  ggsave(paste0(path, "/Layer/", cruise, "_", variable, "_", layer, "层分布图.", format), width = save_width, height = save_height, units = "in", dpi = 300)
  return(p)
}
