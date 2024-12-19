#' @title ceg_timeseries_plot
#' @author Weinan Li
#' @param data A data.frame. including Lon, Lat, Station, Time, Variable, Value, Unit
#' @param variable character. unique(data$Variable). Default is "P3"
#' @param station character. unique(data$Station). Default is "Section1"
#' @param res_x A number. Interpolation resolution, Default is 500
#' @param res_y A number. Interpolation resolution, Default is 500
#' @param colorbar character."auto" or "manual"
#' @param palette A color vector for data$Value. Default is colorRampPalette(c("#00008F","#0000FF","#004FFF", "#00FFFF", "#80FF80", "#FFFF00", "#FFAF00","#FF0000","#800000"))
#' @param mincolor A number. If colorbar = "manual", need a mincolor number
#' @param maxcolor A number. If colorbar = "manual", need a maxcolor number
#' @param shape A number. Shape for Sample points, Default is 16
#' @param point_size A number. Size for Sample points, Default is 2
#' @param text_size A number. Size for text contour, Default is 4
#' @param base_size A number. Base size, default is 16
#' @param family character. "sans"/ Arial, "serif"/Time new roman and any other fronts, Default is "sans"
#' @param save_width A number. width for save figures, Default is 10
#' @param save_height A number. height for save figures, Default is 8
#' @param format character. file type for save figures, Default is "pdf"
#'
#' @return A figure
#' @export
#' @importFrom MBA mba.surf
#' @importFrom reshape2 melt
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 geom_contour
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_reverse
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom metR geom_text_contour
#' @examples NULL
ceg_timeseries_plot <- function(data,
                             variable = "TChla",
                             station = "P3",
                             res_x = 500,
                             res_y = 500,
                             colorbar = "auto",
                             palette = colorRampPalette(c("#00008F","#0000FF","#004FFF", "#00FFFF", "#80FF80", "#FFFF00", "#FFAF00","#FF0000","#800000")),
                             mincolor,
                             maxcolor,
                             shape = 16,
                             point_size = 2,
                             text_size = 4,
                             base_size = 16,
                             family = "sans",
                             save_width = 12,
                             save_height = 6,
                             format = "pdf")
{
  suppressWarnings(dir.create(paste0(path, "/Timeseries/")))  # 忽略已存在目录的警告

  #绘图数据筛选
  plotdata <- data %>%
    filter(Variable == {{variable}} &
             Station == {{station}}) %>%
    mutate(Time = as.numeric(Time))


  #使用 MBA 包对断面进行空间插值
  library(MBA)
  library(tidyverse)
  library(lubridate)

  df2 <- mba.surf(plotdata[c('Time', "Depth", "Value")], #多元插值
                  no.X = 500, no.Y = 500, extend = T,
                  b.box = c(
                    floor(min(plotdata$Time, na.rm = TRUE)/3600)*3600,
                    ceiling(max(plotdata$Time, na.rm = TRUE)/3600)*3600,
                    floor(min(plotdata$Depth/10)),
                    ceiling(max(plotdata$Depth)/10)*10
                  ))  #设置插值空间范围
  dimnames(df2$xyz.est$z) <- list(df2$xyz.est$x, df2$xyz.est$y)
  df2 <- reshape2::melt(df2$xyz.est$z, varnames = c('Time', 'Depth'), value.name = 'Value') %>%
    mutate(Value = ifelse(Value < 0, 0, Value)) %>%
    mutate(Time = as.POSIXct(Time))

  seq <- floor(log10(max(df2$Value, na.rm = TRUE))) - 1
    #绘图
    p <- ggplot() +
      geom_raster(data = df2, aes(x = Time, y = Depth, fill = Value)) +  #使用插值绘制渲染图
      geom_contour(data = df2, aes(x = Time, y = Depth, z = Value),
                   breaks = seq(0, ceiling(max(df2$Value, na.rm = TRUE)/(10^seq))*(10^seq), 10^(seq)),
                   linewidth = 0.5, colour = 'black', alpha = 0.2) +  #等密度线
      metR::geom_text_contour(data = df2, aes(x = Time, y = Depth, z = Value),
                              size = text_size,family = family)+
      geom_point(data = plotdata, aes(x = as.POSIXct(Time), y = Depth), shape = shape, size = point_size) +  #添加站位散点
      theme_bw(base_size = base_size) +
      scale_y_reverse(expand = expansion(mult = c(0, 0)))+
      scale_x_datetime(date_breaks = "4 hours", date_labels = "%m-%d\n%H:%M",
                       expand = expansion(mult = c(0, 0)))+
      labs(x = 'Time', y = 'Depth (m)', fill = NULL,
           title = paste0(variable, " ", unique(plotdata$Unit)), family = family)+
      theme(
        legend.background = element_rect(fill = NA,color = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family = family),
        plot.title = element_text(hjust = 1, margin = margin(t = 10, r = 0, b = 0, l = 0)))


  #自动调整colorbar高度函数
  make_colorbar_full <- function() structure("", class = "fullsizebar")

  ggplot_add.fullsizebar <- function(obj, g, name = "fullsizebar") {
    h <- ggplotGrob(g)$heights
    panel <- which(grid::unitType(h) == "null")
    panel_height <- unit(1, "npc") - sum(h[-panel])

    g +
      guides(fill = guide_colorbar(barheight = panel_height,
                                   title.position = "right"))
  }
  # 在全局环境中保存自动调整colorbar高度函数
  assign("make_colorbar_full", make_colorbar_full, envir = .GlobalEnv)
  assign("ggplot_add.fullsizebar", ggplot_add.fullsizebar, envir = .GlobalEnv)
  # 根据 colorbar 参数选择相应的颜色条设置
  if (colorbar == "auto") {
    p <-  p +
      scale_fill_gradientn(colours = palette(100)) +
      make_colorbar_full()
  } else if (colorbar == "manual") {
    if (is.null(mincolor) || is.null(maxcolor)) {
      stop("Please provide both 'mincolor' and 'maxcolor' when using 'manual' colorbar.")
    }
    p <-  p +
      scale_fill_gradientn(colours = palette(100),
                           limits = c(mincolor, maxcolor),
                           breaks = seq(mincolor, maxcolor, length.out = 5),
                           oob = scales::oob_squish)+
      make_colorbar_full()
  } else {
    stop("Invalid value for 'colorbar'. Must be 'auto' or 'manual'.")
  }
  #保存图片
  ggsave(paste0(path, "/Timeseries/", cruise, "_", unique(plotdata$Station), "站_", variable,"时间序列图.", format), width = save_width, height = save_height, units = "in", dpi = 300)
  return(p)
}
