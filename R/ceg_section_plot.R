#' @title ceg_section_plot
#' @author Weinan Li
#' @param data A data.frame. including Lon, Lat, Station, Variable, Value, unit, Section, Direction
#' @param variable character. unique(data$Variable). Default is "TChla"
#' @param section character. unique(data$Section). Default is "Section1"
#' @param show_Depth A number. Default is 200
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
ceg_section_plot <- function(data,
                             variable = "TChla",
                             section = "Section1",
                             show_Depth = 200,
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
                             save_width = 10,
                             save_height = 8,
                             format = "pdf")
{
  suppressWarnings(dir.create(paste0(path, "/Section/")))  # 忽略已存在目录的警告

  #绘图数据筛选
  plotdata <- data %>%
    filter(Variable == {{variable}} &
             Section == {{section}} &
             !is.na(Section_name) &
             Depth <= show_Depth)


  #使用 MBA 包对断面进行空间插值
  library(MBA)
  library(tidyverse)
  #根据经向或者纬向插值
  if (unique(plotdata$Direction) == "Lon") {
    df2 <- mba.surf(plotdata[c('Lon', "Depth", "Value")], #多元插值
                    no.X = res_x, no.Y = res_y, extend = T,
                    b.box = c(floor(min(plotdata$Lon*10, na.rm = TRUE) - 0.01)/10, ceiling(max(plotdata$Lon*10, na.rm = TRUE) + 0.01)/10, 0, show_Depth))  #设置插值空间范围
    dimnames(df2$xyz.est$z) <- list(df2$xyz.est$x, df2$xyz.est$y)
    df2 <- reshape2::melt(df2$xyz.est$z, varnames = c('Lon', 'Depth'), value.name = 'Value') %>%
      mutate(Value = ifelse(Value < 0, 0, Value))
    #站位底深数据
    bot.depth <- plotdata %>%
      dplyr::select(Lon, Station, Bot.Depth) %>%
      mutate(Bot.Depth = as.numeric(Bot.Depth)) %>%
      distinct() %>%
      arrange(Lon)
    #线性拟合深度
    # 遍历数据集中相邻两个点的组合，并进行拟合
    fit_bot.depth <- data.frame()
    for (i in 1:(nrow(bot.depth) - 1)) {
      # 获取相邻两个点的数据
      data_subset <- bot.depth[c(i, i + 1), ]

      # 拟合数据，这里使用简单的线性模型示例
      model <- lm(Bot.Depth ~ Lon, data = data_subset)

      # 获取拟合线的斜率和截距
      slope <- coef(model)[2]
      intercept <- coef(model)[1]

      #提取df2中Lon在data_subset之间的数据
      df2_subset <- df2 %>%
        filter(Lon >= min(data_subset$Lon) & Lon <= max(data_subset$Lon)) %>%
        mutate(Bot.Depth = Lon*slope + intercept)
      #合并循环数据
      fit_bot.depth <- rbind(fit_bot.depth, df2_subset)
    }

    df3 <- df2 %>%
      left_join(fit_bot.depth, by = c("Lon", "Depth", "Value")) %>%
      mutate(Bot.Depth = ifelse(Lon <= min(bot.depth$Lon), bot.depth$Bot.Depth[which.min(bot.depth$Lon)], Bot.Depth)) %>%
      mutate(Bot.Depth = ifelse(Lon >= max(bot.depth$Lon), bot.depth$Bot.Depth[which.max(bot.depth$Lon)], Bot.Depth)) %>%
      mutate(Value = ifelse(Depth <= Bot.Depth, Value, NA))

    seq <- floor(log10(max(df3$Value, na.rm = TRUE))) - 1

    #绘图
    p <- ggplot() +
      geom_raster(data = df3, aes(x = Lon, y = Depth, fill = Value)) +  #使用插值绘制渲染图
      geom_contour(data = df3, aes(x = Lon, y = Depth, z = Value),
                   breaks = seq(0, ceiling(max(df3$Value, na.rm = TRUE)/(10^seq))*(10^seq), 10^(seq)),
                   linewidth = 0.5, colour = 'black', alpha = 0.2) +  #等密度线
      metR::geom_text_contour(data = df3, aes(x = Lon, y = Depth, z = Value),
                              size = text_size,family = family)+
      geom_point(data = plotdata, aes(x = Lon, y = Depth), shape = shape, size = point_size) +  #添加站位散点
      theme_bw(base_size = base_size) +
      scale_x_continuous(expand = expansion(mult = c(0, 0))) +
      scale_y_reverse(expand = expansion(mult = c(0, 0.04)),
                      limits = c(show_Depth, -7),
                      breaks = seq(0, show_Depth, 50)) +  #翻转 y 轴适用于水深
      labs(x = 'Lonitude', y = 'Depth (m)', fill = NULL,
           title = paste0(variable, " ", unique(plotdata$Unit)), family = family)+
      geom_text(data = plotdata, aes(x = Lon, y = -7, label = Station), size = 4, family = family) +
      geom_hline(yintercept = 0, linewidth = 0.5)+
      theme(
        legend.background = element_rect(fill = NA,color = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family = family),
        plot.title = element_text(hjust = 1, margin = margin(t = 10, r = 0, b = 0, l = 0)))



  } else if (unique(plotdata$Direction) == "Lat"){
    df2 <- mba.surf(plotdata[c('Lat', "Depth", "Value")], #多元插值
                    no.X = res_x, no.Y = res_y, extend = T,
                    b.box = c(floor(min(plotdata$Lat*10, na.rm = TRUE) - 0.01)/10, ceiling(max(plotdata$Lat*10, na.rm = TRUE) + 0.01)/10, 0, show_Depth))  #设置插值空间范围
    dimnames(df2$xyz.est$z) <- list(df2$xyz.est$x, df2$xyz.est$y)
    df2 <- reshape2::melt(df2$xyz.est$z, varnames = c('Lat', 'Depth'), value.name = 'Value') %>%
      mutate(Value = ifelse(Value < 0, 0, Value))
    #站位底深数据
    bot.depth <- plotdata %>%
      dplyr::select(Lat, Station, Bot.Depth) %>%
      mutate(Bot.Depth = as.numeric(Bot.Depth)) %>%
      distinct() %>%
      arrange(Lat)
    #线性拟合深度
    # 遍历数据集中相邻两个点的组合，并进行拟合
    fit_bot.depth <- data.frame()
    for (i in 1:(nrow(bot.depth) - 1)) {
      # 获取相邻两个点的数据
      data_subset <- bot.depth[c(i, i + 1), ]

      # 拟合数据，这里使用简单的线性模型示例
      model <- lm(Bot.Depth ~ Lat, data = data_subset)

      # 获取拟合线的斜率和截距
      slope <- coef(model)[2]
      intercept <- coef(model)[1]

      #提取df2中Lat在data_subset之间的数据
      df2_subset <- df2 %>%
        filter(Lat >= min(data_subset$Lat) & Lat <= max(data_subset$Lat)) %>%
        mutate(Bot.Depth = Lat*slope + intercept)
      #合并循环数据
      fit_bot.depth <- rbind(fit_bot.depth, df2_subset)
    }

    df3 <- df2 %>%
      left_join(fit_bot.depth, by = c("Lat", "Depth", "Value")) %>%
      mutate(Bot.Depth = ifelse(Lat <= min(bot.depth$Lat), bot.depth$Bot.Depth[which.min(bot.depth$Lat)], Bot.Depth)) %>%
      mutate(Bot.Depth = ifelse(Lat >= max(bot.depth$Lat), bot.depth$Bot.Depth[which.max(bot.depth$Lat)], Bot.Depth)) %>%
      mutate(Value = ifelse(Depth <= Bot.Depth, Value, NA))

    seq <- floor(log10(max(df3$Value, na.rm = TRUE))) - 1

    #绘图
    p <- ggplot() +
      geom_raster(data = df3, aes(x = Lat, y = Depth, fill = Value)) +  #使用插值绘制渲染图
      geom_contour(data = df3, aes(x = Lat, y = Depth, z = Value),
                   breaks = seq(0, ceiling(max(df3$Value, na.rm = TRUE)/(10^seq))*(10^seq), 10^(seq)),
                   linewidth = 0.5, colour = 'black', alpha = 0.2) +  #等密度线
      metR::geom_text_contour(data = df3, aes(x = Lat, y = Depth, z = Value),
                              size = text_size,family = family)+
      geom_point(data = plotdata, aes(x = Lat, y = Depth), shape = shape, size = point_size) +  #添加站位散点
      theme_bw(base_size = base_size) +
      scale_x_continuous(expand = expansion(mult = c(0, 0))) +
      scale_y_reverse(expand = expansion(mult = c(0, 0.04)),
                      limits = c(show_Depth, -7),
                      breaks = seq(0, show_Depth, 50)) +  #翻转 y 轴适用于水深
      labs(x = 'Latitude', y = 'Depth (m)', fill = NULL,
           title = paste0(variable, " ", unique(plotdata$Unit)), family = family)+
      geom_text(data = plotdata, aes(x = Lat, y = -7, label = Station), size = 4, family = family) +
      geom_hline(yintercept = 0, linewidth = 0.5)+
      theme(
        legend.background = element_rect(fill = NA,color = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family = family),
        plot.title = element_text(hjust = 1, margin = margin(t = 10, r = 0, b = 0, l = 0)))
  } else {
    stop("Invalid value for 'direction'. Must be 'Lon' or 'Lat'.")
  }

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
  ggsave(paste0(path, "/Section/", cruise, "_", variable, "_", unique(plotdata$Section_name),"断面分布图.", format), width = save_width, height = save_height, units = "in", dpi = 300)
  return(p)
}
