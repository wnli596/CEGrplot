% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ceg_layer_plot.R
\name{ceg_layer_plot}
\alias{ceg_layer_plot}
\title{ceg_layer_plot}
\usage{
ceg_layer_plot(
  data,
  lon_min = 100,
  lon_max = 170,
  lat_min = 0,
  lat_max = 60,
  brief = FALSE,
  variable = "TChla",
  layer = "Surface",
  colorbar = "auto",
  palette = colorRampPalette(c("#00008F", "#0000FF", "#004FFF", "#00FFFF", "#80FF80",
    "#FFFF00", "#FFAF00", "#FF0000", "#800000")),
  mincolor,
  maxcolor,
  shape = 16,
  point_size = 6,
  base_size = 16,
  family = "sans",
  save_width = 10,
  save_height = 8,
  format = "pdf"
)
}
\arguments{
\item{data}{A data.frame. including Lon, Lat, Station, Variable, Value, unit, Layer}

\item{lon_min}{A number. Default is 100}

\item{lon_max}{A number. Default is 125}

\item{lat_min}{A number. Default is 0}

\item{lat_max}{A number. Default is 25}

\item{brief}{logical. If TRUE/FALSE plotting a brief/colorful map, Default is FALSE}

\item{variable}{character. unique(data$Variable). Default is "TChla"}

\item{layer}{character. unique(data$Layer). Default is "Surface"}

\item{colorbar}{character."auto" or "manual"}

\item{palette}{A color vector for data$Value. Default is colorRampPalette(c("#00008F","#0000FF","#004FFF", "#00FFFF", "#80FF80", "#FFFF00", "#FFAF00","#FF0000","#800000"))}

\item{mincolor}{A number. If colorbar = "manual", need a mincolor number}

\item{maxcolor}{A number. If colorbar = "manual", need a maxcolor number}

\item{shape}{A number. Shape for Station points, Default is 16}

\item{point_size}{A number. Size for Station points, Default is 6}

\item{base_size}{A number. Base size, default is 16}

\item{family}{character. "sans"/ Arial, "serif"/Time new roman and any other fronts, Default is "sans"}

\item{save_width}{A number. width for save figures, Default is 10}

\item{save_height}{A number. height for save figures, Default is 8}

\item{format}{character. file type for save figures, Default is "pdf"}
}
\value{
A figure
}
\description{
ceg_layer_plot
}
\examples{
NULL
}
\author{
Weinan Li
}
