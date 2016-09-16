\name{plotmap}
\alias{plotmap}
\title{
plots landmask of a defined region}
\description{
plots the landmask of a region defined by a \code{region}-key word, georgraphical coordinates (\code{lon}gitude and \code{lat}itude), a raster- or \link[raster]{extent}-object. See \link{add.region} to add and save new region definitions. Attention! Unlike \link{add.region}, \code{plotmap} does not include colorbar placement (see: \link{set.colorbar})
}
\usage{
plotmap(region=v_area, lon, lat, center='E', add=F, 
        grid=T, grid.res, resolution=0, 
        main, axeslabels=T, ticklabels=T, cex.lab=0.8, cex.ticks=0.8, 
        fill.land=T, col.land="grey", col.bg=NA, border='black', bwd=1, las=1, v_area
        )
}
\arguments{
  \item{region, v_area}{
Character string identifying regions predefined by the \link{region_definitions}-dataset, Raster* or Extent object (corresponds to \code{v_area} of the \link{v}-function). If missing, region is derived from geographical coordinates, denoted by lat and lon. See \link{add.region} to define new region definitions and \link{delete.region} to delete unproper region definitions.
}
  \item{lon}{
Vector returning longitude coordinates of the area to be plotted.
}
  \item{lat}{
Vector returning latitude coordinates of the area to be plotted.
}
  \item{add}{
whether the a the landmask should be added to an existent figure (default is \code{FALSE})
}
  \item{main}{
title to be plotted
}
  \item{axeslabels}{
whether axis-labels (longitude and latitude) should be added to the axes (default is \code{TRUE}). Can be a single value or a vector of size two.
}
  \item{ticklabels}{
whether tick-labels should be added to the axes (default is \code{TRUE}). Can be a single value or a vector.
}
  \item{cex.lab}{
font size of axis labels
}
  \item{cex.ticks}{
font size of tick labels
}
  \item{grid}{
whether a grid should be plotted (default is \code{TRUE})
}
  \item{grid.res}{
resolution of the grid, in degrees (default is is derived from the region extent)
}
\item{resolution}{
number that specifies the resolution with which to draw the map. Resolution 0 is the full resolution of the database [default]. Otherwise, just before polylines are plotted they are thinned: roughly speaking, successive points on the polyline that are within resolution device pixels of one another are collapsed to a single point (see the Reference for further details). Thinning is not performed if plot = FALSE or when polygons are drawn (fill = TRUE or database is a list of polygons).
}
  \item{bwd}{
width is of the axes bars (default is 1)
}
  \item{fill.land}{
whether the a the landmask should be filled by a color (default is \code{TRUE})
}
  \item{col.land}{
fill color of the landmask to be plotted (default is grey)
}
  \item{col.bg}{
background color (ocean) to be plotted (default is NA)
}
  \item{border}{
country border color of the landmask to be plotted (default is black)
}
  \item{center}{
whether the region should be centered on longitudes E or W (default is 'E')
}

\item{las}{
numeric in {0,1,2,3}; the style of axis labels.

0:
always parallel to the axis,

1:
always horizontal [default],

2:
always perpendicular to the axis,

3:
always vertical
}

}
\details{
\code{plotmap} uses the \code{maps} and \code{maptools} functions to plot the landmask.
}
\seealso{ \code{ \link{v}, \link{regions}} } 


\author{
Robert K. Bauer
}

\examples{
## Example 1: plot landmask of the Mediterranean Sea
par(mfrow=c(2, 1))
plotmap('med4', main="Mediterranean Sea")
plotmap('med4', main="Mediterranean Sea", bwd=2, border='grey', grid=FALSE)


## Example 2: plot landmask of tropical Pacifc
plotmap("tp")


## Example 3: plot landmask of the Baltic Sea defined by longitude and latidtue coordinates
lon <- c(9, 31)
lat <- c(53.5, 66)
plotmap(lon=lon, lat=lat, main="Baltic Sea", grid.res=5, bwd=2)


## Example 4: plot landmask of the Baltic Sea defined by an extent- or raster-object
library('raster')
ext <- extent(lon, lat)
plotmap(ext, main="Baltic Sea") # extent-object

r <- raster(ext)
plotmap(r, main="Baltic Sea") # raster-object

## Example 5: centering the region
plotmap(lon=c(110, -110), lat=c(-50, 10)) # default center on lon E
plotmap(lon=c(110, -110), lat=c(-50, 10), center="W")
plotmap('tp')

## Example 6: plot bathymetry and topography of the western Mediterranean Sea
#get.bathy("medw4",visualize=T,terrain=T,res=3)
#get.bathy("medw4",visualize=T,terrain=F,res=3,levels=c(200,2000)) # show contours
#get.bathy("lion",visualize=T,terrain=F,res=3,levels=c(200,2000),v_image=F) # show only contours

}

