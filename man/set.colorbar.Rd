\name{set.colorbar}
\alias{set.colorbar}
\alias{set.colorbarp}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adds colorbar to an extisting plot device
}
\description{
Adds colorbar to an extisting plot device. If position vectors are not provided, the user will be asked to define the colorbar placement by the mouse cursor.
}
\usage{
set.colorbar(cbx, cby, pal='jet', zlim, ticks=1:10, labels=ticks, gradient, 
             oticks, cb.title="", cb.xlab="", font=1, cex=1, 
             cex.cb.title=0.9, cex.cb.xlab=0.8, cex.cb.ticks=0.7, cb.ticks.srt=90, 
             cb.ticks.length, cb.ticks.ypos, cb.ticks.lwd=1, 
             integer=F, cb.xlab.line=0, ...)
             
set.colorbarp(cbxp, cbyp, total.reg, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cbx, cby}{
 (\code{set.colorparp}-arguments) 
 the horizontal and vertical limits of the colorbar. If missing, the user will be asked for manual colorbar placement.
}
  \item{cbxp, cbyp}{
 (\code{set.colorparp}-arguments)
 the horizontal and vertical limits of the colorbar in percent. If missing, the user will be asked for manual colorbar placement.
}
  \item{total.reg}{
(\code{set.colorparp}-argument)
if colorbar placement is relative to current subplot or entire figure region.
}

  \item{pal}{
color map to be plotted (default is \code{'jet'} for direct calls). See \link{cmap} for available color maps and \link{parameter_definitions} for predefined colormaps of different parameters (for internal function calls, e.g. \link{v}))
}
  \item{zlim}{
  (optional) value limits of the color bar. Overwrites ticks if ticks are povided.
}
  \item{ticks}{
the points at which tick-marks are to be drawn (default is 1:10). Non-finite (infinite, NaN or NA) values are omitted.
Gets verwritten by zlim if povided.
}
  \item{labels}{
character or expression vector of labels to be placed at the tickpoints. (default equals \code{ticks}-values.)
}
  \item{gradient}{
whether to have a horizontal (x) or vertical (y) color gradient.
}
  \item{oticks}{
the margin where to put the colorbar ticks relative to the colorbar rectangle (\code{'l'} left, \code{'r'} right and \code{'b'} for bottom;
}
  \item{cb.title}{
character string indicating the title of the colorbar (default is set to date information/empty string if date information is missing.)
}
  \item{cb.xlab}{
character string indicating the x-axis label of the colorbar.
}
\item{font}{
  Integer specifying font to use for text.
  1=plain [default], 2=bold, 3=italic, 4=bold italic, 5=symbol 
}
\item{cb.xlab.line}{
line of x-axis colorbar label
}


\item{cex, cex.cb.title, cex.cb.xlab, cex.cb.ticks}{
  \emph{cex}: general font size, used as reference for colorbar labels and title
  \emph{cex.cb.xlab}: font size of the x-axis label of the colorbar
  \emph{cex.cb.title}: font size of the title of the colorbar
}

\item{cb.ticks.srt, cb.ticks.length, cb.ticks.ypos,  cb.ticks.lwd}{
 rotation, length, relative y-position and line width of colorbar ticks 
}

\item{integer}{
(default is \code{FALSE}).
}

 \item{\dots}{
additional arguments to be passed to \link[graphics]{text} or \code{set.colorpar}
}
}
\details{
\code{set.colobar} adds a colorbar to the current plot device. If colorbar positions are missing (\code{cbx}, \code{cby}), the user will be asked for manual placement. \code{ticks} and tick-\code{labels} should correspond to \code{zlim}-values of the plot. \code{pal} defines the colormap and should equal \code{col} of the selected plot.
}
\value{
a list of colorbar definition vectors: \code{oticks, gradient, cbx} and \code{cby}. See function argmuments for more details.
}

\author{
Robert K. Bauer
}

\keyword{image plot}


\examples{
## Example 1: matrix 
vals <- rnorm(1000, 500, 100)
vals <- vals[order(vals)]
hist(vals)
z <- matrix(vals, 100, 100)
zlim <- c(0, 1000)
par(mar=rep(5, 4)) # add space for colorbar
data(cmap)
image(z, col=cmap$jet, zlim=zlim)
ticks <- seq(zlim[1], zlim[2], 100)

set.colorbar(cbx=c(0, 1), cby=c(-.16, -.248), ticks=ticks) # bottom
set.colorbar(cby=c(0, 1), cbx=c(-.16, -.088), ticks=ticks) # left
set.colorbar(cbx=c(0, 1), cby=c(1.088, 1.16), ticks=ticks) # top
set.colorbar(cby=c(0, 1), cbx=c(1.088, 1.16), ticks=ticks) # right 

## manual placement calling the widget
# cb <- set.colorbar(ticks=ticks)

## set.colorbarp (percentage call):
par(mar=c(5, 5, 5, 5))
plot(1)
set.colorbarp(cbxp = c(0.9, 0.925), cbyp=c(0.2, 0.85))

## multiple plots
setwd(system.file("test_files", package="oceanmap"))
gz.file <- Sys.glob('*.gz')[1]
par(mfrow=c(2, 2), mar=c(5, 4, 3, 7))
for(i in 1:4) v(gz.file, v_area = "medw4", subplot = TRUE, show.colorbar = FALSE)
set.colorbarp(cbxp = c(0.9, 0.925), cbyp=c(0.2, 0.85), total.reg = TRUE, 
              pal="red", zlim=c(0, 20), cb.title="front prob (percent)")
set.colorbarp(cbxp = c(0.7, 0.72), cbyp=c(0.2, 0.85), total.reg = FALSE, 
              pal="red", zlim=c(0, 20))

## reuse definition
# image(z, col=cmap$jet, zlim=zlim)
# set.colorbar(cbx=cb$cbx, cby=cb$cby, ticks=ticks)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{widget}
\keyword{colorbar}% __ONLY ONE__ keyword per line
