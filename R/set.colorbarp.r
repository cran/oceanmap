set.colorbarp <- function(cbxp,cbyp,total.reg=T,year_bar=F,pal="jet",...){
  if(year_bar){
    if(missing(pal)) pal <- "year.jet"
    ## plots month axes on indexed dates (1:366, or > 366 if more then one year is selected)
    lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
    d <- as.Date(paste0(0,"-",1:12,"-01"))
    d <- c(d,as.Date(paste0(0,"-12-31")))
    dates <- as.Date(d[1]:tail(d,1),origin="1970-01-01")
    ydd <- d <- as.Date(d,origin="1970-01-01")
    months <- unique(.date2month(d))
    
    ydays <- lubridate::yday(dates); #ydays <- ydays[ydays %in% ydd]
    xticks <- which(dates %in% d & dates %in% ydd)
    
    set.colorbar(cbxp = cbxp, cbyp=cbyp, total.reg = total.reg,pal=pal,ticks = xticks,labels = rep("",length(xticks)))
    print(xticks)
    print(ydays)
    d2 <- lubridate::yday(d)
    xticks2 <- (d2[1:(length(d2)-1)]+diff(d2)/2)[which(d %in% ydd)]
    xticks2 <- xticks2[which(!is.na(xticks2))]
    xticks2.labels <- rep("",length(ydays))
    xticks2.labels[round(xticks2)] <- format(.yday2date(xticks2,0),"%b")
    
    set.colorbar(cbxp = cbxp, cbyp=cbyp, total.reg = total.reg,pal=pal,ticks = ydays,labels = xticks2.labels, cb.ticks.length = 0, cb.ticks.lwd=0, ...)
  }else{
    
    if(any(cbxp > 1)) cbxp <- cbxp/100
    if(any(cbyp > 1)) cbyp <- cbyp/100
    
    if(total.reg){
      opar <- par()
      par(new=T,mar=c(0,0,0,0),fig=c(0,1,0,1),xaxs='i',yaxs='i')
    }else{
      par(new=T,mar=c(0,0,0,0),xaxs='i',yaxs='i')
    }
    empty.plot(xlim=c(0,1),ylim=c(0,1))
    
    set.colorbar(cbx=cbxp,cby=cbyp,pal=pal,...)
    if(total.reg) suppressWarnings(par(opar))
  }
}


.date2month <- function(x,numeric=T,abbreviate=T){
  x <- .fact2Date(x)
  
  if(numeric){
    output <- as.numeric(format(as.Date(x),"%m"))
  }else{
    lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
    output <- format(as.Date(x),"%B")
    if(abbreviate) output <- format(as.Date(x),"%b")
  }
  return(output)
}

.yday2date <- function(ydays,year){
  dates <- (as.Date(paste0(year,'-01-01'))-1)+ydays
  return(dates)
}

.fact2Date <- function(x) as.Date(as.character(x))

