set.colorbarp <- function(cbxp,cbyp,total.reg=T,...){
  if(any(cbxp > 1)) cbxp <- cbxp/100
  if(any(cbyp > 1)) cbyp <- cbyp/100
  
  if(total.reg){
    opar <- par()
    par(new=T,mar=c(0,0,0,0),fig=c(0,1,0,1),xaxs='i',yaxs='i')
  }else{
    par(new=T,mar=c(0,0,0,0),xaxs='i',yaxs='i')
  }
  empty.plot(xlim=c(0,1),ylim=c(0,1))
  
  set.colorbar(cbx=cbxp,cby=cbyp,...)
  if(total.reg) suppressWarnings(par(opar))
}