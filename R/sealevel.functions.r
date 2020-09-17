#' Get and plot sea level data and determine rate of change in level from PSMSL database
#'
#' @param id The tide station id number from psmsl
#' @param y1 the first year of data you want to include in the plot
#' @param y2 the breakpoint year you want to force to calculate a new sea level change rate from
#' @description  Queries the PSMSL database (psmsl.org) for monthly RLR data from one of its stations that you specify with id. This
#'               will also fit a linear regression over all the data and calculate the rate of change in sea level (mm/y) from y1 or
#'               or the earliest data year available if y1 is before that year. It will also plot and calculate the rate of change in
#'               sea level from y2 until the end of the time series. Finally, it will make a third plot where a segmented regression
#'               is fitted to the data from y1 until the end of the data series and find the most appropriate single breakpoint
#'               calculating rate of change in sea level before and after the breakpoint. The segmented regression is fitted using the
#'               package segmented.
#' @author Daniel Duplisea
#' @export
#' @examples
sealevel.f= function(id, y1=1920, y2=2000){
  sealevel= fread(paste0("https://www.psmsl.org/data/obtaining/rlr.monthly.data/",id,".rlrdata"))[,1:2]
  #https://www.psmsl.org/data/obtaining/rlr.monthly.data/96.rlrdata
  names(sealevel)= c("year","height")
  #sealevel$year= floor(sealevel$year)
  sealevel2= sealevel[sealevel$year>=y1 & sealevel$height>0,]
  lims= limits(sealevel2$height,100)
  rise= lm(sealevel2$height~sealevel2$year)
  y1.lab= floor(min(sealevel2$year))

  par(mfcol=c(3,1),mar=c(4,4,3,1),omi=c(.5,.1,.1,.1))
  plot(sealevel2,type="p",xlab="Year",ylab="Sea level (mm)",main= id,pch=20,ylim=lims)
  abline(coef(rise),lwd=2)
  legend("topleft",paste("Sea level rise =",legend= round(coef(rise)[2],1),"mm/y since",y1.lab),bty="n",cex=.8)
  sealevel3= sealevel[sealevel$year>=y2 & sealevel$height>0,]
  rise= lm(sealevel3$height~sealevel3$year)
  y2.lab= floor(min(sealevel3$year))
  plot(sealevel3,type="p",xlab="Year",ylab="Sea level (mm)",main="",pch=20,ylim=lims)
  abline(coef(rise),lwd=2)
  legend("topleft",paste("Sea level rise =",legend= round(coef(rise)[2],1),"mm/y since",y2.lab),bty="n",cex=.8)


  rise.segreg= lm(height~year, data=sealevel2)
  bp.start=mean(range(sealevel2$year))
  fit.seg<-segmented(rise.segreg, seg.Z=~year, psi=bp.start)
  plot(sealevel2,type="p",xlab="Year",ylab="Sea level (mm)",main="",pch=20,ylim=lims)
  lines(sealevel2$year,predict(fit.seg),col="red",lwd=3)
  slrise1= round(slope(fit.seg)[1]$year[1,1],1)
  slrise2= round(slope(fit.seg)[1]$year[2,1],1)
  ci=confint(fit.seg)
  abline(v=ci,col="grey",lty=c(1,2,2))
  legend("topleft",c(paste("Sea level rise 1 =",legend= slrise1, "mm/y"),paste("Sea level rise 2 =",legend= slrise2, "mm/y")),bty="n",cex=.8)
}

#' Find variables with a keyword or phrase
#' @param search.term a term to search, e.g. "halifax", "tokyo"
#' @param plot if true it will plot the locations of all the tide stations and the ones matching the search criteria in red
#' @description  Does a fuzzy search for the term in the station list and returns all the information from rows in the station table
#'               that contains the term. Case insensitive.
#' @author Daniel Duplisea
#' @export
#' @examples find.stations.f("halifax")
#'        find.stations.f("MumBaI",T)
find.stations.f= function(search.term, plot=F){
  vars4= stations[grep(search.term, stations$location, ignore.case=T),]
  if (plot){
    plot(stations$long,stations$lat,pch=20,xlab="longitude",ylab="latitude")
    points(vars4$long,vars4$lat,pch=20,col="red")
  }
  vars4
}



#' Outer decimal bounding limits for plotting
#'
#' @param vector a vector of numbers to find the limits
#' @param places number of digits. you need to set the level of rounding by setting the places by factors of 10. So if 100 it will take the bottom limit to the lowest value in the series to closest 100 value of the lower side and vice versa
#' @description  Finds a bounding limit for plotting a vector that is rounded outside the data range
#' @author Daniel Duplisea
#' @export
#' @examples
limits= function(vector,places=100){
  min.val= floor(min(vector)/places)*places
  max.val= ceiling(max(vector)/places)*places
  ranges= c(min.val,max.val)
  ranges
}
