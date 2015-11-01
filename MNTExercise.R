# basic libraries
library(shiny)
require(ggplot2)
require(plyr)
require(EBImage)
require(markdown)
#require(tiff)
library(Cairo)
options(shiny.usecairo=T)

## Setup
# Load in data
img.names<-new.env()
img.names$Cells<-function(...) {
  load("workspace.RData")
  out.img<-cell.img
  attr(out.img, "type") <- "grey"
  out.img
}
img.names$Blobs<-function(...) {
  load("workspace.RData")
  out.img<-1-blob.img
  attr(out.img, "type") <- "grey"
  out.img
}
img.names$Dots<-function(...) {
  load("workspace.RData")
  out.img<-dot.img
  attr(out.img, "type") <- "grey"
  out.img
}
img.names$Tree<-function(...) {
  load("workspace.RData")
  out.img<-t(tree.img)
  attr(out.img, "type") <- "grey"
  out.img
}
img.names$Breast<-function(...) {
  load("workspace.RData")
  out.img<-calc.img
  attr(out.img, "type") <- "grey"
  out.img
}
img.names$Cross<-function(...) {
  nx<-80
  ny<-80
  cross.im<-expand.grid(x=c(-nx:nx)/nx*2*pi,y=c(-ny:ny)/ny*2*pi)
  cross.im<-cbind(cross.im,
             #val=(1.5*with(cross.im,abs(cos(x*y))/(abs(x*y)+(3*pi/nx)))+
              #0.5*runif(nrow(cross.im))))
             val=with(cross.im,1.5*((abs(x)<1) | (abs(y)<1))+
              0.5*runif(nrow(cross.im))-0.5))
  cross.im<-df.to.im(cross.im,inv=TRUE)
}

# Load filters
filter.funs<-new.env()
filter.funs$None<-function(img,size,sigma) img
filter.funs$Gaussian<-function(img,size,sigma) gblur(img, 10*sigma, 10*size)
filter.funs$Median<-function(img,size,sigma) medianFilter(img, size)
filter.funs$EdgeX<-function(img,size,sigma) {
  if(size==3) {
  laplacian<-matrix(
    c(sigma, 0, -sigma, sigma, 0, -sigma, sigma, 0, -sigma),
    nrow=3,
    ncol=3,
    byrow = TRUE)}
  if(size==5) {
    laplacian<-matrix(
      c(sigma, sigma/2, 0, -sigma/2, -sigma, sigma, sigma/2, 0, -sigma/2, -sigma, sigma, sigma/2, 0, -sigma/2, -sigma),
      nrow=5,
      ncol=5,
      byrow = TRUE)}
  if(size==7) {
    laplacian<-matrix(
      c(sigma, 2*sigma/3, sigma/3, 0, -2*sigma/3, -sigma/3, -sigma, 
        sigma, 2*sigma/3, sigma/3, 0, -2*sigma/3, -sigma/3, -sigma, 
        sigma, 2*sigma/3, sigma/3, 0, -2*sigma/3, -sigma/3, -sigma),
      nrow=7,
      ncol=7,
      byrow = TRUE)}
  if(size==9) {
    laplacian<-matrix(
      c(sigma, 3*sigma/4, sigma/2, sigma/4, 0, -3*sigma/4, -sigma/2, -sigma/4, -sigma, 
        sigma, 3*sigma/4, sigma/2, sigma/4, 0, -3*sigma/4, -sigma/2, -sigma/4, -sigma, 
        sigma, 3*sigma/4, sigma/2, sigma/4, 0, -3*sigma/4, -sigma/2, -sigma/4, -sigma),
      nrow=9,
      ncol=9,
      byrow = TRUE)}
  out.im<-filter2(img,t(laplacian))
  out.im<-normalize(out.im)
}
filter.funs$EdgeY<-function(img,size,sigma) {
  if(size==3) {
    laplacian<-matrix(
      c(sigma, 0, -sigma, sigma, 0, -sigma, sigma, 0, -sigma),
      nrow=3,
      ncol=3,
      byrow = TRUE)}
  if(size==5) {
    laplacian<-matrix(
      c(sigma, sigma/2, 0, -sigma/2, -sigma, sigma, sigma/2, 0, -sigma/2, -sigma, sigma, sigma/2, 0, -sigma/2, -sigma),
      nrow=5,
      ncol=5,
      byrow = TRUE)}
  if(size==7) {
    laplacian<-matrix(
      c(sigma, 2*sigma/3, sigma/3, 0, -2*sigma/3, -sigma/3, -sigma, 
        sigma, 2*sigma/3, sigma/3, 0, -2*sigma/3, -sigma/3, -sigma, 
        sigma, 2*sigma/3, sigma/3, 0, -2*sigma/3, -sigma/3, -sigma),
      nrow=7,
      ncol=7,
      byrow = TRUE)}
  if(size==9) {
    laplacian<-matrix(
      c(sigma, 3*sigma/4, sigma/2, sigma/4, 0, -3*sigma/4, -sigma/2, -sigma/4, -sigma, 
        sigma, 3*sigma/4, sigma/2, sigma/4, 0, -3*sigma/4, -sigma/2, -sigma/4, -sigma, 
        sigma, 3*sigma/4, sigma/2, sigma/4, 0, -3*sigma/4, -sigma/2, -sigma/4, -sigma),
      nrow=9,
      ncol=9,
      byrow = TRUE)}
  out.im<-filter2(img,laplacian)
  out.im<-normalize(out.im)
}
filter.funs$LoG<-function(img,size,sigma) {
  if(size==3) {
    lapGauss<-matrix(
      c(0, 1, 0, 1, -4, 1, 0, 1, 0),
      nrow=3,
      ncol=3,
      byrow = TRUE)}
  if(size==5) {
    lapGauss<-matrix(
      c(0, 0, 1, 0, 0,
        0, 1, 2, 1, 0,
        1, 2, -16, 2, 1,
        0, 1, 2, 1, 0,
        0, 0, 1, 0, 0),
      nrow=5,
      ncol=5,
      byrow = TRUE)}
  if(size==7) {
    lapGauss<-matrix(
      c(0, 0, 1, 1, 1, 0, 0,
        0, 1, 3, 3, 3, 1, 0,
        1, 3, 0, -7, 0, 3, 1,
        1, 3, -7, -24, -7, 3, 1,
        1, 3, 0, -7, 0, 3, 1,
        0, 1, 3, 3, 3, 1, 0,
        0, 0, 1, 1, 1, 0, 0),
      nrow=7,
      ncol=7,
      byrow = TRUE)}
  if(size==9) {
    lapGauss<-matrix(
      c(0, 0, 3, 2, 2, 2, 3, 0, 0,
        0, 2, 3, 5, 5, 5, 3, 2, 0,
        3, 3, 5, 3, 0, 3, 5, 3, 3,
        2, 5, 3, -12, -23, -12, 3, 5, 2,
        2, 5, 0, -23, -40, -23, 0, 5, 2,
        2, 5, 3, -12, -23, -12, 3, 5, 2,
        3, 3, 5, 3, 0, 3, 5, 3, 3,
        0, 2, 3, 5, 5, 5, 3, 2, 0,
        0, 0, 3, 2, 2, 2, 3, 0, 0),
      nrow=9,
      ncol=9,
      byrow = TRUE)}
  out.im<-filter2(img,sigma*lapGauss/5)
  out.im<-normalize(out.im)
}


# Utils
# functions for converting images back and forth
im.to.df<-function(in.img) {
  out.im<-expand.grid(x=1:nrow(in.img),y=1:ncol(in.img))
  out.im$val<-as.vector(in.img)
  out.im
}
df.to.im<-function(in.df,val.col="val",inv=F) {
  in.vals<-in.df[[val.col]]
  if(class(in.vals[1])=="logical") in.vals<-as.integer(in.vals*1)
  if(inv) in.vals<-1-in.vals
  out.mat<-matrix(in.vals,nrow=length(unique(in.df$x)),byrow=F)
  attr(out.mat,"type")<-"grey"
  out.mat
}



## Show images
show.img<-function(im.data) {
  ggplot(im.data,aes(x=x,y=y))+
    geom_raster(aes(fill=val))+
    scale_fill_gradient(low="black",high="white")+
    labs(fill="Intensity",color="")+
    theme_bw(20)
}
show.thresh.img<-function(im.data,thresh.val,thres.invert=FALSE) {
  if(thres.invert==FALSE) {
    im.data$thresh<-(im.data$val<=thresh.val)
    show.img(im.data)+
      geom_tile(data=subset(im.data,thresh),aes(color="Below\nThreshold"),fill="red",alpha=0.3)
  } else {
    im.data$thresh<-(im.data$val>=thresh.val)
    show.img(im.data)+
      geom_tile(data=subset(im.data,thresh),aes(color="Above\nThreshold"),fill="red",alpha=0.3)
  }
}
get.hist.comparison<-function(dataA,colorName) {
  ggplot(dataA,aes(x=val))+
    geom_density(aes(color=ctype))+
    labs(title="Image Intensity Histogram",color=colorName)+
    theme_bw(20)
}

## Metrics Table
get.metrics.table<-function(im.data,thresh.val,thres.invert=FALSE) {
  if(thres.invert==FALSE) {
    thres.im<-(im.data$val<=thresh.val)
  } else {
    thres.im<-(im.data$val>=thresh.val)
  }
  # get x and y dim, than reshape into image
  thres.im.mat<-matrix(thres.im,nrow=length(unique(im.data$x)),byrow=F)
  labeled.im<-bwlabel(thres.im.mat)
  metrics.table.basic<-computeFeatures.basic(labeled.im,df.to.im(im.data))
  metrics.table.shape<-computeFeatures.shape(labeled.im)
  metrics.table.moment<-computeFeatures.moment(labeled.im,df.to.im(im.data))
  metrics.table<-cbind(metrics.table.basic,metrics.table.shape,metrics.table.moment)
  # Inverse eccentricity to get circularity
  metrics.table[,18]<-1-metrics.table[,18]
  `colnames<-`(metrics.table, c("Intensity (mean)","Intensity (sdv)","Total Intensity","1% Quantile","5% Quantile","50% QUantile","95% Quantile","99% Quantile","Area","Perimeter","Radius (mean)","Radius (sdv)","Radius (min)","Radius (max)","Cntr. Mass (x)","Cntr. Mass (y)","Major Axis","Circularity","Angle (rad)"))
  #metrics.table[, !colnames(metrics.table) %in% c("1% Quantile","5% Quantile","50% QUantile","95% Quantile","99% Quantile")]
}

get.metrics.summary<-function(im.data,thresh.val,thres.invert=FALSE) {
  all.metrices<-get.metrics.table(im.data,thresh.val,thres.invert)
  # Calc data
  obj.int.mean<-mean(all.metrices[,1])
  obj.int.sdv<-sd(all.metrices[,1])
  obj.size.mean<-mean(all.metrices[,9])
  obj.size.sdv<-sd(all.metrices[,9])
  obj.per.mean<-mean(all.metrices[,10])
  obj.per.sdv<-sd(all.metrices[,10])
  obj.rad.mean<-mean(all.metrices[,11])
  obj.rad.sdv<-sd(all.metrices[,11])
  obj.circ.mean<-mean(all.metrices[,18])
  obj.circ.sdv<-sd(all.metrices[,18])
  obj.ang.mean<-mean(all.metrices[,19])
  obj.ang.sdv<-sd(all.metrices[,19])
  summary.data<-cbind(obj.int.mean,obj.int.sdv,obj.size.mean,obj.size.sdv,obj.per.mean,obj.per.sdv,obj.rad.mean,obj.rad.sdv,obj.circ.mean,obj.circ.sdv,obj.ang.mean,obj.ang.sdv)
  tab<-matrix(summary.data,nrow=1)
  colnames(tab) <- c("Intensity (mean)","Intensity (sdv)","Size (mean)","Size (sdv)","Perimeter (mean)","Perimeter (sdv)","Radius (mean)","Radius (sdv)","Circularity (mean)","Circularity (sdv)","Angle (mean)","Angle (sdv)")
  tab
}





