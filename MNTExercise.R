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
  out.img<-blob.img
  attr(out.img, "type") <- "grey"
  out.img
}
# Load filters
filter.funs<-new.env()
filter.funs$None<-function(img,size,sigma) img
filter.funs$Median<-function(img,size,sigma) medianFilter(img,size)
filter.funs$Gaussian<-function(img,size,sigma) gblur(img, 10*sigma, 10*size)
#filter.funs$Disc<-function(img,size,sigma) filter2(img,makeBrush(size, "disc", sigma=sigma, step=FALSE))/size
# Utils
# functions for converting images back and forth
im.to.df<-function(in.img) {
  out.im<-expand.grid(x=1:nrow(in.img),y=1:ncol(in.img))
  out.im$val<-as.vector(in.img)
  out.im
}



## Show images
show.img<-function(im.data) {
  ggplot(im.data,aes(x=x,y=y))+
    geom_raster(aes(fill=val))+
    scale_fill_gradient(low="black",high="white")+
    labs(fill="Intensity",color="")+
    theme_bw(20)
}
show.thresh.img<-function(im.data,thresh.val) {
  im.data$thresh<-(im.data$val<=thresh.val)
  show.img(im.data)+
    geom_tile(data=subset(im.data,thresh),aes(color="Below\nThreshold"),fill="red",alpha=0.3)
}
get.hist.comparison<-function(dataA,colorName) {
  ggplot(dataA,aes(x=val))+
    geom_density(aes(color=ctype))+
    labs(title="Image Intensity Histogram",color=colorName)+
    theme_bw(20)
}