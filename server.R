source("MNTExercise.R")

shinyServer(function(input, output) {
  # Base Code
  get.array.image<-reactive({
    img.names[[input$image_name]]()
  })
  get.raw.image<-reactive({
    im.to.df(get.array.image())
  })
  
  ## Filtering
  get.filtered.image<-reactive({
    img<-get.array.image()
    filt<-filter.funs[[input$filter_name]]
    im.to.df(filt(img,input$filter_size,input$filter_sigma/10))
  })
  output$filterPlot <- renderPlot({
    dual.image<-rbind(cbind(get.raw.image(),ftype="Original"),
                      cbind(get.filtered.image(),ftype=input$filter_name))
    print(
      show.img(dual.image)+
        facet_wrap(~ftype)
    )
  })
  output$fhistPlot <- renderPlot({
    gam.data<-rbind(
      cbind(get.filtered.image(),ctype="Filtered"),
      cbind(get.raw.image(),ctype="Original")
    )
    print(
      get.hist.comparison(gam.data,"Filtering")
    )
  })
  
  ## Threshold
  output$threshPlot <- renderPlot({
    print(
      show.thresh.img(get.filtered.image(),input$threshold))
  })
  
  output$thistPlot <- renderPlot({
    gam.data<-rbind(
      cbind(get.filtered.image(),ctype="Filtered"),
      cbind(get.raw.image(),ctype="Original")
    )
    print(
      get.hist.comparison(gam.data,"Step")+
        geom_vline(aes(color="Threshold"),xintercept=input$threshold)
    )
  })
  
  ## Metrics
  #output$metrics_summary <-renderTable()
  
  
})