
#######################################################
###  Server    ########################################
#######################################################

cols <- c(
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
  "#A6761D", "#666666", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3",
  "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"
)

server <- shinyServer(function(input, output, session) {

values <- reactiveValues(dem_out=NULL, country=NULL, pop_out=NULL, year0 = NULL, year1 = NULL, year2 = NULL)
  
  observeEvent(c(input$loc, input$range[1], input$range[2]),{
    if(!is.null(input$loc)){
      is         = input$loc
      y0         = input$range[1]
      y2         = input$range[2]
      y1         = 0.5*(y0 + y2)
      
##############################################################################################################################    
###   Projection    ##############################################################################################
##############################################################################################################################    

   Projection            <- project_pop(is, y0, y2, wpp_input)
   
   values$dem_out        <- Projection %>%
                            add_lt(., is, y0, y2) %>%
                            add_obs(., obs_wpp, is, y0, y2)
   values$pop_out        <- Projection$population
   values$country        <- is
   values$year0          <- y0
   values$year1          <- y1
   values$year2          <- y2
    }
  }) 

  # The demography tab outputs
  
  output$deathp <- renderHighchart ({
    if(is.null(values)) return(NULL)
    y   <- values[["dem_out"]]
    loc <- values[["country"]]  
    
    y %>% 
      hchart(., 
             type = "spline", 
             hcaes(x = year, 
                   y = deaths_both,
                   group = group)) %>%
      hc_title(text = paste(loc," total deaths")) %>% 
      hc_yAxis(title = list(text = "Deaths"), softMin=0) %>% 
      hc_xAxis(type = "category", title = list(text = "Year")) %>%       
      hc_tooltip(crosshairs = TRUE, backgroundColor = "transparent", shared = T) %>% 
      hc_colors(c("#e5b13a", "#4AA942", "#4bd5ee")) %>% hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$birthp <- renderHighchart ({
    if(is.null(values)) return(NULL)
    y   <- values[["dem_out"]]
    loc <- values[["country"]]  
    
    y %>% 
      hchart(., 
             type = "spline", 
             hcaes(x = year, 
                   y = births,
                   group = group)) %>%
      hc_title(text = paste(loc," total births")) %>% 
      hc_yAxis(title = list(text = "Births"), softMin=0) %>% 
      hc_xAxis(type = "category", title = list(text = "Year")) %>%       
      hc_tooltip(crosshairs = TRUE, backgroundColor = "transparent", shared = T) %>% 
      hc_colors(c("#e5b13a", "#4AA942", "#4bd5ee")) %>% hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$e0p <- renderHighchart ({
    if(is.null(values)) return(NULL)
    y   <- values[["dem_out"]]
    loc <- values[["country"]]  
    
    y %>% 
      hchart(., 
             type = "spline", 
             hcaes(x = year, 
                   y = e0,
                   group = group)) %>%
      hc_title(text = paste(loc," life-expectancy at birth")) %>% 
      hc_yAxis(title = list(text = "Years"), softMin=0) %>% 
      hc_xAxis(type = "category", title = list(text = "Year")) %>%       
      hc_tooltip(crosshairs = TRUE, backgroundColor = "transparent", shared = T) %>% 
      hc_colors(c("#e5b13a", "#4AA942", "#4bd5ee")) %>% hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$imrp <- renderHighchart ({
    if(is.null(values)) return(NULL)
    y   <- values[["dem_out"]]
    loc <- values[["country"]]  
    
    y %>% 
      hchart(., 
             type = "spline", 
             hcaes(x = year, 
                   y = e0,
                   group = group)) %>%
      hc_title(text = paste(loc," infant mortality rate")) %>% 
      hc_yAxis(title = list(text ="Deaths per 1,000 live births"), softMin=0) %>% 
      hc_xAxis(type = "category", title = list(text = "Year")) %>%       
      hc_tooltip(crosshairs = TRUE, backgroundColor = "transparent", shared = T) %>% 
      hc_colors(c("#e5b13a", "#4AA942", "#4bd5ee")) %>% hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$u5mrp <- renderHighchart ({
    if(is.null(values)) return(NULL)
    y   <- values[["dem_out"]]
    loc <- values[["country"]]  
    
    y %>% 
      hchart(., 
             type = "spline", 
             hcaes(x = year, 
                   y = u5mr,
                   group = group)) %>%
      hc_title(text = paste(loc," under-5 mortality rate")) %>% 
      hc_yAxis(title = list(text = "Deaths per 1,000 live births"), softMin=0) %>% 
      hc_xAxis(type = "category", title = list(text = "Year")) %>%       
      hc_tooltip(crosshairs = TRUE, backgroundColor = "transparent", shared = T) %>% 
      hc_colors(c("#e5b13a", "#4AA942", "#4bd5ee")) %>% hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$poppy0 <- renderHighchart ({
    if(is.null(values)) return(NULL)
    y   <- values[["pop_out"]] 
    loc <- values[["country"]]
    y0  <- values[["year0"]]    
    
    xd <- y[1:96,1]/1000
    yd <- y[97:192,1]/1000
    xm <- -1*xd
    
    xx <- -100*xd/sum(xd,yd); yy <- 100*yd/sum(xd,yd)
    xx <- round(xx, 1); yy <- round(yy,1)
    
    categories = 0:95
    
    highchart() %>%
      hc_chart(type= 'bar')%>%
      hc_title(text= paste0(loc," ",y0, ", N = ",formatC(1000*sum(xd,yd), format="f", big.mark = ",", digits=0))) %>%
      hc_subtitle(text="Population pyramid is given in thousands") %>%
      hc_xAxis(list(lineColor= 'transparent', tickLength= 0, categories=categories,reversed=FALSE,labels=list(step= 5)),
               list(lineColor= 'transparent', tickLength= 0, categories= categories,opposite= TRUE,reversed= FALSE,linkedTo= 0,labels=list(step= 5)))%>%
      hc_tooltip(backgroundColor = "transparent", shared = FALSE, 
                 formatter = JS("function () {return '<b>' + 'Age ' + '</b> ' + this.point.category + '<br/>' + 
'<b>' + this.series.name + '</b> ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}")) %>%
      hc_yAxis(gridLineWidth= 0, minorGridLineWidth= 0, lineColor= 'transparent', tickLength= 0, title= list(text= NULL), labels=list(formatter=JS("function () {return Math.abs(this.value);}")))%>%
      hc_plotOptions(series=list(stacking= 'normal')) %>%
      hc_series(list(name= 'Male',  data= xm), list(name= 'Female', data= yd)) %>% 
      hc_exporting(enabled = TRUE) %>% hc_colors(cols[2:4]) 
})
  
  output$poppy1 <- renderHighchart ({
    if(is.null(values)) return(NULL)
    y   <- values[["pop_out"]] 
    loc <- values[["country"]]
    y1  <- values[["year1"]]    
    
    ncn <- 0.5*(ncol(y) + 1)
    
    xd <- y[1:96, ncn]/1000
    yd <- y[97:192, ncn]/1000
    xm <- -1*xd
    
    xx <- -100*xd/sum(xd,yd); yy <- 100*yd/sum(xd,yd)
    xx <- round(xx, 1); yy <- round(yy,1)
    
    categories = 0:95
    
    highchart() %>%
      hc_chart(type= 'bar')%>%
      hc_title(text= paste0(loc," ",y1, ", N = ",formatC(1000*sum(xd,yd), format="f", big.mark = ",", digits=0))) %>%
      hc_subtitle(text="Population pyramid is given in thousands") %>%
      hc_xAxis(list(lineColor= 'transparent', tickLength= 0, categories=categories,reversed=FALSE,labels=list(step= 5)),
               list(lineColor= 'transparent', tickLength= 0, categories= categories,opposite= TRUE,reversed= FALSE,linkedTo= 0,labels=list(step= 5)))%>%
      hc_tooltip(backgroundColor = "transparent", shared = FALSE, 
                 formatter = JS("function () {return '<b>' + 'Age ' + '</b> ' + this.point.category + '<br/>' + 
'<b>' + this.series.name + '</b> ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}")) %>%
      hc_yAxis(gridLineWidth= 0, minorGridLineWidth= 0, lineColor= 'transparent', tickLength= 0, title= list(text= NULL), labels=list(formatter=JS("function () {return Math.abs(this.value);}")))%>%
      hc_plotOptions(series=list(stacking= 'normal')) %>%
      hc_series(list(name= 'Male',  data= xm), list(name= 'Female', data= yd)) %>% 
      hc_exporting(enabled = TRUE) %>% hc_colors(cols[2:4]) 
  })
  
  output$poppy2 <- renderHighchart ({
    if(is.null(values)) return(NULL)
    y   <- values[["pop_out"]] 
    loc <- values[["country"]]
    y2  <- values[["year2"]]  
    
    ncn <- ncol(y)
    
    xd <- y[1:96, ncn]/1000
    yd <- y[97:192, ncn]/1000
    xm <- -1*xd
    
    xx <- -100*xd/sum(xd,yd); yy <- 100*yd/sum(xd,yd)
    xx <- round(xx, 1); yy <- round(yy,1)
    
    categories = 0:95
    
    highchart() %>%
      hc_chart(type= 'bar')%>%
      hc_title(text= paste0(loc," ",y2, ", N = ",formatC(1000*sum(xd,yd), format="f", big.mark = ",", digits=0))) %>%
      hc_subtitle(text="Population pyramid is given in thousands") %>%
      hc_xAxis(list(lineColor= 'transparent', tickLength= 0, categories=categories,reversed=FALSE,labels=list(step= 5)),
               list(lineColor= 'transparent', tickLength= 0, categories= categories,opposite= TRUE,reversed= FALSE,linkedTo= 0,labels=list(step= 5)))%>%
      hc_tooltip(backgroundColor = "transparent", shared = FALSE, 
                 formatter = JS("function () {return '<b>' + 'Age ' + '</b> ' + this.point.category + '<br/>' + 
'<b>' + this.series.name + '</b> ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}")) %>%
      hc_yAxis(gridLineWidth= 0, minorGridLineWidth= 0, lineColor= 'transparent', tickLength= 0, title= list(text= NULL), labels=list(formatter=JS("function () {return Math.abs(this.value);}")))%>%
      hc_plotOptions(series=list(stacking= 'normal')) %>%
      hc_series(list(name= 'Male',  data= xm), list(name= 'Female', data= yd)) %>% 
      hc_exporting(enabled = TRUE) %>% hc_colors(cols[2:4]) 
  })
  
  
  output$pin_table <- DT::renderDataTable({
    datatable(values$all.pin.df, 
              options = list(pageLength = 15))
  })
  

  output$downloaddem <- downloadHandler(
    filename = function() {
      loc <- values[["country"]]
      paste0(loc," DEM.csv")
    },
    content = function(file) {
      write.csv(values$dem_out, file, row.names = FALSE)
    }
  )
  
  output$downloadpin <- downloadHandler(
    filename = function() {
      loc <- values[["country"]]
      paste0(loc," PIN data.csv")
    },
    content = function(file) {
      write.csv(values$all.pin.df, file, row.names = FALSE)
    }
  )
  
})

