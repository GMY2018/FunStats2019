## -----------------------------------------------
## Live demo of data analysis (server function)
## -----------------------------------------------

library(shiny)
library(leaflet)
library(maps)
library(rgeos)
library(geosphere)
library(gmapsdistance)
library(stringr)
library(rdrop2)



## -------------------------------------------
## Get the essential matrices, vectors, etc
## -------------------------------------------

## set the directory for saving and reading data (local use only)
outputDir <- "update"


## load the volunteer data
basedata.ini <- readRDS("basedata0.rds")  # for testing
basedata.ini <- basedata.ini[, -4]


## load the postcode data and carbon footpring conversion factors
post2loc <- readRDS("partialpost.rds")
CF.vec <- readRDS("CFfactor.rds")
# WARNING: The conversion factors are taken from different documents produced in different years,
# so some data might be outdated. It is not a big problem for demonstration, but you might wish to 
# update the CF.vec for better estimation.


## define an object for storing reactive values
basedata <- reactiveValues()   
basedata$mat <- basedata.ini


## get the coordinates for the curves on the map
lon.ini <- lat.ini <- 1:nrow(basedata.ini)
for (i in 1:nrow(basedata.ini)) {
  id <- post2loc$postcode4 %in% basedata.ini$postcode[i]
  lon1 <- post2loc$med_lon[id]
  lat1 <- post2loc$med_lat[id]
  lon.ini[i] <- lon1
  lat.ini[i] <- lat1
}


## choose a few nice colors
mycolor <- c("#195D94", "#5186B1", "#548E1B", "#94C622", "#FFC300", "#EE6D2D")
# slategray (light): #859AB8
# slategray (dark): #29528E
# steelblue: #5C7FB4
# gray: #505050
# white: #FFFFFF
# brown3: #A5151E


## set Google API (you might need to register to get your own)
set.api.key("AIzaSyA6051uiw89Uxx3RqmS-Fzqrmfyn-xxx-x")



## -------------------------------------------------------
## The app functions (for everything on the ui.R script)
## -------------------------------------------------------

function(input, output, session) {
  
  ## (1) The map output
  
  output$basemapUK <- renderLeaflet({
    map0 <- leaflet() %>%
         addTiles() %>%
         setView(lng=-2.8556, lat=54.4969, zoom=5)
    map0})
  
  leafletProxy("basemapUK") %>%
    addMarkers(lng=-1.0783, lat=52.8791, popup="BGS, Keyworth, Nottingham") 
  
  p1.ini <- cbind(lon.ini, lat.ini)
  p2.ini <- c(-1.08628, 52.87352)
  curve.ini <- gcIntermediate(p1=p1.ini, p2=p2.ini, breakAtDateLine = F, n = 25, 
                           addStartEnd = TRUE, sp = T)
  leafletProxy("basemapUK") %>%
    leaflet::addPolylines(data=curve.ini, weight=2, color="#5C7FB4")
  
  
  ## (2) Add popups and curves when new visitors come
  
  visitor <- eventReactive(input$update, {
    post4 <- input$post4
    # formatting the post code
    # remove spaces at two ends
    post4 <- str_trim(post4, side="both")  
    # count the digits
    nd <- str_count(post4)
    # insert space if it's missing, then remove duplicated spaces in the middle
    part1 <- substr(post4, start=1, stop=nd-1)
    part2 <- substr(post4, start=nd, stop=nd)
    post4 <- str_squish(paste(part1, part2)) 
    # convert to upper case
    post4 <- toupper(post4)  # good to go!
    
    travel4 <- input$travel
    num4 <- as.numeric(input$groupN)
    
    list(post4, travel4, num4)
  })
  
  observeEvent(input$update, {
    post4 <- visitor()[[1]]
    id4 <- match(post4, post2loc$postcode4)
    
    if (!is.na(id4)) {
      lon4 <- post2loc$med_lon[id4]
      lat4 <- post2loc$med_lat[id4]
      
      p1 <- cbind(lon4, lat4)
      p2 <- c(-1.08628, 52.87352)
      acurve <- gcIntermediate(p1, p2, breakAtDateLine = F, n = 25, 
                               addStartEnd = TRUE, sp = T)
      leafletProxy("basemapUK") %>%
        leaflet::addPolylines(data=acurve, weight=2, color="#5C7FB4")
      
      content <- as.character(tagList(
        tags$strong(paste0("Post code ", post4, ", you are here!")),
        tags$br()
      ))
      leafletProxy("basemapUK") %>%
        leaflet::addPopups(lon4, lat4, content)
    } else {
      id <<- showNotification("Warning: postcode not valid!", type="warning", duration=10)
      # This is too small, and it is in the corner. How to change this?
    }
   
  })
  
  
  ## (3) Update the data set for new analysis
  
  observeEvent(input$update, {
    post4 <- visitor()[[1]]
    transp <- visitor()[[2]]
    gn <- visitor()[[3]]
    
    id4 <- match(post4, post2loc$postcode4)
    if (!is.na(id4)) {
      lon4 <- post2loc$med_lon[id4]
      lat4 <- post2loc$med_lat[id4]
      
      # calculate the distance and carbon footprint
      # WARNING: This is only a rough estimation, as the package takes all public transporations as "transit"
      # and it doesn't have an option of plane. I will try to find better code for this purpose in the future.
      if ((transp=="bus") | (transp=="train") | (transp=="plane")) {transp0 <- "transit"}
      if (transp=="walk") {transp0 <- "walking"}
      if (transp=="cycle") {transp0 <- "bicycling"}
      if (transp=="car") {transp0 <- "driving"}
      dist4 <- gmapsdistance(origin = paste(lat4, lon4, sep='+'), 
                             destination = "52.87352+-1.08628", 
                             mode = paste(transp0),
                             dep_date = as.character(Sys.Date()+1), 
                             dep_time = "09:00:00")
      dist4m <- round(dist4$Distance/1609.34, 3)  # convert to miles, keep 3 decimal places
      if (dist4m == 0) {dist4m <- 0.5}  # if you live in Keyworth, I assume you travelled 0.5 miles
      
      CF.new <- dist4m * CF.vec[match(transp, names(CF.vec))] / 1000
      if (transp == "car") {CF.new <- CF.new/gn}
      CF.new <- round(CF.new, 3)
      
      adddata <- data.frame(postcode=post4, transport=transp, group_of=gn, 
                            distance=dist4m, footprint=CF.new)
      basedata$mat <- rbind(basedata$mat, adddata)
  
    } else {
      basedata$mat <- basedata$mat
    }
    
    # return the data table
    output$newdat <- renderDataTable(basedata$mat[nrow(basedata$mat):1, ])
    # if postcode correct, this will be the new table, 
    # if not, this will be the old table
    
    # save the data table every 5 new visitors, so we won't lose information
    if ((nrow(basedata$mat) %% 5) == 0) {
      fileName <- paste0("basedata", ".rds")
      saveRDS(basedata$mat, file=file.path(outputDir, fileName))
    }
    
  })

  
  ## (4) Basic statistical analysis
  
  output$piechart <- renderPlot({
    if (input$pie) {
      pie.tab <- tapply(as.numeric(basedata$mat[,3]), 
                        INDEX=basedata$mat[,2], FUN=sum)
      par(mfrow=c(1, 1), mar=c(3, 3, 2.5, 2.5))
      pie(pie.tab, labels=paste0(names(pie.tab), " - ", round(100*pie.tab/sum(pie.tab), 1), "%"), 
          edge=100, radius=0.8, col=mycolor, border="#FFFFFF", init.angle=45, cex.lab=1)
    }
  })
  
  output$carshare <- renderText({
    if (input$pie) {
      shareN <- as.numeric(basedata$mat[,3])[basedata$mat[,2]=="car"]
      shareSum <- sum(shareN[shareN >= 2])
      paste("Note: among the visitors travelling by car,", shareSum,
            "used a car share (2 to 4 passengers).")
    }
  })
  
  output$boxplots <- renderPlot({
    if (input$box) {
      par(mfrow=c(1, 1), mar=c(4, 4.5, 2.5, 2.5)+0.5, fg="#FFFFFF")
      boxplot(basedata$mat$distance ~ basedata$mat$transport, 
              horizontal=FALSE, col="#859AB8", border="#29528E",
              ylab="miles", xlab="", 
              cex.lab=1, cex.axis=1)
    }
  })
  
  
  ## (5) Calculate carbon footprint and make a histogram
  output$carbon <- renderText({
    CF.dat <- basedata$mat$footprint[nrow(basedata$mat)]
    paste("The carbon footprint of your journey to the BGS Open Day is",
          CF.dat, "kg carbon dioxide equivalence.")
  })
  
  
  observeEvent(input$carbonD, {
    isolate({
      # so that the information is only updated when you click the bottom
      hist.data <- rep(basedata$mat$footprint, times=basedata$mat$group_of)
      your.data <- basedata$mat$footprint[nrow(basedata$mat)]
    })
    output$cfdistribution <- renderPlot({
      par(mfrow=c(1,1), mar=c(4.5, 4.5, 3, 2)+0.5)
      hist(hist.data, probability=T, breaks=20, col="#859AB8", border="#FFFFFF",
           xlab="kg carbon dioxide / person", ylab="probability", 
           main="Distribution of carbon footprint",
           col.main="#505050", col.axis="#505050", cex.lab=1.1, cex.main=1.2)
      abline(v=your.data, lwd=2, lty=2, col="#A5151E")
      points(x=your.data, y=0, pch=17, lwd=3, col="#A5151E")
    })
  })
  
  
  ## (6) The fun fact
  
  output$earth <- renderImage({
    imageName <- normalizePath(file.path("./www", "earth.png"))
    list(src = imageName, 
         width = "200", height = "160",
         alt = "Image not found")
  }, deleteFile = FALSE)
  
  observeEvent(input$funfact,  {
    output$earthspin <- renderImage({
      imageName <- normalizePath(file.path("./www", "earth.gif"))
      # if (input$funfact) {
      list(src = imageName, 
           width = "300", height = "300",
           alt = "Image not found")
      # }
    }, deleteFile = FALSE)
    
    isolate({
      dist <- basedata$mat$distance[nrow(basedata$mat)]    
    })
    
    output$speed <- renderText({
      # Earth rotation speed used here is 1674.4 km/h (source: Wikipaedia)
      earch.sp1 <- cos(52/180*pi) * 1674.4 / 3.6  # UK speed (m/s)
      # earch.sp2 <- cos(0/180*pi) * 1674.4 / 3.6  # equator speed (m/s)
      earth.t <- (dist * 1609.34 / earch.sp1) / 60  # time in minutes
      earth.t <- round(earth.t, 3)
      paste0("It may take you quite a while to get here, but it only takes the earth ", earth.t, 
             " minutes to rotate the distance (that's about ", dist, " miles) you travelled today! ",
             "Yet you barely notice it's moving.")
    })
  })
  
  
  ## (7) The reset button (for remove the last record if you submitted a wrong record)
  
  observeEvent(input$reset, {
    basedata$mat <- basedata$mat[-nrow(basedata$mat), ]
  })

}

