
library(shiny)
library(leaflet)
library(maps)
library(rgeos)
library(geosphere)
library(gmapsdistance)
library(stringr)
library(rdrop2)


## set the directory for saving and reading data
# outputDir <- "update"  # for local use

token <- readRDS("dropbox_token.rds")
drop_acc(dtoken = token)
outputDir <- "OpenDay"   # for DropBox access

## load the postcode data and CF factors
post2loc <- readRDS("partialpost.rds")
CF.vec <- readRDS("CFfactor.rds")

## load the volunteer data
basedata.ini <- readRDS("basedata0.rds")  # for testing
# basedata.ini <- readRDS(file.path(outputDir, "basedata.rds"))
basedata.ini <- basedata.ini[, -4]

## load the data from Dropbox (this will run everytime we relaunch the app)
filesInfo <- drop_dir(outputDir)
if (length(filesInfo) > 0) {
  filePaths <- filesInfo$path_display
  dropdata <- lapply(filePaths, drop_read_csv)   # stringsAsFactors = FALSE
  dropdata <- do.call(rbind, dropdata)[, -1]
  
  # merge the two (this is the most up-to-date version of data)
  basedata.ini <- rbind(basedata.ini, dropdata)
}

# get the coordinates for the curves on the map
lon.ini <- lat.ini <- 1:nrow(basedata.ini)
for (i in 1:nrow(basedata.ini)) {
  id <- post2loc$postcode4 %in% basedata.ini$postcode[i]
  lon1 <- post2loc$med_lon[id]
  lat1 <- post2loc$med_lat[id]
  lon.ini[i] <- lon1
  lat.ini[i] <- lat1
}

# mycolor <- c("skyblue4", "skyblue3", "darkolivegreen3", "darkolivegreen4", "goldenrod1", "tan1")
mycolor <- c("#195D94", "#5186B1", "#548E1B", "#94C622", "#FFC300", "#EE6D2D")
# slategray (light): #859AB8
# slategray (dark): #29528E
# steelblue: #5C7FB4
# gray: #505050
# white: #FFFFFF
# brown3: #A5151E
  
## set Google API
set.api.key("AIzaSyA6051uiw89Uxx3RqmS-Fzqrmfyn-pA4-Q")

## define an object for storing reactive values
basedata <- reactiveValues()   
basedata$mat <- basedata.ini

fileName <- reactiveValues()
fileName$name <- NULL


## App functions
function(input, output, session) {
  
  ## (1) The map output
  # map0 <- leaflet() %>%   
  #   addTiles(urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
  #   setView(lng=-2.8556, lat=54.4969, zoom=5)
  # 
  # output$basemapUK <- leaflet::renderLeaflet(map0)
  
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
  
  
  ## (2) add popups and curves when new visitor comes
  visitor <- eventReactive(input$update, {
    post4 <- input$post4
    # formatting the post code
    post4 <- str_trim(post4, side="both")  # remove spaces at two ends
    nd <- str_count(post4)  # count the digits
    part1 <- substr(post4, start=1, stop=nd-1)
    part2 <- substr(post4, start=nd, stop=nd)
    post4 <- str_squish(paste(part1, part2))
    # insert space if there is nont, then remove duplicated spaces in the middle
    post4 <- toupper(post4)  # to upper case
    travel4 <- input$travel
    num4 <- as.numeric(input$groupN)
    list(post4, travel4, num4)
  })
  
  observeEvent(input$update, {
    post4 <- visitor()[[1]]
    # id4 <- post2loc$postcode4 %in% post4
    id4 <- match(post4, post2loc$postcode4)
    
    # validate(need(!is.na(id4), 'Invalid postcode, please double check!'))
    
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
      id <<- showNotification("Warning: postcode not valid!", type="error", duration=10)
      # this is too small, and it appeared in the corner...
    }
   
  })
  
  
  ## (3) Update the data set for new analysis
  # basedata <- reactiveValues()   # returns an object for storing reactive values
  # basedata$mat <- basedata.ini
  
  observeEvent(input$update, {
    post4 <- visitor()[[1]]
    transp <- visitor()[[2]]
    gn <- visitor()[[3]]
    
    # id4 <- post2loc$postcode4 %in% post4
    id4 <- match(post4, post2loc$postcode4)
    if (!is.na(id4)) {
      lon4 <- post2loc$med_lon[id4]
      lat4 <- post2loc$med_lat[id4]
      
      # calculate the distance and carbon footprint
      if ((transp=="bus") | (transp=="train") | (transp=="plane")) {transp0 <- "transit"}
      if (transp=="walk") {transp0 <- "walking"}
      if (transp=="cycle") {transp0 <- "bicycling"}
      if (transp=="car") {transp0 <- "driving"}
      dist4 <- gmapsdistance(origin = paste(lat4, lon4, sep='+'), 
                             destination = "52.87352+-1.08628", 
                             mode = paste(transp0), 
                             dep_date="2019-10-19", dep_time="08:00:00")
      dist4m <- round(dist4$Distance/1600, 2)  # convert to miles, keep 3 decimal places
      if (dist4m == 0) {dist4m <- 0.5}
      
      CF.new <- dist4m * CF.vec[match(transp, names(CF.vec))] / 1000
      if (transp == "car") {CF.new <- CF.new/gn}
      CF.new <- round(CF.new, 3)
      
      # update the database
      adddata <- data.frame(postcode=post4, transport=transp, group_of=gn, 
                            distance=dist4m, footprint=CF.new)
      basedata$mat <- rbind(basedata$mat, adddata)
      
      # maybe I shouldn't adding things here, just in case it gets messed up?
      # dropfilesInfo <- drop_dir(outputDir)
      # dropfilePaths <- dropfilesInfo$path_display
      # dropdata <- lapply(filePaths, drop_read_csv)   # stringsAsFactors = FALSE
      # dropdata <- do.call(rbind, dropdata)[, -1]
      
      # save the new data to DropBox
      fileName$name <- paste0(as.integer(Sys.time()), ".csv")
      filePath <- file.path(tempdir(), fileName$name)
      write.csv(adddata, file=filePath)
      drop_upload(filePath, path=outputDir)
      
    } else {
      basedata$mat <- basedata$mat
    }
    
    # return the data table
    output$newdat <- renderDataTable(basedata$mat[nrow(basedata$mat):1, ])
    # if postcode correct, this will be the new table, 
    # if not, this will be the old table
  })
  
  # The following code has the same effect
  # output$newdat <- renderDataTable({
  #   post4 <- visitor()[[1]]
  #   transp <- visitor()[[2]]
  #   gn <- visitor()[[3]]
  #   adddata <- data.frame(postcode=post4, transport=transp, group_of=gn)
  #   isolate(basedata$mat <- rbind(basedata$mat, adddata))
  #   basedata$mat
  # })
  
  
  ## (4) Basic statistical analysis
  
  output$piechart <- renderPlot({
    if (input$pie) {
      # methodmat <- basedata$mat[, 2:3]
      pie.tab <- tapply(as.numeric(basedata$mat[,3]), 
                        INDEX=basedata$mat[,2], FUN=sum)
      par(mfrow=c(1, 1), mar=c(3, 3, 2.5, 2.5))
      pie(pie.tab, labels=paste0(names(pie.tab), " - ", round(100*pie.tab/sum(pie.tab), 1), "%"), 
          edge=100, radius=0.8, col=mycolor, border="#FFFFFF", init.angle=45, cex.lab=1)
      # try associate one colour to one transportation method
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
              ylab="miles", xlab="", # main='Transportation methods and distances', 
              cex.lab=1, cex.axis=1)
    }
  })
  
  
  ## (5) Calculate carbon footprint related
  output$carbon <- renderText({
    CF.dat <- basedata$mat$footprint[nrow(basedata$mat)]
    paste("The carbon footprint of your journey to the BGS Open Day is",
          CF.dat, "kg carbon dioxide equivalence.")
  })
  
  
  observeEvent(input$carbonD, {
    hist.data <- rep(basedata$mat$footprint, times=basedata$mat$group_of)
    your.data <- basedata$mat$footprint[nrow(basedata$mat)]
    output$cfdistribution <- renderPlot({
      par(mfrow=c(1,1), mar=c(4.5, 4.5, 3, 2)+0.5)
      hist(hist.data, probability=T, breaks=20, col="#859AB8", border="#FFFFFF",
           xlab="kg carbon dioxide", ylab="probability", 
           main="Distribution of carbon footprint",
           col.main="#505050", col.axis="#505050", cex.lab=1.1, cex.main=1.2)
      abline(v=your.data, lwd=2, lty=2, col="#A5151E")
      points(x=your.data, y=0, pch=17, lwd=3, col="#A5151E")
    })
  })
  
  # output$cfdistribution <- renderPlot({
    # if (input$carbonD) {
    #   hist.data <- rep(basedata$mat$footprint, times=basedata$mat$group_of)
    #   par(mfrow=c(1,1), mar=c(4.5, 4.5, 3, 2)+0.5)
    #   hist(hist.data, probability=T, breaks=20, col='slategray3', border='white', 
    #        xlab='kg CO2 / person', ylab='probability', main='Distribution of carbon footprint',
    #        col.main='gray30', col.axis='gray30', cex.lab=1.1, cex.main=1.2)
    #   abline(v=basedata$mat$footprint[nrow(basedata$mat)], lwd=2, lty=2, col='brown3')
    #   points(x=basedata$mat$footprint[nrow(basedata$mat)], y=0, pch=17, lwd=3, col='brown3')
    #   
    #   # sm.density <- density(hist.data, bw=3, kernel='gaussian')
    #   # lines(sm.density$x, sm.density$y, lwd=3, col='slategray')
    # }
  # })
  
  
  ## (6) Some fun facts
  
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
  })
  
  observeEvent(input$funfact,  {
    output$speed <- renderText({
      # earch.sp1 <- cos(52/180*pi) * 1674.4 / 3.6  # UK speed (m/s)
      earch.sp2 <- cos(0/180*pi) * 1674.4 / 3.6  # equator speed (m/s)
      dist <- basedata$mat$distance[nrow(basedata$mat)]    
      earth.t <- (dist * 1609.34 / earch.sp2) / 60  # time in minutes
      earth.t <- round(earth.t, 3)
      
      paste0("It may take you quite a while to get here, but it only takes the earth ", earth.t, 
             " minutes to rotate the distance (that's about ", dist, " miles) you travelled today! ",
             "Yet you barely notice it's moving.")
    })
  })
    
  
  
  ## (7) The reset button!
  observeEvent(input$reset, {
    last.post <- basedata$mat$postcode[nrow(basedata$mat)]
    basedata$mat <- basedata$mat[-nrow(basedata$mat), ]
    # HOW TO remove the curve on the map
    
    # also need to delete the corresponding file on Dropbox (need reactive)
    drop_delete(file.path(outputDir, fileName$name))
  })

## The end
}


## save and reload data (save one record a time)

# outputDir <- "responses"
# 
# saveData <- function(data) {
#   data <- t(data)
#   # Create a unique file name
#   fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
#   # Write the data to a temporary file locally
#   filePath <- file.path(tempdir(), fileName)
#   write.csv(data, filePath, row.names = FALSE, quote = TRUE)
#   # Upload the file to Dropbox
#   drop_upload(filePath, path = outputDir)
# }
# 
# loadData <- function() {
#   # Read all the files into a list
#   filesInfo <- drop_dir(outputDir)
#   filePaths <- filesInfo$path   # not quite right...
#   data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
#   # Concatenate all data together into one data.frame
#   data <- do.call(rbind, data)
#   data
# }

