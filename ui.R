## -----------------------------------------------
## Live demo of data analysis (user interface)
## -----------------------------------------------

library(shiny)
library(leaflet)
library(maps)



fluidPage(
  
  ## (1) The header and the data input section
  
  fluidRow(
    column(9, offset=1,
           titlePanel("Live demo of data analysis", windowTitle="Live demo"))
  ),
  
  fluidRow(
    column(4, offset=1,
           br(),
           textInput(inputId="post4", label="Type in partial postcode (omitting last 2 digits)", value=NULL, placeholder="NG12 5"),
           strong("For example, if the postcode is 'NG12 5GG', type 'NG12 5'.", style = "color:#505050"),
           br(),
           br(),
           
           radioButtons(inputId="travel", label="Select a transportation method",
                        choiceNames=list(list(icon("walking"), "Walking"), 
                                         list(icon("bicycle"), "Cycling"), 
                                         list(icon("car-side"), "Car"), 
                                         list(icon("bus"), "Bus"), 
                                         list(icon("train"), "Train"),
                                         list(icon("plane"), "Plane")),
                        choiceValues=list("walk", "cycle", "car", "bus", "train", 'plane'),
                        selected="walk"),
           
           br(),
           numericInput(inputId="groupN", label="Travelled as a group of", value=1, 
                        min=1, max=50, step=1),
           br(),
           column(3, actionButton(inputId="update", label="Submit", icon=icon("caret-square-right"))),
           column(3, actionButton(inputId="reset", label="Reset", icon=icon("undo"))),
           br(),
           br(),
           h6("Note: After clicking the 'Submit' button, you will see your record on the first row of the table.
               If you see a warning sign popped out at the bottom right corner, 
               just close it and type in the correct partial postcode again. 
               Only use the 'Reset' button when you accidentially put in someone else's postcode!",
              style="color:gray"),
           br(),
           br()
           ),
    
    column(6, offset=1,
           br(),
           leafletOutput("basemapUK", width="80%"),
           br(),
           br()
           )
    
  ## end of the header (fluid row)
  ),
  
  
  ## (2) A multiple panels section for simple data analysis
  
  fluidRow(
    column(1),
    
    column(10,
           tabsetPanel(
             
             ## The 1st panel: a data table displaying the data collected
             tabPanel("Visitors' survey",  
                      column(1),
                      column(10, dataTableOutput("newdat")),
                      column(1)),
             
             
             ## The 2nd panel: a piechart and a boxplot showing the choices of transportation methods
             tabPanel("Data analysis",
                      column(2,
                             br(),
                             h4(icon("chart-line"), "Some basic statistical analysis"),
                             br(),
                             checkboxInput("pie", strong("Make a piechart", style = "color:#505050"), FALSE),
                             checkboxInput("box", strong("Make a boxplot", style = "color:#505050"), FALSE),
                             br(),
                             br()),
                      
                      column(5,
                             br(),
                             h5(strong("Pie chart: proportions of usage of different transportation methods"), style = "color:#505050"),
                             plotOutput("piechart"),
                             h5(textOutput("carshare"), style = "color:#505050"),
                             br()),
                      
                      column(5,
                             br(),
                             h5(strong("Boxplot: distribution of distance by transportation methods"), style = "color:#505050"),
                             plotOutput("boxplots"),
                             br()
                             )),
             
             
             ## The 3rd panel: the carbon footprint info, a histogram and some fun fact
             tabPanel("Carbon footprint",  
                      column(3, 
                             br(),
                             h5(icon("shoe-prints"), icon("shoe-prints"), icon("shoe-prints"), 
                                icon("shoe-prints"), icon("shoe-prints"), icon("shoe-prints")),
                             h4(textOutput('carbon')),
                             br(),
                             h4("Click to see some personalized data", style = "color:#505050"),
                             br(), 
                             h5("How well you did today?"),
                             actionButton('carbonD', label='You are here', icon=icon("flag-checkered")),
                             br(),
                             br(),
                             h5("Do you know how fast the earth spin?"),
                             actionButton("funfact", label="You v.s the Earth", icon=icon("rocket")),
                             br(),
                             br(),
                             imageOutput("earth"),
                             br()
                             ),
                      column(6, 
                             br(),
                             plotOutput("cfdistribution"),
                             br()),
                      column(3,
                             br(),
                             h4(textOutput("speed")),
                             br(),
                             imageOutput("earthspin"),
                             br())
                      )
             
           ## end of tabsetPanel
           )),
    
    column(1)
  )
  
## end of fluid page
)

