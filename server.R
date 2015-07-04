library(leaflet)
library(rgdal)

aushp <- readRDS("data/au15.rds")

shinyServer(function(input, output, session){

   sessionvars <- reactiveValues(profile="", loc1=NULL, POIs=NULL, page=1)

   #page 1.
   #displays buttons for profiles...
   topPage <- function(){
      sessionvars$page <- 1
      list(
         div(style="display:block; padding: 5px; width: 320px; margin-left: auto; margin-right: auto;",
            actionButton("singleparentbutton", "Single Parent", width=150),
            actionButton("studentbutton", "Student", width=150)),
         div(style="display:block; padding: 5px; width: 320px; margin-left: auto; margin-right: auto;",
            actionButton("familybutton", "Family", width=150),
            actionButton("youngprofessionalbutton", "Young Professional", width=150)),
         div(style="display:block; padding: 5px; width: 320px; margin-left: auto; margin-right: auto;",
            actionButton("custombutton", "Custom", width=305))
      )
   }

   #page 2.
   #simple map.  takes user clicks and adds to loc1 session variable (a data frame of lat/lng).
   #see
   locations <- function(){
      sessionvars$page <- 2
      list(
         leafletOutput("map1", height=550, width=442), p(),
         div(actionButton("next1", "next"), style="display: inline-block; float: right;")
      )
   }

   #page 3.
   #checkbox list of important POIs.
   #clicking next2 will store choices in POIs session variable.
   #enumerated list as described below.  e.g. "Work" has value 1...
   foo <- function(str){
      if (str=="singleparent") "Single Parent Family"
      else if (str=="youngprofessional") "Young Professional"
      else if (str=="student") "Student"
      else if (str=="family") "Two Parent Family"
      else "Custom"
   }
   choices <- reactive({
      selected <- c()
      if (sessionvars$profile=="singleparent") selected <- c(1,2,4,7,9)
      else if (sessionvars$profile=="youngprofessional") selected <- c(2,12,13,5,7)
      else if (sessionvars$profile=="student") selected <- c(3,12,5,14,16,11)
      else if (sessionvars$profile=="family") selected <- c(3,4,6,7,8)
      else if (sessionvars$profile=="custom") selected <- c()
      list(h3(foo(sessionvars$profile)), checkboxGroupInput("choices", "",
         choices=list("Work"=1, "Supermarket"=2, "Sports clubs"=3,
            "School (specific)"=4, "Transport"=5, "Parking"=6, "Gym"=7,
            "Farmer's market"=8, "Daycare (for children)"=9, "Church"=10,
            "Safety (Low crime-rate)"=11, "Bars / clubs"=12, "Cinemas"=13,
            "Restaurants"=14, "School / uni"=15, "Mall"=16), selected
      ), div(actionButton("next2", "next"), style="display: inline-block; float: right; "))
   })

   #do some fancy stuff to get recommended suburbs
   #will run every time loc1 or POIs changes.
   #that might be too often.
   #currently I just cherry-picked 4 AUs
   suburbrecs <- reactive({
      sessionvars$loc1
      sessionvars$POIs
      return(c("575300", "573000", "573200", "575200"))
   })

   #factory method for suburb components
   suburbfactory <- function(n){
      au <- suburbrecs()[n]
      nme <- aushp@data[aushp@data$au2015==au, "au2015_nam"]
      condition <- paste0("input.au", au, " % 2")
      res <- list(div(style="display:block; padding: 5px;  width: 320px; margin-left: auto; margin-right: auto;",
                      actionButton(paste0("au", au), nme, width=303)),
                  conditionalPanel(condition, div(style="display:block; padding: 5px;  width: 320px; margin-left: auto; margin-right: auto;",
                     actionButton(paste0("au", n, "_map"), "map", width=150),
                     actionButton(paste0("au", n, "_houses"), "houses", width=150)
                  )))
      res
   }

   #page 4.
   #list of suburbs
   #i got a little lazy and have arbitrarily hard-coded an upper limit of 5 suburbs.
   #making it dynamic would have been tricky, and its late...
   suburbs <- reactive({
      sessionvars$page <- 4
      suburbs <- suburbrecs()
      res <- list()
      if (length(suburbs)>0){
         for (i in 1:length(suburbs)){
            x <- suburbfactory(1)
            res[[length(res)+1]] <- suburbfactory(i)
         }
      }
      res
   })

   #page 5.
   #map of clicked suburb.
   fetchmap <- function(n){
      suburb <- suburbrecs()[n]
      shp <- aushp[aushp@data$au2015==suburb,]
      m <- leafletProxy(paste0("suburbmap")) %>% addTiles %>% addPolygons(data=shp)
      if (!is.null(sessionvars$loc1)) m <- m %>% addMarkers(data=sessionvars$loc1)
      m
   }

   #avert your eyes...
   observeEvent(input$au1_map, {
      sessionvars$page <- 5
      output$main <- renderUI(leafletOutput("suburbmap", height=550, width=442))
      output$suburbmap <- renderLeaflet(fetchmap(1))
   })

   observeEvent(input$au2_map, {
      sessionvars$page <- 5
      output$main <- renderUI(leafletOutput("suburbmap", height=550, width=442))
      output$suburbmap <- renderLeaflet(fetchmap(2))
   })

   observeEvent(input$au3_map, {
      sessionvars$page <- 5
      output$main <- renderUI(leafletOutput("suburbmap", height=550, width=442))
      output$suburbmap <- renderLeaflet(fetchmap(3))
   })

   observeEvent(input$au4_map, {
      sessionvars$page <- 5
      output$main <- renderUI(leafletOutput("suburbmap", height=550, width=442))
      output$suburbmap <- renderLeaflet(fetchmap(4))
   })

   observeEvent(input$au2_map, {
      sessionvars$page <- 5
      output$main <- renderUI(leafletOutput("suburbmap", height=550, width=442))
      output$suburbmap <- renderLeaflet(fetchmap(4))
   })

   #page 6.
   #list of houses in clicked suburb.
   #again, avert your eyes...
   observeEvent(input$au1_houses, {
      sessionvars$page <- 6
      output$main <- renderUI(verbatimTextOutput("houselist"))
      output$houselist <- renderText("A list of houses...")
   })

   observeEvent(input$au2_houses, {
      sessionvars$page <- 6
      output$main <- renderUI(verbatimTextOutput("houselist"))
      output$houselist <- renderText("A list of houses...")
   })

   observeEvent(input$au3_houses, {
      sessionvars$page <- 6
      output$main <- renderUI(verbatimTextOutput("houselist"))
      output$houselist <- renderText("A list of houses...")
   })

   observeEvent(input$au4_houses, {
      sessionvars$page <- 6
      output$main <- renderUI(verbatimTextOutput("houselist"))
      output$houselist <- renderText("A list of houses...")
   })

   observeEvent(input$au5_houses, {
      sessionvars$page <- 6
      output$main <- renderUI(verbatimTextOutput("houselist"))
      output$houselist <- renderText("A list of houses...")
   })

   #render the main UI.
   #dynamic, depending on user input.
   #matches pages above.
   output$main <- renderUI({
      if(sessionvars$profile=="") return(topPage())
      return(locations())
   })

   #render map on page 2.
   #centered on Stout St., but ideally would be centered on current location?
   output$map1 <- renderLeaflet({
      sessionvars$page
      m <- leaflet() %>% addTiles()
      loc <- isolate(sessionvars$loc1)
      if (!is.null(loc)) m <- m %>% addMarkers(data=loc)
      else m <- m %>% setView(lng=174.7767, lat=-41.28097, zoom=16)
      #if you just want to center on NZ:
      # m <- m %>% fitBounds(166.45031, -46.91964, 178.57724, -34.39263)
      m
   })

   #'event handler' for page 2 map clicks
   #pretty dumb--just adds click locations to a session variable called loc1 (a data frame of lat/lng)
   observeEvent(input$map1_click, {
      df <- data.frame(lat=input$map1_click$lat, lng=input$map1_click$lng)
      sessionvars$loc1 <- rbind(sessionvars$loc1, df)
      assign("x", sessionvars$loc1, .GlobalEnv)
      leafletProxy("map1") %>% addMarkers(lng=input$map1_click$lng, lat=input$map1_click$lat)
   })

   #go back a page...
   observeEvent(input$back, {
      if (sessionvars$page==2) output$main <- {sessionvars$page <- 1; renderUI(topPage())}
      else if(sessionvars$page==3) output$main <- {sessionvars$page <- 2; renderUI(locations())}
      else if(sessionvars$page==4) output$main <- {sessionvars$page <- 3; renderUI(choices())}
      else if(sessionvars$page==5) output$main <- {sessionvars$page <- 4; renderUI(suburbs())}
      else if(sessionvars$page==6) output$main <- {sessionvars$page <- 4; renderUI(suburbs())}
   })

   #handle button choices on page 1...
   observeEvent(input$singleparentbutton, {
      sessionvars$profile <- "singleparent"
      output$main <- renderUI({locations()})
   })

   observeEvent(input$studentbutton, {
      sessionvars$profile <- "student"
      output$main <- renderUI({locations()})
   })

   observeEvent(input$familybutton, {
      sessionvars$profile <- "family"
      output$main <- renderUI({locations()})
   })

   observeEvent(input$youngprofessionalbutton, {
      sessionvars$profile <- "youngprofessional"
      output$main <- renderUI({locations()})
   })

   observeEvent(input$custombutton, {
      sessionvars$profile <- "custom"
      output$main <- renderUI({locations()})
   })
   #done.

   #move from page 2 to page 3.
   observeEvent(input$next1, {
      sessionvars$page <- 3
      output$main <- renderUI(choices())
   })

   #move from page 3 to page 4.
   observeEvent(input$next2, {
      sessionvars$POIs <- input$choices
      sessionvars$page <- 4
      output$main <- renderUI(suburbs())
   })
})