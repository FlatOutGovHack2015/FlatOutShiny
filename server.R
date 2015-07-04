library(leaflet)

shinyServer(function(input, output, session){

   sessionvars <- reactiveValues(profile="", loc1=NULL, POIs=NULL)

   #page 1.
   #displays buttons for profiles...
   topPage <- list(
      div(style="display:block; padding: 5px;",
         actionButton("singleparentbutton", "Single Parent", width=150),
         actionButton("studentbutton", "Student", width=150)),
      div(style="display:block; padding: 5px; width: 500px; ",
         actionButton("familybutton", "Family", width=150),
         actionButton("youngprofessionalbutton", "Young Professional", width=150)),
      div(style="display:block; padding: 5px;",
         actionButton("custombutton", "Custom", width=305))
   )

   #page 2.
   #simple map.  takes user clicks and adds to loc1 session variable (a data frame of lat/lng).
   #see
   locations <- list(
      leafletOutput("map1"),
      div(actionButton("next1", "next"), style="display: inline-block; float: right; ")
   )

   #page 3.
   #checkbox list of important POIs.
   #clicking next2 will store choices in POIs session variable.
   #enumerated list as described below.  e.g. "Work" has value 1...
   foo <- function(str){
      if (str=="singleparent") "Single Parent"
      else if (str=="youngprofessional") "Young Professional"
      else if (str=="student") "Student"
      else if (str=="family") "Two Parent Family"
      else "Custom"
   }
   choices <- reactive({
      selected <- c()
      if (sessionvars$profile=="singleparent") selected <- c(1,2,4,7,9)
      else if (sessionvars$profile=="youngprofessional") selected <- c(2,4,5,8,9)
      else if (sessionvars$profile=="student") selected <- c(3,4,6,9,10,12)
      else if (sessionvars$profile=="family") selected <- c(3,4,6,7,8)
      else if (sessionvars$profile=="custom") selected <- c()
      list(h2(foo(sessionvars$profile)), checkboxGroupInput("choices", "",
         choices=list("Work"=1, "Supermarket"=2, "Sports clubs"=3,
            "School (specific)"=4, "Transport"=5, "Parking"=6, "Gym"=7,
            "Farmer's market"=8, "Daycare (for children)"=9, "Church"=10,
            "Safety (Low crime-rate)"=11), selected
      ), div(actionButton("next2", "next"), style="display: inline-block; float: right; "))
   })

   #render the main UI.
   #dynamic, depending on user input.
   #matches pages above.
   output$main <- renderUI({
      if(sessionvars$profile=="") return(topPage)
      return(locations)
   })

   #render map on page 2.
   #centered on Stout St., but ideally would be centered on current location?
   output$map1 <- renderLeaflet({
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

   })

   #handle button choices on page 1...
   observeEvent(input$singleparentbutton, {
      sessionvars$profile <- "singleparent"
   })

   observeEvent(input$studentbutton, {
      sessionvars$profile <- "student"
   })

   observeEvent(input$familybutton, {
      sessionvars$profile <- "family"
   })

   observeEvent(input$youngprofessionalbutton, {
      sessionvars$profile <- "youngprofessional"
   })

   observeEvent(input$custombutton, {
      sessionvars$profile <- "custom"
   })
   #done.

   #move from page 2 to page 3.
   observeEvent(input$next1, {
      output$main <- renderUI(choices())
   })

   #move from page 3 to page 4.
   observeEvent(input$next2, {

   })
})