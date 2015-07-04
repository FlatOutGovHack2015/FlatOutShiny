library(leaflet)

shinyServer(function(input, output, session){

   sessionvars <- reactiveValues(profile="", page="1")

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

   locations <- list(
      leafletOutput("map1"),
      div(actionButton("next1", "next"), style="display: inline-block; float: right; ")
   )

   choices <- reactive({
      selected <- c()
      if (sessionvars$profile=="singleparent") selected <- c(1,2,4,7,9)
      else if (sessionvars$profile=="youngprofessional") selected <- c(2,4,5,8,9)
      else if (sessionvars$profile=="student") selected <- c(3,4,6,9,10,12)
      else if (sessionvars$profile=="family") selected <- c(3,4,6,7,8)
      else if (sessionvars$profile=="custom") selected <- c()
      list(h2(sessionvars$profile), checkboxGroupInput("choices", "",
         choices=list("Work"=1, "Supermarket"=2, "Sports clubs"=3,
            "School (specific)"=4, "Transport"=5, "Parking"=6, "Gym"=7,
            "Farmer's market"=8, "Daycare (for children)"=9, "Church"=10,
            "Safety (Low crime-rate)"=11), selected
      ), div(actionButton("next2", "next"), style="display: inline-block; float: right; "))
   })

   output$main <- renderUI({
      if(sessionvars$profile=="") return(topPage)
      return(locations)
   })

   observeEvent(input$back, {

   })

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

   observeEvent(input$next1, {
      output$main <- renderUI(choices())
   })

   output$map1 <- renderLeaflet({
      leaflet() %>% addTiles()
   })
})