shinyUI(div(style="width: 450px;", fluidPage(title="flatout",
   fluidRow(div(actionButton("back", "back")), style="padding: 5px; height: 45px; background-color: grey;"),
   fluidRow(div(uiOutput("main"), style="padding: 5px;"))
)))