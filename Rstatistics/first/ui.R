library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  HTML('<style type="text/css">
        .content-wrapper,
       .right-side {
       background-color:#f2f2f2;font-family: Myriad Pro;
       }
       
       </style>'),
  tags$style("body {background-color: #f2f2f2;font-family: Myriad Pro;}"),
  
  tags$style(type='text/css', '#summary {background-color:white  ; color: black;}'), 
  tags$style(type='text/css', '#view {background-color:white  ; color: black;}'),
  tags$style(type='text/css', '#corr {background-color:white  ; color: black;}'),
  
  HTML('<style type="text/css">
       .well { background-color: white;
       }
       .nav-tabs>li>a:hover{background-color: #a9a9a9;}
      
        table, td, th {
    border: 1px solid green;
       }
       
       th {
       background-color: green;
       color: white;
       }

       </style>'),

  sidebarLayout(position="right",
    sidebarPanel(
      textInput("caption", "Caption:", "Data Summary Statistics"),
      
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("rock", "pressure", "cars")),
      
      numericInput("obs", "Number of observations to view:", 10)
    ),
    
    # Show the caption, a summary of the dataset and an HTML 
    # table with the requested number of observations
    mainPanel(
      h3(textOutput("caption", container = span)),
      
      tableOutput("summary"), 
      h4("Correlation"), 
      tableOutput("corr"),
      h4("Datatableview"), 
      tableOutput("view")
    )
  )
))
