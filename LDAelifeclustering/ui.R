library(shiny)
#anytime a file under the assets folder is changed, LDAvis must be reinstalled (to reflect the change)!
addResourcePath('assets', file.path(getwd(), "assets"))

# Thanks Winston! https://github.com/wch/testapp/blob/master/custom-style/ui.R
widget_style <-
  "display: inline-block;
  vertical-align: text-top;
  padding: 7px;
  border: solid;
  border-width: 1px;
  border-radius: 4px;
  border-color: #CCC;"

side_margins <-
  "margin-right:50px;
    margin-left:50px;"

#ugly hack to line up the documents below the scatterplot
top_margin <- 
  "margin-top:-550px;"

shinyUI(bootstrapPage(
  
  tags$head(
    tags$script(src = "assets/d3.v3.js"),
    tags$script(src ="assets/topicz.js"),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'assets/topicz.css')
  ),

  wellPanel(
    div(style = widget_style,
        selectInput("distance", "Topical Distance Calculation", choices = c("Jensen-Shannon" = "JS", 
                                                                            "Symmetric Kullback-Leibler" = "KL"))
    ),
    div(style = widget_style,
        selectInput("scaling", "Multidimensional Scaling Method", choices = c("Classical (PCA)" = "PCA", 
                                                                              "Kruskal's Non-Metric" = "kruskal",
                                                                              "Sammon's Non-Linear Mapping" = "sammon"))
    ),
    div(style = widget_style, sliderInput("kmeans", "Number of clusters", min=1, max=10, value=1, width='200px')),
    div(style = widget_style,
        sliderInput("nTerms", "Number of terms", min=1, max=50, value=30, width='200px')
    ), 
    div(style = widget_style,
        sliderInput("lambda", "Value of lambda", min=0, max=1, value=0.6, width='200px')
    )
  ),
  
  #the el parameter in the js code selects the outputIds
  mainPanel(HTML("<div id=\"mdsDat\" class=\"shiny-scatter-output\"><svg /></div>"))
))
