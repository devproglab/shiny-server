#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# rsconnect::deployApp()

require(shiny)
require(dygraphs)
require(magrittr)
require(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage(HTML("<strong><a href=\"http://econom.space\" style='color:white; text-decoration: none;'>Econom.space</a></strong>"),
                 selected="Trade Diversification Dashboard",
                 header = 
                 tagList(
                   tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                   )),
                 footer = tagList(HTML('
                 <strong>Contacts:</strong>
                 Ilya Gulenkov | 
          <ul class="list-unstyled" style="display:inline">
            <li style="display:inline"><a href="mailto:iluagulenkov@gmail.com" class="text-muted">iluagulenkov@gmail.com</a> |</li>
            <li style="display:inline"><a href="mailto:igulenkov@hse.ru" class="text-muted">igulenkov@hse.ru</a></li>
</ul>')),
                 windowTitle='Trade Diversification Dashboard | Econom.space',
                 inverse=FALSE,
    tabPanel("Trade Diversification Dashboard",
    # Application title
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("index",
                        "Diversification Index:",
                        choices = list("Theil, overall" = 'overall', "Theil, intensive" = 'intensive',
                                       "Theil, extensive" = 'extensive')),
            uiOutput("country"), width = 3
        ),

        # Show a plot of the generated distribution
        mainPanel(
          p('A tool at early stages of development which aims to provide access to a set of measures for trade diversification. 
Currently the Theil index (alongside with its "between" and "within" decomposition) is provided, which allows to examine export basket product diversification along the intensive and extensive margins. Future releases will feature
the HHI index for product diversification, as well as various measures of market diversification. Bulk download options are scheduled to be available in summer 2021.'),
            dygraphOutput("dygraph"),
          p('The current values are provisional and are scheduled to be recalculated in June 2021
              following an updated methodology. The latter will take advantage of the available mirror
              data for countries with poor trade statistics coverage, as well as use the BACI database
              instead of direct TradeMap data. Most significant changes are expected to the index values 
              for the least developed countries.
              '),
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    extensive <- read.csv2(file='csv/extensive.csv', fileEncoding="UTF-8-BOM", header=TRUE, check.names=FALSE)
    intensive <- read.csv2(file='csv/intensive.csv', fileEncoding="UTF-8-BOM", header=TRUE, check.names=FALSE)
    overall <- read.csv2(file='csv/overall.csv', fileEncoding="UTF-8-BOM", header=TRUE, check.names=FALSE)
    countries <- read.csv2(file='csv/countrylist.csv', fileEncoding="UTF-8-BOM", header=TRUE, check.names=FALSE)
    
    extensive[extensive==0] <- NA
    intensive[intensive==0] <- NA
    overall[overall==0] <- NA

    output$country <- renderUI({
      selectInput("country", "Country:", unique(countries[,1]))
    })
    

    output$dygraph <- renderDygraph({
        index    <- get(input$index)
        country    <- input$country
        if (input$index=='overall') {
            graph_name <- paste('Overall diversification (Theil index) for', input$country)
        } else if (input$index=='extensive') {
            graph_name <- paste('Extensive diversification (Theil index, between component) for', input$country)
        } else if (input$index=='intensive') {
            graph_name <- paste('Intensive diversification (Theil index, within component) for', input$country)
        }
        timespan <- length(index$year)
        dygraph(ts(index[,country], start=index[1,'year']), main=graph_name) %>%
            dyOptions(disableZoom = TRUE, includeZero=TRUE) %>%
            dySeries("V1", label = "Index value") %>%
            dyLegend(show = "always", hideOnMouseOut = TRUE)
        
    })
}

# Run the application 
shinyApp(ui =  ui, server = server)
