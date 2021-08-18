# CelineDion2021Encore
# ssh ilya@econom.space
# cd /srv/shiny_server
# git_pull

require(shiny)
require(dygraphs)
require(magrittr)
require(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage(HTML("<strong><a href=\"http://econom.space\" style='color:white; text-decoration: none;'>Econom.space</a></strong>"),
                 selected="Trade Diversification Dashboard",
                 header = 
                 tagList(
                   tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                   tags$head(HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
                                    <script async src='https://www.googletagmanager.com/gtag/js?id=G-31CXK40FPP'></script>
                                    <script>
                                    window.dataLayer = window.dataLayer || [];
                                  function gtag(){dataLayer.push(arguments);}
                                  gtag('js', new Date());
                                  
                                  gtag('config', 'G-31CXK40FPP');
                                  </script>"))
                   )),
                 footer = tagList(HTML('
                 <strong>Contacts:</strong>
                 Ilya Gulenkov | 
          <ul class="list-unstyled" style="display:inline">
            <li style="display:inline"><a class="text-muted" href="mail&#116;o&#58;&#105;&#108;&#117;a&#103;ul%65nk%6&#70;v&#64;gm&#97;il&#46;c&#37;&#54;&#70;m">&#105;lu&#97;g&#117;l&#101;nkov&#64;&#103;&#109;ail&#46;com</a> |</li>
            <li style="display:inline"><a class="text-muted" href="m&#97;ilt&#111;&#58;i&#37;67u&#108;&#101;nkov&#64;hs&#101;&#46;ru">&#105;gul&#101;nkov&#64;hse&#46;&#114;u</a></li>
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
          p('A tool which aims to provide access to a set of measures for trade diversification. 
Currently the Theil index (alongside with its "between" and "within" decomposition) is provided, which allows to examine export basket product diversification along the intensive and extensive margins. Future releases will feature
the HHI index for product diversification, as well as various measures of market diversification. Bulk download options are scheduled to be available in summer 2021.'),
            dygraphOutput("dygraph"),
          # p('The current values are provisional and are scheduled to be recalculated in June 2021
          #     following an updated methodology. The latter will take advantage of the available mirror
          #     data for countries with poor trade statistics coverage, as well as use the BACI database
          #     instead of direct TradeMap data. Most significant changes are expected to the index values 
          #     for the least developed countries.
          #     '),
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    extensive <- read.csv(file='csv/theil_ext.csv', header=TRUE, check.names=FALSE)
    extensive <- extensive[,-1]
    intensive <- read.csv(file='csv/theil_int.csv', header=TRUE, check.names=FALSE)
    intensive <- intensive[,-1]
    overall <- read.csv(file='csv/theil.csv', header=TRUE, check.names=FALSE)
    overall <- overall[,-1]
    countries <- read.csv(file='csv/country_codes.csv', header=TRUE, check.names=FALSE)
    countries <- data.frame(countries[,2])
    
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
