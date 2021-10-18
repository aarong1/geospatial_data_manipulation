#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Geofacet Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("geofacet")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$geofacet<- renderPlot({
       ggplot(doh,aes(fill=`Positive Cases`,x=1, y=`Positive Cases`),
       show.legend=F)+
  geom_col(show.legend = F,position='stack')+
  geom_text(mapping = aes(1,`Positive Cases`,label=`Positive Cases`),nudge_y = 1)+
  scale_fill_gradient(low = 'green',high='red')+
  theme_void()+
  #facet_wrap(~Postal_District)
  geofacet::facet_geo(facets = ~Postal_District,grid = geo_pc)

    })
    
    output$selected <- render
}

# Run the application 
shinyApp(ui = ui, server = server)
