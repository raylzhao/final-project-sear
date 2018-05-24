library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")
library("maps")
library("leaflet")

country_map <- map_data("world")
data <- read.csv(file = "2016.csv", stringsAsFactors = FALSE)
hdi <- read.csv(file = "hdi_data.csv", stringsAsFactors = FALSE)
hdi <- hdi %>%
  select(Country, X2015)

hdi_map_data <- left_join(country_map, hdi, by = c("region" = "Country"))
View(hdi_map_data)


View(hdi)
View(country_map)
#left_join not working
test <- left_join(data, hdi, by = c("Country" = "Country"))

##############
##    UI    ##
##############

ui <- fluidPage(
  includeCSS("style.css"),
  
  titlePanel("What makes people happy?"),
  mainPanel(
    tabsetPanel(
      tabPanel("Overview", textOutput("intro")),
      tabPanel("Happiness Vs Development", textOutput("question"), "Happiness Map", plotOutput("plot"),
              "HDI Map", plotOutput("map"), "Graph", plotOutput("graph"))
    )
  )
)

##############
##  SERVER  ##
##############

server <- function(input,output) {
  output$intro <- renderText({
    description <- 
"We are using a dataset called World Happiness Report from Kaggle. The dataset is comprised of 
happiness scores for 155 countries from the Gallup World Poll. The poll asked people to think of 
their best possible life as a 10 and their worst possible life being a 0 and to rate their own current 
life based on that scale. The columns after the happiness score estimate include different factors that 
may be affecting the overall happiness score; which include, economic production, social support, life 
expectancy, freedom, absence of corruption, and generosity. There is data for the years from 2013 to 2016 
and use the Gallup weights to make the estimate representative for each happiness score for each country. 

This report was released to the United Nations at an event and continues to gain global recognition and is 
used to improve public policy. We will be comparing the GDP per capita, life expectancy, and freedom 
levels of each country to their happiness scores to see the relationship between the factors and 
happiness score. We will use different visuals to represent the relationships and have the analysis 
used to make better decisions for overall happiness to increase.

In our report, we will be analyzing how different factors contribute to a country's happiness. The four
main factors we will analyze is Economic State, Level of Freedom, Development of a country, and Region
    
For More Information, visit the following links:
    - https://www.kaggle.com/aida1alim/happiness-data/data
    - http://worldhappiness.report"
    
    description
  })
  
  output$question <- renderText({
    question <- "One question we will be analyzing is how the development of a country affects the 
    happiness of individials in a country. To do this, we have obtained data for each countries
    human development index, which measures a country development in relation to their life
    expectancy, standard of living, access to knowledge/eduation, and gross national income per 
    capita. The level of development is a measure of average achievement in key dimensions of human
    development: a long and healthy life, being knowledgable, and have a decent standard of living.
    Each country is given a Human Development Index score from 0 to 1, where the higher the number,
    the higher achievement of human development in the given year. In this section, we will compare
    the relationship between Happiness Scores and HDI in countries to see if the development of a 
    country affects the happiness of its citizens."
    
    question
    
  })
  
  output$plot <- renderPlot({
    data_map <- left_join(country_map, data, by = c("region" = "Country"))
    caption <- "This map shows the data for happiness scores around the world for the year 2016."
    
    happy_map <- ggplot(data = data_map) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = Happiness.Score)) +
      coord_quickmap() +
      ggtitle("World Happiness Levels") +
      labs(x = "Longitude", y = "Latitude", caption = caption) +
      theme(plot.title = element_text(hjust = .5), plot.caption = element_text(hjust = .5))
    
    happy_map
  })
  
  output$map <- renderPlot({
    hdi <- hdi %>%
      select(Country, X2015)
    hdi_map_data <- left_join(country_map, hdi, by = c("region" = "Country"))
    caption <- "This map shows the data for Human Development Index scores around the world for the year 2015"
      
    hdi_map <- ggplot(data = hdi_map_data) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = X2015)) +
      coord_quickmap()  +
      ggtitle("Human Development Index Levels") +
      labs(x = "Longitude", y = "Latitude", caption = caption) +
      theme(plot.title = element_text(hjust = .5), plot.caption = element_text(hjust = .5))
    
    hdi_map
  })
  
  output$graph <- renderPlot({
    
  })
}

shinyApp(ui = ui, server = server)
