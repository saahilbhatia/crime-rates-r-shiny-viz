# Loading required packages
library(ggplot2)
library(shiny)
library(ggvis)
library(mapview)
library(leaflet)
library(leafpop)
library(rgdal)
library(RColorBrewer)
library(scales)
# sf and terra dependencies

# Loading data files
nsw.rate <- read.csv('Annual Crime Rate in NSW.csv')
region.mapping <- read.csv('LGA-Region Mapping.csv')
region.rate <- read.csv('Crime Rate by Region Recent Trends.csv')
lga.rate <- read.csv('Crime Rate by LGA Recent Trends.csv')
offence.counts <- read.csv('Offence Counts.csv')
offence.rates <- read.csv('Offence Rates.csv')
nsw <- readOGR("NSW_LGA_POLYGON_shp", "NSW_LGA_POLYGON_shp")
exploratory.data <- read.csv('Exploratory_Tab_Data.csv')

# changing region and lga to factor var
region.mapping$LGA <- as.factor(region.mapping$LGA)
region.mapping$Region <- as.factor(region.mapping$Region)
region.rate$Region <- as.factor(region.rate$Region)
lga.rate$LGA <- as.factor(lga.rate$LGA)
offence.counts$offence.type <- as.factor(offence.counts$offence.type)
offence.rates$Offence <- as.factor(offence.rates$Offence)
exploratory.data$region <- as.factor(exploratory.data$region)
exploratory.data$offence <- as.factor(exploratory.data$offence)

# merging region mapping and nsw spatial poylgons df
nsw_merge <- merge(nsw, region.mapping, by.x = "NSW_LGA__3", by.y = "LGA")
colnames(nsw_merge@data)[1] = "LGA"

# choices for the dropdown in the exploratory tab
regions_choices = c("All Regions",levels(region.rate$Region))
offence_choices = c("All Offences",levels(offence.counts$offence.type))

# reorder regions to closely relate to their spatial position
region.rate$Region <- factor(region.rate$Region,
                  levels(region.rate$Region)[c(2,9,10,6,11,7,8,1,4,13,12,5,3)])

# Shiny part
# UI
ui <- fluidPage(
  # Title of the application
  navbarPage(title = "Safety in NSW",
    # Tab1: Overview Tab           
    tabPanel("Overview",
      # Text for the tab
      tags$h3("Overview of the Project"),
      tags$h5("The project used publicly available
      datasets from the NSW Government to understand the level of safety
      in NSW. The key questions explored in the project were:"),
      tags$ul(
        tags$li("What is the crime rate in NSW? How has it changed over the years?"),
        tags$li("How does the crime rate vary by region? What are the recent trends 
                  in the crime rates for each region?"),
        tags$li("What are the most common offences in NSW? 
                  What is the trend in the crime rate for these offences?")),
      tags$p("The follwing datasets were used in this project:"),
      tags$ul(
        tags$li(tags$a(href = "https://www.bocsar.nsw.gov.au/Pages/bocsar_datasets/Datasets-.aspx",
                       "Monthly number of Crimes in NSW by LGA from 
                       NSW Bureau of Crime Statistics and Research")),
        tags$li(tags$a(href = "http://stat.data.abs.gov.au/Index.aspx?DataSetCode=ABS_ERP_LGA2017",
                       "Estimated Resident Population by LGA from
                        Australian Bureau of Statistics")),
        tags$li(tags$a(href = "https://data.gov.au/data/dataset/f6a00643-1842-48cd-9c2f-df23a3a1dc1e",
                       "LGA Boundaries Shapefile from the
                          Department of Industry, Innovation and Science"))
        ),
      tags$p("For the readers who are interested to go beyond the findings covered, an
                exploratory tab is provided at the end. It shows the trends in the 
                crime rates for various offences by region and in the entire state.")
      ),
    # Tab2: Crime in NSW
    tabPanel("Crime Rate in NSW",
      sidebarLayout(
        sidebarPanel(style = "background-color: #ffffff;",      
          # Annotation text describing the trends
          tags$h3("Crime Rate Trends"),
          tags$h5("NSW has become a much safer place over the years. 
                    The crime rate in NSW has dropped significantly in the last 2 decades. 
                    It has decreased from ~12,000 crimes per 100,000 people in 2001 to 
                    ~9,000 crimes per 100,000 people in 2017; that is a 25% decrease in
                    the crime rate from 2001 to 2017. However, the crime rate in last 5-6 years
                    has not changed too much and has been around the 9000 mark."),
          tags$br(),
          tags$br(),
          # Directions for the reader
          tags$p(tags$em("Note that the chart is interactive. Click on a point to see the
                          number of offences and the population in that year. How has the
                          population changed over the years?"))
        ),
        mainPanel(
          tags$h4("Crime Rate in NSW"),
          ggvisOutput("crime_rate_nsw") # plot
        )
      )
    ),
    # Tab 3: Crime by Region
    tabPanel("Crime Rate by Region",
      sidebarLayout(
        sidebarPanel(style = "background-color: #ffffff;",
          # Annotation text describing the graphs
          tags$h3("Crime Rate Variation by Region"),
          tags$h5("Crime rate is much higher in the North Western and Far West regions,
                    compared to the other regions. The South Eastern Region has the lowest crime
                    rate. The crime rate varies from more than 15,000 in the North Western region
                    to less than 7,000 in the South Eastern Region."),
          tags$p(" The key findings from the recent trends in the crime rate by
                    region are:"),
          tags$ul(
            tags$li("In the North Western Region, the crime rate has decreased 
                      significantly in 2017"),
            tags$li("In the Far West Region, the crime rate has increased in the last two years"),
            tags$li("In all other regions, the crime rate has been more or less consistent over
                      the last 4 years")),
          tags$br(),
          tags$br(),
          # Directions for the reader
          tags$p(tags$em("Note that the map might take a few seconds to load. It is interactive 
                            allowing you to pan and zoom. Clicking on a LGA in the map 
                            shows the crime rate in the last 4 years for
                            the highlighted LGA. What is the crime rate in Sydney LGA?
                            Is it much higher than its surrounding LGAs?"))
          ),
        mainPanel(
          tags$h3("Crime Rate by Region"),
          leafletOutput("map"),
          plotOutput("region_rates")
          )
      )      
    ),
    # Tab 4: Common Offences
    tabPanel("Common Offences",
      sidebarLayout(
        sidebarPanel(style = "background-color: #ffffff;",
          # Annotation text to describe the graphs
          tags$h3("Most Common Offences"),
          tags$h5(" The most common offence in the recent years is theft, followed by 
                    transport regulatory offences, property damage and assault. The theft
                    rate has been declining over the years and has decreased from  
                    ~7,000 in 2001 to 3,000 in 2017. The crime rate of property related 
                    offences has decreased from ~1,500 in 2008 to ~800 in 2017.
                    Even the assault rate has decreased slightly from ~1000 in 2001 to
                    ~800 in 2017. However, the tranport regulatory offence rate has increased
                    from ~125 in 2001 to ~1650 in 2017.
                    "),
          tags$br(),
          tags$br(),
          # Directions for the reader
          tags$p(tags$em("Note that the below line chart showing the crime rates of
                          common offences is interactive. Clicking on a point in the chart 
                          shows the number of offences and population in that year for the
                          selected offence. How does the number of transport regulatory 
                          offences compare in 2001 and 2017?"))
          ),
        mainPanel(
            h3("Common Offences in NSW"),
            plotOutput("offences_plot"),
            h4("Crime Rate of Common Offences in NSW"),
            ggvisOutput("offences_trends")
          )
      )
    ),
    # Tab 5: Exploratory Tab
    tabPanel("Exploratory Tab",
      sidebarLayout(
        sidebarPanel(style = "background-color: #ffffff;",
          tags$h3("Exploratory Tab"),
          tags$h5("Select the offence and the region to see the trend in the crime rate 
                    for the selected offence type in the selected region.
                    Select 'All Regions' for the entire state and 'All Offences'
                    to include all the offence types.")
            ),
          mainPanel(
            h3("Crime Rate Trend"),
            selectInput(inputId = "selected_region",
                        label = "Select the region:",
                        choices = regions_choices,
                        selected = c("All Regions")),
            selectInput(inputId = "selected_offence",
                        label = "Select the type of offence:",
                        choices = offence_choices,
                        selected = c("All Offences")),
            plotOutput("exploratory_plot")
          )
        )
     )
  )                  
)

     
# Server
server <- function(input, output, session) {
    # tab 2
    # Function for generating tooltip text for line chart
    tooltip <- function(x) {
      if (is.null(x)) return(NULL)
      record <- nsw.rate[nsw.rate$year == x$year,] 
      paste0("Number of Offences: ", 
             format(record$total.offences,big.mark = ",", scientific = F),
             "<br>",
              "Population: ", 
              format(record$erp,big.mark = ",", scientific = F))
    }
    # interactive line chart for crime rate in NSW
    plot <- reactive({
      nsw.rate %>%
        ggvis(~year,~rate) %>%
        layer_smooths(stroke := "blue", se = T) %>%
        layer_points(fill := "blue", size := 50, size.hover := 200) %>%
        add_axis("x", title = "Year") %>%
        add_axis("y", title = "Crime Rate per 100,000 people", title_offset = 50) %>%
        add_tooltip(tooltip, "hover") %>%
        set_options(width = "auto")
      })
    plot %>% bind_shiny("crime_rate_nsw")
    
    # tab 3
    # leaflet map
    color_pal <- c(brewer.pal(n = 12, "Paired"), "yellow") # color palette
    pal_func <- colorFactor(color_pal, nsw_merge@data$Region) # palette function
    # Interactive map with label as LGA and pop up a table with relevant columns
    output$map <- renderLeaflet({
      leaflet(data = nsw_merge) %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal_func(Region),
          weight = 1,
          opacity = 1,
          color = "#666",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = ~LGA, 
          popup = popupTable(nsw_merge, 
                              zcol = c("LGA", "Region","CrimeRate.2014",
                              "CrimeRate.2015", "CrimeRate.2016",
                              "CrimeRate.2017"),
                              row.numbers = F, feature.id = F)) %>%
          leaflet::addLegend(pal = pal_func, values = nsw_merge@data$Region, 
                             opacity = 0.7, title = "Region",position = "topright")
    })
    # facet plot of crime rate by region
    output$region_rates <- renderPlot(
      ggplot(region.rate,aes(x = year, y = Crime.Rate)) +
      geom_point(color = "blue", size = 1) +
      geom_line(color = "blue") +
      facet_wrap(~Region, nrow = 3) +
      ylab('Crime Rate per 100,000 population') +
      ggtitle('Crime Rate in the last 4 years')
    ) 
    # Tab 4
    # Bar chart with count of offences
    
    output$offences_plot <- renderPlot({
      ggplot(offence.counts, aes(x = reorder(offence.type, number.of.crimes)
                             , y = number.of.crimes)) +
        geom_bar(stat = "identity") +
        facet_wrap(~year, nrow = 1) +
        ylab('Number of Crimes') +
        xlab('Offence Type') +
        scale_y_continuous(breaks = c(0,100000,200000),labels = comma) +
        coord_flip() +
        ggtitle('Number of Crimes by Offence Type')
    })
    # Function for tooltip text for interactive line chart
    offence_tooltip <- function(x) {
      if (is.null(x)) return(NULL)
      record <- offence.rates[(offence.rates$year == x$year) & (
                                  offence.rates$Offence == x$Offence),] 
      paste0("Number of Offences: ", 
             format(record$offences,big.mark = ",", scientific = F),
             "<br>",
             "Population: ", 
             format(record$erp,big.mark = ",", scientific = F))
    }
    
  # interactive line chart for crime rate of different offences in NSW
    offences.plot <- reactive({
      offence.rates %>%
        ggvis(~year,~crime.rate, stroke = ~Offence) %>%
        layer_lines() %>%
        layer_points(fill = ~Offence,size := 15,size.hover := 200) %>%
        add_axis("x", title = "Year") %>%
        add_axis("y", title = "Crime Rate per 100,000 people", title_offset = 50) %>%
        add_tooltip(offence_tooltip, "hover") %>%
        set_options(width = "auto")
        
    })
    offences.plot %>% bind_shiny("offences_trends")
    # Tab 5: Exploratory plot
    output$exploratory_plot <- renderPlot({
      expl.plt.data <- exploratory.data[exploratory.data$offence == input$selected_offence
                                        & exploratory.data$region == input$selected_region,]
      ggplot(aes(x = year, y = crime.rate), data = expl.plt.data) +
        geom_point(color = "blue") + 
        geom_line(color = "blue") +
        ylab("Incident rate per 100,000 people") +
        xlab("Year") +
        xlim(2000,2020) 
        #geom_smooth(color = "blue")
    })
}



shinyApp(ui, server)