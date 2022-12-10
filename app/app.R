rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)

page_one <- tabPanel("Introduction",
                     h3("Analyzing CO2 Emission Trends Across the World Over Time"),
                     img(src = "https://luminategroup.com/storage/633/shutterstock_336888689.jpg",
                         width = "50%,", height = "50%"),
                     p("Global CO2 emissions have risen over time, which is a problem because it contributes to global warming."),
                     p("The measures of CO2 that this report focuses on are total CO2 emissions by country as well as total
                       CO2 emissions per capita for each country. I chose these measures because I wanted to see whether or not
                       population size would play a role in CO2 emissions.")
                     
)


page_two <- tabPanel(
  "CO2 Emission Visualizations", 
  titlePanel("CO2 Emission Trends"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId="plottype",
        label="Show CO2 Emission per person or total",
        choices = c("CO2 Emission per person","CO2 Emission Total")
      ),
      
      pickerInput(
        inputId = "countries",
        label = "Select countries",
        ####top 10 countries
        choices = c("China","United States","India","Russia","Japan","Iran","Germany","Saudi Arabia","Indonesia","South Korea"),
        multiple = TRUE,
        selected = c("China","United States","Russia","Japan")
      ),
      submitButton(text = "Update Chart", icon = NULL, width = NULL)
    ),
    mainPanel(  
      plotlyOutput(outputId = "myplot"),
      p("More developed countries tend to have higher CO2 emission per person. The United States has consistently had the highest CO2 per capita."),
      p("Both China's total CO2 emission and CO2 emission per capita has been noticeably increasing."),
      p("We need to focus on developing more green and lower CO2 emission alternatives in industries.")
      
    )
  )
)
# Pass each page to a multi-page layout (`navbarPage`)
app_ui <- navbarPage(
  "My Application", # application title
  page_one,         # include the first page content
  page_two,         # include the second page content page_three        # include the third page content
)

############################################################################

# The server is a function that takes `input` and `output` arguments
app_server <- function(input, output) {
  ##Page One Introduction
  
  output$page1plot <- renderPlotly({
    url <- "https://nyc3.digitaloceanspaces.com/owid-public/data/co2/owid-co2-data.csv"
    destination <- "/Users/clairezhang/Documents/info201/assignments/a5-clairezhaang/data/co2.csv"
    download.file(url, destination)
    co2df_original <- read.csv("/Users/clairezhang/Documents/info201/assignments/a5-clairezhaang/data/co2.csv", stringsAsFactors = FALSE) %>%
      na.omit(co2df_original)
    
    #View(co2df_original)
    
    co2_total_2018 <- co2df_original %>%
      filter(year == 2018) %>%
      select(country, co2) %>%
      arrange(desc(co2)) %>%
      
      top5_co2total_2018 <- co2_total_2018[-1,] %>%
      head(5)
    
    max_co2total_2018 <- top5_co2total_2018 %>%
      filter(co2 == max(co2)) %>%
      pull(country)
    # China
    
    co2_capita_2018 <- co2df_original %>%
      filter(year == 2018) %>%
      select(country, co2_per_capita) %>%
      arrange(desc(co2_per_capita))
    
    top5_co2capita_2018 <- co2_capita_2018 %>%
      head(5)
    
    max_co2_capita_2018 <- co2_capita_2018 %>%
      filter(co2_per_capita == max(co2_per_capita)) %>%
      pull(country)
    # Kazakhstan
    
    co2_gdp_2018 <- co2df_original %>%
      filter(year == 2018) %>%
      select(country, co2_per_gdp) %>%
      arrange(desc(co2_per_gdp))
    
    #Average annual total production-based emissions of CO₂, measured in kilograms per dollar of GD, across all the countries
    Avg_co2_per_gdp_allcountries_2018 <- co2_gdp_2018 %>%
      summarise(avg_co2_per_gdp2018 = mean(co2_per_gdp, na.rm=TRUE))
    #  0.2572955
    
    #Where is co2 per GDP the highest / lowest in 2018?
    highest_co2_per_gdp_2018 <- co2_gdp_2018 %>% 
      filter(co2_per_gdp == max(co2_per_gdp, na.rm=TRUE)) %>% 
      pull(country)
    highest_co2_per_gdp_2018
    
    lowest_co2_per_gdp_2018 <- co2_gdp_2018 %>% 
      filter(co2_per_gdp == min(co2_per_gdp,na.rm=TRUE)) %>% 
      pull(country)
    lowest_co2_per_gdp_2018
    
    #How much has my variable change over the last N years?
    thiscountry = input$country
    
    countriesAvgco2_per_gdp <- co2df_original %>%
      filter(nchar(iso_code)>0) %>%
      filter(year>1980)%>%
      filter(country==thiscountry)%>%
      select(year,country,co2_per_gdp)
    #View(countriesAvgco2_per_gdp)
    
    lineplot <-ggplot(data=countriesAvgco2_per_gdp)+
      geom_col(aes(x=year,y=co2_per_gdp,color=thiscountry))
    labs(
      title = "world average co2 per gdp over the years", # plot title
      x = "year", # x-axis label
      y = "co2 per gdp", # y-axis label
      color = "Country" 
    ) 
    ggplotly(lineplot)
  })
  
  ##Page Two Content Plotting the CO2 Emissions 
  output$myplot <- renderPlotly({
    url <- "https://nyc3.digitaloceanspaces.com/owid-public/data/co2/owid-co2-data.csv"
    destination <- "owid-co2-data.csv"
    download.file(url,destination)
    co2df_original <- read.csv("owid-co2-data.csv",stringsAsFactors = FALSE) %>% filter(nchar(iso_code)>0)
    #View(co2df_original)
    
    #Find top 10 co2 emission countries in year 2021
    co2df <- co2df_original %>%
      filter(year==2021) %>%
      #select(year,country,iso_code,population,gdp,co2,co2_per_gdp,share_global_co2,primary_energy_consumption)
      select(year,country,population,share_global_co2,co2)
    #View(co2df)
    top10countries <- co2df %>%
      arrange(desc(share_global_co2)) %>%
      slice_head(n=10)%>%
      pull(country)
    #View(top10countries)
    is.vector(top10countries)
    
    ######Generate data frame according to input$plottype
    ##Generate Emission Per Person data frame
    if(input$plottype=="CO2 Emission per person"){
      top10_co2_years <- co2df_original %>%
        filter(year>1950) %>% filter(nchar(iso_code)>0) %>%
        filter(country %in% top10countries)%>%
        mutate(co2perperson=co2/population)%>%
        select(year,country,co2perperson)%>%
        spread(key=country,value=co2perperson)
      #View(top10_co2_years)
      plottitle = "Countries CO2 Emission per person"
      ylab = "CO2 Emission Per Person"
      
    }else{ ##Generate Total CO2 Emissions data frame
      top10_co2_years <- co2df_original %>%
        filter(year>1950) %>% filter(nchar(iso_code)>0) %>%
        filter(country %in% top10countries)%>%
        select(year,country,share_global_co2)%>%
        spread(key=country,value=share_global_co2)
      #View(top10_co2_years)
      plottitle = "Countries CO2 Global Share"
      ylab = "CO2 Global Share"
    }
    
    countries <-input$countries
    linescount = length(countries)
    
    if(linescount==1){
      lineplot <-ggplot(data=top10_co2_years)+
        geom_line(aes(x=year,y=top10_co2_years[[countries[1]]],color=countries[1]))+
        labs(
          title = plottitle, # plot title
          x = "year", # x-axis label
          y = ylab, # y-axis label
          color = "Countries" # legend label for the "color" property
        ) 
    }else if(linescount==2){
      lineplot <-ggplot(data=top10_co2_years)+
        geom_line(aes(x=year,y=top10_co2_years[[countries[1]]],color=countries[1]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[2]]],color=countries[2]))+
        labs(
          title = plottitle, # plot title
          x = "year", # x-axis label
          y = ylab, # y-axis label
          color = "Countries" # legend label for the "color" property
        ) 
      
    }else if(linescount==3){
      lineplot <-ggplot(data=top10_co2_years)+
        geom_line(aes(x=year,y=top10_co2_years[[countries[1]]],color=countries[1]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[2]]],color=countries[2]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[3]]],color=countries[3]))+
        labs(
          title = plottitle, # plot title
          x = "year", # x-axis label
          y = ylab, # y-axis label
          color = "Countries" # legend label for the "color" property
        ) 
      
    }else if(linescount==4){
      lineplot <-ggplot(data=top10_co2_years)+
        geom_line(aes(x=year,y=top10_co2_years[[countries[1]]],color=countries[1]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[2]]],color=countries[2]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[3]]],color=countries[3]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[4]]],color=countries[4]))+
        labs(
          title = plottitle, # plot title
          x = "year", # x-axis label
          y = ylab, # y-axis label
          color = "Countries" # legend label for the "color" property
        ) 
      
    }else if(linescount==5){
      lineplot <-ggplot(data=top10_co2_years)+
        geom_line(aes(x=year,y=top10_co2_years[[countries[1]]],color=countries[1]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[2]]],color=countries[2]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[3]]],color=countries[3]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[4]]],color=countries[4]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[5]]],color=countries[5]))+
        labs(
          title = plottitle, # plot title
          x = "year", # x-axis label
          y = ylab, # y-axis label
          color = "Countries" # legend label for the "color" property
        ) 
      
    }else if(linescount==6){
      lineplot <-ggplot(data=top10_co2_years)+
        geom_line(aes(x=year,y=top10_co2_years[[countries[1]]],color=countries[1]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[2]]],color=countries[2]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[3]]],color=countries[3]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[4]]],color=countries[4]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[5]]],color=countries[5]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[6]]],color=countries[6]))+
        labs(
          title = plottitle, # plot title
          x = "year", # x-axis label
          y = ylab, # y-axis label
          color = "Countries" # legend label for the "color" property
        ) 
      
    }else if(linescount==7){
      lineplot <-ggplot(data=top10_co2_years)+
        geom_line(aes(x=year,y=top10_co2_years[[countries[1]]],color=countries[1]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[2]]],color=countries[2]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[3]]],color=countries[3]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[4]]],color=countries[4]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[5]]],color=countries[5]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[6]]],color=countries[6]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[7]]],color=countries[7]))+
        labs(
          title = plottitle, # plot title
          x = "year", # x-axis label
          y = ylab, # y-axis label
          color = "Countries" # legend label for the "color" property
        ) 
      
    }else if(linescount==8){
      lineplot <-ggplot(data=top10_co2_years)+
        geom_line(aes(x=year,y=top10_co2_years[[countries[1]]],color=countries[1]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[2]]],color=countries[2]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[3]]],color=countries[3]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[4]]],color=countries[4]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[5]]],color=countries[5]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[6]]],color=countries[6]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[7]]],color=countries[7]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[8]]],color=countries[8]))+
        labs(
          title = plottitle, # plot title
          x = "year", # x-axis label
          y = ylab, # y-axis label
          color = "Countries" # legend label for the "color" property
        ) 
      
    }else if(linescount==9){
      lineplot <-ggplot(data=top10_co2_years)+
        geom_line(aes(x=year,y=top10_co2_years[[countries[1]]],color=countries[1]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[2]]],color=countries[2]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[3]]],color=countries[3]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[4]]],color=countries[4]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[5]]],color=countries[5]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[6]]],color=countries[6]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[7]]],color=countries[7]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[8]]],color=countries[8]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[9]]],color=countries[9]))+
        labs(
          title = plottitle, # plot title
          x = "year", # x-axis label
          y = ylab, # y-axis label
          color = "Countries" # legend label for the "color" property
        ) 
      
    }else if(linescount==10){
      lineplot <-ggplot(data=top10_co2_years)+
        geom_line(aes(x=year,y=top10_co2_years[[countries[1]]],color=countries[1]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[2]]],color=countries[2]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[3]]],color=countries[3]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[4]]],color=countries[4]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[5]]],color=countries[5]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[6]]],color=countries[6]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[7]]],color=countries[7]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[8]]],color=countries[8]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[9]]],color=countries[9]))+
        geom_line(aes(x=year,y=top10_co2_years[[countries[10]]],color=countries[10]))+
        labs(
          title = plottitle, # plot title
          x = "year", # x-axis label
          y = ylab, # y-axis label
          color = "Countries" # legend label for the "color" property
        ) 
    }
    
    #lineplot
    ggplotly(lineplot)
  })
}

# Run the application 
shinyApp(ui = app_ui, server = app_server)

