# Title: FIT5147 Visualisation Project
# Author: Shruti Sandeep Mahajan
# Student ID: 31235786
# Tutor: Mohit Gupta

## --------------------------------------
## Importing required libraries
## --------------------------------------

#install.packages("shiny")
library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("dplyr")
library(dplyr)
#install.packages("leaflet")
library(leaflet)
#install.packages("tidyr")
library(tidyr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("rgdal")
library(rgdal)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("plotly")
library(plotly)
#install.packages("reshape2")
library(reshape2)
#install.packages("gapminder")
library(gapminder)
#install.packages("shinythemes")
library(shinythemes)
#install.packages("shinyWidgets")
library(shinyWidgets)
#install.packages("shinyalert")
library(shinyalert)
#install.packages("shinycssloaders")
library(shinycssloaders)
#install.packages("shinybusy")
library(shinybusy)

## --------------------------------------
## Reading data
## --------------------------------------

demographic_imdb <- read.csv("demographic_imdb.csv")
demographic_imdb <- demographic_imdb %>% drop_na()
ratings_data <-read.csv("IMDb ratings.csv")
page_votes <-read.csv("gender_votes.csv")

## --------------------------------------
## Transforming data
## --------------------------------------

# Considering data after the year 1924
demographic_imdb <- demographic_imdb %>% filter(year > 1924)

##############
# Retrieving name of the country with count of movies
demo_country_cnt <- as.data.frame(table(demographic_imdb$country))
# Renaming the column names
colnames(demo_country_cnt) <- c("NAME","movie_cnt")

# Changing the name of the countries as per international norms
demographic_imdb$country <-gsub("USA","United States", demographic_imdb$country)
# Changing the name of country to map data on the worldmap
demo_country_cnt$NAME <-  gsub("Soviet Union","Russia", demo_country_cnt$NAME)
demo_country_cnt$NAME <- gsub("USA","United States", demo_country_cnt$NAME)

# Loading thematic mapping for countries on world map
dsn <- "TM_WORLD_BORDERS_SIMPL-0.3"
layer1<- "TM_WORLD_BORDERS_SIMPL-0.3"
# Using readOGR() to read the thematic mappings of the file
map_world = readOGR(dsn,layer1)

# Performing cleaning of thematic data to suit our purpose
map_world@data$POP2005 <- map_world@data$POP2005 %>% replace_na(0)
# Performing computations to simplify the values
map_world@data$POP2005 <- as.numeric(as.character(map_world@data$POP2005)) / 1000000 %>% round(3)

# Selecting required features for the data
map_world@data <- data.frame(map_world@data , demo_country_cnt[match(map_world@data$NAME, demo_country_cnt$NAME),])
# Retrieving required features
map_world@data <- map_world@data[,c("FIPS","ISO2","ISO3","LON","LAT","UN","NAME","AREA","POP2005",
                                      "REGION","SUBREGION","movie_cnt")]

# Setting up palette for for the world map for bins set for range of number of movies
pal_hue <- colorBin( palette="YlGn", domain=map_world@data$movie_cnt ,na.color="transparent", bins=c(0,750,1500,2250,3000,Inf))

######

# Assigning two categories for gender (to be used in page 3)
gender_choice <- c("Male","Female")

## --------------------------------------
## Creating UI
## --------------------------------------

ui <- navbarPage(
   # Setting aestheitcs for tab
   position = c("static-top", "fixed-top", "fixed-bottom"),
   header = NULL,
   footer = NULL,
   inverse = TRUE,
   fluid = TRUE,
   
   # Title of web-based application
   title= h4("Analysis of IMDb data"), 
   
    # First tab
    tabPanel(h5("Introduction"),
            # Shiny alert widget as pop up message for users 
            useShinyalert(), 
            # Setting background color
            setBackgroundColor("ghostwhite",gradient ="linear"),
            # Setting the aesthetics for the Introduction page
            fluidRow(
              h3(htmlOutput("welcome_page"), align="center", style = "font-family: 'times';color: #000000; margin-left:80px; margin-right:80px;")
                    ),
            # Loading image on the web page
            div(tags$img(src = "mov.jpg", height = 500, width = 1000), style="text-align:center;"),
               ),
              
    # Second tab
    tabPanel(h5("Demographic Analysis"),
          fluidRow(column(4,offset=4,h3("Demographic Analysis of IMDb data"))),
          fluidRow(
          
            column(2,
            wellPanel(title = "User Guide", 
                     solidHeader = TRUE, 
                     status = "primary", 
                     style = "background: yellow",
                     htmlOutput("usr_guide_pg1"))
                  ),
            
            column(10,(wellPanel(title = "Insights", 
                                solidHeader = TRUE, 
                                status = "primary", 
                                #style = "background:blue",
                                htmlOutput("Insight_pg1"))))
                  ),
          
          fluidRow( 
            
            column(2,wellPanel(selectInput("features","Genre:",unique(demographic_imdb$genre),
                                           multiple = TRUE,
                                           selected = c("Drama","Horror","Thriller","Romance")))
                   ),
            
            column(5,
            wellPanel(withSpinner(leafletOutput("country_plot",height=400)),align="center")
                  ),
            
            column(5,
                   wellPanel(withSpinner(plotOutput("genre_count",height=400)))
                   )
          )),
   
    # Third tab
    tabPanel(h5("Time-based Analysis"),
            fluidRow(column(4,offset=4,h3("Time based Analysis of IMDb data"))),
       
            fluidRow(
              column(2,
                wellPanel(title = "User Guide", 
                solidHeader = TRUE, 
                status = "primary",
                style = "background: yellow",
                htmlOutput("usr_guide_pg2"))
                ),
              
              column(10,wellPanel(title = "Insights", 
                                 solidHeader = TRUE, 
                                 status = "primary", 
                                 htmlOutput("insight_pg2"),
                                 height=200)
                    )),
                
             fluidRow(         
               column(2,wellPanel(
                 selectInput("select_genre",
                             "Select one or more Genre:",
                              unique(demographic_imdb$genre),
                              multiple = TRUE,
                              selected = c("Drama","Crime","Mystery","Comedy")),
                                  
                 selectInput("sun_year","Select an year",unique(demographic_imdb$year),selected = c("2020")),
                  
                 selectInput("heat_lang","Select languages",unique(demographic_imdb$language),multiple=TRUE,
                              selected = c("English","Hindi","French","German","Russian","Spanish")))
                      ),
                  
                 column(10,wellPanel(withSpinner(plotlyOutput("genre_trend",height=350)))
                        )
                      ),
               
            fluidRow(
              column(5,offset=2,wellPanel(withSpinner(plotlyOutput("sunburst",height=400)))),
              column(5,wellPanel(withSpinner(plotlyOutput("heat_map",height=400)))))
    ),
              
    # Fourth tab  
    tabPanel(h5("Popularity based Analysis"),
          fluidRow(column(4,offset=4,h3("Popularity based Analysis of IMDb data"))),
     
          fluidRow(
            column(2,
            wellPanel(
              title = "User Guide", 
              solidHeader = TRUE, 
              status = "primary", 
              style = "background: yellow",
              htmlOutput("usr_guide_pg3"))),
          
            column(10,
                   wellPanel(title = "Insight", 
                             solidHeader = TRUE, 
                             status = "primary", 
                             htmlOutput("insight_pg3")))),
                          
         fluidRow(
           column(2,
                  selectInput("gender_sel","Select Gender",choices=gender_choice,multiple= TRUE,selected =c("Female")),
                     
                  checkboxInput('all','Select All/None',value =TRUE),
                        
                  selectInput("age_sel","Select Age group",c("0-18 years","18-30 years","30-45 years","Above 45 years"),
                                    multiple=FALSE, selected=c("18-30 years")),
                        
                  selectInput("gnr_sel","Select Genre",unique(demographic_imdb$genre),multiple=TRUE,
                                        selected=c("Drama","Comedy","Biography","Action","Animation"))),
              
          column(5,wellPanel(withSpinner(plotlyOutput("box_chart")))),
              
          column(5,wellPanel(withSpinner(plotlyOutput("scatter"))))
                 
                      ))
   
            )

## -----------------------------------------------------
## Configuring the server 
## -----------------------------------------------------            

server<- function(input,output,session){
  

  # Pop up message when application loads
  shinyalert("Please wait..the movies are loading!",
  imageUrl ="https://media.istockphoto.com/vectors/cinema-hall-movie-interior-with-coming-soon-text-on-white-screen-and-vector-id1066635730?k=6&m=1066635730&s=612x612&w=0&h=5THdaHrMGqqRVJHYnQFkrKHbCRdExr53Rn1RyrougOk=")
  
  # Text for introduction page
  output$welcome_page <- renderUI({HTML(paste("<B> Movies have been contributing to entertainment worldwide for many years.Let us explore the 
                             multitude of features and insights gathered from the IMDb data from the year 1924 to 2020.",
                             "IMDb has been performing credit indexing for movies worldwide and has a vast dataset with multiple attributes. These atributes will help 
                              us to garner insights into the popularity of movies and audience perspective for various movies ",sep="<br/>"))})
  
  # User guide for page 1
  output$usr_guide_pg1 <- renderUI({HTML(paste("<B> User Guide", 
                                      "1. Select desired genre(s).",
                                      "2. Click on a country over the map.",
                                      sep="<br/>"))})
  
  # Insights from page 1
  output$Insight_pg1 <- renderUI({HTML(paste("Here we aim to get insights into the distribution of number of movies made in different genres
                                worldwide,i.e. in different countries. This helps us understand the popularity of genres demographically.",
                                "<B> For example: In India, most of the movies are from Drama genre , followed by Action and Comedy.
                                Animation can be observed as a popular genre in Japan when compared to other countries.This is particularly
                                helpful for publication houses to understand the global market to make informed decisions. The distribution of movies
                                in different genre to total movies in the country can also be observed here.",sep="<br/>"))})
  
  # User guide for page 2
  output$usr_guide_pg2 <- renderUI({HTML(paste("<B> User Guide",
                                      "1. Select desired genre(s).",
                                      "2. Select desired year.",
                                      "3. Select desired language(s)",sep="<br/>"))})
  # Insights from page 2
  output$insight_pg2 <-renderUI({HTML(paste("Here we aim understand the trend of genres across the years
                                      and also get insights into the dependency between genre and languages for a given year.This visualisation is helpful for 
                                      understanding the effect languages have on genre and how the viewership has grown over the years.",
                                      "<B> For example: The movies for Comedy genre rose significantly from 2000 to 2004 with most maximum number of movies
                                      made in English language.","These insights are helpful for movie buffs and movie critics to analyse the
                                      distibution of genre and language for various years","",sep="<br/>"))})
  
  # User guide for page 3
  output$usr_guide_pg3 <- renderUI({HTML(paste("<B> User Guide", 
                                      "1. Select a gender type.",
                                      "2. Select desired age group.",
                                      "3. Select desired genres",sep="<br/>"))})
  
  # Insights from page 3
  output$insight_pg3 <- renderUI({HTML(paste("Here we aim to understand the popularity of various genres within different genders and age 
                                             group. We also understand how the popularity of genres is changing within age groups wrt duration of the movie.
                                             Duration of a movie is a major factor towards which various age groups and genders can have different affinity.",
                                             "<B> For example: Animation genre has received high votes for the age group of 0-18 years from 
                                             both Male and Female gender. However, it can be observed that it did not receive higher ratings
                                             from viewers in the age group of 45 above years.","It can also be observed that with increase in duration, the 
                                             votes received for 0-18 age group the average votes were more when copared to age group of 45 and above years.",sep="<br/>"))})
  
  # World map visualisation
  output$country_plot <- renderLeaflet({
    leaflet(map_world) %>%
      # Now add tiles to it
      addTiles() %>%  
      addPolygons( 
        fillColor = ~pal_hue(movie_cnt), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        weight=0.3,
        label = paste( "Country:", map_world@data$NAME,",",
                         "Number of Movies: ", map_world@data$movie_cnt, 
                         sep=" "),
        labelOptions = labelOptions( 
          style = list("font-weight" = "bold", padding = "4px 8px"), 
          textsize = "13px", 
          direction = "auto",
          highlight = highlightOptions(weight=5,color="red",fillOpacity = 0.7,bringToFront = TRUE)
        ),
        layerId = map_world@data$NAME) %>%
      # Setting the default zoom level for map
      setView(lng= 0, lat=30, zoom = 2)%>%
      addLegend(pal= pal_hue, values= ~movie_cnt, title="Number of Movies",position = "bottomleft")
                                      })

  observe(
    {
      click = input$country_plot_shape_click
      
      
      sub = map_world@data %>% filter(NAME %in% click$id)
      
      if(is.null(click) || is.null(sub))
        return()
      
        output$genre_count<- renderPlot({
        
            
        sel_country <-demographic_imdb %>%
            filter(country %in% sub$NAME)
        
        
        data_genre <- sel_country %>%
        filter(genre %in% input$features) %>%
        group_by(genre)%>%
        summarise(Num_genre_mov = length(genre))
        
        
        ggplot(data= data_genre, aes(x=genre, y=Num_genre_mov)) +
          geom_segment(aes(x = genre, y = 0, xend = genre, yend = Num_genre_mov ), color="black") +
          geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2,
                       hoverinfo = 'text',
                       text = ~paste('</br> Number of Movies: ', Num_genre_mov))+
          theme_light() +
          theme(
            panel.grid.major.x = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x  = element_text(angle=0, hjust=0.5, size=11,colour="black")
          ) +
          xlab("Genre") +
          ylab("Number of movies") +
          labs(title = paste("Number of movies in selected genre for",click$id))
                                        })
    })
    
  # Visualisation for line chart
  output$genre_trend <- renderPlotly({
    
    trn_genre <- demographic_imdb %>%
      filter(genre %in% input$select_genre) %>%
      group_by(genre,year)%>%
      summarise(Num_genre_mov = length(genre))
    
    plot_ly(trn_genre, x = ~year, y = ~Num_genre_mov, type = 'scatter', mode = 'lines',
            linetype = ~genre,line = list(color = 'Pastel1', width = 2,dash = 'solid'),
            hoverinfo = 'text',
            text = ~paste('</br> Year: ', year,
                          '</br> Number of movies: ', Num_genre_mov)) %>%
      layout(title = "Trend of Genres",
             xaxis = list(title = "Year"),
             yaxis = list (title = "Number of movies"))
    
                                      })
  # Visualisation for Sunburst
  output$sunburst <-renderPlotly({
    
    sun_burst <- demographic_imdb %>%
      filter(year %in% input$sun_year)  %>% 
      filter(genre %in% input$select_genre) %>%
      group_by(year,genre,language) %>% 
      summarise(Num_lang_mov = length(imdb_title_id))
    
    plot_ly(
      labels = sun_burst$language,
      parents = sun_burst$year,
      values = sun_burst$Num_lang_mov,
      type = 'sunburst',
      maxdepth = 3,
      domain = list(column = 1),
      insidetextorientation='radial',
      extendsunburstcolors = TRUE
            )%>%
      layout(title = "Distribution of movies in different language with years")
                                })
  
  #Visualisation for heat map
  output$heat_map <-renderPlotly({
    
    heat_data <- demographic_imdb %>%
      
      filter(year %in% input$sun_year)  %>%  
      filter(genre %in% input$select_genre) %>%
      filter(language %in% input$heat_lang) %>%
      group_by(year,genre,language) %>% 
      summarise(Num_lang_mov = length(imdb_title_id)) 
      
    heat_data$Num_lang_mov[is.na(heat_data$Num_lang_mov)] <- 0
    
    plot_ly(x=heat_data$genre, y=heat_data$language, 
            z = heat_data$Num_lang_mov, 
            type = "heatmap", 
            colorscale='Bluered_r',
            hoverinfo = 'text',
            text = ~paste('</br> Language: ', heat_data$language,
                          '</br> Genre: ', heat_data$genre,
                          '</br> Number of movies: ', heat_data$Num_lang_mov))%>%
      layout(title = "Distribution of genre with language",
             xaxis = list(title = "Genre"),
             yaxis = list (title = "Language"))
  })
  
  observe({
    updateSelectInput(session,"gender_sel",choices= gender_choice,selected=if(input$all)gender_choice)
  })
  
  
  # Visualisation for Box Plot
  output$box_chart <-renderPlotly({
    # For selected gender = Male
    if(input$gender_sel == "Male"){
      male_age_votes <- page_votes %>% select(genre, males_0age_avg_vote,males_18age_avg_vote,
                                              males_30age_avg_vote,males_45age_avg_vote)
      if(input$age_sel == "0-18 years"){
        male_age_votes <- male_age_votes %>% select(genre,males_0age_avg_vote)
        male_age_votes <- male_age_votes %>% rename(male_under_18_years = males_0age_avg_vote)
      }
      if(input$age_sel == "18-30 years"){
        male_age_votes <- male_age_votes %>% select(genre,males_18age_avg_vote)
        male_age_votes <- male_age_votes %>% rename(male_18_to_30_years = males_18age_avg_vote)
      }
      if(input$age_sel == "30-45 years"){
        male_age_votes <- male_age_votes %>% select(genre,males_30age_avg_vote)
        male_age_votes <- male_age_votes %>% rename(male_30_to_45_years = males_30age_avg_vote)
      }
      if(input$age_sel == "Above 45 years"){
        male_age_votes <- male_age_votes %>% select(genre,males_45age_avg_vote)
        male_age_votes <- male_age_votes %>% rename(male_above_45_years = males_45age_avg_vote)
      }
      male_age_votes <- male_age_votes %>% filter(genre %in% input$gnr_sel)
      age_votes <-melt(male_age_votes)
    }
    
    # For selected gender = Female
    if(input$gender_sel == c("Female")){
      female_age_votes <- page_votes %>% select(genre, females_0age_avg_vote,females_18age_avg_vote,
                                                females_30age_avg_vote,females_45age_avg_vote)
      if(input$age_sel == "0-18 years"){
        female_age_votes <- female_age_votes %>% select(genre,females_0age_avg_vote)
        female_age_votes <- female_age_votes %>% rename(female_under_18_years = females_0age_avg_vote)
      }
      if(input$age_sel == "18-30 years"){
        female_age_votes <-female_age_votes %>% select(genre,females_18age_avg_vote)
        female_age_votes <- female_age_votes %>% rename(female_18_to_30_years = females_18age_avg_vote)
      }
      if(input$age_sel == "30-45 years"){
        female_age_votes <- female_age_votes %>% select(genre,females_30age_avg_vote)
        female_age_votes <- female_age_votes %>% rename(female_30_to_45_years = females_30age_avg_vote)
      }
      if(input$age_sel == "Above 45 years"){
        female_age_votes <- female_age_votes %>% select(genre,females_45age_avg_vote)
        female_age_votes <- female_age_votes %>% rename(female_above_45_years = females_45age_avg_vote)
      }
      female_age_votes <- female_age_votes %>% filter(genre %in% input$gnr_sel)
      age_votes <-melt(female_age_votes)
    }
    # For selected gender = Male and Female
    if(input$all == TRUE){
      gndr_age_votes <- page_votes %>% select(genre,males_0age_avg_vote,males_18age_avg_vote,males_30age_avg_vote,males_45age_avg_vote,
                                              females_0age_avg_vote,females_18age_avg_vote,females_30age_avg_vote,females_45age_avg_vote)
      if(input$age_sel == "0-18 years"){
        gndr_age_votes <- gndr_age_votes %>% select(genre,females_0age_avg_vote,males_0age_avg_vote)
        gndr_age_votes <- gndr_age_votes %>% rename(male_under_18_years = males_0age_avg_vote)
        gndr_age_votes <- gndr_age_votes %>% rename(female_under_18_years = females_0age_avg_vote)
      }
      if(input$age_sel == "18-30 years"){
        gndr_age_votes <- gndr_age_votes %>% select(genre,females_18age_avg_vote,males_18age_avg_vote)
        gndr_age_votes <- gndr_age_votes %>% rename(male_18_to_30_years = males_18age_avg_vote)
        gndr_age_votes <- gndr_age_votes %>% rename(female_18_to_30_years = females_18age_avg_vote)
      }
      if(input$age_sel == "30-45 years"){
        gndr_age_votes <- gndr_age_votes %>% select(genre,females_30age_avg_vote,males_30age_avg_vote)
        gndr_age_votes <- gndr_age_votes %>% rename(male_30_to_45_years = males_30age_avg_vote)
        gndr_age_votes <- gndr_age_votes %>% rename(female_30_to_45_years = females_30age_avg_vote)
      }
      if(input$age_sel == "Above 45 years"){
        gndr_age_votes <- gndr_age_votes %>% select(genre,females_45age_avg_vote,males_45age_avg_vote)
        gndr_age_votes <- gndr_age_votes %>% rename(male_above_45_years = males_45age_avg_vote)
        gndr_age_votes <- gndr_age_votes %>% rename(female_above_45_years = females_45age_avg_vote)
      }
      gndr_age_votes <- gndr_age_votes %>% filter(genre %in% input$gnr_sel)
      age_votes <-melt(gndr_age_votes)
    }
    
    
    
    fig <- plot_ly(age_votes, x=~variable, y = ~value, color = ~genre, type = "box")
    fig <- fig %>%
      layout(title="Genre popularity with Age group and Gender",
             xaxis= list(title="Audience of selected gender and age group"),
             yaxis=list(title= "Number of votes"),
             boxmode = "group")
    fig
  
  })
  
  # Visualisation for scatter plot
  output$scatter<-renderPlotly({ 
    # For selected gender = Male
    if(input$gender_sel == "Male"){
      
      if(input$age_sel == "0-18 years"){
      
      rate_pg <- page_votes %>% select(imdb_title_id,genre,continent,country,year,duration,males_0age_avg_vote)
      rate_pg <- rate_pg %>% rename(male_vote = males_0age_avg_vote)
      }
      if(input$age_sel == "18-30 years"){
        
        rate_pg <- page_votes %>% select(imdb_title_id,genre,continent,country,year,duration,males_18age_avg_vote)
        rate_pg <- rate_pg %>% rename(male_vote = males_18age_avg_vote)
      }
      if(input$age_sel == "30-45 years"){
        
        rate_pg <- page_votes %>% select(imdb_title_id,genre,continent,country,year,duration,males_30age_avg_vote)
        rate_pg <- rate_pg %>% rename(male_vote = males_30age_avg_vote)
      }
      if(input$age_sel == "Above 45 years"){
        
        rate_pg <- page_votes %>% select(imdb_title_id,genre,continent,country,year,duration,males_45age_avg_vote)
        rate_pg <- rate_pg %>% rename(male_vote = males_45age_avg_vote)
      }
      
      rate_pg <- rate_pg %>% filter(genre %in% input$gnr_sel)
      scat_data<-rate_pg %>% group_by(imdb_title_id,genre, duration,male_vote)%>% summarize(no_mov = length(imdb_title_id),
                                                                          no_dur = mean(duration),no_votes = mean(male_vote))
      
    }
    # For selected gender = Female
    if(input$gender_sel == "Female"){
      
      if(input$age_sel == "0-18 years"){
        
        rate_pg <- page_votes %>% select(imdb_title_id,genre,continent,country,year,duration,females_0age_avg_vote)
        rate_pg <- rate_pg %>% rename(female_vote = females_0age_avg_vote)
      }
      if(input$age_sel == "18-30 years"){
        
        rate_pg <- page_votes %>% select(imdb_title_id,genre,continent,country,year,duration,females_18age_avg_vote)
        rate_pg <- rate_pg %>% rename(female_vote = females_18age_avg_vote)
      }
      if(input$age_sel == "30-45 years"){
        
        rate_pg <- page_votes %>% select(imdb_title_id,genre,continent,country,year,duration,females_30age_avg_vote)
        rate_pg <- rate_pg %>% rename(female_vote = females_30age_avg_vote)
      }
      if(input$age_sel == "Above 45 years"){
        
        rate_pg <- page_votes %>% select(imdb_title_id,genre,continent,country,year,duration,females_45age_avg_vote)
        rate_pg <- rate_pg %>% rename(female_vote = females_45age_avg_vote)
      }
      rate_pg <- rate_pg %>% filter(genre %in% input$gnr_sel)
      scat_data<-rate_pg %>% group_by(imdb_title_id,genre, duration,female_vote) %>% summarize(no_mov = length(imdb_title_id),
                                                                           no_dur = mean(duration),no_votes = mean(female_vote))
      
    }
    # For selected gender = Male and Female
    if(input$all == TRUE){
      
      if(input$age_sel == "0-18 years"){
        
        rate_pg <- page_votes %>% select(imdb_title_id,genre,continent,country,year,duration,allgenders_0age_avg_vote)
        rate_pg <- rate_pg %>% rename(all_vote = allgenders_0age_avg_vote)
      }
      if(input$age_sel == "18-30 years"){
        
        rate_pg <- page_votes %>% select(imdb_title_id,genre,continent,country,year,duration,allgenders_18age_avg_vote)
        rate_pg <- rate_pg %>% rename(all_vote = allgenders_18age_avg_vote)
      }
      if(input$age_sel == "30-45 years"){
        
        rate_pg <- page_votes %>% select(imdb_title_id,genre,continent,country,year,duration,allgenders_30age_avg_vote)
        rate_pg <- rate_pg %>% rename(all_vote = allgenders_30age_avg_vote)
      }
      if(input$age_sel == "Above 45 years"){
        
        rate_pg <- page_votes %>% select(imdb_title_id,genre,continent,country,year,duration,allgenders_45age_avg_vote)
        rate_pg <- rate_pg %>% rename(all_vote = allgenders_45age_avg_vote)
      }
      rate_pg <- rate_pg %>% filter(genre %in% input$gnr_sel)
      scat_data<-rate_pg %>% group_by(imdb_title_id,genre, duration,all_vote) %>% summarize(no_mov = length(imdb_title_id),
                                                                           no_dur = mean(duration),no_votes = mean(all_vote))
      
    }
    
    scat_data <-scat_data %>% group_by(genre) %>% summarize(no_mov = length(imdb_title_id),
                                                            no_dur = mean(duration),no_votes = mean(no_votes))
    
    scat_data %>% 
      plot_ly() %>% 
      add_markers(x = ~no_votes,
                  y = ~no_dur,
                  color = ~genre,
                  size = ~no_mov,
                  text = ~paste("Average duration of movie: ", round(no_dur,1), 
                                "<br>", 
                                "Average votes:", round(no_votes,1), 
                                "<br>", 
                                "Average Number of movies:", round(no_mov,1)
                  ), 
                  hoverinfo = "text") %>% 
      layout(title="Genre Popularity for various duration",
             xaxis=list(title="Votes received(scale :1-10)"),
             yaxis=list(title= "Duration(in minutes)")) 
    
  })

  }
  
## -----------------------------------------------------
## Running the Shiny App
## -----------------------------------------------------

shinyApp(ui,server)