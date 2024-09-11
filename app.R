# load libraries
library(leaflet)
library(shiny)
library(readr)
library(shinydashboard)
library(rgeos)
library(rgdal)
library(ggmap)
library(shinyjs)
library(maptools)
library(raster)
library(tidyr)
library(stringr)
library(leaflet.extras)
library(erer)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(sp)
library(readr)
library(shinyBS)
library(ggthemes)
library(DT)
library(data.table)
library(viridis)
library(tm) # Janice added this - corpus function not found in r
library(tidyverse) # Janice added this
library(ggpubr) # Janice added this
library(dplyr) # Janice moved this - dplyr needs to be loaded after plyr
library(gridExtra) # error - could not find function "grid.arrange"
library(wordcloud) # Error - could not find function "wordcloud"
library(SnowballC) # not sure if we need this..?
library(RColorBrewer) # not sure if we need this..?


########################################   Code to read in data    ####################################################
# load data
listing <- read.csv(paste(getwd(), "/listings_px_rating.csv", sep = ''))
listing <- listing[complete.cases(listing),]

reviews <- read.csv('reviews3.csv')
######################################################################################################################


### WORD CLOUD ####
text <- unlist(strsplit(as.character(reviews$comments), split=" "))
reviewsDF <- data.frame("word" = text)

# aggregate and count all of the words for each review
wordDF <- dplyr::count(reviewsDF, reviewsDF$word, sort=TRUE)
wordDF <- ungroup(wordDF)

# create document for analysis
docs <- Corpus(VectorSource(text)) 

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove punctuations
docs <- tm_map(docs,removePunctuation)

# Remove words we believed were unnescesary - our own stopwords
docs <- tm_map(docs, removeWords, c("1", "etc", "place", "host", "stay", "et", "de", "est",
                                    "just", "bit", "la", "will", "'s", "us", "also", "next", 
                                    "back", "overall", "two", "ist", "un", "alex", "felt", 
                                    "places", "never", "day", "und", "ever", "get", "one",
                                    "even", "many", "lot", "located", "stop"))

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# filter to ensure no empty words, sort by frequency
corpusdf <- data.frame(text=sapply(docs, identity), 
                       stringsAsFactors=F)
filteredcorpus <- filter(corpusdf, text != "")
wordDF <- count(filteredcorpus, text, sort=TRUE)
wordDF <- ungroup(wordDF)
#### END OF WORD CLOUD ####
######################################################################################################################


#### SEASONALITY ####
annualCount <- group_by(reviews, date = format(as.Date(reviews$date, format="%Y-%m-%d"),"%Y"))
annualCount <- tally(annualCount, wt = NULL, sort = FALSE, name = "n")

#### DEMAND ####
# Number of Reviews overtime by year
ggplot(annualCount, aes(x = date, y = n)) +
  geom_point(color = "#ed2d05") + geom_line(aes(group=1)) +
  ggtitle("Airbnb Popularity in Singapore",
          subtitle = "Reviews are used as a proxy for the demand of Airbnb rentals") +
  labs(x = "Year", y = "Number of Unique Listings Reviewed", face = "bold") + theme(plot.title = element_text(face = "bold"))

# Aggregate every observation by month
dailyCount <- group_by(reviews[year(reviews$date) == 2011,], date = format(as.Date(reviews[year(reviews$date) == 2011,]$date, format="%Y-%m-%d"),"%m"))
dailyCount <- tally(dailyCount, wt = NULL, sort = FALSE, name = "n")

reviews[year(reviews$date) == 2011,]

# 2011
y1 <- group_by(reviews[year(reviews$date) == 2011,], date = format(as.Date(reviews[year(reviews$date) == 2011,]$date, format="%Y-%m-%d"),"%m"))
y1 <- tally(y1, wt = NULL, sort = FALSE, name = "n")

y1D <- ggplot(y1, aes(x = date, y = n)) +
  geom_point(na.rm=TRUE, color = "#ed2d05") + geom_line(group=1) +
  ggtitle("Seasonality in Demand",
          subtitle = "Number of Reviews across Months in 2011") +
  labs(x = "Month", y = "Unique listings recieving reviews") + theme(plot.title = element_text(face = "bold"))


# 2012
y2 <- group_by(reviews[year(reviews$date) == 2012,], date = format(as.Date(reviews[year(reviews$date) == 2012,]$date, format="%Y-%m-%d"),"%m"))
y2 <- tally(y2, wt = NULL, sort = FALSE, name = "n")

y2D <- ggplot(y2, aes(x = date, y = n)) +
  geom_point(na.rm=TRUE, color = "#ed2d05") + geom_line(group=1) +
  ggtitle("Seasonality in Demand",
          subtitle = "Number of Reviews across Months in 2012") +
  labs(x = "Month", y = "Unique listings recieving reviews") + theme(plot.title = element_text(face = "bold"))

# 2013
y3 <- group_by(reviews[year(reviews$date) == 2013,], date = format(as.Date(reviews[year(reviews$date) == 2013,]$date, format="%Y-%m-%d"),"%m"))
y3 <- tally(y3, wt = NULL, sort = FALSE, name = "n")

y3D <- ggplot(y3, aes(x = date, y = n)) +
  geom_point(na.rm=TRUE, color = "#ed2d05") + geom_line(group=1) +
  ggtitle("Seasonality in Demand",
          subtitle = "Number of Reviews across Months in 2013") +
  labs(x = "Month", y = "Unique listings recieving reviews") + theme(plot.title = element_text(face = "bold"))

# 2014
y4 <- group_by(reviews[year(reviews$date) == 2014,], date = format(as.Date(reviews[year(reviews$date) == 2014,]$date, format="%Y-%m-%d"),"%m"))
y4 <- tally(y4, wt = NULL, sort = FALSE, name = "n")

y4D <- ggplot(y4, aes(x = date, y = n)) +
  geom_point(na.rm=TRUE, color = "#ed2d05") + geom_line(group=1) +
  ggtitle("Seasonality in Demand",
          subtitle = "Number of Reviews across Months in 2014") +
  labs(x = "Month", y = "Unique listings recieving reviews") + theme(plot.title = element_text(face = "bold"))

# 2015
y5 <- group_by(reviews[year(reviews$date) == 2015,], date = format(as.Date(reviews[year(reviews$date) == 2015,]$date, format="%Y-%m-%d"),"%m"))
y5 <- tally(y5, wt = NULL, sort = FALSE, name = "n")

y5D <- ggplot(y5, aes(x = date, y = n)) +
  geom_point(na.rm=TRUE, color = "#ed2d05") + geom_line(group=1)+
  ggtitle("Seasonality in Demand",
          subtitle = "Number of Reviews across Months in 2015") +
  labs(x = "Month", y = "Unique listings recieving reviews") + theme(plot.title = element_text(face = "bold"))

# of Reviews in 2016
y6 <- group_by(reviews[year(reviews$date) == 2016,], date = format(as.Date(reviews[year(reviews$date) == 2016,]$date, format="%Y-%m-%d"),"%m"))
y6 <- tally(y6, wt = NULL, sort = FALSE, name = "n")

y6D <- ggplot(y6, aes(x = date, y = n)) +
  geom_point(na.rm=TRUE, color = "#ed2d05") + geom_line(group=1)+
  ggtitle("Seasonality in Demand",
          subtitle = "Number of Reviews across Months in 2016") +
  labs(x = "Month", y = "Unique listings recieving reviews") + theme(plot.title = element_text(face = "bold"))

# # of Reviews in 2017
y7 <- group_by(reviews[year(reviews$date) == 2017,], date = format(as.Date(reviews[year(reviews$date) == 2017,]$date, format="%Y-%m-%d"),"%m"))
y7 <- tally(y7, wt = NULL, sort = FALSE, name = "n")

y7D <- ggplot(y7, aes(x = date, y = n)) +
  geom_point(na.rm=TRUE, color = "#ed2d05") + geom_line(group=1) +
  ggtitle("Seasonality in Demand",
          subtitle = "Number of Reviews across Months in 2017") +
  labs(x = "Month", y = "Unique listings recieving reviews") + theme(plot.title = element_text(face = "bold"))

# # of Reviews in 2018
y8 <- group_by(reviews[year(reviews$date) == 2018,], date = format(as.Date(reviews[year(reviews$date) == 2018,]$date, format="%Y-%m-%d"),"%m"))
y8 <- tally(y8, wt = NULL, sort = FALSE, name = "n")

y8D <- ggplot(y8, aes(x = date, y = n)) +
  geom_point(na.rm=TRUE, color = "#ed2d05") + geom_line(group=1) +
  ggtitle("Seasonality in Demand",
          subtitle = "Number of Reviews across Months in 2018") +
  labs(x = "Month", y = "Unique listings recieving reviews") + theme(plot.title = element_text(face = "bold"))

# 2019
y9 <- group_by(reviews[year(reviews$date) == 2019,], date = format(as.Date(reviews[year(reviews$date) == 2019,]$date, format="%Y-%m-%d"),"%m"))
y9 <- tally(y9, wt = NULL, sort = FALSE, name = "n")

y9D <- ggplot(y9, aes(x = date, y = n)) +
  geom_point(na.rm=TRUE, color = "#ed2d05") + geom_line(group=1) +
  ggtitle("Seasonality in Demand",
          subtitle = "Number of Reviews across Months in 2019") +
  labs(x = "Month", y = "Unique listings recieving reviews") + theme(plot.title = element_text(face = "bold"))
#### END OF DEMAND ####
######################################################################################################################

#### POPULARITY ####
## Airbnb Popularity in Singapore
# reviews <- read.csv("reviews3.csv", stringsAsFactors = FALSE)
reviewsNum <- reviews %>% group_by(date = as.Date(reviews$date)) %>% summarise(number = n())
Singapore <- ggplot(reviewsNum, aes(date, number)) +
  geom_point(color = "blue", alpha=0.2) + geom_smooth(method = "loess", color = 'red') + scale_x_date() +
  ggtitle("How popular is Airbnb in Singapore?", subtitle = "Number of Reviews for listings in Singapore across years") +
  labs(x = "Year", y = "Unique listings recieving reviews") 

## Airbnb Popularity in New York
NY_review <- read.csv("NY_reviews.csv", stringsAsFactors = FALSE)
NY_reviewsNum <- NY_review %>% group_by(date = as.Date(NY_review$date)) %>% summarise(NY_number = n())
NY <- ggplot(NY_reviewsNum, aes(date, NY_number)) +
  geom_point(color="blue", alpha=0.1) + geom_smooth(method = "loess", color = 'Yellow')+ scale_x_date() +
  ggtitle("How popular is Airbnb in NY?", subtitle = "Number of Reviews for listings in New York across years") +
  labs(x = "Year", y = "Unique listings recieving reviews")

## Airbnb Popularity in Beijing
BJ_review <- read.csv("BJreviews.csv", stringsAsFactors = FALSE)
BJ_reviewsNum <- BJ_review %>% group_by(date = as.Date(BJ_review$date)) %>% summarise(BJ_number = n())
BJ <- ggplot(BJ_reviewsNum, aes(date, BJ_number)) +
  geom_point(color="blue", alpha=0.1) + geom_smooth(method = "loess", color = 'orange')+ scale_x_date() +
  ggtitle("How popular is Airbnb in Beijing?", subtitle = "Number of Reviews for listings in Beijing across years") +
  labs(x = "Year", y = "Unique listings recieving reviews")

## Airbnb Popularity in Tokyo
Tokyo_review <- read.csv("Tokyo_reviews.csv", stringsAsFactors = FALSE)
Tokyo_reviewsNum <- Tokyo_review %>% group_by(date = as.Date(Tokyo_review$date)) %>% summarise(TK_number = n())
Tokyo <- ggplot(Tokyo_reviewsNum, aes(date, TK_number)) +
  geom_point(color="blue", alpha=0.1) + geom_smooth(method = "loess", color = 'pink')+ scale_x_date() +
  ggtitle("How popular is Airbnb in Tokyo?", subtitle = "Number of Reviews for listings in Tokyo across years") +
  labs(x = "Year", y = "Unique listings recieving reviews")

# show all 4 city popularity graphs in "figure"
# figure <- ggarrange(Singapore, NY, BJ, Tokyo, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
# figure

#### END OF POPULARITY ####
######################################################################################################################


#### PROPERTY INFO ####
listing_filtered <- filter(listing, listing$property_type %in% c("Apartment","Condominium","House","Serviced apartment","Hostel"))
properties <- listing_filtered %>% group_by(listing_filtered$neighbourhood_group_cleansed, listing_filtered$property_type) %>% summarize(f = n())
property_count_per_area <- listing_filtered %>% group_by(listing_filtered$neighbourhood_group_cleansed) %>% summarize(sum = n())
ratio_per_area <- merge(properties, property_count_per_area, by = "listing_filtered$neighbourhood_group_cleansed")

ratio_per_area$ratio <- ratio_per_area$f / ratio_per_area$sum
names(ratio_per_area) <- c("area", "property.type", "Freq", "Sum", "Ratio")
#### END OF PROPERTY INFO ####
######################################################################################################################


#### Superhost ####
## WHAT DOES IT TAKE TO BE A SUPER HOST? ##
num_reviews_per_id <- reviews %>% group_by(reviews$listing_id) %>% summarise(num = n())
colnames(num_reviews_per_id) = c("id","num")

df <- listing %>% select(c("host_response_rate", "review_scores_rating", "host_is_superhost", "id"))
df$review_scores_rating = as.integer(gsub("%","",df$review_scores_rating))
df$host_response_rate = as.integer(gsub("%","",df$host_response_rate))
df$host_is_superhost <- factor(df$host_is_superhost)

dataframe = merge(df, num_reviews_per_id, by = "id")
colnames(dataframe)[which(names(dataframe) == "host_is_superhost")] <- "Superhost"
#### END OF SUPERHOST ####
######################################################################################################################



#############################################################################################################
########################################   UI Interface   ###################################################
#############################################################################################################
header <- dashboardHeader(
  title=strong("AirBnb Navigator", style="font-family: 'Tahoma'; font-size: 17px;", img(src="logo.png",height=40)), 
  titleWidth = 230)

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",menuItem(strong("Overview"), tabName = "Overview", icon= icon("map")),
              menuItem(strong("Global Growth"), tabName="growth", icon = icon("globe")),
              menuItem(strong("Word Cloud"), tabName="wordcloud", icon = icon("cloud")),
              menuItem(strong("Property Info"), tabName="property", icon = icon("home")),
              menuItem(strong("Spatial Analysis"), tabName="region", icon = icon("picture", lib = "glyphicon")),
              menuItem(strong("Seasonality"), tabName="seasonality", icon = icon("chart-line")), 
              menuItem(strong("Hotel Comparison"), tabName="hotel", icon = icon("hotel")),
              menuItem(strong("Superhost"), tabName="superhost", icon = icon("user")))
)

body<- dashboardBody(
  tabItems (
    tabItem(tabName = "Overview",
          fluidRow(
            column(12,
            box(width = 8, leafletOutput(outputId = 'map1')),
            box(width = 4,
            selectInput(inputId = 'neighbourhood',
                        label = 'Select Neighbourhood',
                        choices = unique(listing$neighbourhood_cleansed)
            ),
            
            selectInput(inputId = 'apartment_type',
                        label = 'Select Apartment Type',
                        choices = unique(listing$property_type)
            ),
            radioButtons(inputId = 'superhost',
                         label = 'Is Host A Superhost?',
                         choices = unique(listing$host_is_superhost)
            ),
            sliderInput(inputId = 'budget',
                        label = 'Set Budget Range',
                        min = min(listing$price),
                        max = max(listing$price),
                        value =  c(min(listing$price),
                                   max(listing$price))
            ),
            sliderInput(inputId = 'rating',
                        label = 'Set Rating Range',
                        min = min(listing$review_scores_rating),
                        max = max(listing$review_scores_rating),
                        value =  c(min(listing$review_scores_rating),
                                   max(listing$review_scores_rating))
            ))))
    ), ## END OF TAB ITEM "OVERVIEW"
    
    tabItem(tabName = "property",
            h2("Property Info"),
            fluidRow(
              box(width = 12, plotOutput(outputId = 'property'),
                  "Click Below to Add/Remove Property Type Selection", br(),
                  selectInput(inputId = 'types',
                              label = 'Property Type',
                              selected = unique(ratio_per_area$property.type),
                              choices = unique(ratio_per_area$property.type), multiple = T
                  )))
    ), ## END OF TAB ITEM "SEASONALITY"
    
    tabItem(tabName = "seasonality",
            h2("Seasonality of AirBnb Demand in Singapore"),
            fluidRow(
              box(width = 12, plotOutput(outputId = 'seasonality')))
    ), ## END OF TAB ITEM "SEASONALITY"
    
    tabItem(tabName = "wordcloud",
            h2("Word Cloud"),
            fluidRow(
              box(width = 12, plotOutput("wordcloud"))
            ) 
    ), ## END OF TAB ITEM "WORD CLOUD"
    
    tabItem(tabName = "superhost",
            h2("Superhost - What does it take to be a superhost? "),
            fluidRow(
              box(width = 12, plotOutput("superhost"))
            ) 
    ), ## END OF TAB ITEM "SUPERHOST"
    
    tabItem(tabName = "growth",
            h2("Global Growth - How Popular is AirBnb?"),
            fluidRow(
              tabBox(width = 12,
              tabPanel(title = "Overview", plotOutput(outputId = 'growth_summary')),
              tabPanel(title = "Singapore", plotOutput(outputId = 'growth_dashboard_sg'),
                    sliderInput(inputId = 'year_sg',label = 'Set Year Range',
                                min = as.numeric(min(format(as.Date(reviewsNum$date),"%Y"))),
                                max = as.numeric(max(format(as.Date(reviewsNum$date),"%Y"))),
                                sep = "",
                                value =  c(min(format(as.Date(reviewsNum$date),"%Y")),
                                           max(format(as.Date(reviewsNum$date),"%Y")))
                       )),
              tabPanel(title = "New York", plotOutput(outputId = 'growth_dashboard_ny'),
                       sliderInput(inputId = 'year_ny',label = 'Set Year Range',
                                   min = as.numeric(min(format(as.Date(NY_reviewsNum$date),"%Y"))),
                                   max = as.numeric(max(format(as.Date(NY_reviewsNum$date),"%Y"))),
                                   sep = "",
                                   value =  c(min(format(as.Date(NY_reviewsNum$date),"%Y")),
                                              max(format(as.Date(NY_reviewsNum$date),"%Y")))
                       )),
              tabPanel(title = "Beijing", plotOutput(outputId = 'growth_dashboard_bj'),
                       sliderInput(inputId = 'year_bj',label = 'Set Year Range',
                                   min = as.numeric(min(format(as.Date(BJ_reviewsNum$date),"%Y"))),
                                   max = as.numeric(max(format(as.Date(BJ_reviewsNum$date),"%Y"))),
                                   sep = "",
                                   value =  c(min(format(as.Date(BJ_reviewsNum$date),"%Y")),
                                              max(format(as.Date(BJ_reviewsNum$date),"%Y")))
                       )),
              tabPanel(title = "Tokyo", plotOutput(outputId = 'growth_dashboard_tokyo'),
                       sliderInput(inputId = 'year_tk',label = 'Set Year Range',
                                   min = as.numeric(min(format(as.Date(Tokyo_reviewsNum$date),"%Y"))),
                                   max = as.numeric(max(format(as.Date(Tokyo_reviewsNum$date),"%Y"))),
                                   sep = "",
                                   value =  c(min(format(as.Date(Tokyo_reviewsNum$date),"%Y")),
                                              max(format(as.Date(Tokyo_reviewsNum$date),"%Y")))
                       ))
              ))
    ) ## END OF TAB ITEM "GROWTH"
  )
)


ui<- dashboardPage(header, sidebar,  body, skin="red") 

########################################   Server Handling   #############################################

server<-function(input,output){ 
  output$map1 <- renderLeaflet(
    { listing <- listing %>% filter(neighbourhood_cleansed == input$neighbourhood)
      listing <- listing %>% filter(property_type == input$apartment_type)
      listing <- listing %>% filter(host_is_superhost == input$superhost)
      listing <- listing %>% filter(price >= input$budget[1] & price <= input$budget[2])
      listing <- listing %>% filter(review_scores_rating >= input$rating[1] & review_scores_rating <= input$rating[2])
      
      listing_url <- as.character(listing$listing_url)
      listing_name <- as.character(listing$name)
      listing_url <- paste0("<b><a href=\'",listing_url,"\'>")
      listing_url <- paste0(listing_url,listing_name,"</a></b>")
      message <- paste(
        "URL: ", listing_url, "<br/>", 
        "Neighbourhood: ", str_to_title(listing$neighbourhood_cleansed),"<br/>",
        "Region: ", str_to_title(listing$neighbourhood_group_cleansed),"<br/>",
        "Overall Rating: ", (round(listing$review_scores_rating, 2)), "<br/>",
        "Property Type: ", listing$property_type, "<br/>",
        "Room Type: ", listing$room_type, "<br/>",
        "Accomodates: ", listing$accommodates , "<br/>",
        'Bathrooms: ', listing$bathrooms, "<br/>",
        'Bedrooms: ', listing$bedrooms , "<br/>",
        'Bed Type: ', listing$bed_type , "<br/>",
        'Amenities: ', listing$amenities , "<br/>",
        'Price: ', listing$price, "<br/>",
        'Security Deposit: ', listing$security_deposit , "<br/>",
        'Cleaning Fee: ', listing$cleaning_fee, "<br/>",
        sep="")
      
      m <- leaflet(listing) %>% 
        addTiles() %>%
        addMarkers(lng = ~longitude,
                   lat = ~latitude,
                   popup = message) %>%
        setView( lat=1.35, lng=103.82 , zoom=10.5) 
      
      m <- addProviderTiles(m,"Esri.WorldImagery", group = "Esri")
      m <- addProviderTiles(m,"Stamen.Toner", group = "Toner")
      m <- addProviderTiles(m, "Stamen.TonerLite", group = "Toner Lite")
      m <- addLayersControl(m, baseGroups = c("Default","Esri","Toner Lite","Toner"))
    }) ## END OF MAP1 OUTPUT
  
  output$wordcloud <- renderPlot({
    # create the word cloud
    set.seed(1234)
    wordcloud(words = wordDF$text, freq = wordDF$n, min.freq = 1000,
              max.words=500, random.order=FALSE, rot.per=0.35, scale=c(3.5,0.25),
              colors=brewer.pal(8, "Paired"))
  }) ## END OF WORD CLOUD OUTPUT
  
  # output for property
  output$property <- renderPlot({
    ratio_per_area <- subset(ratio_per_area, property.type %in% input$types)
    # plot the graph
    ggplot(ratio_per_area, aes(x=area, y=Ratio, fill=property.type)) +
      geom_bar(position = "dodge", stat="identity") + 
      scale_y_continuous(labels = scales::percent) +
      ggtitle("Property types in Singapore") + xlab("Area") + ylab("Percentage") +
      scale_fill_manual("Property Type", values=c("#e06f79","#357b8a", "#00CC66", "#24E1F6", "#FDD227"))
  }) ## END OF PROPERTY OUTPUT
  
  # output for seasonality
  output$seasonality <- renderPlot({
    grid.arrange(y1D, y2D, y3D, y4D, y5D, y6D, y7D, y8D, y9D, nrow = 3)
  }) ## END OF SEASONALITY OUTPUT
  
  # output for superhost
  output$superhost <- renderPlot({
    # plot superhost graph
    ggplot(dataframe,aes(host_response_rate,review_scores_rating,color=Superhost)) + geom_point(alpha = .5,stroke=0) + 
      xlab("Host Response Rate (%)") + ylab("Average Ratings (out of 100)") +
      ggtitle("Superhost vs. Normal host",subtitle = "Average Rating by Host Response Rate") + 
      theme(axis.text.x = element_text(angle = 90))
  }) ## END OF WORD CLOUD OUTPUT
  
  ## Growth Section - AirBnb Popularity Overtime
  # output for growth (AirBnb Popularity) OVERVIEW
  output$growth_summary <- renderPlot({
    ggarrange(Singapore, NY, BJ, Tokyo, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
  })
  # output for sg growth (AirBnb Popularity)
  output$growth_dashboard_sg <- renderPlot({
    reviewsNum <- reviewsNum %>% filter(format(as.Date(reviewsNum$date),"%Y") >= input$year_sg[1] & format(as.Date(reviewsNum$date),"%Y") <= input$year_sg[2])
    ggplot(reviewsNum, aes(date, number)) +
      geom_point(color = "blue", alpha=0.2) + geom_smooth(method = "loess", color = 'red') + scale_x_date() +
      ggtitle("How popular is Airbnb in Singapore?", subtitle = "Number of Reviews for listings in Singapore across years") +
      labs(x = "Year", y = "Unique listings recieving reviews") 
    })
  # output for tokyo growth (AirBnb Popularity)
  output$growth_dashboard_tokyo <- renderPlot({
    Tokyo_reviewsNum <- Tokyo_reviewsNum %>% filter(format(as.Date(Tokyo_reviewsNum$date),"%Y") >= input$year_tk[1] 
                                                    & format(as.Date(Tokyo_reviewsNum$date),"%Y") <= input$year_tk[2])
    ggplot(Tokyo_reviewsNum, aes(date, TK_number)) +
      geom_point(color="blue", alpha=0.1) + geom_smooth(method = "loess", color = 'pink')+ scale_x_date() +
      ggtitle("How popular is Airbnb in Tokyo?", subtitle = "Number of Reviews for listings in Tokyo across years") +
      labs(x = "Year", y = "Unique listings recieving reviews")
  })
  # output for NY growth (AirBnb Popularity)
  output$growth_dashboard_ny <- renderPlot({
    NY_reviewsNum <- NY_reviewsNum %>% filter(format(as.Date(NY_reviewsNum$date),"%Y") >= input$year_ny[1] 
                                                 & format(as.Date(NY_reviewsNum$date),"%Y") <= input$year_ny[2])
    ggplot(NY_reviewsNum, aes(date, NY_number)) +
      geom_point(color="blue", alpha=0.1) + geom_smooth(method = "loess", color = 'Yellow')+ scale_x_date() +
      ggtitle("How popular is Airbnb in NY?", subtitle = "Number of Reviews for listings in New York across years") +
      labs(x = "Year", y = "Unique listings recieving reviews")
  })
  # output for Beijing growth (AirBnb Popularity)
  output$growth_dashboard_bj <- renderPlot({
    BJ_reviewsNum <- BJ_reviewsNum %>% filter(format(as.Date(BJ_reviewsNum$date),"%Y") >= input$year_bj[1] 
                                              & format(as.Date(BJ_reviewsNum$date),"%Y") <= input$year_bj[2])
    ggplot(BJ_reviewsNum, aes(date, BJ_number)) +
      geom_point(color="blue", alpha=0.1) + geom_smooth(method = "loess", color = 'orange')+ scale_x_date() +
      ggtitle("How popular is Airbnb in Beijing?", subtitle = "Number of Reviews for listings in Beijing across years") +
      labs(x = "Year", y = "Unique listings recieving reviews")
  })
  ## END OF GROWTH OUTPUT
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  
}

shinyApp(ui,server)



