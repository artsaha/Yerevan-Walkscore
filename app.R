if(!require(shiny)) install.packages("shiny")
if(!require(plotly)) install.packages("plotly")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(shinycssloaders)) install.packages("shinycssloaders")
if(!require(sf)) install.packages("sf")
if(!require(DT))install.packages("DT")
if(!require(osmdata)) install.packages("osmdata")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggmap)) install.packages("ggmap")
#if(!require(leaflet)) install.packages("leaflet")
if(!require(plotly)) install.packages("plotly")
library(shiny)
library(plotly)  
library(shinydashboard)
library(sf)
library(shinycssloaders)

library(DT)
library(osmdata)
library(dplyr)
library(ggmap)
#library(leaflet)
library(plotly)

working_directrory <- getwd()
print(working_directrory)

source("WalkScore\\global_functions.R")

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Yerevan public life"),
  
  # User Input which Might be needed for some of the plots
  dashboardSidebar(
    # will change only the names for the corresponding sections
    sidebarMenu(
      id = "tabs",
      menuItem("Walk score", tabName = "section1", icon = icon("chart-line")),
      menuItem("Taxes", tabName = "section2", icon = icon("chart-bar")),
      menuItem("Map my walk", tabName = "section3", icon = icon("chart-pie"))
    ),
    
    sidebarMenu(
      menuItem("by Artur Sahakyan x Elen X Sharlot "),
      menuItem("X Shoghik X Gevorg")
    )
  ),
  
  dashboardBody(
    # Section 1
    tabItems(
      tabItem(tabName = "section1",
              fluidRow(
                tabsetPanel(
                  
                  tabPanel("Walk Score of Yerevan",
                          
                           column(12 ,
                                                           h2("Walkscore of Yerevan is" ) ,
                                                           h2(textOutput("mean_score")) , 
                                                           h3("Walkscores inside squares"), DTOutput("score_table") ),
                           column(12, h2("Walkscore on more squares is "), h2(textOutput("mean_score2")), DTOutput("score_table2")),
                           h2("We simulate real life,proceed for more")   ),
                  tabPanel("Main data points of Yerevan", column(
                    12,h2("We obtain several amenity types, then apply exponential decay on them.
                          The further away an important amenity is the less its score will be inside a matrix we create. "),
                    h3("Let's call that transitional matrix(Markov Chains)")
                  ),
                  column(6, align = "center",plotlyOutput("yerevan_data_points")),
                  column(12, h2("Then we simulate a random process, which aims to simulate behavior of real people visiting amenities, simulation repeats as many times as we assign, theoretically we cover the entire road map and beyond"), plotlyOutput("yerevan_road_map") )),
                  
                  
                  tabPanel("Algorithm",
                           fluidRow(
                             
                            column(8,h2("The grid can be divided as many times as we assign"),
                                   sliderInput("square_value", "Select square size coefficient", min = 0.5, max = 2.6, 
                                               value = 2, step = 0.3),
                                   plotlyOutput("myImage") ),
                            column(4,h2("The stochastic process, random events"),img(src="https://fictionhorizon.com/wp-content/uploads/2022/12/card-suits.jpeg", width = "500px", height = "300px"),
                            h4("Image credit to NETFLIX,Alice in Borderland")),
                            column(12, h1("In each cell and in the entire cell we simulate people walking"), h3(", each time selecting a random place, applying exponential decay to amenities and randomly simulating walks")),
                            column(6,h2("Markov chains"),img(src="https://image.slideserve.com/1075812/representation-of-a-markov-chain-as-a-digraph-l.jpg", width="600px", height = "400px" )),
                            column(6,h2("Random walk"), img(src="https://www.codespeedy.com/wp-content/uploads/2020/05/2-D-Random-Walk.png", width = "600px", height="400px"),
                            ),
                    
                           )
                  
                           ),
                  tabPanel("Additional observation", 
                           column(8, h3("Let's visualise the number of buildings by their type,which also reveals a lot about Yerevan.."), h3("The number of schools leads, also restaurants are quite popular"),
                                  plotlyOutput("building_frequency_plot")
                           ), 
                          
                ),
                tabPanel("Calculator tool", 
                         column(12,h2("Computing takes time, please wait"),
                                h3(" smaller square size => more precise outcome"),
                                sliderInput("square_size", "Select square size", 
                                            min = 0.01, max = 0.1, value = 0.1, step = 0.01),
                                h2(withSpinner(textOutput("mean_score3")))     ),
                         )
              )
              ),
              
            
              
      ),
      # Section 2
      tabItem(tabName = "section2",
              tabsetPanel(
                tabPanel("Center leads",
                         
                          fluidRow(
                            column(12, h1("Sight from above"), h3("The data we filtered out and cleaned shows us geospatially.."), p("..That the Kentron district has most taxpayers, since it has the largest share of businesses.  "), 
                                   p("This lead us to hypothesis that Kentron district features a highly competitive small/median income business environment and new businesses will find a hard time competing")),
                            column(6, style="height:600px;",
                                 plotlyOutput("center_king")
                            )
                          )
              ),
                tabPanel(
                  "Tax clusters",
                  fluidRow(
                    column(12, h2("Not only geographically, but also statistically Kentron district leads by a margin")),
                    column(12, h2("Shengavit and Arabkir are on 2-nd and 3-rd places respectively ")),
                    column(12, plotlyOutput("tax_clusters"))
                  )
                ),
              tabPanel(
                "Tax boxplot",
                fluidRow(
                  column(12, h2("The boxplots show each districts' payment performance, from which we infer the saturation of business environment"),
                         h2("At the same time we observe that due to Shengavit's factories/plants total tax amount almost reaches Kentron's, and since Kentron has more businesses, it means small/medium size businesses predominate")),
                  column(12, plotlyOutput("tax_boxplot"))
                )
              ),
              tabPanel(
                "Taxes per month",
                fluidRow(
                  column(12, h3("The less you earn the less you pay. "), h4("Histograms allow us to see that simple correlation")),
                  column(12, plotlyOutput("taxes_per_month"))
                )
              ),
              tabPanel(
                "Quantiles and arrangement",
                fluidRow(
                  column(12, h2("By 75% point of data we have, Kentron already leads tax payments total amount, without even counting the outliers")),
                  column(12, DTOutput("quantile_taxes"))
                )
              )
              
              ),
              
              
      ),
      # Section 3
      tabItem(tabName = "section3",
              tabsetPanel(
                
                tabPanel("Walk analysis",
                         fluidRow(
                           h1("Given time and coordinates we measured Velocity"),
                           
                         ),
                         fluidRow(
                           column(12,
                                  plotlyOutput("plot3")
                           )
                         ),
                        
                         fluidRow(
                           column(12, h2("Examining the given boxplot we decided to analyze walking types"),plotlyOutput("walk_pattern")),
                         )
                          
                         ),
                        
                        
                tabPanel("Walk habits: Rush or Relax?", 
                         fluidRow(
                           column(12, h1("How we walk"), p("Observations show that ..... ")),
                           column(8, plotlyOutput("session_frequency_plot")),
                           column(8, h2("Very often we rush! "))
                         )
                         
                         )
              )
              
      )
    )
  )
)

server <- function(input, output) {
  # Here  will use our functions
  register_google(key = 'AIzaSyApOOlcazPmOKDgJDcWFsjuhUYhonQYMOQ')
  
  ## Warning - getbb() may have some problems during year due to osmdata servers
  #yerevan_map <- get_map(getbb("Yerevan"), maptype = "terrain")
  
  ymin <- 44.39080 ## 44.36211
  ymax <- 44.60057 #44.62177
  xmin <- 40.10177  ##06585
  xmax <- 40.23295 ##40.24177
  
  yerevan_map <- get_map(location= c(lon = mean(c(ymin, ymax)), lat = mean(c(xmin,xmax))), 
                         zoom =12, maptype="terrain", scale = 2)
  
  yerevan_map_sat <- get_map(location= c(lon = mean(c(ymin, ymax)), lat = mean(c(xmin,xmax))), 
                             zoom =12, maptype="satellite", scale = 2)
  
  #end 
  # Render Plot 1
  walkscore_data <- read.csv("WalkScore\\final_data.csv")
  walkscore_points <- read.csv("WalkScore\\final_data_addition.csv")
  
  building_freq <- table(walkscore_points$type)
  
  
 
  # Create a data frame for ggplot
  building_data <- data.frame(Building_Type = names(building_freq),
                              Frequency = as.numeric(building_freq), mycolor = as.factor(names(building_freq)))
  ####### Frequency should be given as.numeric() value
  
  # Create the bar plot using ggplot2
  
  output$building_frequency_plot <- renderPlotly({
    # Might be modified for our function
    ggplot(building_data, aes(x = Building_Type, y = Frequency,fill = mycolor)) +
      geom_bar(stat = "identity") +
      labs(title = "Frequency of Each Building Type",
           x = "Building Type",
           y = "Frequency") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #roads <- getbb("Yerevan") %>% 
  #  opq(timeout = 3500) %>%
  #  add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary", "tertiary", "residential", "living_street")) %>%
  #  osmdata_sf()
  
  #info <- getbb("Yerevan")
  #xmin = info["x", "min"]
  #xmax = info["x", "max"]
  #ymin = info["y", "min"]
  #ymax = info["y", "max"]
  
  
  output$yerevan_road_map <- renderPlotly({
#ggplot() + 
 #     geom_sf(data = roads$osm_lines, inherit.aes = FALSE, color = "black",) +
      ##geom_sf(data = restaurants$osm_points, inherit.aes = FALSE, color = 'blue', alpha = 0.5) +
  #    coord_sf(xlim = c(xmin, xmax),
   #            ylim = c(ymin, ymax),
    #           expand = FALSE)
  })
  
  
  output$yerevan_data_points <- renderPlotly({
    
    ggmap(yerevan_map)+
      geom_point(data = walkscore_points, aes(lat,lon, fill = as.factor(type), stroke=0.001 )) + labs(fill="")
  })
  
  prob_df <- read.csv("WalkScore\\prob_df.csv")
  
  prob_df2 <- read.csv("WalkScore\\prob_df_2.csv")
  
  output$score_table <- renderDT({
      datatable(prob_df, options = list(pageLength = 10))
  })
  
  output$score_table2 <- renderDT({
    datatable(prob_df2, options = list(pageLength = 10))
  })
  
  output$mean_score2 <- renderText({
    mean_value <- prob_df2 %>% 
      filter(scores != 0) %>%
      summarise(mean_column = mean(scores, na.rm = TRUE)) %>%
      pull(mean_column)
    
    paste("Mean:", mean_value)
  })
  
  
  
  
  output$mean_score3 <- renderText({
    
    square_size <- input$square_size
    
    
    # Calculate the number of squares in each direction
    num_x_squares <- ceiling((xmax-xmin) / square_size)
    num_y_squares <- ceiling((ymax - ymin) / square_size)
    cat(num_x_squares,num_y_squares)
    # Function to get the coordinates of the small square given its index
    get_square_coords <- function(row_idx, col_idx) {
      xmin <- xmin + (col_idx - 1) * square_size
      xmax <- min(xmin + col_idx * square_size, xmax)
      ymin <- ymin + (row_idx - 1) * square_size
      ymax <- min(ymin + row_idx * square_size, ymax)
      return(c("xmin"=xmin, "xmax" = xmax, "ymin" = ymin, "ymax"=ymax))
    }
    # Calculate the number of squares in each direction
    
    scores <- list()
    
    for (i in seq(1, num_x_squares,1)){
      
      
      for(j in seq(1, num_y_squares,1)){
        ##cat (i, ",", j, "\n") 
        
        rio <- get_square_coords(i,j)
        retin <- subset(walkscore_points, lon >= rio["xmin"] & lon <= rio["xmax"] & lat >= rio["ymin"] & lat <= rio["ymax"])
        
        #mindal <- retin %>%
        #  slice(seq(1, n(), by = 20))
        if(length(retin$lon != 0)){
          mindal <- retin %>%  slice(seq(1, n(), by = 100))
        }
        
        ## 
        if(length(retin$lon != 0)){
          
          res <- calculate_walkscore(mindal$lon, mindal$lat,mindal$decay_rate)
          score <- res["score"]
          cat(res["prob_vec"] , " ")
          cat(rio["xmin"]," ", rio["xmax"], " ", rio["ymin"], rio["ymax"], length(retin$lon),length(mindal$lon), score ,"\n")
          #write.csv(res["prob_vec"], paste("probvector",length(scores)))
          scores[[length(scores) + 1]] <- res["score"]
        }
      }
    }
    
    
    prob_df <- data.frame(scores = unlist(scores))
    
    mindal <- walkscore_points %>%
      slice(seq(1, n(), by = 180))
    
    mindal
    
    entire_square <- calculate_walkscore(mindal$lon,mindal$lat, mindal$decay_rate)
    entire_square["score"]
    
    prob_df <- rbind(prob_df, entire_square["score"])
    
    prob_df
    mean_value <- prob_df %>% 
      filter(scores != 0) %>%
      summarise(mean_column = mean(scores, na.rm = TRUE)) %>%
      pull(mean_column)
    
    mean_value
    
    paste("Mean:", mean_value)
  })
  output$mean_score <- renderText({
    paste("Mean: ", mean(prob_df$scores))
  })
  
  length_x = xmax - xmin
  length_y = ymax - ymin
  
  generate_coordinates_df <- function(xmin, xmax, ymin, ymax, num_points_per_row) {
    x_values <- seq(xmin, xmax, length.out = num_points_per_row)
    y_values <- seq(ymin, ymax, length.out = num_points_per_row)
    
    
    num_squares <- num_points_per_row*num_points_per_row
    colors <- sample(colors(), num_squares, replace=TRUE)
    coordinates <- data.frame(x = rep(x_values, times = num_points_per_row),
                              y = rep(y_values, each = num_points_per_row),
                              width = length_x/3.75,height=length_y/3.75,
                              colors = colors)
    return(coordinates)
  }
  
  
  num_points_per_row <- 7
  
  
  
  # Generate the data frame of coordinates
  coordinates_df <- generate_coordinates_df(ymin, ymax, xmin, xmax, num_points_per_row)
  
  coordinates_df
  
  # Print the resulting data frame
  print(coordinates_df)
  
  squares <- coordinates_df %>%
    slice(seq(1, n(), by = 2))
  squares
  
  # Convert the ggmap object to a ggplot object
  
 
  output$myImage <- renderPlotly({
    div_coeff = input$square_value
    ggmap(yerevan_map) + 
      geom_rect(data = coordinates_df, aes(NULL,NULL,xmin = x - width/div_coeff, xmax = x + width/div_coeff , ymin = y - height / div_coeff, ymax = y + height / div_coeff), 
                fill = coordinates_df$colors, alpha = 0.5)+
      labs(title="A visual representation of grid cell selection in algorithm")
    
  })
  
  # Render Plot 2: same, change only the function
  
  district_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", 
                       "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99", "#7570b3")
  
  taxes <- read.csv("Taxes\\taxes_mod.csv")
  
  
  
  output$tax_boxplot <- renderPlotly({
    #ggplot(taxes,aes(y=region,x=total_amount,fill = region)) + geom_boxplot() + xlim(0,1000000) +
    #  theme(axis.title.x = element_text(margin=margin(10,0,0,0,"pt")),axis.text.x = element_text(angle=-60,size = 7),axis.text.y = element_text(size=7))+
    #  labs(y="district", x="total payed taxes", title="Total Payed Taxes In AMD For Each District, from 0 to 1 mln ")
    
    p <- plot_ly(taxes, x = ~total_amount, y = ~region, type="box", color = ~factor(region)) 
    
    p <- p %>% layout(title = "Boxplot for medians,quantiles", yaxis = list(title="Value"), xaxis= list(title="Total tax amount", range = c(0,5400000)))
    p
  })
  

  #myMap <- get_map(location = getbb("Yerevan"), source="google", maptype="satellite",zoom=12)
  
  taxes_ripped <- taxes %>%
    slice(seq(1, n(), by = 5))
  
  output$center_king <- renderPlotly({
    ggmap(yerevan_map_sat)+
      geom_point(data = taxes_ripped, aes(longitude,latitude),color = taxes_ripped$district_color, alpha=taxes_ripped$alpha_log)
     # ggplot(data = taxes, aes(x = latitude, y = longitude),color = taxes$district_color)+
    #   labs(x="lng",title="All taxpayers in Yerevan by Registered District", color = "District markers")

  })
  
  
 
  
  output$tax_clusters <- renderPlotly({
    ggplot(taxes, aes(factor(region), fill = factor(region))) + geom_bar()+theme(axis.text.x = element_blank())+
      labs(x="District",y="Number of Businesses",fill="District", title = "Number of Tax-Paying Businesses Per District")
  }) 
  
  
  output$taxes_per_month <- renderPlotly({
    ggplot(taxes, aes(x = total_amount, y = after_stat(density))) + 
      geom_histogram(bins=25, color="darkblue", fill="skyblue") +
      geom_density(color = "green", linewidth = .1) + theme(axis.title.x = element_text(margin=margin(10,0,0,0,"pt")),axis.text.x = element_text(angle=-60,size = 7)) + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
      xlim(0,1000000) + 
      facet_wrap(~month) +
      labs(x="", y="Density", title="Density of Total Taxes Per Month")
  })
  
  
  quantiles_df <- taxes %>%
    group_by(region) %>%
    summarize(
      q25 = quantile(total_amount, 0.25, na.rm = T),
      q50 = quantile(total_amount, 0.50, na.rm = T),
      q75 = quantile(total_amount, 0.75, na.rm=T)
    )
  
  ## 
  arranged <- quantiles_df %>%
    arrange(desc(q75))
  
  output$quantile_taxes <- renderDT({
    datatable(arranged, options = list(pageLength = 10))
  })
  
  
  
  # Render Plot 3: same, change only the function
  
  mapping_walk <- read.csv("MapMyWalk\\final_walking.csv")

  output$plot3 <- renderPlotly({
    ggplot(mapping_walk, aes(speed_kmh))+ geom_histogram(bins = 70, fill = "skyblue", color = "darkblue") + 
      labs(x="velocity", y = "frequency", title="measuring central tendency and overtime trends") + xlim(0,20)
  })

  
  output$walk_pattern <- renderPlotly({
   # ggplot(mapping_walk,aes(x = speed_kmh, y= Session ), color=as.factor(mapping_walk$Session)) + geom_boxplot()
    p <- plot_ly(mapping_walk, x = ~speed_kmh, y = ~Session, type="box",color = ~factor(Session))
    
    p <- p %>% layout(title = "Boxplot for medians/quantiles", yaxis = list(title="Session"))
    
    p
  }) 
  
  
  new_map_walk <- read.csv("MapMyWalk\\new_walking.csv")
  

  
  session_quantiles <- read.csv("MapMyWalk\\quantile_sessions.csv")
  fitness_freq <- table(session_quantiles$fitness)
  
  walking_fitness <- data.frame(Session = names(fitness_freq), Frequency = as.numeric(fitness_freq), Eachcolor = as.factor(names(fitness_freq)))
  
  
  output$session_frequency_plot <- renderPlotly({
    ggplot(walking_fitness, aes(x=Session, y = Frequency,fill = Eachcolor)) + 
      geom_bar(stat="identity", color="darkblue")
  })
  
  
}

shinyApp(ui, server)
