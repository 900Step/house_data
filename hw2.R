# read in the data
library(tidyverse)
library(ggplot2)
library(shiny)
house<- read.csv("https://raw.githubusercontent.com/900Step/house_data/main/house.csv")

# #################################
# https://github.com/900Step/house_data/blob/main/house.csv
# data cleaning
# change the columns to the factors
house$built_year <-as.factor(house$built_year)
house$orginal_level<- as.factor(house$orginal_level)
house$present_level<- as.factor(house$present_level)
house$exist_dis<- as.factor(house$exist_dis)
house$assecss_dis<- as.factor(house$assecss_dis)

distance <- function(x, y, i, j){
  # y, j north and south
  # x, i west and east
  dif_x = abs(x - i)*111320
  dif_y = abs(y - j)*100000
  dis = sqrt(dif_x**2+dif_y**2)
  return(dis)
}

select_sample <- function(x, y, radius_min, radius_max, dat){ 
  # y is the center longitude, x is the center latitude
  # dat is the input dataset for the house
  # radius_min and radius_max comes from the slider input
  
  # vector for the plot
  x_cod = c()
  y_cod = c()
  
  # vector for the table
  index = c()
  
  # check all the distance for the samples and select all the points.
  for (i in c(1:dim(dat)[1])){
    dis = distance(x, y, as.numeric(dat[i,1]), as.numeric(dat[i,2]))
    
    if (radius_max > dis & dis> radius_min){
      
      index <- append(index, i)
      x_cod <- append(x_cod, as.numeric(dat[i,1]))
      y_cod <- append(y_cod, as.numeric(dat[i,2]))
    }
  }
  
  #res <- data.frame(x_cod, y_cod, index)
  
  
  return(house[index,])
}

dotplot <- function(df, x,y) {
  ggplot(df) +
    # color is determained by the exist disaster
    # shape is given by the building type
    geom_point(aes(longitude, latitude, col = exist_dis,shape = building_type, size = log(1+const_scale) ), alpha = 0.5)+
    geom_point(aes(x, y, size = 10)) +
    coord_cartesian(xlim = c(128, 130), ylim = c(42, 44))
}

# unique_type <- function(df){
#   return(unique(df$building_type))
# }

building_types <- pull(house, building_type) %>%
  unique() %>%
  na.omit()

exist_dis <- unique(house$exist_dis)
built_year = unique(house$built_year)

pieplot <- function(df){
  df <- df%>%
    group_by(built_year)%>%
    summarise(count = n())
  pie(df$count, labels = paste0(df$built_year, ": ", df$count), 
      col = topo.colors(length(df$built_year)), main =  "year distribution")
  legend("topright", legend = df$built_year, fill = topo.colors(length(df$built_year)),
         title = "Year", cex = 0.8)
}


# add a histgram aside from pieplot
histplot <- function(df){
  ggplot(df)+
    geom_histogram(aes(x = log(1+const_scale), fill = "red"),alpha = 0.5, bins = 15)
}

text <- p("This is an app to select the building in the certain region. 
              The center is a simulation of the earthquake center. 
              Distance is the region we interested in. 
              There are 3 types of buildings and 3 levels of exist disaster we can choose.")
statsment <- p("Because of the data itself, the region of the longitude and the latitude is limited. 
               From the background research, we set the maximize distance to be 10km. ")
# Shiny part
ui <- fluidPage(
  titlePanel("House Information"),
  mainPanel(text),
  sidebarPanel(statsment),
  # check box for built years
  # select the dictance region todo the selection
  # set the center
  numericInput("longitude", "Center longitude,Suggest region: [129.0, 129.8]", 129.45),
  numericInput("latitude", "Center latitude,Suggest region: [42.7, 43.4]", 43.15),
  
  sliderInput("distance", "Distance(scale: m)", 0, 100000, c(0, 50000), sep = ""),
  checkboxGroupInput("building_type", "Building Type", choices = building_types, selected = building_types),
  # selectInput("building_type", "Building Type", building_types, multiple = TRUE),
  selectInput("exist_dis", "Exist Disaster Level", exist_dis, multiple = TRUE),

  # first, generate a plot as the output
  textOutput("checkbox"),
  # left and right
  plotOutput("House"),
  
  fluidRow(
    column(6, plotOutput("Build_year")),
    column(6, plotOutput("Area_Scale"))
  ),

  dataTableOutput("dt")

  
)



server <-function(input, output){
  
  house_select<- reactive({
    select_sample(input$longitude, input$latitude, input$distance[1], input$distance[2], house)%>%
      filter(building_type %in% input$building_type)%>%
      filter(exist_dis %in% input$exist_dis)
    
  })
  
  # output of the plot: scatter plot of the house
  
  output$checkbox <- renderText(input$building_type)
  
  # output$House_Type <-renderText(cat(building_types))
  output$House <- renderPlot(dotplot(house_select(), input$longitude, input$latitude))
  
  # output of the data table
  # contains all the columns
  output$dt <- renderDataTable(house_select()[,c("longitude", "latitude", "built_year","exist_dis" ,"building_type")])
  output$Build_year <- renderPlot(pieplot(house_select()))
  output$Area_Scale <- renderPlot(histplot(house_select()))
}

shinyApp(ui, server)

