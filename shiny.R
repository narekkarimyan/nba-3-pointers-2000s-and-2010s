library(shinydashboard)
library(ggplot2)
library(dplyr)
library(stringr)
library(patchwork)


#Data Cleanup
nba <- read.csv('nba.csv')

nba$min <- str_extract(nba$min, pattern = "^[0-9]{1,}")

nba$min <- as.numeric(nba$min)

nba <- nba %>%
  mutate(decade = case_when(game %in% c(2000:2009) ~ "2000s",  game %in% c(2010:2018) ~ "2010s"))

nba <- nba %>%
  mutate(three_pointer = case_when(fg3m>0 ~ TRUE,  fg3m==0 ~ FALSE))

nba <- nba %>%
  mutate(three_pointer_more_than_five = case_when(fg3m>=6 ~ TRUE,  fg3m<6 ~ FALSE))

nba <- nba %>%
  mutate(three_pointer_attempt = case_when(fg3a>0 ~ TRUE,  fg3a==0 ~ FALSE))

nba <- nba %>% mutate(fg3_pct = case_when(game == 2018 ~ fg3_pct / 100, TRUE  ~ fg3_pct))

nba$aver_min_three_pointer <- nba$min / nba$fg3m

percentage_three_pointer <- nba %>%
  group_by(three_pointer) %>%
  summarise(count = n()) %>%
  mutate(count = count / dim(nba)[1] * 100) %>%
  as.data.frame()

percentage_three_pointer_attempt <- nba %>%
  group_by(three_pointer_attempt) %>%
  summarise(count = n()) %>%
  mutate(count = count / dim(nba)[1] * 100) %>%
  as.data.frame()

nba_2000_2009 <- nba %>%
  filter(game %in% c(2000:2009))

nba_2010_2018 <- nba %>%
  filter(game %in% c(2010:2018))

percentage_diff_attempts <- (sum(nba_2010_2018$fg3a) - sum(nba_2000_2009$fg3a))/( sum(nba_2000_2009$fg3a)) * 100

percentage_diff_scored <- (sum(nba_2010_2018$fg3m) - sum(nba_2000_2009$fg3m))/( sum(nba_2000_2009$fg3m)) * 100

nba_2000_three_pointer_a_per <- nba_2000_2009 %>%
  filter(three_pointer_attempt == T) %>%
  summarise(n()) / dim(nba_2000_2009)[1]

nba_2010_three_pointer_a_per <- nba_2010_2018 %>%
  filter(three_pointer_attempt == T) %>%
  summarise(n()) / dim(nba_2010_2018)[1]

nba_2000_three_pointer_s_per <- nba_2000_2009 %>%
  filter(three_pointer == T) %>%
  summarise(n()) / dim(nba_2000_2009)[1]

nba_2010_three_pointer_s_per <- nba_2010_2018 %>%
  filter(three_pointer == T) %>%
  summarise(n()) / dim(nba_2010_2018)[1]

#END

ui <- fluidPage(
  fluidRow(
    column(6, h4("This graphs show the distribution of scored and attempted three pointers of the selected season")),
    column(3, selectInput(inputId="season", 
                          label = "Select the season",
                          choices = c(2000:2018), selected =2015))
  ),
  fluidRow(
    column(8, plotOutput("threePointerPie", width = "auto", height="300px"))
  ),
  fluidRow(
    column(7, h4("This graph shows the count of the selected number of three pointers scored in a single game by a single player distributed by decade")),
    column(3, numericInput(inputId = "num_of_threes",
                           label = "Write number of three pointers",
                           min = 1, max = 10, value = 3))
  ),
  fluidRow(
    column(6, plotOutput("compareDecadeThreePointer", width = "auto", height = "300px"))
  ),
  fluidRow(
    column(7, h4("These graphs show the average of minutes needed for players to score a 3 pointer arranged by count in the selected interval")),
    column(3, sliderInput(inputId = "xlim",
                          label = "Minutes inerval",
                          min = 0, max = 60, value = c(0,60)))
  ),
  fluidRow(
    column(6, plotOutput("minutesthreepointer", width = "auto", height = "300px"))
  ),
  fluidRow(
    column(7, h4("This graph shows the percentage of a particular number of 3 pointers scored in the selected interval arranged by decade")),
    column(3,  sliderInput(inputId = "xlim2",
                           label = "Number of 3 pointers scored",
                           min = 0, max = 11, value = c(0,11)))
  ),
  fluidRow(
    column(6, plotOutput("percentageOf3pointers", width = "auto", height = "300px"))
  )
)

server <- function(input, output){
  output$threePointerPie <- renderPlot({
    g1 <- nba %>%
      filter(game == input$season) %>%
      ggplot(aes(x = factor(1), fill = three_pointer)) + 
      geom_bar(width = 1) + 
      coord_polar(theta = 'y') + 
      theme_void() + 
      labs(title = "The percentage of three pointers made/attempted", fill="Scored three pointer") +
      scale_fill_manual(values=c("orange", "black")) 
    
    g2 <- nba %>%
      filter(game == input$season) %>%
      
      ggplot(aes(x = factor(1), fill = three_pointer_attempt)) + 
      geom_bar(width = 1) + 
      coord_polar(theta = 'y') + 
      theme_void() + 
      labs(title = "", fill="Attempted three pointer") +
      scale_fill_manual(values=c("orange", "black")) 
    
    g1+g2
  })
  output$compareDecadeThreePointer <- renderPlot({
    nba  %>%
      
      mutate(three_pointer_more_than = case_when(fg3m>= input$num_of_threes ~ TRUE,  fg3m<input$num_of_threes ~ FALSE)) %>%
      filter(three_pointer_more_than == T) %>%
      group_by(decade)  %>%
      ggplot(aes(x=decade, fill=decade)) + geom_bar() + 
      theme_bw() + 
      scale_fill_manual('Decade', values=c("2000s" = "orange", "2010s" = "black")) +
      labs(title = "Three pointers of 2010s vs. 2000s", x="Decade", y="Count") 
    
  })
  
  output$minutesthreepointer <- renderPlot({
    ggplot(nba, aes(x=aver_min_three_pointer, fill=decade)) + geom_histogram() + 
      facet_grid(.~decade) + 
      theme_bw() +
      scale_fill_manual('Decade', values=c("2000s" = "orange", "2010s" = "black")) +
      theme(legend.position = "none") + 
      labs(y="Count", x="Minutes", title = "Average minutes needed for a player to score 3 pointer in a certain game by decade")+
      coord_cartesian(xlim = input$xlim)
  })
  
  output$percentageOf3pointers <- renderPlot({
    ggplot(nba, aes(x=fg3m, fill=decade)) + geom_bar(position = "fill") +
      theme_bw() + scale_fill_manual('Decade', values=c("2000s" = "orange", "2010s" = "black")) + 
      labs(title = "Percentage of particular number of 3 pointers scored by decade", y="Percentage", x = "Number of 3 poiners scored", fill = "Decade") +
      coord_cartesian(xlim = input$xlim2)
  })
}



shinyApp(ui, server)