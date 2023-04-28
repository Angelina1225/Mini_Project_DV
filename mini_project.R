# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggcorrplot)
library(GGally)
library(shinydashboard)
library(caret)

fitness_file_path <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "CardioGoodFitness.csv")
fitness <- read.csv(fitness_file_path, header = TRUE)
num_cat_fitness <- fitness %>% select_if(is.numeric)

# Define UI for Shiny app
ui <- dashboardPage(
  
  # App title
  dashboardHeader(title = "DATA ANALYSIS"),
  
  # Sidebar with a menu of dataset options
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Fitness", tabName = "fitness", icon = icon("heartbeat")),
      menuItem("Iris", tabName = "iris", icon = icon("tree"))
    )
  ),
  
  # Show different tabs based on the selected dataset
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h3("EXPLORATORY DATA ANALYSIS."),
              fluidRow(
                box(title = "IRIS DATASET", "The Iris flower dataset is a classic dataset in exploratory data analysis. It is 
                    a small dataset that consists of 150 observations of iris flowers, with 50 observations per species. Each 
                    observation includes four features: sepal length, sepal width, petal length, and petal width.", width = 6),
                
                box(title = "CARDIOGOODFITNESS DATASET", "The CardioGoodFitness is a fictional dataset created by the Minitab Express 
                software that contains data about customers who purchased a treadmill from a store called CardioGoodFitness. 
                The dataset includes variables: Product, Age, Gender, Education, Marital Status, Usage, Fitness, Income, Miles.", 
                    width = 6)
              ),
              fluidRow(
                box(title = "Scatter Plot", "A scatter plot is a data visualisation tool that shows the relationship between 
                two numerical variables. It is a graph that plots the values of two variables along two axes, with a point 
                plotted for each pair of values. It aims to aid in determining the type of relationship that exists between 
                variables. This is useful for spotting outliers and discovering potential sources of correlation between variables.", width = 4),
                
                box(title = "Box Plot", "A box plot is a type of data visualization that is used to display the distribution 
                of dataset through their quartiles. It is a graph that displays the five-number summary of a set of data 
                (minimum, lower quartile, median, upper quartile, and maximum) and any outliers or extreme values. It's main purpose is 
                to identify the shape, spread, and skewness of a dataset. It's useful in detecting outliers and 
                comparing the distribution.", width = 4),
                
                box(title = "Histogram", "A histogram is a type of data visualization that is used to display the distribution 
                of a numerical dataset. It is a graph that consists of a series of bars, where each bar represents a range of values 
                and the height of the bar represents the frequency or count of observations that fall within that range.
                It's main purpose is to show the underlying frequency distribution of a dataset, such as whether it 
                is symmetrical, skewed, or has multiple peaks.", width = 4)
              ),
              fluidRow(
                box(title = "Density Plot", "A density plot is a type of plot that visualizes the distribution of a continuous variable. 
                It is also known as a kernel density plot. It is a smoothed version of a histogram, which uses a continuous curve instead 
                of bars to represent the data. In a density plot, the x-axis represents the range of the data and the y-axis represents 
                the density, which shows how frequently the data occurs within a certain range. The shape of the curve represents 
                the underlying distribution of the data.", width = 4),
                
                box(title = "Correlation Matrix", "A correlation matrix is a table that displays the coefficients of correlation between 
                    variables. The correlation coefficient is a statistical measure that reflects how closely two variables are related 
                    and has a value between -1 and +1, with (-1,0) indicating negative correlation, 0 indicating no association, 
                    and (0,+1) indicating positive correlation. It is useful for determining the strength and direction of the 
                    association between two or more variables and also determines which has significant correlation.", width = 4),
                
                box(title = "Scatter Plot Matrix", "A scatter plot matrix is a type of data visualization that is used to display the pairwise 
                relationships between multiple variables in a dataset. It consists of a matrix of scatter plots, where each plot represents 
                the relationship between two variables, with one variable on the x-axis and the other variable on the y-axis. The main purpose 
                    of a scatter plot matrix is to show how the variables in a dataset are related to one another, and whether there are any 
                    patterns or trends in the data.", width = 4)
              )
      ),
      
      tabItem(tabName = "fitness",
              fluidRow(
                tabBox(width = 16,
                    tabPanel("Summary", verbatimTextOutput("summary_fitness")),
                    tabPanel("Scatter Plot",  
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("xcol_1", "X-axis Variable", names(num_cat_fitness)),
                                 selectInput("ycol_1", "Y-axis Variable", names(num_cat_fitness),
                                             selected = names(fitness)[[2]])
                               ),
                               mainPanel(
                                 plotlyOutput("scatterplot_fitness")
                               )
                             )
                    ),
                    tabPanel("Box Plot",  
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("ycol_2", "Y-axis Variable", names(num_cat_fitness),
                                             selected = names(fitness)[[1]])
                               ),
                               mainPanel(
                                 plotlyOutput("boxplot_fitness"),
                                 plotlyOutput("boxplot_fitness_1")
                               )
                             )
                    ),
                    tabPanel("Histogram", 
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("xcol_3", "X-axis Variable", names(num_cat_fitness),
                                             selected = names(fitness)[[1]]),
                                 sliderInput("slider", "Slider input:", 1, 50, 30)
                               ),
                               mainPanel(
                                 plotlyOutput("histogram_fitness"),
                                 plotlyOutput("histogram_fitness_1")
                               )
                             )
                    ),
                    tabPanel("Density Plot", 
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("xcol_4", "X-axis Variable", names(num_cat_fitness),
                                             selected = names(fitness)[[1]]),
                                 numericInput(inputId = "bandwidth", label = "Bandwidth:",
                                              value = 0.2, min = 0.00, max = 1, step = 0.05),
                               ),
                               mainPanel(
                                 plotlyOutput("densityplot_fitness"),
                                 plotlyOutput("densityplot_fitness_1")
                               )
                             )
                    ),
                    tabPanel("Correlation Matrix", plotlyOutput("correlation_matrix_fitness")),
                    tabPanel("Scatter Plot Matrix", plotlyOutput("scatterplot_matrix_fitness", height = "700px"))
                  )
                )
              
      ),
      
      tabItem(tabName = "iris",
              fluidRow(
                tabBox(width = 16, 
                  tabPanel("Summary", verbatimTextOutput("summary_iris")),
                  tabPanel("Scatter Plot",  
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("xcol", "X-axis Variable", names(iris)[1:4]),
                         selectInput("ycol", "Y-axis Variable", names(iris)[1:4],
                                     selected = names(iris)[[2]])
                       ),
                       mainPanel(
                         plotlyOutput("scatterplot_iris")
                       )
                    )
                  ),
                  tabPanel("Box Plot", 
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("xcol1", "X-axis Variable", names(iris)[5]),
                         selectInput("ycol1", "Y-axis Variable", names(iris)[1:4],
                                     selected = names(iris)[[2]])
                       ),
                       mainPanel(
                         plotlyOutput("boxplot_iris")
                       )
                    )
                  ),
                  tabPanel("Histogram", 
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("xcol2", "X-axis Variable", names(iris)[1:4],
                                     selected = names(iris)[[1]]),
                         sliderInput("slider", "Slider input:", 1, 50, 30)
                       ),
                       mainPanel(
                         plotlyOutput("histogram_iris")
                       )
                    )
                  ),
                  tabPanel("Density Plot", 
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("xcol3", "X-axis Variable", names(iris)[1:4],
                                     selected = names(iris)[[1]]),
                         numericInput(inputId = "bandwidth", label = "Bandwidth:",
                                      value = 0.2, min = 0.00, max = 1, step = 0.05),
                       ),
                       mainPanel(
                         plotlyOutput("densityplot_iris")
                       )
                     )
                  ),
                  tabPanel("Correlation Matrix", plotlyOutput("correlation_matrix_iris")),
                  tabPanel("Scatter Plot Matrix", plotlyOutput("scatterplot_matrix_iris", height = "700px"))
              )
          )
      )
    )
  )
  
)

# Define server logic for the Shiny app
server <- function(input, output) {
  
  # Load selected dataset
  dataset <- reactive({
    switch(input$tabName,
           "fitness" = fitness,
           "iris" = iris)
  })
  
  # Show summary of selected dataset
  output$summary_fitness <- renderPrint({
    summary(fitness)
  })
  output$summary_iris <- renderPrint({
    summary(iris)
  })
  
  # Create scatterplot of selected dataset
  output$scatterplot_fitness <- renderPlotly({
    ggplot(fitness, aes_string(x = input$xcol_1, y = input$ycol_1, color = names(fitness)[3], shape = names(fitness)[3])) +
      geom_point() +
      labs(x = input$xcol_1, y = input$ycol_1)
  })
  
  output$scatterplot_iris <- renderPlotly({
    ggplot(iris, aes_string(x = input$xcol, y = input$ycol, color = names(iris)[5], shape = names(iris)[5])) +
      geom_point() +
      xlim(min(iris[,input$xcol]), max(iris[,input$xcol])) +
      ylim(min(iris[,input$ycol]), max(iris[,input$ycol])) +
      labs(x = input$xcol, y = input$ycol)
  })
  
  # Create boxplot of selected dataset
  output$boxplot_fitness <- renderPlotly({ 
    ggplot(fitness, aes_string(x = names(fitness)[5], y = input$ycol_2, fill = names(fitness)[5], 
                            color = names(fitness)[5], text = names(fitness)[5])) +
      geom_boxplot() +
      geom_jitter(position = position_jitter(0.2))+
      theme_minimal() +
      labs(x = names(fitness)[5], y = input$ycol_2)
  })
  
  output$boxplot_fitness_1 <- renderPlotly({ 
    ggplot(fitness, aes_string(x = names(fitness)[3], y = input$ycol_2, fill = names(fitness)[3], 
                               color = names(fitness)[3], text = names(fitness)[3])) +
      geom_boxplot() +
      geom_jitter(position = position_jitter(0.2))+
      theme_minimal() +
      labs(x = names(fitness)[3], y = input$ycol_2)
  })
  
  output$boxplot_iris <- renderPlotly({ 
    ggplot(iris, aes_string(x = names(iris)[5], y = input$ycol1, fill = names(iris)[5], 
                            color = names(iris)[5], text = names(iris)[5])) +
      geom_boxplot() +
      geom_jitter(position = position_jitter(0.2))+
      theme_minimal() +
      labs(x = names(iris)[5], y = input$ycol1)
    
  })
  # Create histogram of selected dataset
  
  output$histogram_fitness <- renderPlotly({
    ggplot(fitness, aes_string(x = input$xcol_3, fill = names(fitness)[5])) +
      geom_histogram(bins = input$slider, alpha = 0.75, position = "identity") +
      theme_minimal() +
      labs(x = input$xcol_3, y = "Frequency")
  })
  
  output$histogram_fitness_1 <- renderPlotly({
    ggplot(fitness, aes_string(x = input$xcol_3, fill = names(fitness)[3])) +
      geom_histogram(bins = input$slider, alpha = 0.75, position = "identity") +
      theme_minimal() +
      labs(x = input$xcol_3, y = "Frequency")
  })
 
  output$histogram_iris <- renderPlotly({
    ggplot(iris, aes_string(x = input$xcol2, fill = names(iris)[5])) +
      geom_histogram(bins = input$slider, alpha = 0.75, position = "identity") +
      theme_minimal() +
      labs(x = input$xcol2, y = "Frequency")
  })
  
  # Create density plot of selected dataset
  output$densityplot_iris <- renderPlotly({ 
    ggplot(iris, aes_string(x = input$xcol3, fill = names(iris)[5])) +
      geom_density(kernel = "gaussian", bw = input$bandwidth) +
      theme_minimal() +
      labs(x = input$xcol3, y = names(iris)[5])
    
  })
  
  output$densityplot_fitness <- renderPlotly({ 
    ggplot(fitness, aes_string(x = input$xcol_4, fill = names(fitness)[5])) +
      geom_density(kernel = "gaussian", bw = input$bandwidth) +
      theme_minimal() +
      labs(x = input$xcol_4, y = names(fitness)[5])
    
  })
  
  output$densityplot_fitness_1 <- renderPlotly({ 
    ggplot(fitness, aes_string(x = input$xcol_4, fill = names(fitness)[3])) +
      geom_density(kernel = "gaussian", bw = input$bandwidth) +
      theme_minimal() +
      labs(x = input$xcol_4, y = names(fitness)[3])
    
  })
  
  # Create correlation matrix of selected dataset
  output$correlation_matrix_iris <- renderPlotly({
    corr <- cor(iris[1:4])
    ggcorrplot(corr, hc.order = TRUE, lab = TRUE, colors = c("#6D9EC1", "white", "#E46726"), outline.col = "white")
  })
  
  output$correlation_matrix_fitness <- renderPlotly({
    corr <- cor(num_cat_fitness)
    ggcorrplot(corr, hc.order = TRUE, lab = TRUE, outline.col = "white")
  })
  
  # Create scatterplot matrix of selected dataset
  output$scatterplot_matrix_iris <- renderPlotly({
    ggpairs(iris, columns = 1:4, aes(color = iris[,5], alpha = 0.5),
            upper = list(continuous = "points")) 
  })
  
  output$scatterplot_matrix_fitness <- renderPlotly({
    ggpairs(num_cat_fitness, columns = 1:6, aes(alpha = 0.5),
            upper = list(continuous = "points")) 
  })
}

# Run the application
shinyApp(ui = ui, server = server)
