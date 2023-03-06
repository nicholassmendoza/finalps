library(tidyverse)
library(shiny)
library(plotly)

college <- read_delim("college.csv")

topprivate <- college %>% 
  group_by(State) %>% 
  filter(Type == "Private") %>% 
  summarise(avgcost = mean(Value)) %>% 
  arrange(desc(avgcost))

toppublicinstate <- college %>% 
  group_by(State) %>% 
  filter(Type == "Public In-State") %>% 
  summarise(avgcost = mean(Value)) %>% 
  arrange(desc(avgcost))

toppublicoutofstate <- college %>% 
  group_by(State) %>% 
  filter(Type == "Public Out-of-State") %>% 
  summarise(avgcost = mean(Value)) %>% 
  arrange(desc(avgcost))

college_info <- college %>% 
  sample_n(5)

ui <- fluidPage(
  titlePanel("PS6: College Tuition Data"),
  tabsetPanel(
    tabPanel("General Info",
             br(),
             HTML("<h3>Dataset Information</h3>
                  <p>The dataset being used is called <b>Average Cost of Undergraduate College by State.</b> Collected by the National Center of Education Statistics Annual Digest. Found on Kaggle - @kenmoretoast, updated a month ago. The dataset focuses on average undergraduate tuition and fees and room and board rates charged for full-time students in degree-granting postsecondary institutions, by control and level of institution and state or jurisdiction from the years 2013-2021.</p>
                  <br>
                  <h4>Target Audience</h4>
                  <p>Our target audience is prospective college students. Prospective college students would be interested in visualizing the cost of colleges based on their location (state), so transforming this data about college costs into a friendly and presentable way would be helpful for them.</p>
                  <br>
                  <h4>Random Sample of the Data:</h4>
                  <p>This dataset contains 3548 observations and 6 variables. Here is a small (random) sample of data:</p>
                  </ul>"),
             tableOutput("sample_table")),
    
    tabPanel("Plot",
             titlePanel("Tuition by State"),
             sidebarPanel(
               fluidRow(
                 column(4,
                        uiOutput("checkboxState"))
               )
             ),
             column(4,
                    uiOutput("radioButtonTrendline")),
             
             mainPanel(
               plotlyOutput("myplot")
             )
    ),
    
    tabPanel("Table",
             titlePanel("Average Tuition by Type of College for Each State"),
             sidebarPanel(
               fluidRow(
                 column(10,
                        radioButtons(inputId = "tabletype" , label = "Select the type of college", choices = c("Public In-State", "Public Out-of-State", "Private"))
                 ),
               )
             ),
             mainPanel(
               dataTableOutput("mytable")
             ),
    )
  ),
)
server <- function(input, output) {
  output$sample_table <- renderTable({
    college_info
  })
  output$checkboxState <- renderUI({
    checkboxGroupInput("State", "Choose State",
                       choices = unique(college$State)
    )
  })
  sample <- reactive({
    s1 <- college %>% 
      filter(State %in% input$State)
  })
  output$myplot <- renderPlotly({
    p <- plot_ly(data = sample(),
                 x = ~Year, y = ~Value, color = ~State,
                 marker = list(size=10),
                 type = "scatter")
    p <- p %>% 
      add_annotations(text = paste("The number of data points on this graph is:", nrow(sample())),
                      xref = "paper", yref = "paper", x = 1, y = -0.1, showarrow = FALSE)
    
    if(input$Trendline == "yes") {
      p <- p %>% add_trace(type = "scatter", mode = "lines",
                           x = ~Year, y = ~fitted(loess(Value ~ Year)), 
                           line = list(color = "black", width = 2))
    }
    
    p
  })
  output$radioButtonTrendline <- renderUI({
    radioButtons("Trendline", "Choose Trendline",
                 choices = c("no", "yes"))
  })
  output$mytable <- renderDataTable({
    if (input$tabletype == "Public In-State") {
      toppublicinstate
    } else if (input$tabletype == "Public Out-of-State") {
      toppublicoutofstate
    }
    else if (input$tabletype == "Private") {
      topprivate
      
    }
  })
}

shinyApp(ui = ui, server = server)
