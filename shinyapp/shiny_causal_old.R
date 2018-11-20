library(zoo)
library(iqlr)
library(CausalImpact)
library("dplyr")
library(shiny)

clean_iql_date <- function(data) {
    return (substr(data, start = 2, stop = 11))
}

show_results <- function(data) {
    paste0(
    paste('posterior: ',format(round(100*(1 - data$summary["Cumulative","p"]), 2), nsmall=2), '%\n'),
    paste('cumulative effect: ', format(round(100*data$summary["Cumulative","RelEffect"], 2), nsmall=2), '%\n'),
    paste('cumulative effect lower: ', format(round(100*data$summary["Cumulative","RelEffect.lower"], 2), nsmall=2), '%\n'),
    paste('cumulative effect upper: ', format(round(100*data$summary["Cumulative","RelEffect.upper"], 2), nsmall=2), '%\n')
    )
}

generic_iql <- function(query) {
    return(suppressMessages(iql2(query, clean=TRUE)))
}

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Upload File"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

      # Horizontal line ----
      tags$hr(),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "File has header?", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Tab = "\t",
                               Comma = ","),
                   selected = "\t"),

      # Horizontal line ----
      tags$hr(),
      dateInput("intervention", "Intervention:", value = "2018-07-01", format = "yyyy-mm-dd"),
      numericInput("alpha", "alpha:", 0.10, min = 0.01, max = 0.15),
      tags$hr(),
      textInput("qry", "IQL Query", "FROM usersession 2018-01-01 2018-09-02 WHERE bot=0 country='AT' landing_page_type='jobsearch' GROUP BY time(1d) SELECT session_from='googleorg',   [IF (session_from IN ('bingorg','yahooorg')) THEN 1 ELSE 0],   [IF (session_from='unknown' AND device!='mobileapp') THEN 1 ELSE 0]"),
      checkboxInput("use_qry", "Use IQL Query?", FALSE),

      verbatimTextOutput("test")

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("plot1"),
      verbatimTextOutput("info"),
      tableOutput("final")

    )

  )
)


server <- function(input, output) {

  iql_data <- reactive({
    data <- generic_iql(input$qry)
    data$time <- as.character(data$time)
    n_columns = ncol(data)
    cols <- c("date", "test")
    for (i in 1:(n_columns - 2)) {
        cols <- c(cols, paste0("control", i))
    }
    names(data) <- cols
      
    start = as.character(data$date[1])
    intervention = as.character(input$intervention)
    end = as.character(data$date[nrow(data)])
    pre.period <- as.Date(c(start, as.character(as.Date(intervention)-1)))
    post.period <- as.Date(c(intervention, end))
      
    keep <- c()
    for (i in 2:(ncol(data))) {
        keep <- c(keep, i)
    }

    time.points <- seq.Date(as.Date(start), by = 1, length.out = nrow(data))
    data <- zoo(data[keep], time.points)

      
    pre.period <- as.Date(c(start,  as.character(as.Date(intervention)-1)))
    post.period <- as.Date(c(intervention, end))
    cx <- CausalImpact(data, pre.period, post.period, alpha=input$alpha)
    
    return(cx)
      
  })
        

  ci <- reactive({      
    req(input$file1)  
    data <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = "")
    data[c(1)] <- lapply(data[c(1)], clean_iql_date)
    start = as.character(data[c(1)][,1][1])
    intervention = as.character(input$intervention)
    end = as.character(data[c(1)][,1][nrow(data)])
    pre.period <- as.Date(c(start, as.character(as.Date(intervention)-1)))
    post.period <- as.Date(c(intervention, end))
      
    keep <- c()
    for (i in 2:(ncol(data))) {
        keep <- c(keep, i)
    }

    time.points <- seq.Date(as.Date(start), by = 1, length.out = nrow(data))
    data <- zoo(data[keep], time.points)

      
    pre.period <- as.Date(c(start,  as.character(as.Date(intervention)-1)))
    post.period <- as.Date(c(intervention, end))
    cx <- CausalImpact(data, pre.period, post.period, alpha=input$alpha)

    return(cx)

  })
    
    
  output$final <- renderTable({

    if (input$use_qry) {
      df <- iql_data()
      return(df$series)
    } else {
      df <- ci()
      return(df$series)
    }

  })
    
  output$plot1 <-renderPlot({
    if (input$use_qry) {
      return(plot(iql_data()))
    } else {
      return(plot(ci()))
    }
  })
    
  output$info <- renderText({
    if (input$use_qry) {
      return(show_results(iql_data()))
    } else {
      return(show_results(ci()))
    }
  })

  output$test <- renderText({
     input$qry
  })


}

# Create Shiny app ----
shinyApp(ui, server, options=list(port = 8080))


