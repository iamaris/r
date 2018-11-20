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
    paste('Posterior prob. of a causal effect: ',format(round(100*(1 - data$summary["Cumulative","p"]), 2), nsmall=2), '%\n'),
    paste('Cumulative effect: ', format(round(100*data$summary["Cumulative","RelEffect"], 2), nsmall=2), '%\n'),
    paste('Cumulative effect lower: ', format(round(100*data$summary["Cumulative","RelEffect.lower"], 2), nsmall=2), '%\n'),
    paste('Cumulative effect upper: ', format(round(100*data$summary["Cumulative","RelEffect.upper"], 2), nsmall=2), '%\n')
    )
}

generic_iql <- function(query) {
    return(suppressMessages(iql2(query, clean=TRUE)))
}

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Input Panel"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Choose CSV/TSV File",
                multiple = TRUE,
                accept = c("text/csv/tsv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "File has header?", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Tab = "\t",
                               Comma = ","),
                   selected = "\t"),
      
      radioButtons("dformat", "Date Format",
                   choices = c('IQL Style' = "iql",
                               'YYYY-MM-DD' = "ymd"),
                   selected = "iql"),

      # Horizontal line ----
      tags$hr(),
      dateInput("intervention", "Intervention:", value = "2018-07-20", format = "yyyy-mm-dd"),
      numericInput("alpha", "alpha:", 0.10, min = 0.01, max = 0.15),
      tags$hr(),
      textInput("qry", "IQL Query", "FROM usersession 2018-05-01 2018-08-20 WHERE bot=0 country='UA' landing_page_type='jobsearch' GROUP BY time(1d) SELECT session_from='googleorg', session_from='unknown', session_from='job_alert'"),
      checkboxInput("use_qry", "Use IQL Query?", FALSE),
      wellPanel(
        textOutput("test")
      ),
      actionButton("show", "How to use this app?")

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
    if (input$dformat=='iql') {
        data[c(1)] <- lapply(data[c(1)], clean_iql_date)
    }
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
  
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "How to use this app",
      tags$strong("IQL query mode:"),tags$br(),
      tags$ol(tags$li("Input intervention date"),
              tags$li("Input IQL query"),
              tags$li("Make sure the first field after SELECT is the response series"),
              tags$li("Make sure that all succeeding fields are your control series"),
              tags$li("Check the 'Use IQL Query?' box"),
              tags$li("Wait a little bit for the output to appear")), tags$br(),
      tags$strong("Upload file mode:"),tags$br(),
      tags$ol(
              tags$li("Make sure that the first column of your file is the 'date' information"),
              tags$li("Make sure the second column is the response series"),
              tags$li("Make sure that all succeeding columns are your control series"),
              tags$li("Input intervention date"),
              tags$li("Under the Separator section, choose Tab for TSV or Comma for CSV"),
              tags$li("Under the Date Format section, choose 'IQL Style' if the file is from an IQL query otherwise choose 'YYYY-MM-DD' "),
              tags$li("Uncheck 'File has header?' box if your file has no header (column names)"),
              tags$li("Upload file by clicking on 'Browse..' button"),
              tags$li("Wait a little bit for the output to appear")), tags$br(),
      tags$strong("Notes:"),tags$br(),"alpha is similar to alpha in an A/B test. The smaller it is the more strict you are in detecting false positive.",
      easyClose = TRUE
    ))
  })



}

# Create Shiny app ----
shinyApp(ui, server, options=list(port = 8080))


