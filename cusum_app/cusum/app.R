#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(readr)
library(readxl)
library(tidyverse)
library(DT)
source("CUSUM_help_fun.R")


# Define UI for application that draws a histogram
ui <- page_sidebar(

    title="CUSUM Chart for Monitoring  Increase of cases during epidemic ",
    sidebar = sidebar(
        helpText("This application visualizes CUSUM charts for monitoring increases in case counts during an epidemic."), 
        
        
        card(
            card_header("File input"),
            fileInput("file", label = NULL),
            helpText(
                "Upload a file in as a csv or xlsx.
                The file should contain two columns: 'date' and 'daily_cases'."
            ), 
            
            width = 40), 
        
        
        
        card(
            card_header("CUSUM parameters "), 
            actionButton("cusum", "calculate CUSUM values"),
            actionButton("plot_cusum_chart", "Plot CUSUM chart"),
            downloadButton("download", "Download CUSUM calculated data"),
            width = 40 ), 
        
        width = 400
    ),
    
    card(
        card_header("CUSUM Chart"),
        plotOutput(outputId = "cusum_chart"),
    ),
    
    card(
        card_header("CUSUM calculated Results"),
        DT::dataTableOutput(outputId = "cusum_table"),
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    data <- reactiveVal(NULL)  # reactive container for data with CUSUM results
    
    observeEvent(input$cusum, {
        req(input$file)
        file_path <- input$file$datapath
        
        # Detect file type by extension
        ext <- tools::file_ext(file_path)
        if (ext == "csv") {
            data <- readr::read_csv(file_path)
        } else if (ext %in% c("xls", "xlsx")) {
            data <- readxl::read_excel(file_path)
        } else {
            showNotification("Unsupported file type.", type = "error")
            return()
        }
        
        # Validate columns
        if (!all(c("date", "daily_cases") %in% colnames(data))) {
            showNotification("File must contain 'date' and 'daily_cases' columns", type = "error")
            return()
        }
        
        # Run cusum_fun on the data
        cusum_table <- cusum_fun(data)
        
        data(cusum_table)  # store result reactively
        
        showNotification("CUSUM values calculated.", type = "message")
    })
    
    
    
    observeEvent(input$plot_cusum_chart, {
        req(data())
        result <- data()
        
        # Plot CUSUM chart using ggplot2
        library(ggplot2)
        
        cusum_chart <- result %>%
            ggplot(aes(x = date)) +
            geom_line(aes(y = daily_cases), color = "blue") +
            geom_point(aes(y = c1_signal * 1000, color = "c1_signal")) +
            geom_point(aes(y = c2_signal * 1000, color = "c2_signal")) +
            geom_point(aes(y = c3_signal * 1000, color = "c3_signal")) +
            geom_point(aes(y = c4_signal * 1000, color = "c4_signal")) +
            geom_point(aes(y = c5_signal * 1000, color = "c5_signal")) +
            geom_point(aes(y = c6_signal * 1000, color = "c6_signal")) +
            scale_color_manual(
                name = "Signal",
                values = c(
                    "c1_signal" = "red",
                    "c2_signal" = "green",
                    "c3_signal" = "purple",
                    "c4_signal" = "grey",
                    "c5_signal" = "gold2",
                    "c6_signal" = "black"
                )
            ) +
            labs(title = "CUSUM CHART for Daily cases",
                 y = "Counts / CUSUM values",
                 x = "Date") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(
                sec.axis = sec_axis(~ ./1000, name = "Signal (0/1)")
            ) +
            theme(
                axis.title.y.right = element_text(color = "black"),
                axis.text.y.right = element_text(color = "black"),
                legend.title = element_text(face = "bold"),
                legend.position = "right"
            )
        
        rendered_plot <- renderPlot({ cusum_chart })
        
        output$cusum_chart <- rendered_plot
        
        showNotification("CUSUM chart plotted.", type = "message")
    })

    
    output$cusum_table <- DT::renderDataTable({
        req(data())
        DT::datatable(data(), options = list(pageLength = 10))
    })
    
    output$download <- downloadHandler(
        filename = function() {
            paste0("cusum_data_", Sys.Date(), ".csv")
        },
        content = function(file) {
            req(data())
            write.csv(data(), file, row.names = FALSE)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
