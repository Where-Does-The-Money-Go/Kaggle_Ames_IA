library(corrplot)
library(shiny)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("variable", "Variables to show:",
                       train_df %>%
                           select_if(is.numeric) %>%
                           colnames()
           ),
           sliderInput("numeric", "Threshold:",
                       min = 0, max = 1,
                       value = .5)
        )

    ,
        mainPanel(
            plotOutput("data")
        )
    )
)

server <- function(input, output, session) {
    output$data <- renderPlot(
            corrplot(train_df%>%
                         select_if(is.numeric) %>%
                         select(c(input$variable)) %>%
                             cor()>input$numeric, 
                                col = c("white", "red"),
                                #bg = "lightblue",
                                method = "square",
                                order = "hclust",
                     addrect = 4
                     
                     )
                        
            )
    
}

shinyApp(ui, server)