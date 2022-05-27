library(shiny)
library(ggplot2)
library(dplyr)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fragile Families"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("scale",
                         "Scale",
                         unique(ff_meta_subset$clean_scale)[-1]),
            
            selectizeInput(
                inputId = "varlab",
                label = "Item",
                choices = NULL),
            
            radioButtons("display_choice",
                         "Show by",
                         c("By race", "By race and gender"))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observeEvent(input$scale,
                 {
                    updateSelectizeInput(session, input = "varlab",
                    choices = ff_meta_subset %>% 
                             filter(clean_scale %in% input$scale) %>% 
                             select(clean_label) 
                    )
    })
    
    response <- reactive(
        as.character(ff_meta_subset %>% 
        filter(clean_scale %in% input$scale &
                   clean_label %in% input$varlab) %>%
        select(new_name)
    ))
    
    output$distPlot <- 
        renderPlot({
            df_subset <- total_response_race(response())
            df_manipulate <- total_response_subgroup(df_subset, 
                                                     input$display_choice)
            descriptive_plot(df_manipulate, 
                             response(), input$display_choice)
})
}
# Run the application 

shinyApp(ui = ui, server = server)
