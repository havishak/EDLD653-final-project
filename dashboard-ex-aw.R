# Load packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(here)
library(rio)
library(janitor)
library(colorblindr)
library(glue)

# Load the data and calc subscale scores
ff_sub_orig <- import(here("data","ff_sub.Rda")) 

ff_sub <- ff_sub_orig %>% mutate_all(~ifelse(.x %in% 
                                                 c(-1, -2, -3, -4, -5, 
                                                   -6, -7,-8, -9), NA, .x)) %>% 
    na.omit()

ff_sub2 <- ff_sub %>% 
    mutate(int_scores = (k5g2a + k5g2c + k5g2e + k5g2g + 
                             k5g2i + k5g2j + k5g2k + k5g2l) / 8, 
           ext_scores = (k5g2b + k5g2d + k5g2f + 
                             k5g2h + k5g2m + k5g2n) / 6)

ff_sub_lm <- ff_sub2 %>% 
    rowwise() %>% 
    select(idnum, 
           starts_with("k6d6"), 
           starts_with("k5f1"), 
           int_scores, 
           ext_scores, 
           cm1bsex, 
           ck6ethrace, 
           p5l12g, 
           p6c22) %>% #AW: If you look at the ff_meta_subset df, p6c21 is a binary yes/no variable (C21. Youth ever been suspended/expelled?). I think we want p6c22 (# times reported by parent) or k6b30 (# times reported by student) instead. I changed this and subsequent instances to p6c22
    filter(rowSums(across(where(is.numeric))) >= 0 & 
               ck6ethrace >= 0 & 
               p5l12g >= 0 & 
               p6c22 >= 0) %>% 
    mutate(del_beh_9 = sum(c_across(starts_with("k5f1"))), # binary yes/no
           del_beh_15 = sum(c_across(starts_with("k6d6"))),#AW: I think this should be k6d61 (responses 1-4, with 4 being highest - see ff_meta_subset file) because the k6d62 variables are from the peer delinquency portion of the scale and have different responses (1, 2, 3), where lower scores correspond to greater delinquent behaviors (1 = often, 3 = never). I'm not sure negative responses are getting filtered out since there's a -8 in the resulting column 
           del_beh_15_self_rep = sum(c_across(starts_with("k6d61"))))

# First step - recode race/eth

ethrace <- ff_sub_lm %>%
    mutate(ck6ethrace = recode(ck6ethrace, 
                               "1" = "White",
                               "2" = "Black",
                               "3" = "Hispanic/Latino",
                               "4" = "Other", 
                               "5" = "Multiracial"))

# the ui
ui <- dashboardPage(
    dashboardHeader(title = "Exploring the Fragile Families Dataset"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Distributions", tabName = "dist", icon = icon("chart-bar")),
            menuItem("Distributions Copy", tabName = "dist2", icon = icon("chart-bar"))
        )
    ),
    dashboardBody(
        fluidRow(
            tabItems(
                tabItem(
                    "dist",
                    box(
                        title = "Distributions of Subscale Scores and Suspensions/Expulsions by Race/Ethnicity and Gender",
                        plotOutput("plots1", height = 900)
                    ),
                    box(
                        radioButtons(
                            inputId = "var",
                            label = "Subscale:",
                            choiceValues = c("int_scores", "ext_scores", 
                                             "del_beh_15_self_rep", "p6c22"),
                            choiceNames = c("Internalizing Behaviors Age 9", 
                                            "Externalizing Behaviors Age 9", 
                                            "Delinquent Behaviors Age 15",
                                            "Suspensions and Expulsions Age 15"),
                            select = "int_scores",
                            inline = TRUE
                        ),
                        radioButtons(
                            inputId = "gender",
                            label = "Gender:",
                            choiceValues = c("1", "2"),
                            choiceNames = c("Male", "Female"),
                            select = "1",
                            inline = TRUE
                        )
                    )                        
  
                ),
                tabItem(
                    "dist2",
                    box(
                        title = "Distributions Copy",
                        plotOutput("plots2", height = 900)
                ),
                box(
                    radioButtons(
                        inputId = "var",
                        label = "Subscale:",
                        choiceValues = c("int_scores", "ext_scores", 
                                         "del_beh_15_self_rep", "p6c22"),
                        choiceNames = c("Internalizing Behaviors Age 9 (copy)", 
                                        "Externalizing Behaviors Age 9 (copy)", 
                                        "Delinquent Behaviors Age 15 (copy)",
                                        "Suspensions and Expulsions Age 15 (copy)"),
                        select = "int_scores",
                        inline = TRUE
                    ),
                    radioButtons(
                        inputId = "gender",
                        label = "Gender:",
                        choiceValues = c("1", "2"),
                        choiceNames = c("Male (copy)", "Female (copy)"),
                        select = "1",
                        inline = TRUE
                    )
                )                        
                
                )
            )
        )
    )
)


server <- function(input, output) {
    
    output$plots1 <- renderPlot({
        ethrace %>% 
            filter(cm1bsex == input$gender) %>% 
            ggplot(aes(!!sym(input$var))) +
            geom_histogram(
                fill = "#56B4E9",
                color = "white") +
            facet_wrap(~ck6ethrace) +
            theme_minimal(30)
    })
    
    output$plots2 <- renderPlot({
        ethrace %>% 
            filter(cm1bsex == input$gender) %>% 
            ggplot(aes(!!sym(input$var))) +
            geom_histogram(
                fill = "#56B4E9",
                color = "white") +
            facet_wrap(~ck6ethrace) +
            theme_minimal(30)
    })
}

shinyApp(ui, server)
