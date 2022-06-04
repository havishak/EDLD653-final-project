dashboard_header <- dashboardHeader(title = "Exploring the Fragile Families Dataset",
                                    titleWidth = 400)

dashboard_sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Participant Responses", tabName = "descrip", icon = icon("chart-bar")),
        menuItem("Distributions", tabName = "dist", icon = icon("chart-bar")),
        menuItem("Regression Models", tabName = "reg", icon = icon("chart-bar"))
    )
)

tabitems_plots0 <- tabItem(
    "descrip",
    box(
        title = "Participant Responses",
        plotOutput("plots0", height = 600, width = 600)
    ),
    box(
        radioButtons("scale",
                     "Scale",
                     unique(ff_meta_subset$clean_scale)[-1]),
        selectizeInput(
            inputId = "varlab",
            label = "Item",
            choices = NULL
            ),
        radioButtons("display_choice",
                     "Show by",
                     c("By race", 
                       "By race and gender")
                     )
        )
    )                        

tabitems_plots1 <- tabItem(
    "dist",
    box(
        title = "Distributions",
        plotOutput("plots1", height = 600, width = 600)
    ),
    box(
        selectizeInput(
            inputId = "var",
            label = "Subscale or Outcome:",
            choices = c("Internalizing Behaviors Age 9" = "int_scores",
                             "Externalizing Behaviors Age 9" = "ext_scores",
                             "Delinquent Behaviors Age 15" = "del_beh_15_self_rep",
                             "Suspensions and Expulsions Age 15" = "p6c22"
                         ),
            select = "int_scores"
        ),
        selectizeInput(
            inputId = "gender",
            label = "Gender:",
            choices = c("Male", 
                        "Female"),
            select = "Male"
        )
    )                        
)

tabitems_plots2 <- tabItem(
    "reg",
    box(
        title = "What is the relationship between externalizing or internalizing scores at age 9 and delinquent behaviors or suspensions/expulsions at age 15?", 
        plotOutput("plots2", height = 400, width = 400),
        p("We found that higher externalizing and internalizing behaviors at age 9 strongly predicted higher self-reported delinquency behaviors at age 15. However, externalizing and internalizing behaviors did not significantly predict suspensions and expulsions at age 15.")
    ),
    box(
        radioButtons(
            inputId = "dv",
            label = "Dependent variables:",
            choiceValues = c("del_beh_15_self_rep", "p6c22"),
            choiceNames = c("Delinquent Behaviors Age 15",
                            "Suspensions and Expulsions Age 15"),
            select = "del_beh_15_self_rep",
            inline = TRUE
        ),
        radioButtons(
            inputId = "iv",
            label = "Independent variable:",
            choiceValues = c("int_scores", "ext_scores"),
            choiceNames = c("Externalizing behaviors", 
                            "Internalizing behaviors"),
            select = "int_scores",
            inline = TRUE
        )
    )
)

dashboard_body <- dashboardBody(
    fluidRow(
        tabItems(
            tabitems_plots0,
            tabitems_plots1, 
            tabitems_plots2
            )
        )
    )



