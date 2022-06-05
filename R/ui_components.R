# header

dashboard_header <- dashboardHeader(title = "Exploring the Fragile Families Dataset",
                                    titleWidth = 400)

# sidebar

dashboard_sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("About", 
                 tabName = "about", icon = icon("info-circle")),
        menuItem("Participant Responses", 
                 tabName = "descrip", icon = icon("chart-bar")),
        menuItem("Distributions", 
                 tabName = "dist", icon = icon("chart-bar")),
        menuItem("Regression Models", 
                 tabName = "reg", icon = icon("chart-bar")),
        menuItem("Scatterplots", 
                 tabName = "scatterplot", icon = icon("chart-bar")),
        menuItem("Source Code", icon = icon("github"), 
                 href = "https://github.com/havishak/EDLD653-final-project.git")
    )
)

# body is at the bottom. Components are stored independently beforehand. 

# the "about" page

tabitems_about <- tabItem(
    "about",
    box(title = "About this Project",
        tags$div(
            tags$p("This dashboard was created by Ksenia Gordeeva, 
                    Rebecca Gordon, Havi Khurana, and Amy Warnock for
                    Spring 2022 EDLD 653 Functional Programming at the 
                   University of Oregon."), 
        tags$p("For this project, we used data from the 
               ", tags$a(
                   href = "https://fragilefamilies.princeton.edu/documentation", 
                        "Fragile Families & Child Well-Being Study.")
               ,),  
       tags$p("The Fragile Families Study is longitudinal, 
                with data collected from child participants and their 
                parents/guardians and teachers at multiple time points 
                (e.g., Baseline, Year 9, Year 15)."), 
       tags$p("Our research questions were:"),  
       tags$ol(
        tags$li("What is the association between internalizing or externalizing
                behaviors at age 9 and rates of delinquent behaviors at 
                age 15?"), 
       tags$li("Do race/ethnicity, gender, or other demographic 
                characteristics impact this association?"))
       , 
       tags$p("For this project, we examined the following variables:"),

       tags$ol(
           
           tags$li("Demographic characteristics of participants"),
           tags$ol(
               tags$li(type = "a", "Participant's self-reported race/ethnicity 
                                    at age 15"),
               tags$li(type = "a", "Mother's report of gender at birth"),
           ),
           
           tags$li("Scales:"),
           tags$ol(
                tags$li(type = "a", "Self-Description Questionnaire (SDQ) 
                                    (internalizing and externalizing 
                                    behaviors) at age 9"),
           tags$li(type = "a", "Delinquent Behavior Scale at age 9 and age 15")
           ),
           
           tags$li("Outcomes from questionnaires:"),
           tags$ol(
                    tags$li(type = "a", "Number of times suspended or expelled 
                                        at age 15")
           )
        )
    )
))

# bar plots of participant responses for scales by demographic variables  

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

# density plots of subscales and outcomes by race/ethnicity and gender

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

# Linear regression graphs between SDQ subscales and outcomes 

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

#Scatterplots with fitted regression lines
tabitems_plots3 <- tabItem(
    "scatterplot",
    fluidRow(
        box(
            title = "Does the relationship between externalizing or internalizing scores at age 9 and delinquent behaviors at age 15 vary across races?",
            "The plots on the previous tab illustrate that higher externalizing and internalizing behaviors at age 9 predict higher self-reported delinquency behaviors at age 15 at statitsically significant levels. However, the strength of this association does not differ by gender. Let's see if the patterns will change if we account for ethnicity/race.", width = 12)),
    fluidRow(
        box(
            radioButtons(
                inputId = "race",
                label = "Ethnicity/race:",
                choiceValues = c("Black", "Hispanic/Latino", "Multiracial and Other", "White"),
                choiceNames = c("Black", "Hispanic/Latino", "Multiracial and Other", "White"),
                select = "Black",
                inline = TRUE), width = 8)),
    fluidRow(
        box(plotOutput("plots3a"), width = 6),
        box(plotOutput("plots3b"), width = 6)),
    fluidRow(
        box(
            "While the effect of both externalizing and internalizing behaviors is relatively similar for Black males and females, the patterns of association between delinquency at the age of 15 and behaviors at the age of 9 differ between genders in other racial groups. For instance, Hispanic/Latino females exhibit the opposite tendency of higher bevavioral scores resulting in less delinquent behavior later in life. White participants diverge from the rest of the population by shwoing an overal negative correlation between the variables: higher externalizing beahviors yield lower delinquency rates for both gender, and higher internalizing scores lead to less delinquency in white women. As for the multiracial participants and representatives of other races, the effect on internalizing behavior differs betwen men and women, with females demonstrating a negative correlation between internalizing behavior and delinquency.", width = 12)),
    )
    

#dashboard body

dashboard_body <- dashboardBody(
    fluidRow(
        tabItems(
            tabitems_about,
            tabitems_plots0,
            tabitems_plots1, 
            tabitems_plots2,
            tabitems_plots3
            )
        )
    )



