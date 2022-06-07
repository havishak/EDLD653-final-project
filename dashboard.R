# Load packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(here)
library(rio)
library(janitor)
library(colorblindr)
library(glue) 
library(purrr)
library(tidyr)


# the ui (dashboard components are stored as objects in "ui_components" R file)
ui <- dashboardPage(
    dashboard_header,
    dashboard_sidebar,
    dashboard_body
    )

# the server
server <- function(input, output, session) {

        observeEvent(input$scale,
                 {updateSelectizeInput(session, 
                                       input = "varlab",
                                       choices = ff_meta_subset %>% 
                                           filter(clean_scale %in% 
                                                      input$scale) %>% 
                                           select(clean_label) 
                                       )
                     })
    
    response <- reactive(
        as.character(ff_meta_subset %>% 
                         filter(clean_scale %in% input$scale &
                                    clean_label %in% input$varlab) %>%
                         select(new_name)
        ))
    
    output$plots0 <- 
        renderPlot({
            df_subset <- total_response_race(response())
            df_manipulate <- total_response_subgroup(df_subset, 
                                                     input$display_choice)
            descriptive_plot(df_manipulate, 
                             response(), input$display_choice)
        })
    
    output$plots1 <- renderPlot({
        ethrace %>% 
            filter(cm1bsex == input$gender) %>% 
            ggplot(aes(!!sym(input$var))) +
            ggridges::geom_density_ridges(aes(y = ck6ethrace, 
                                              fill = ck6ethrace),
                                          color = "white",
                                          alpha = 0.8) +
            scale_fill_OkabeIto(guide = "none") +
            scale_x_continuous(limits = c(0, NA)) +
            scale_y_discrete(expand = c(0, 0)) +
            labs(x = "",
                 y = "",
                 caption = paste0("Dotted line represents the mean of the selected subscale/outcome for all ", tolower(input$gender), " participants.")) +
            theme_minimal(15) +
            ggridges::theme_ridges(grid = FALSE) +
            geom_vline(aes(xintercept = 
                               by_gender$mean[by_gender$cm1bsex==input$gender & 
                                                   by_gender$scale==input$var]), 
                       color = "#29211F",
                       size = 0.5,
                       alpha = .8,
                       linetype = 'dotted') 
    })

    
    output$plots2 <- renderPlot({
        
        fit <- lm(as.data.frame(ethrace)[,input$dv] ~ 
                      as.data.frame(ethrace)[,input$iv] + 
                      as.data.frame(ethrace)[,"cm1bsex"] + 
                      as.data.frame(ethrace)[,"ck6ethrace"])
        
        names(fit$model) <- c(input$dv, input$iv, "cm1bsex", "ck6ethrace")

        ggplot(fit$model, aes_string(x = names(fit$model)[2], 
                                     y = names(fit$model)[1])) +
                stat_smooth(method = "lm", color = "orange") + 
                labs(caption = 
                     paste("Adj. R2 = ",signif(summary(fit)$adj.r.squared, 5), 
                           " | Intercept = ",signif(fit$coef[[1]], 5), 
                           " | Slope = ",signif(fit$coef[[2]], 5),
                           " | p = ",signif(summary(fit)$coef[2, 4], 5))) +
                labs(
                    x = "Behaviors",
                    y = "Outcomes",
                    label = "",
                ) +
            facet_wrap(~cm1bsex) +
            theme_minimal()
    })
    
    output$plots3a <- renderPlot({
        ethrace1 <- ethrace %>% 
            filter(ck6ethrace == input$race) %>% 
            group_by(ck6ethrace) %>% 
            nest() %>% 
            mutate(plots = pmap(list(ck6ethrace, data),
                                ~{ggplot(..2) +
                                        geom_point(aes(x = int_scores, 
                                                       y = del_beh_15_self_rep), 
                                                   color = "gray50",  
                                                   stroke = 0, 
                                                   alpha = .6, 
                                                   size = 2) +
                                        geom_smooth(method = lm, 
                                                    se = FALSE, 
                                                    aes(x = int_scores, 
                                                        y = del_beh_15_self_rep, 
                                                        color = cm1bsex)) +
                                        scale_y_continuous(expand = c(0, 
                                                                      0), 
                                                           breaks = c(15, 
                                                                      20, 
                                                                      25)) +
                                        coord_cartesian(ylim = c(10, 
                                                                 30)) +
                                        theme_minimal(15) + 
                                        labs(x = "Internalizing behavior at age 9",
                                             y = "Delinquency at 15") +
                                        scale_color_OkabeIto(name = "Gender") +
                                        theme(panel.grid.minor.y = 
                                                  element_blank(),
                                              panel.grid.minor.x = 
                                                  element_blank(),
                                              axis.text = 
                                                  element_text(size = 10))}))
        
        walk(ethrace1$plots, print)
    })
    
    output$plots3b <- renderPlot({
        ethrace2 <- ethrace %>%
            filter(ck6ethrace == input$race) %>% 
            group_by(ck6ethrace) %>% 
            nest() %>% 
            mutate(plots = map2(ck6ethrace, data,
                                ~ggplot(.y)+
                                    geom_point(aes(x = ext_scores, 
                                                   y = del_beh_15_self_rep), 
                                               color = "gray50", 
                                               stroke = 0, 
                                               alpha = .6, 
                                               size = 2) +
                                    geom_smooth(method = lm, se = FALSE, 
                                                aes(x = ext_scores, 
                                                    y = del_beh_15_self_rep, 
                                                    color = cm1bsex)) +
                                    scale_y_continuous(expand = c(0, 
                                                                  0), 
                                                       breaks = c(15, 
                                                                  20, 
                                                                  25)) +
                                    coord_cartesian(ylim = c(10, 
                                                             30)) +
                                    theme_minimal(15) +
                                    labs(x = "Externalizing behavior at age 9",
                                         y = "Delinquency at 15") +
                                    scale_color_OkabeIto(name = "Gender") +
                                    theme(panel.grid.minor.y = element_blank(),
                                          panel.grid.minor.x = element_blank(),
                                          axis.text=element_text(size = 10))))
        
        walk(ethrace2$plots, print) 
        
    })
}

shinyApp(ui, server)