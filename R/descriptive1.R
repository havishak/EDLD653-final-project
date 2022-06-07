#bar-plots1
library(dplyr)
library(tidyr)
library(ggplot2)

#clean labels
ff_meta_subset <- ff_meta_subset %>% 
    mutate(
        clean_label = gsub("(\\w*\\.\\s)(.*)","\\2", varlab),
        clean_scale = ifelse(scale != "",
                             paste(scale, wave, sep = ": "),
                             "")
    )

race_response <- attr(attributes(ff_sub_orig$ck6ethrace)$labels,"names")[1:9]
race_response <- gsub("(\\d\\s)(\\w*)(.*)","\\2", race_response)


#create two plots for race; and race and gender

total_response_race <- function(var_choice) {
    
    ff_sub_orig %>%
        select(var_choice, "ck6ethrace", "cm1bsex") %>%
        filter(if_any(ends_with(var_choice), ~. > 0)) %>%
        sjlabelled::as_label() %>%
        pivot_longer(
            cols = 1,
            names_to = "var",
            values_to = "val"
        ) %>%
        mutate(
            sex = stringr::str_to_title(gsub("\\d\\s","", cm1bsex)),
            race = gsub("(\\d\\s)(\\w*)(.*)","\\2", ck6ethrace),
            race = ifelse(race %in% race_response,
                          "Missing",
                          ifelse(race == "Multi","Other",
                                 race)),
            sex = ifelse(sex == "Boy", "Male", "Female")
        ) %>%
        group_by(race) %>%
        mutate(
            n_race = n()
        ) %>% 
        ungroup
}
    
total_response_subgroup <- function(df, display_choice) {
    if(display_choice == "By race"){
      
    temp <- df %>%
        group_by(race, val, n_race) %>%
        summarize(
            n_category = n()
        ) 
        
    }
    
    if(display_choice == "By race and gender"){
    
    temp <- df %>%
        group_by(race, sex, val, n_race) %>%
        summarize(
            n_category = n()
        ) 
        
    }    
        
    temp %>% 
        mutate(
                prop_category = n_category/n_race
            ) %>%
        ungroup() 
    
}

descriptive_plot <- function(df, var_choice, display_choice) {
    temp <- df %>% 
        mutate(
            val_n = stringr::str_to_title(gsub("(^\\d\\s)(.*)","\\2", val)),
        ) %>%
        ggplot() +
        geom_col(aes(x = forcats::fct_inorder(val_n),
                     y = prop_category,
                     fill = race),
                 position = "dodge",
                 alpha = 0.7) + #AW: Could consider adding some transparency (e.g., alpha = .8)
        geom_text(aes(x = forcats::fct_inorder(val_n),
                      y = prop_category,
                      group = race,
                      label = round(prop_category * 100, digits = 0)),
                  position = position_dodge(width = .9),
                  vjust = 0)+
        colorblindr::scale_fill_OkabeIto(name = "") +
        theme_minimal(11) +
        labs(
            x = "",
            y = "",
            title = attributes(ff_sub_orig[[var_choice]])$scale,
            label = "",
            subtitle = gsub("(\\w*\\.\\s)(.*)",
                            "\\2",
                            attributes(ff_sub_orig[[var_choice]])$label),
            caption = paste0("Data Collected in ",
                             attributes(ff_sub_orig[[var_choice]])$wave)
        ) +
        theme(
            legend.position = "bottom", #AW: What would you think about bottom-center for the legend?
            legend.justification = "center",
            axis.text.y = element_blank(),
            panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45)
        )
    
    if(display_choice == "By race and gender"){
        temp <- temp +
            facet_wrap(~sex) #AW: Suggest considering recoding "Boy" / "Girl" to "Male" and "Female"
    }
     temp
    
}
