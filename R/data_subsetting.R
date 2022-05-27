#data-cleaning

library(dplyr)

ff_meta <- rio::import(here::here("data","FFMetadata_v09.csv")) %>%
    janitor::clean_names() %>%
    as_tibble()

#subsetting-meta
scales <- c("Aggravation in Parents", 
            "Self-Description Questionnaire (SDQ)",
            "Delinquent Behavior") 

#further narrow the meta-data file to variables of interest
ff_meta_subset <- ff_meta %>% 
    #demographic characteristics - race/ethnicity
    filter((topics == "Demographics" &
            stringr::str_detect(varlab,
            "self-description of race/ethnicity")) |
            #gender
            (subtopics == "sex/gender" &
            stringr::str_detect(varlab, "Focal baby's gender")) |
            #scales
            scale %in% scales|
            #outcomes from questionnaire
            (source == "questionnaire" & 
            subtopics == "student experiences" &
            stringr::str_detect(varlab,"suspen"))) %>% 
    #only include relevant columns
    select(new_name, 
           varlab, 
           type, 
           scale, 
           respondent, 
           wave)

#ff_vars <- ff_meta_subset$new_name

ff_sub_orig <- rio::import(here::here("data","ff_sub.Rda")) 

#assigning attributes to variables

var <-colnames(ff_sub_orig)[-1]
atr <- c("wave", "respondent", "scale")    

#parallel iteration
#Tried with a for loop
for(i in seq_along(var)){
    for(j in seq_along(atr))
    {
        attr(ff_sub_orig[[i+1]], atr[j]) <- as.character(
            ff_meta_subset %>% 
                filter(new_name == var[i]) %>% 
                select(atr[j]))
        
    }
}
