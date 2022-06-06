#data-cleaning

library(dplyr)
library(tidyr)
library(purrr)

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

# Load the data and calc subscale scores
#ff_sub_orig <- import(here("data","ff_sub.Rda")) 
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


# Remove negative values
ff_sub <- ff_sub_orig %>% mutate_all(~ifelse(.x %in% 
                                                  c(-1:-9), NA, .x)) %>%
    na.omit()

# Calculate age 9 internalizing and externalizing subscale scores
ff_sub2 <- ff_sub %>% 
    mutate(int_scores = (k5g2a + k5g2c + k5g2e + k5g2g + 
                             k5g2i + k5g2j + k5g2k + k5g2l) / 8, 
           ext_scores = (k5g2b + k5g2d + k5g2f + 
                             k5g2h + k5g2m + k5g2n) / 6) 

# Subset to variables of interest
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

# Recode race/eth

ethrace <- ff_sub_lm %>%
    mutate(ck6ethrace = recode(ck6ethrace, 
                               "1" = "White",
                               "2" = "Black",
                               "3" = "Hispanic/Latino",
                               "4" = "Multiracial and Other", 
                               "5" = "Multiracial and Other"),
           cm1bsex = recode(cm1bsex,
                            "1" = "Male",
                            "2"= "Female"))

by_gender <-  ethrace %>%
    select(idnum, 
           ck6ethrace, 
           cm1bsex, 
           p6c22, 
           int_scores, 
           ext_scores, 
           del_beh_15_self_rep) %>%
    pivot_longer(cols = 4:7,
                 names_to = "scale",
                 values_to = "score") %>% 
    group_by(cm1bsex, scale) %>%
    nest() %>%
    mutate(
        mean = map_dbl(data, ~mean(.x$score)) #uses map_dbl
    )

# AW need to figure out a way to extract means based on gender and scale selection to use as vlines on density ridge plots
by_gender[1, 4]
by_gender$mean[[1]]

by_gender2 <-  by_gender %>% 
    select(cm1bsex, scale, mean)

by_gender2 %>% 
    filter(cm1bsex == "Male" & scale == "p6c22") %>% 
    summarize(mean)





