Functions (Rebecca):
 
Calculate means for coded groups
Runs all linear models
Internalizing year 9 x # suspensions/expulsions in year 15
Externalizing behaviors year 9 x # suspensions/expulsions in year 15



#Calculates subscores for internalizing and externalizing behaviors
ff_sub <- ff_sub %>% 
mutate(int_scores = (k5g2a + k5g2c + k5g2e + k5g2g + k5g2i + k5g2j + k5g2k + k5g2l)/8, ext_scores = (k5g2b + k5g2d + k5g2f + k5g2h + k5g2m + k5g2n)/6) %>%
filter(int_scores >=0, ext_scores >= 0)
)


ff_sub_lm <- ff_sub %>% 
    rowwise() %>% 
    select(idnum, starts_with("k6d6"), starts_with("k5f1"), int_scores, ext_scores, cm1bsex, ck6ethrace, p5l12g, p6c21) %>% 
    filter(rowSums(across(where(is.numeric)))>=0 & ck6ethrace >=0 & p5l12g >=0 & p6c21 >=0) %>% 
    mutate(del_beh_9 = sum(c_across(starts_with("k5f1"))),
    del_beh_15 = sum(c_across(starts_with("k6d6"))))



means_df <- function(df, ...) {
    means <- map(df, mean, ...) # calculate means
    nulls <- map_lgl(means, is.null) # find null values
    means_l <- means[!nulls] # subset list to remove nulls
    as.data.frame(means_l) # return a df
}

means_df(ff_sub)

ff_sub$idnum <- as.numeric(ff_sub$idnum)
ff_sub$ck6ethrace <- as.numeric(ff_sub$ck6ethrace)


summary(lm(del_beh_15 ~ ext_scores + cm1bsex + ck6ethrace, data = ff_sub))

summary(lm(del_beh_15 ~ int_scores + cm1bsex + ck6ethrace , data = ff_sub))

mod_db_int <- ff_sub %>%
    group_by(idnum) %>%
    nest() %>%
    mutate(
        model = map(
            data, ~lm(del_beh_15 ~ int_scores + cm1bsex + ck6ethrace, data = .x)
        )
    )

mod_db_ext <- ff_sub %>%
    group_by(idnum) %>%
    nest() %>%
    mutate(
        model = map(
            data, ~lm(del_beh_15 ~ ext_scores + cm1bsex + ck6ethrace, data = .x)
        )
    )


pull_coef <- function(model, coef_name) {
    coef(model)[coef_name]
}

mod_db_ext %>%
    mutate(intercept = map_dfr(model, pull_coef))

mod_db_int %>%
    mutate(intercept = map_dfr(model, pull_coef))


mods <- function(data, x, y, points = FALSE, ...) {
    p <- ggplot(data, aes({{x}}, {{y}})) 
    if (points) {
        p <- p + geom_point(color = "gray80")
    }   
    p + 
        geom_smooth(method = "lm",
                    color = "magenta", 
                    ...) +
        geom_smooth(...)
}

mods(ff_sub, int_scores, del_beh_15) +
    labs(title = "Checking linearity",
         subtitle = "Linear vs LOESS fits",
         x = "Engine Displacement",
         y = "Miles Per gallon")

mods(ff_sub, ext_scores, del_beh_15) +
    labs(title = "Checking linearity",
         subtitle = "Linear vs LOESS fits",
         x = "Engine Displacement",
         y = "Miles Per gallon")
