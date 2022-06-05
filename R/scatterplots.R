library(purrr)
library(tidyr)
library(ggplot2)
library(colorblindr)

ethrace1 <- ethrace %>% 
  select( int_scores, ext_scores, cm1bsex, ck6ethrace, del_beh_9, del_beh_15)

ethrace1$cm1bsex <- factor(ethrace1$cm1bsex)
ethrace1$ck6ethrace <- as.factor(ethrace1$ck6ethrace)

#Function for a scatterplot with a fitted regression line (Internalizing behavior * delinquency)
scatter1 <- function(df, group) {
  p = df %>% 
    ggplot() +
    geom_point(aes(x = int_scores, y = del_beh_15), color = "gray50", 
               stroke = 0, alpha = .6) +
    geom_smooth(method = lm, se = FALSE, 
                aes(x = int_scores, y = del_beh_15, color = cm1bsex)) +
    scale_y_continuous(expand = c(0,0), 
                       breaks = c(35, 40, 45, 50)) +
    coord_cartesian(ylim = c(35, 55 )) +
    theme_minimal(15) +
    scale_color_OkabeIto(name = "Gender") +
    theme(plot.title.position = "plot",
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          title =element_text(size=8))
  
  p + labs (title = paste("Delinquent Behavior at 15 predicted by Internalizing scores at 9", group, sep = ": "))
}

#Function for a scatterplot with a fitted regression line (Externalizing behavior * delinquency)
scatter2 <- function(df, group) {
  p = df %>% 
    ggplot() +
    geom_point(aes(x = ext_scores, y = del_beh_15), color = "gray50", 
               stroke = 0, alpha = .6) +
    geom_smooth(method = lm, se = FALSE, 
                aes(x = int_scores, y = del_beh_15, color = cm1bsex)) +
    scale_y_continuous(expand = c(0,0), 
                       breaks = c(35, 40, 45, 50)) +
    coord_cartesian(ylim = c(35, 55 )) +
    theme_minimal(15) +
    scale_color_OkabeIto(name = "Gender") +
    theme(plot.title.position = "plot",
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          title =element_text(size=8))
  
  p + labs (title = paste("Delinquent Behavior at 15 predicted by Externalizing scores at 9", group, sep = ": "))
}

plotsInt <- map2(nest_df$data, nest_df$ck6ethrace,
               ~scatter1(.x, .y))

plotsEx <- map2(nest_df$data, nest_df$ck6ethrace,
               ~scatter2(.x, .y))


