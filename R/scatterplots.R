library(purrr)
library(colorblindr)

ethrace <- ethrace %>% 
    select( int_scores, ext_scores, cm1bsex, ck6ethrace, del_beh_9, del_beh_15)

ethrace$cm1bsex <- factor(ethrace$cm1bsex)
ethrace$ck6ethrace <- as.factor(ethrace$ck6ethrace)

scatter1 <- function( DV, IV, group) {
    var1 <- deparse(substitute(DV))
    var2 <- deparse(substitute(IV))
    
    p = ethrace %>% 
        ggplot() +
        geom_point(aes(x = IV, y = DV), color = "gray50", stroke = 0, alpha = .6) +
        geom_smooth(method = lm, se = FALSE, 
                    aes(x = IV, y = DV, color = group)) +
        scale_y_continuous(expand = c(0,0), 
                           breaks = c(35, 40, 45, 50)) +
        coord_cartesian(ylim = c(35, 55 )) +
        theme_minimal(15) +
        labs(x = print(var2),
             y = print(var1)) +
        theme(plot.title.position = "plot",
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())
    
    ifelse(var2 == "ff_sub_lm1$int_scores",
           p <- p + labs(title = "Delinquent Behavior at 15 predicted by Internalizing scores at 9"),
           ifelse (var2 == "ff_sub_lm1$ext_scores",
                   p <- p + labs(title = "Delinquent Behavior at 15 predicted by Externalizing scores at 9"),
                   p <- p))
    
    p
}

##scatter1(ethrace$int_scores, ethrace$ext_scores, ethrace$cm1bsex)

#ethrace %>% 
    #group_by(ck6ethrace) %>% 
    #nest() %>% 
   # filter(ck6ethrace == "White") %>% 
   # mutate(plots = pmap(list(int_scores, ext_scores, cm1bsex),
         #~scatter1(..1, ..2, ..3)))


