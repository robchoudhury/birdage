Bird Age
--------

I stumbled across a dataset of bird ages on a [USGS website](https://www.pwrc.usgs.gov/bbl/longevity/longevity_main.cfm), and wanted to know if there was a relationship between bird age and other variables (weight, wingspan, etc).

``` r
birdage=read_csv("data_bird_age/bird age.csv") %>% #this data is from https://www.pwrc.usgs.gov/bbl/longevity/longevity_main.cfm
 separate(., "Minimum age at encounter", into=c("year","year2","month","month2"), sep=" ") %>%
  filter(complete.cases(.)) %>% #I just chucked out one row because the bird age was less than a year, I'll figure out how to keep it later. Had to do this versus using the dates because the dates are screwed up (some say '99' for the day of the month)
  mutate(age=as.numeric(year)+(as.numeric(month)/12))  #so that we can factor in partial years
```

    ## Parsed with column specification:
    ## cols(
    ##   COMMONNAME = col_character(),
    ##   `Minimum age at encounter` = col_character(),
    ##   `Banding date` = col_character(),
    ##   `Banding region` = col_character(),
    ##   `Age at banding` = col_character(),
    ##   Sex = col_character(),
    ##   `Encounter date` = col_character(),
    ##   `Encounter region` = col_character(),
    ##   `How bird and band were obtained` = col_character(),
    ##   `Condition of bird/band at encounter` = col_character()
    ## )

    ## Warning: Expected 4 pieces. Missing pieces filled with `NA` in 1 rows
    ## [1306].

``` r
gapnames=read_csv("data_bird_age/gapnames.csv") #I manually found some on Wiki quickly, not sure theyre perfect to species
```

    ## Parsed with column specification:
    ## cols(
    ##   COMMONNAME = col_character(),
    ##   SCINAME = col_character(),
    ##   Order = col_character(),
    ##   Family = col_character()
    ## )

``` r
gapnames=gapnames[!duplicated(gapnames$COMMONNAME),] #maybe if I get rid of duplicates?

birdnames=read_csv("data_bird_age/birdnames.csv") %>%   #this data is from http://fielddata.blogspot.com/p/field-notes-data-tips-download-page.html
  full_join(gapnames)
```

    ## Parsed with column specification:
    ## cols(
    ##   SPEC = col_character(),
    ##   COMMONNAME = col_character(),
    ##   SCINAME = col_character(),
    ##   SPEC6 = col_character(),
    ##   Order = col_character(),
    ##   Family = col_character(),
    ##   Subfamily = col_character()
    ## )

    ## Joining, by = c("COMMONNAME", "SCINAME", "Order", "Family")

``` r
birdnameage=merge(birdage, birdnames, all=T) %>% #merge the two datasets together
filter(., !is.na(select(., age)), !is.na(select(., Order)), !is.na(select(., Family))) 
```

Link Between Bird Age and Phylogenetic Order?
---------------------------------------------

I wanted to know if there was any signal for how old birds get if theyre closely related. Ultimately, I would love to know how age is affected by size (weight, wingspan, whatever) but I cant find open datasets for that.

``` r
ggplot(birdnameage, aes(x=age, y=reorder(Order, age, FUN=median), fill=..x..))+
  geom_density_ridges_gradient() +
  scale_fill_viridis(name = "Age (Years)", option = "C") + 
  guides(fill = guide_colorbar(barwidth = 2, barheight = 10))+
  #geom_histogram()+
  #geom_vline(xintercept = median(birdnameage$age), color = "red", linetype = "dashed")+
  theme_ridges()+
  ylab("Number of Birds Captured")+
  xlab("Age of Captured Bird")
```

    ## Picking joint bandwidth of 2.39

![](birdage_files/figure-markdown_github/histogram-1.png)

``` r
ggsave(filename = "img_bird_age/joy_plot.png",width = 10, height = 7, units = "in", dpi = 300, type="cairo-png")
```

    ## Picking joint bandwidth of 2.39

![](img_bird_age/joy_plot.png)

Looks like some bird orders have individuals that live longer than others, but it's difficult to say for some whether thats just a sampling issue.

Mean Variance Relationship
--------------------------

The relationship between the mean and the variance of a dataset can tell you ... stuff (see <https://en.wikipedia.org/wiki/Variance_function>). Honestly I was just curious if there was a relationship, I need to revisit why this bit is important.

``` r
bird_summary<- birdnameage %>% 
  group_by(Family, Order) %>%
  summarise(mean=mean(age), var=var(age), n=length(age)) %>%
  filter(., n>1) #drop singletons

ggplot(bird_summary, aes(log(mean), log(var)))+
  geom_point(aes(color=Order, size=n))+
  geom_smooth(method = "lm")+
  theme(legend.position = "bottom")
```

![](birdage_files/figure-markdown_github/mean%20variance-1.png)

``` r
summary(lm(log(mean)~log(var), data=bird_summary))
```

    ## 
    ## Call:
    ## lm(formula = log(mean) ~ log(var), data = bird_summary)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.54849 -0.25347 -0.08761  0.23297  1.03582 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.07713    0.07046  29.481  < 2e-16 ***
    ## log(var)     0.14848    0.02416   6.145 6.66e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3402 on 61 degrees of freedom
    ## Multiple R-squared:  0.3823, Adjusted R-squared:  0.3722 
    ## F-statistic: 37.76 on 1 and 61 DF,  p-value: 6.663e-08

``` r
ggsave(filename = "img_bird_age/mean_variance.png",width = 10, height = 7, units = "in", dpi = 300, type="cairo-png")
```

![](img_bird_age/mean_variance.png)

It looks like there is a pretty strong relationship between log mean and log variance, with an $R^2 of 0.3722, a slope of 0.14848.

Next Up?
--------

Ideally, it would be awesome to find a dataset that tied bird name with bird weight or wingspan or other variables. That way you could see if bird weight/wingspan correlated with age or phylogeny. It would also be cool to see a tree of bird orders to see if more closely related things had similar ages.

Session Info
------------

``` r
sessionInfo()
```

    ## R version 3.5.1 (2018-07-02)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 16299)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] bindrcpp_0.2.2    ggridges_0.5.0    viridis_0.5.1    
    ##  [4] viridisLite_0.3.0 forcats_0.3.0     stringr_1.3.1    
    ##  [7] dplyr_0.7.6       purrr_0.2.5       readr_1.1.1      
    ## [10] tidyr_0.8.1       tibble_1.4.2      ggplot2_3.0.0    
    ## [13] tidyverse_1.2.1  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_0.2.4 haven_1.1.2      lattice_0.20-35  colorspace_1.3-2
    ##  [5] htmltools_0.3.6  yaml_2.2.0       rlang_0.2.2      pillar_1.3.0    
    ##  [9] glue_1.3.0       withr_2.1.2      modelr_0.1.2     readxl_1.1.0    
    ## [13] bindr_0.1.1      plyr_1.8.4       munsell_0.5.0    gtable_0.2.0    
    ## [17] cellranger_1.1.0 rvest_0.3.2      evaluate_0.11    labeling_0.3    
    ## [21] knitr_1.20       broom_0.5.0      Rcpp_0.12.18     scales_1.0.0    
    ## [25] backports_1.1.2  jsonlite_1.5     gridExtra_2.3    hms_0.4.2       
    ## [29] digest_0.6.15    stringi_1.2.4    grid_3.5.1       rprojroot_1.3-2 
    ## [33] cli_1.0.0        tools_3.5.1      magrittr_1.5     lazyeval_0.2.1  
    ## [37] crayon_1.3.4     pkgconfig_2.0.2  xml2_1.2.0       lubridate_1.7.4 
    ## [41] assertthat_0.2.0 rmarkdown_1.10   httr_1.3.1       rstudioapi_0.7  
    ## [45] R6_2.2.2         nlme_3.1-137     compiler_3.5.1
