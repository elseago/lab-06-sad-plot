Lab 06 - Sad plots
================
Elayna Seago
2/22/22

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

### Exercise 1

``` r
staff <- read_csv("data/instructional-staff.csv")
```

    ## Rows: 5 Columns: 12

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): faculty_type
    ## dbl (11): 1975, 1989, 1993, 1995, 1999, 2001, 2003, 2005, 2007, 2009, 2011

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year") %>%
  mutate(value = as.numeric(value))

staff_long
```

    ## # A tibble: 55 × 3
    ##    faculty_type              year  value
    ##    <chr>                     <chr> <dbl>
    ##  1 Full-Time Tenured Faculty 1975   29  
    ##  2 Full-Time Tenured Faculty 1989   27.6
    ##  3 Full-Time Tenured Faculty 1993   25  
    ##  4 Full-Time Tenured Faculty 1995   24.8
    ##  5 Full-Time Tenured Faculty 1999   21.8
    ##  6 Full-Time Tenured Faculty 2001   20.3
    ##  7 Full-Time Tenured Faculty 2003   19.3
    ##  8 Full-Time Tenured Faculty 2005   17.8
    ##  9 Full-Time Tenured Faculty 2007   17.2
    ## 10 Full-Time Tenured Faculty 2009   16.8
    ## # … with 45 more rows

``` r
staff_long %>%
  ggplot(aes(x = year, y = value, color = faculty_type)) +
  geom_line()
```

    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?

![](lab-06_files/figure-gfm/plot-bad-example-1.png)<!-- -->

<img src="lab-06_files/figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

``` r
staff_long %>% 
  ggplot(aes(x = year, y = value , fill = faculty_type)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
```

![](lab-06_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Exercises 2

``` r
fisheries <- read_csv("data/fisheries.csv")
```

    ## Rows: 216 Columns: 4

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): country
    ## dbl (3): capture, aquaculture, total

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fisheries
```

    ## # A tibble: 216 × 4
    ##    country             capture aquaculture  total
    ##    <chr>                 <dbl>       <dbl>  <dbl>
    ##  1 Afghanistan            1000        1200   2200
    ##  2 Albania                7886         950   8836
    ##  3 Algeria               95000        1361  96361
    ##  4 American Samoa         3047          20   3067
    ##  5 Andorra                   0           0      0
    ##  6 Angola               486490         655 487145
    ##  7 Antigua and Barbuda    3000          10   3010
    ##  8 Argentina            755226        3673 758899
    ##  9 Armenia                3758       16381  20139
    ## 10 Aruba                   142           0    142
    ## # … with 206 more rows

``` r
big_fisheries <- fisheries %>% 
  filter(total > 500000)

big_fisheries
```

    ## # A tibble: 41 × 4
    ##    country     capture aquaculture    total
    ##    <chr>         <dbl>       <dbl>    <dbl>
    ##  1 Argentina    755226        3673   758899
    ##  2 Bangladesh  1674770     2203554  3878324
    ##  3 Brazil       705000      581230  1286230
    ##  4 Cambodia     629950      172500   802450
    ##  5 Canada       874727      200765  1075492
    ##  6 Chile       1829238     1050117  2879355
    ##  7 China      17800000    63700000 81500000
    ##  8 Denmark      670344       36337   706681
    ##  9 Ecuador      715495      451090  1166585
    ## 10 Egypt        335614     1370660  1706274
    ## # … with 31 more rows

### ### For my first attempted improvement, I filtered the data to only include countries with more than 500,000 tons of harvest, made a bar plot, and flipped it sideways so all the names could be read.

<img src="lab-06_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

### For my second attempted improvement I also did a log transformation of tons harvested.

<img src="lab-06_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

### I dont really think either of these visualizations are that good. I think to make a better visualization I would need to think about what info I was trying to convey. I think a more interesting visualization would be a graph showing the breakdown of aquaculture vs capture in the 10 countries with the greatest fish harvest.

``` r
big_fisheries %>% 
  arrange(desc(total))
```

    ## # A tibble: 41 × 4
    ##    country        capture aquaculture    total
    ##    <chr>            <dbl>       <dbl>    <dbl>
    ##  1 China         17800000    63700000 81500000
    ##  2 Indonesia      6584419    16600000 23184419
    ##  3 India          5082332     5703002 10785334
    ##  4 Vietnam        2785940     3634531  6420471
    ##  5 United States  4931017      444369  5375386
    ##  6 Russia         4773413      173840  4947253
    ##  7 Japan          3275263     1067994  4343257
    ##  8 Philippines    2027992     2200914  4228906
    ##  9 Peru           3811802      100187  3911989
    ## 10 Bangladesh     1674770     2203554  3878324
    ## # … with 31 more rows

``` r
biggest_fisheries_total <- fisheries %>% 
  filter(total > 3800000)

biggest_fisheries_total
```

    ## # A tibble: 10 × 4
    ##    country        capture aquaculture    total
    ##    <chr>            <dbl>       <dbl>    <dbl>
    ##  1 Bangladesh     1674770     2203554  3878324
    ##  2 China         17800000    63700000 81500000
    ##  3 India          5082332     5703002 10785334
    ##  4 Indonesia      6584419    16600000 23184419
    ##  5 Japan          3275263     1067994  4343257
    ##  6 Peru           3811802      100187  3911989
    ##  7 Philippines    2027992     2200914  4228906
    ##  8 Russia         4773413      173840  4947253
    ##  9 United States  4931017      444369  5375386
    ## 10 Vietnam        2785940     3634531  6420471

``` r
biggest_fisheries_total_long <- biggest_fisheries_total %>%
  pivot_longer(cols = -country, names_to = "harvest") %>%
  filter(harvest %in% c("capture" , "aquaculture")) %>% 
  mutate(value = as.character(value))

biggest_fisheries_total_long
```

    ## # A tibble: 20 × 3
    ##    country       harvest     value   
    ##    <chr>         <chr>       <chr>   
    ##  1 Bangladesh    capture     1674770 
    ##  2 Bangladesh    aquaculture 2203554 
    ##  3 China         capture     17800000
    ##  4 China         aquaculture 63700000
    ##  5 India         capture     5082332 
    ##  6 India         aquaculture 5703002 
    ##  7 Indonesia     capture     6584419 
    ##  8 Indonesia     aquaculture 16600000
    ##  9 Japan         capture     3275263 
    ## 10 Japan         aquaculture 1067994 
    ## 11 Peru          capture     3811802 
    ## 12 Peru          aquaculture 100187  
    ## 13 Philippines   capture     2027992 
    ## 14 Philippines   aquaculture 2200914 
    ## 15 Russia        capture     4773413 
    ## 16 Russia        aquaculture 173840  
    ## 17 United States capture     4931017 
    ## 18 United States aquaculture 444369  
    ## 19 Vietnam       capture     2785940 
    ## 20 Vietnam       aquaculture 3634531

``` r
biggest_fisheries_total_long %>% 
  ggplot(aes(x = reorder(country , harvest) , y = value , fill = harvest)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
```

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

![](lab-06_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
