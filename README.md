Homework 2 (STAT 433)
================
Saloni Bhogale
2022-10-09

Here’s a link to my [github
repository](https://github.com/salonibhogale/stat433)

``` r
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(nycflights13)
library(ggplot2)
library(knitr)
library(broom)
```

In this assignment, I evaluate three variables that might affect the the
hour to fly to avoid delays as much as possible: season, weather and
airport. I find that all three variables are important to take into
account (i.e. they are statistically significant). I show each in turn
below:

``` r
# basic data manipulations 
head(flights)
```

    ## # A tibble: 6 × 19
    ##    year month   day dep_time sched_dep…¹ dep_d…² arr_t…³ sched…⁴ arr_d…⁵ carrier
    ##   <int> <int> <int>    <int>       <int>   <dbl>   <int>   <int>   <dbl> <chr>  
    ## 1  2013     1     1      517         515       2     830     819      11 UA     
    ## 2  2013     1     1      533         529       4     850     830      20 UA     
    ## 3  2013     1     1      542         540       2     923     850      33 AA     
    ## 4  2013     1     1      544         545      -1    1004    1022     -18 B6     
    ## 5  2013     1     1      554         600      -6     812     837     -25 DL     
    ## 6  2013     1     1      554         558      -4     740     728      12 UA     
    ## # … with 9 more variables: flight <int>, tailnum <chr>, origin <chr>,
    ## #   dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>,
    ## #   time_hour <dttm>, and abbreviated variable names ¹​sched_dep_time,
    ## #   ²​dep_delay, ³​arr_time, ⁴​sched_arr_time, ⁵​arr_delay

``` r
flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)
```

    ## # A tibble: 20 × 2
    ##     hour arr_delay
    ##    <dbl>     <dbl>
    ##  1     7    -5.30 
    ##  2     5    -4.80 
    ##  3     6    -3.38 
    ##  4     9    -1.45 
    ##  5     8    -1.11 
    ##  6    10     0.954
    ##  7    11     1.48 
    ##  8    12     3.49 
    ##  9    13     6.54 
    ## 10    14     9.20 
    ## 11    23    11.8  
    ## 12    15    12.3  
    ## 13    16    12.6  
    ## 14    18    14.8  
    ## 15    22    16.0  
    ## 16    17    16.0  
    ## 17    19    16.7  
    ## 18    20    16.7  
    ## 19    21    18.4  
    ## 20     1   NaN

``` r
flights %>%
  group_by(hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(dep_delay)
```

    ## # A tibble: 20 × 2
    ##     hour dep_delay
    ##    <dbl>     <dbl>
    ##  1     5     0.688
    ##  2     6     1.64 
    ##  3     7     1.91 
    ##  4     8     4.13 
    ##  5     9     4.58 
    ##  6    10     6.50 
    ##  7    11     7.19 
    ##  8    12     8.61 
    ##  9    13    11.4  
    ## 10    14    13.8  
    ## 11    23    14.0  
    ## 12    15    16.9  
    ## 13    16    18.8  
    ## 14    22    18.8  
    ## 15    17    21.1  
    ## 16    18    21.1  
    ## 17    21    24.2  
    ## 18    20    24.3  
    ## 19    19    24.8  
    ## 20     1   NaN

# What time of the day should you fly if you want to avoid delays?

From the summary above, it appears that flying during the morning hours
is best to avoid delays. A 1 hour increase in the departure time,
increases the delay by 1.66 units. We can check this using regressions
and plots as give below.

``` r
# fly in the morning 

# regression relationship 
plm <- lm(arr_delay ~ hour, data = flights)
summary(plm) %>% tidy() %>% kable()
```

| term        |   estimate | std.error | statistic | p.value |
|:------------|-----------:|----------:|----------:|--------:|
| (Intercept) | -14.926786 | 0.2297823 | -64.96057 |       0 |
| hour        |   1.660615 | 0.0164795 | 100.76832 |       0 |

``` r
# plot the relationship?
p <- ggplot(data = flights,
            mapping = aes(x = hour,
                          y = arr_delay))
p + geom_point() + geom_smooth(method = "lm") 
```

![](hw2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# Season (based on month)

Since seasons for different locations would be different, I use the
month as a proxy for season. Here, I turn `month` into a categorical
variable using `as.character(month)`. The result from the regression
tells us that month (“season”) is not associated with delays in a
consistent pattern. I would have assumed there to be more delays during
the winter months but that does not appear to be the case here.

``` r
# season (based on month)

plm <- lm(arr_delay ~ hour + as.character(month), data=flights)
summary(plm) %>% tidy() %>% kable()
```

| term                  |    estimate | std.error |   statistic |   p.value |
|:----------------------|------------:|----------:|------------:|----------:|
| (Intercept)           | -15.6465216 | 0.3428025 | -45.6429689 | 0.0000000 |
| hour                  |   1.6574285 | 0.0163034 | 101.6614896 | 0.0000000 |
| as.character(month)10 |  -6.2102792 | 0.3710834 | -16.7355362 | 0.0000000 |
| as.character(month)11 |  -5.7120413 | 0.3764799 | -15.1722356 | 0.0000000 |
| as.character(month)12 |   8.7033510 | 0.3763109 |  23.1280851 | 0.0000000 |
| as.character(month)2  |  -0.5313866 | 0.3895044 |  -1.3642633 | 0.1724857 |
| as.character(month)3  |  -0.5266402 | 0.3733654 |  -1.4105225 | 0.1583864 |
| as.character(month)4  |   4.9668846 | 0.3744720 |  13.2637018 | 0.0000000 |
| as.character(month)5  |  -2.6098779 | 0.3726300 |  -7.0039385 | 0.0000000 |
| as.character(month)6  |  10.4305050 | 0.3761226 |  27.7316612 | 0.0000000 |
| as.character(month)7  |  10.6073376 | 0.3721037 |  28.5064001 | 0.0000000 |
| as.character(month)8  |  -0.1083914 | 0.3706549 |  -0.2924322 | 0.7699564 |
| as.character(month)9  |  -9.9861580 | 0.3763485 | -26.5343338 | 0.0000000 |

``` r
p <- ggplot(data = flights,
            mapping = aes(x = hour,
                          y = arr_delay))
p + geom_point() + geom_smooth(method = "lm", mapping=aes(y=predict(plm, flights))) 
```

![](hw2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# Does the starting airport matter?

Here, I regress the origin airport on delays and the results tell us
that the

``` r
# airport

plm <- lm(arr_delay ~ hour + origin, data=flights)
summary(plm) %>% tidy() %>% kable()
```

| term        |   estimate | std.error | statistic | p.value |
|:------------|-----------:|----------:|----------:|--------:|
| (Intercept) | -12.682791 | 0.2488749 | -50.96050 |       0 |
| hour        |   1.688406 | 0.0165244 | 102.17641 |       0 |
| originJFK   |  -4.919515 | 0.1852375 | -26.55788 |       0 |
| originLGA   |  -3.139140 | 0.1884817 | -16.65488 |       0 |

``` r
p <- ggplot(data = flights,
            mapping = aes(x = hour,
                          y = arr_delay))
p + geom_point() + geom_smooth(method = "lm", mapping=aes(y=predict(plm, flights))) 
```

![](hw2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# Does Weather Matter?

In order to answer this, I first merge `weather` and `flights`, and then
report the results. It appears that weather has a very strong
relationship with delays, however the time of the day remains
significant.

``` r
# weather 
head(weather)
```

    ## # A tibble: 6 × 15
    ##   origin  year month   day  hour  temp  dewp humid wind_dir wind_speed wind_gust
    ##   <chr>  <int> <int> <int> <int> <dbl> <dbl> <dbl>    <dbl>      <dbl>     <dbl>
    ## 1 EWR     2013     1     1     1  39.0  26.1  59.4      270      10.4         NA
    ## 2 EWR     2013     1     1     2  39.0  27.0  61.6      250       8.06        NA
    ## 3 EWR     2013     1     1     3  39.0  28.0  64.4      240      11.5         NA
    ## 4 EWR     2013     1     1     4  39.9  28.0  62.2      250      12.7         NA
    ## 5 EWR     2013     1     1     5  39.0  28.0  64.4      260      12.7         NA
    ## 6 EWR     2013     1     1     6  37.9  28.0  67.2      240      11.5         NA
    ## # … with 4 more variables: precip <dbl>, pressure <dbl>, visib <dbl>,
    ## #   time_hour <dttm>

``` r
head(flights)
```

    ## # A tibble: 6 × 19
    ##    year month   day dep_time sched_dep…¹ dep_d…² arr_t…³ sched…⁴ arr_d…⁵ carrier
    ##   <int> <int> <int>    <int>       <int>   <dbl>   <int>   <int>   <dbl> <chr>  
    ## 1  2013     1     1      517         515       2     830     819      11 UA     
    ## 2  2013     1     1      533         529       4     850     830      20 UA     
    ## 3  2013     1     1      542         540       2     923     850      33 AA     
    ## 4  2013     1     1      544         545      -1    1004    1022     -18 B6     
    ## 5  2013     1     1      554         600      -6     812     837     -25 DL     
    ## 6  2013     1     1      554         558      -4     740     728      12 UA     
    ## # … with 9 more variables: flight <int>, tailnum <chr>, origin <chr>,
    ## #   dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>,
    ## #   time_hour <dttm>, and abbreviated variable names ¹​sched_dep_time,
    ## #   ²​dep_delay, ³​arr_time, ⁴​sched_arr_time, ⁵​arr_delay

``` r
flights<-merge(flights, weather, by=c("origin","year","month","day","hour"),all.x=TRUE)
plm <- lm(arr_delay ~ hour + wind_speed + pressure + precip, data=flights)
summary(plm) %>% tidy() %>% kable()
```

| term        |    estimate |  std.error |  statistic |   p.value |
|:------------|------------:|-----------:|-----------:|----------:|
| (Intercept) | 633.2442699 | 10.7836979 |  58.722368 | 0.0000000 |
| hour        |   1.3888556 |  0.0164769 |  84.291210 | 0.0000000 |
| wind_speed  |   0.0255612 |  0.0142868 |   1.789152 | 0.0735914 |
| pressure    |  -0.6359815 |  0.0105477 | -60.295762 | 0.0000000 |
| precip      | 188.5382327 |  6.0632584 |  31.095200 | 0.0000000 |

``` r
p + geom_point() + geom_smooth(method = "lm", mapping=aes(y=predict(plm, flights))) 
```

![](hw2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Putting it all together

It turns out that the hour of the day, origin airport, season as well as
weather are all strongly associated with the amount of delay for each
flight.

``` r
# all of the above 
plm <- lm(arr_delay ~ hour + origin + as.character(month) + wind_speed + pressure + precip, data=flights)
summary(plm) %>% tidy() %>% kable()
```

| term                  |    estimate |  std.error |  statistic | p.value |
|:----------------------|------------:|-----------:|-----------:|--------:|
| (Intercept)           | 669.0520528 | 11.5864142 |  57.744531 |   0e+00 |
| hour                  |   1.4024154 |  0.0163498 |  85.775526 |   0e+00 |
| originJFK             |  -4.7805014 |  0.1840240 | -25.977598 |   0e+00 |
| originLGA             |  -3.0883321 |  0.1856929 | -16.631399 |   0e+00 |
| as.character(month)10 |  -7.3430715 |  0.3683365 | -19.935768 |   0e+00 |
| as.character(month)11 |  -4.5287190 |  0.3720564 | -12.172129 |   0e+00 |
| as.character(month)12 |   4.5557009 |  0.3833721 |  11.883236 |   0e+00 |
| as.character(month)2  |  -4.5548807 |  0.3901157 | -11.675719 |   0e+00 |
| as.character(month)3  |  -7.4476522 |  0.3788856 | -19.656731 |   0e+00 |
| as.character(month)4  |   4.6572850 |  0.3668456 |  12.695492 |   0e+00 |
| as.character(month)5  |  -5.6051269 |  0.3746703 | -14.960160 |   0e+00 |
| as.character(month)6  |   3.6486756 |  0.3841362 |   9.498391 |   0e+00 |
| as.character(month)7  |   7.1011486 |  0.3741764 |  18.978075 |   0e+00 |
| as.character(month)8  |  -3.4565966 |  0.3713720 |  -9.307640 |   0e+00 |
| as.character(month)9  | -12.0176123 |  0.3722121 | -32.287003 |   0e+00 |
| wind_speed            |   0.0773638 |  0.0147571 |   5.242472 |   2e-07 |
| pressure              |  -0.6672234 |  0.0112931 | -59.082249 |   0e+00 |
| precip                | 171.1553100 |  6.0216641 |  28.423258 |   0e+00 |

``` r
p + geom_point() + geom_smooth(method = "lm", mapping=aes(y=predict(plm, flights))) 
```

![](hw2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
