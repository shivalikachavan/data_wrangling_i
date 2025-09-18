data_manipulation
================
Shivalika Chavan
2025-09-18

``` r
library(tidyverse)
litters_df = 
  read_csv(
    "./data/FAS_litters.csv", 
    na = c("NA", ".", "")
    )
litters_df = 
  janitor::clean_names(litters_df)
pups_df = 
  read_csv(
    "./data/FAS_pups.csv", 
    na = c("NA", ".", ""), 
    skip = 3
    )
pups_df = 
  janitor::clean_names(pups_df)
```

## Select

``` r
select(litters_df, group, litter_number, gd0_weight, pups_born_alive)
## # A tibble: 49 × 4
##    group litter_number   gd0_weight pups_born_alive
##    <chr> <chr>                <dbl>           <dbl>
##  1 Con7  #85                   19.7               3
##  2 Con7  #1/2/95/2             27                 8
##  3 Con7  #5/5/3/83/3-3         26                 6
##  4 Con7  #5/4/2/95/2           28.5               5
##  5 Con7  #4/2/95/3-3           NA                 6
##  6 Con7  #2/2/95/3-2           NA                 6
##  7 Con7  #1/5/3/83/3-3/2       NA                 9
##  8 Con8  #3/83/3-3             NA                 9
##  9 Con8  #2/95/3               NA                 8
## 10 Con8  #3/5/2/2/95           28.5               8
## # ℹ 39 more rows
select(litters_df, group:gd_of_birth)
## # A tibble: 49 × 5
##    group litter_number   gd0_weight gd18_weight gd_of_birth
##    <chr> <chr>                <dbl>       <dbl>       <dbl>
##  1 Con7  #85                   19.7        34.7          20
##  2 Con7  #1/2/95/2             27          42            19
##  3 Con7  #5/5/3/83/3-3         26          41.4          19
##  4 Con7  #5/4/2/95/2           28.5        44.1          19
##  5 Con7  #4/2/95/3-3           NA          NA            20
##  6 Con7  #2/2/95/3-2           NA          NA            20
##  7 Con7  #1/5/3/83/3-3/2       NA          NA            20
##  8 Con8  #3/83/3-3             NA          NA            20
##  9 Con8  #2/95/3               NA          NA            20
## 10 Con8  #3/5/2/2/95           28.5        NA            20
## # ℹ 39 more rows
select(litters_df, -pups_survive)
## # A tibble: 49 × 7
##    group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
##    <chr> <chr>                <dbl>       <dbl>       <dbl>           <dbl>
##  1 Con7  #85                   19.7        34.7          20               3
##  2 Con7  #1/2/95/2             27          42            19               8
##  3 Con7  #5/5/3/83/3-3         26          41.4          19               6
##  4 Con7  #5/4/2/95/2           28.5        44.1          19               5
##  5 Con7  #4/2/95/3-3           NA          NA            20               6
##  6 Con7  #2/2/95/3-2           NA          NA            20               6
##  7 Con7  #1/5/3/83/3-3/2       NA          NA            20               9
##  8 Con8  #3/83/3-3             NA          NA            20               9
##  9 Con8  #2/95/3               NA          NA            20               8
## 10 Con8  #3/5/2/2/95           28.5        NA            20               8
## # ℹ 39 more rows
## # ℹ 1 more variable: pups_dead_birth <dbl>
select(litters_df, GROUP = group, LiTtEr_NuMbEr = litter_number)
## # A tibble: 49 × 2
##    GROUP LiTtEr_NuMbEr  
##    <chr> <chr>          
##  1 Con7  #85            
##  2 Con7  #1/2/95/2      
##  3 Con7  #5/5/3/83/3-3  
##  4 Con7  #5/4/2/95/2    
##  5 Con7  #4/2/95/3-3    
##  6 Con7  #2/2/95/3-2    
##  7 Con7  #1/5/3/83/3-3/2
##  8 Con8  #3/83/3-3      
##  9 Con8  #2/95/3        
## 10 Con8  #3/5/2/2/95    
## # ℹ 39 more rows
select(litters_df, starts_with("gd"))
## # A tibble: 49 × 3
##    gd0_weight gd18_weight gd_of_birth
##         <dbl>       <dbl>       <dbl>
##  1       19.7        34.7          20
##  2       27          42            19
##  3       26          41.4          19
##  4       28.5        44.1          19
##  5       NA          NA            20
##  6       NA          NA            20
##  7       NA          NA            20
##  8       NA          NA            20
##  9       NA          NA            20
## 10       28.5        NA            20
## # ℹ 39 more rows
select(litters_df, ends_with("birth"))
## # A tibble: 49 × 2
##    gd_of_birth pups_dead_birth
##          <dbl>           <dbl>
##  1          20               4
##  2          19               0
##  3          19               0
##  4          19               1
##  5          20               0
##  6          20               0
##  7          20               0
##  8          20               1
##  9          20               0
## 10          20               0
## # ℹ 39 more rows
select(litters_df, contains("pups"))
## # A tibble: 49 × 3
##    pups_born_alive pups_dead_birth pups_survive
##              <dbl>           <dbl>        <dbl>
##  1               3               4            3
##  2               8               0            7
##  3               6               0            5
##  4               5               1            4
##  5               6               0            6
##  6               6               0            4
##  7               9               0            9
##  8               9               1            8
##  9               8               0            8
## 10               8               0            8
## # ℹ 39 more rows
select(litters_df, litter_number, pups_survive, everything()) #moves litter_number and pups to front of selection
## # A tibble: 49 × 8
##    litter_number   pups_survive group gd0_weight gd18_weight gd_of_birth
##    <chr>                  <dbl> <chr>      <dbl>       <dbl>       <dbl>
##  1 #85                        3 Con7        19.7        34.7          20
##  2 #1/2/95/2                  7 Con7        27          42            19
##  3 #5/5/3/83/3-3              5 Con7        26          41.4          19
##  4 #5/4/2/95/2                4 Con7        28.5        44.1          19
##  5 #4/2/95/3-3                6 Con7        NA          NA            20
##  6 #2/2/95/3-2                4 Con7        NA          NA            20
##  7 #1/5/3/83/3-3/2            9 Con7        NA          NA            20
##  8 #3/83/3-3                  8 Con8        NA          NA            20
##  9 #2/95/3                    8 Con8        NA          NA            20
## 10 #3/5/2/2/95                8 Con8        28.5        NA            20
## # ℹ 39 more rows
## # ℹ 2 more variables: pups_born_alive <dbl>, pups_dead_birth <dbl>
relocate(litters_df, litter_number, pups_survive) #similar to everything()
## # A tibble: 49 × 8
##    litter_number   pups_survive group gd0_weight gd18_weight gd_of_birth
##    <chr>                  <dbl> <chr>      <dbl>       <dbl>       <dbl>
##  1 #85                        3 Con7        19.7        34.7          20
##  2 #1/2/95/2                  7 Con7        27          42            19
##  3 #5/5/3/83/3-3              5 Con7        26          41.4          19
##  4 #5/4/2/95/2                4 Con7        28.5        44.1          19
##  5 #4/2/95/3-3                6 Con7        NA          NA            20
##  6 #2/2/95/3-2                4 Con7        NA          NA            20
##  7 #1/5/3/83/3-3/2            9 Con7        NA          NA            20
##  8 #3/83/3-3                  8 Con8        NA          NA            20
##  9 #2/95/3                    8 Con8        NA          NA            20
## 10 #3/5/2/2/95                8 Con8        28.5        NA            20
## # ℹ 39 more rows
## # ℹ 2 more variables: pups_born_alive <dbl>, pups_dead_birth <dbl>

# Learning Assessment
select(pups_df, litter_number, sex, pd_ears)
## # A tibble: 313 × 3
##    litter_number   sex pd_ears
##    <chr>         <dbl>   <dbl>
##  1 #85               1       4
##  2 #85               1       4
##  3 #1/2/95/2         1       5
##  4 #1/2/95/2         1       5
##  5 #5/5/3/83/3-3     1       5
##  6 #5/5/3/83/3-3     1       5
##  7 #5/4/2/95/2       1      NA
##  8 #4/2/95/3-3       1       4
##  9 #4/2/95/3-3       1       4
## 10 #2/2/95/3-2       1       4
## # ℹ 303 more rows
```

## Filter

``` r
filter(litters_df, gd_of_birth == 20)
## # A tibble: 32 × 8
##    group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
##    <chr> <chr>                <dbl>       <dbl>       <dbl>           <dbl>
##  1 Con7  #85                   19.7        34.7          20               3
##  2 Con7  #4/2/95/3-3           NA          NA            20               6
##  3 Con7  #2/2/95/3-2           NA          NA            20               6
##  4 Con7  #1/5/3/83/3-3/2       NA          NA            20               9
##  5 Con8  #3/83/3-3             NA          NA            20               9
##  6 Con8  #2/95/3               NA          NA            20               8
##  7 Con8  #3/5/2/2/95           28.5        NA            20               8
##  8 Con8  #1/6/2/2/95-2         NA          NA            20               7
##  9 Con8  #3/5/3/83/3-3-2       NA          NA            20               8
## 10 Con8  #3/6/2/2/95-3         NA          NA            20               7
## # ℹ 22 more rows
## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>
filter(litters_df, pups_born_alive >= 2) # all litters have 3+ pups born alive
## # A tibble: 49 × 8
##    group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
##    <chr> <chr>                <dbl>       <dbl>       <dbl>           <dbl>
##  1 Con7  #85                   19.7        34.7          20               3
##  2 Con7  #1/2/95/2             27          42            19               8
##  3 Con7  #5/5/3/83/3-3         26          41.4          19               6
##  4 Con7  #5/4/2/95/2           28.5        44.1          19               5
##  5 Con7  #4/2/95/3-3           NA          NA            20               6
##  6 Con7  #2/2/95/3-2           NA          NA            20               6
##  7 Con7  #1/5/3/83/3-3/2       NA          NA            20               9
##  8 Con8  #3/83/3-3             NA          NA            20               9
##  9 Con8  #2/95/3               NA          NA            20               8
## 10 Con8  #3/5/2/2/95           28.5        NA            20               8
## # ℹ 39 more rows
## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>
filter(litters_df, pups_survive != 4)
## # A tibble: 44 × 8
##    group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
##    <chr> <chr>                <dbl>       <dbl>       <dbl>           <dbl>
##  1 Con7  #85                   19.7        34.7          20               3
##  2 Con7  #1/2/95/2             27          42            19               8
##  3 Con7  #5/5/3/83/3-3         26          41.4          19               6
##  4 Con7  #4/2/95/3-3           NA          NA            20               6
##  5 Con7  #1/5/3/83/3-3/2       NA          NA            20               9
##  6 Con8  #3/83/3-3             NA          NA            20               9
##  7 Con8  #2/95/3               NA          NA            20               8
##  8 Con8  #3/5/2/2/95           28.5        NA            20               8
##  9 Con8  #5/4/3/83/3           28          NA            19               9
## 10 Con8  #1/6/2/2/95-2         NA          NA            20               7
## # ℹ 34 more rows
## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>
filter(litters_df, !(pups_survive == 4)) # same as line above
## # A tibble: 44 × 8
##    group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
##    <chr> <chr>                <dbl>       <dbl>       <dbl>           <dbl>
##  1 Con7  #85                   19.7        34.7          20               3
##  2 Con7  #1/2/95/2             27          42            19               8
##  3 Con7  #5/5/3/83/3-3         26          41.4          19               6
##  4 Con7  #4/2/95/3-3           NA          NA            20               6
##  5 Con7  #1/5/3/83/3-3/2       NA          NA            20               9
##  6 Con8  #3/83/3-3             NA          NA            20               9
##  7 Con8  #2/95/3               NA          NA            20               8
##  8 Con8  #3/5/2/2/95           28.5        NA            20               8
##  9 Con8  #5/4/3/83/3           28          NA            19               9
## 10 Con8  #1/6/2/2/95-2         NA          NA            20               7
## # ℹ 34 more rows
## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>
filter(litters_df, group %in% c("Con7", "Con8")) #useful for sets
## # A tibble: 15 × 8
##    group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
##    <chr> <chr>                <dbl>       <dbl>       <dbl>           <dbl>
##  1 Con7  #85                   19.7        34.7          20               3
##  2 Con7  #1/2/95/2             27          42            19               8
##  3 Con7  #5/5/3/83/3-3         26          41.4          19               6
##  4 Con7  #5/4/2/95/2           28.5        44.1          19               5
##  5 Con7  #4/2/95/3-3           NA          NA            20               6
##  6 Con7  #2/2/95/3-2           NA          NA            20               6
##  7 Con7  #1/5/3/83/3-3/2       NA          NA            20               9
##  8 Con8  #3/83/3-3             NA          NA            20               9
##  9 Con8  #2/95/3               NA          NA            20               8
## 10 Con8  #3/5/2/2/95           28.5        NA            20               8
## 11 Con8  #5/4/3/83/3           28          NA            19               9
## 12 Con8  #1/6/2/2/95-2         NA          NA            20               7
## 13 Con8  #3/5/3/83/3-3-2       NA          NA            20               8
## 14 Con8  #2/2/95/2             NA          NA            19               5
## 15 Con8  #3/6/2/2/95-3         NA          NA            20               7
## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>
filter(litters_df, group == "Con7" & gd_of_birth == 20)
## # A tibble: 4 × 8
##   group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
##   <chr> <chr>                <dbl>       <dbl>       <dbl>           <dbl>
## 1 Con7  #85                   19.7        34.7          20               3
## 2 Con7  #4/2/95/3-3           NA          NA            20               6
## 3 Con7  #2/2/95/3-2           NA          NA            20               6
## 4 Con7  #1/5/3/83/3-3/2       NA          NA            20               9
## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>
```

It’s common to omit missing observations. use `drop_na` for this from
the `tidyr` package.

``` r
drop_na(litters_df) # drop rows with any value missing
## # A tibble: 31 × 8
##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
##    <chr> <chr>              <dbl>       <dbl>       <dbl>           <dbl>
##  1 Con7  #85                 19.7        34.7          20               3
##  2 Con7  #1/2/95/2           27          42            19               8
##  3 Con7  #5/5/3/83/3-3       26          41.4          19               6
##  4 Con7  #5/4/2/95/2         28.5        44.1          19               5
##  5 Mod7  #59                 17          33.4          19               8
##  6 Mod7  #103                21.4        42.1          19               9
##  7 Mod7  #3/82/3-2           28          45.9          20               5
##  8 Mod7  #5/3/83/5-2         22.6        37            19               5
##  9 Mod7  #106                21.7        37.8          20               5
## 10 Mod7  #94/2               24.4        42.9          19               7
## # ℹ 21 more rows
## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>
# Learning Assessment

filter(pups_df, sex == 1)
## # A tibble: 155 × 6
##    litter_number   sex pd_ears pd_eyes pd_pivot pd_walk
##    <chr>         <dbl>   <dbl>   <dbl>    <dbl>   <dbl>
##  1 #85               1       4      13        7      11
##  2 #85               1       4      13        7      12
##  3 #1/2/95/2         1       5      13        7       9
##  4 #1/2/95/2         1       5      13        8      10
##  5 #5/5/3/83/3-3     1       5      13        8      10
##  6 #5/5/3/83/3-3     1       5      14        6       9
##  7 #5/4/2/95/2       1      NA      14        5       9
##  8 #4/2/95/3-3       1       4      13        6       8
##  9 #4/2/95/3-3       1       4      13        7       9
## 10 #2/2/95/3-2       1       4      NA        8      10
## # ℹ 145 more rows
filter(pups_df, sex == 2, pd_walk < 11)
## # A tibble: 127 × 6
##    litter_number   sex pd_ears pd_eyes pd_pivot pd_walk
##    <chr>         <dbl>   <dbl>   <dbl>    <dbl>   <dbl>
##  1 #1/2/95/2         2       4      13        7       9
##  2 #1/2/95/2         2       4      13        7      10
##  3 #1/2/95/2         2       5      13        8      10
##  4 #1/2/95/2         2       5      13        8      10
##  5 #1/2/95/2         2       5      13        6      10
##  6 #5/5/3/83/3-3     2       5      13        8      10
##  7 #5/5/3/83/3-3     2       5      14        7      10
##  8 #5/5/3/83/3-3     2       5      14        8      10
##  9 #5/4/2/95/2       2      NA      14        7      10
## 10 #5/4/2/95/2       2      NA      14        7      10
## # ℹ 117 more rows
```

## Mutate

``` r
mutate(litters_df,
  wt_gain = gd18_weight - gd0_weight,
  group = str_to_lower(group)
)
## # A tibble: 49 × 9
##    group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
##    <chr> <chr>                <dbl>       <dbl>       <dbl>           <dbl>
##  1 con7  #85                   19.7        34.7          20               3
##  2 con7  #1/2/95/2             27          42            19               8
##  3 con7  #5/5/3/83/3-3         26          41.4          19               6
##  4 con7  #5/4/2/95/2           28.5        44.1          19               5
##  5 con7  #4/2/95/3-3           NA          NA            20               6
##  6 con7  #2/2/95/3-2           NA          NA            20               6
##  7 con7  #1/5/3/83/3-3/2       NA          NA            20               9
##  8 con8  #3/83/3-3             NA          NA            20               9
##  9 con8  #2/95/3               NA          NA            20               8
## 10 con8  #3/5/2/2/95           28.5        NA            20               8
## # ℹ 39 more rows
## # ℹ 3 more variables: pups_dead_birth <dbl>, pups_survive <dbl>, wt_gain <dbl>

#Learning Assessment
mutate(
  pups_df, 
  pd_pivot_sub_7 = pd_pivot - 7,
  pd_sum = pd_ears + pd_eyes + pd_pivot + pd_pivot_sub_7 + pd_walk
  )
## # A tibble: 313 × 8
##    litter_number   sex pd_ears pd_eyes pd_pivot pd_walk pd_pivot_sub_7 pd_sum
##    <chr>         <dbl>   <dbl>   <dbl>    <dbl>   <dbl>          <dbl>  <dbl>
##  1 #85               1       4      13        7      11              0     35
##  2 #85               1       4      13        7      12              0     36
##  3 #1/2/95/2         1       5      13        7       9              0     34
##  4 #1/2/95/2         1       5      13        8      10              1     37
##  5 #5/5/3/83/3-3     1       5      13        8      10              1     37
##  6 #5/5/3/83/3-3     1       5      14        6       9             -1     33
##  7 #5/4/2/95/2       1      NA      14        5       9             -2     NA
##  8 #4/2/95/3-3       1       4      13        6       8             -1     30
##  9 #4/2/95/3-3       1       4      13        7       9              0     33
## 10 #2/2/95/3-2       1       4      NA        8      10              1     NA
## # ℹ 303 more rows
```

## Arrange

``` r
head(arrange(litters_df, group, pups_born_alive), 10)
## # A tibble: 10 × 8
##    group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
##    <chr> <chr>                <dbl>       <dbl>       <dbl>           <dbl>
##  1 Con7  #85                   19.7        34.7          20               3
##  2 Con7  #5/4/2/95/2           28.5        44.1          19               5
##  3 Con7  #5/5/3/83/3-3         26          41.4          19               6
##  4 Con7  #4/2/95/3-3           NA          NA            20               6
##  5 Con7  #2/2/95/3-2           NA          NA            20               6
##  6 Con7  #1/2/95/2             27          42            19               8
##  7 Con7  #1/5/3/83/3-3/2       NA          NA            20               9
##  8 Con8  #2/2/95/2             NA          NA            19               5
##  9 Con8  #1/6/2/2/95-2         NA          NA            20               7
## 10 Con8  #3/6/2/2/95-3         NA          NA            20               7
## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>
```

## /\>

super clunky options

``` r
litters_df_raw = 
    read_csv("./data/FAS_litters.csv", na = c("NA", ".", ""))
## Rows: 49 Columns: 8
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): Group, Litter Number
## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
litters_df_clean_names = janitor::clean_names(litters_df_raw)
litters_df_selected_cols = select(litters_df_clean_names, -pups_survive)
litters_df_with_vars = 
  mutate(
    litters_df_selected_cols, 
    wt_gain = gd18_weight - gd0_weight,
    group = str_to_lower(group))
litters_df_with_vars_without_missing = 
  drop_na(litters_df_with_vars, wt_gain)
litters_df_with_vars_without_missing
## # A tibble: 31 × 8
##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
##    <chr> <chr>              <dbl>       <dbl>       <dbl>           <dbl>
##  1 con7  #85                 19.7        34.7          20               3
##  2 con7  #1/2/95/2           27          42            19               8
##  3 con7  #5/5/3/83/3-3       26          41.4          19               6
##  4 con7  #5/4/2/95/2         28.5        44.1          19               5
##  5 mod7  #59                 17          33.4          19               8
##  6 mod7  #103                21.4        42.1          19               9
##  7 mod7  #3/82/3-2           28          45.9          20               5
##  8 mod7  #5/3/83/5-2         22.6        37            19               5
##  9 mod7  #106                21.7        37.8          20               5
## 10 mod7  #94/2               24.4        42.9          19               7
## # ℹ 21 more rows
## # ℹ 2 more variables: pups_dead_birth <dbl>, wt_gain <dbl>
```

less clunky second option

``` r
litters_df_clean = 
  drop_na(
    mutate(
      select(
        janitor::clean_names(
          read_csv("./data/FAS_litters.csv", na = c("NA", ".", ""))
          ), 
      -pups_survive
      ),
    wt_gain = gd18_weight - gd0_weight,
    group = str_to_lower(group)
    ),
  wt_gain
  )
## Rows: 49 Columns: 8
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): Group, Litter Number
## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
litters_df_clean
## # A tibble: 31 × 8
##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
##    <chr> <chr>              <dbl>       <dbl>       <dbl>           <dbl>
##  1 con7  #85                 19.7        34.7          20               3
##  2 con7  #1/2/95/2           27          42            19               8
##  3 con7  #5/5/3/83/3-3       26          41.4          19               6
##  4 con7  #5/4/2/95/2         28.5        44.1          19               5
##  5 mod7  #59                 17          33.4          19               8
##  6 mod7  #103                21.4        42.1          19               9
##  7 mod7  #3/82/3-2           28          45.9          20               5
##  8 mod7  #5/3/83/5-2         22.6        37            19               5
##  9 mod7  #106                21.7        37.8          20               5
## 10 mod7  #94/2               24.4        42.9          19               7
## # ℹ 21 more rows
## # ℹ 2 more variables: pups_dead_birth <dbl>, wt_gain <dbl>
```

Both give the same output but the code is not easy to read at all Yay
for piping!!! We \<3 piping

``` r
litters_df = 
  read_csv("./data/FAS_litters.csv", na = c("NA", ".", "")) |> 
  janitor::clean_names() |> 
  select(-pups_survive) |> 
  mutate(
    wt_gain = gd18_weight - gd0_weight,
    group = str_to_lower(group)) |> 
  drop_na(wt_gain)
## Rows: 49 Columns: 8
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): Group, Litter Number
## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
litters_df
## # A tibble: 31 × 8
##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
##    <chr> <chr>              <dbl>       <dbl>       <dbl>           <dbl>
##  1 con7  #85                 19.7        34.7          20               3
##  2 con7  #1/2/95/2           27          42            19               8
##  3 con7  #5/5/3/83/3-3       26          41.4          19               6
##  4 con7  #5/4/2/95/2         28.5        44.1          19               5
##  5 mod7  #59                 17          33.4          19               8
##  6 mod7  #103                21.4        42.1          19               9
##  7 mod7  #3/82/3-2           28          45.9          20               5
##  8 mod7  #5/3/83/5-2         22.6        37            19               5
##  9 mod7  #106                21.7        37.8          20               5
## 10 mod7  #94/2               24.4        42.9          19               7
## # ℹ 21 more rows
## # ℹ 2 more variables: pups_dead_birth <dbl>, wt_gain <dbl>
```

Sometime you have to tell the data where to go in the pipe (never really
needed in `tidyverse`)

``` r
litters_df |>
  lm(wt_gain ~ pups_born_alive, data = _) |>
  broom::tidy()
## # A tibble: 2 × 5
##   term            estimate std.error statistic  p.value
##   <chr>              <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)       13.1       1.27      10.3  3.39e-11
## 2 pups_born_alive    0.605     0.173      3.49 1.55e- 3

# Learning Assessment
pups_df_LA =
  read_csv("./data/FAS_pups.csv", na = c("NA", ".", ""), skip = 3) |> 
  janitor::clean_names() |> 
  filter(sex == 1) |> 
  select(-pd_ears) |> 
  mutate(pd_pivot_more_than_7 = pd_pivot > 7)
## Rows: 313 Columns: 6
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (1): Litter Number
## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
pups_df_LA 
## # A tibble: 155 × 6
##    litter_number   sex pd_eyes pd_pivot pd_walk pd_pivot_more_than_7
##    <chr>         <dbl>   <dbl>    <dbl>   <dbl> <lgl>               
##  1 #85               1      13        7      11 FALSE               
##  2 #85               1      13        7      12 FALSE               
##  3 #1/2/95/2         1      13        7       9 FALSE               
##  4 #1/2/95/2         1      13        8      10 TRUE                
##  5 #5/5/3/83/3-3     1      13        8      10 TRUE                
##  6 #5/5/3/83/3-3     1      14        6       9 FALSE               
##  7 #5/4/2/95/2       1      14        5       9 FALSE               
##  8 #4/2/95/3-3       1      13        6       8 FALSE               
##  9 #4/2/95/3-3       1      13        7       9 FALSE               
## 10 #2/2/95/3-2       1      NA        8      10 TRUE                
## # ℹ 145 more rows
```
