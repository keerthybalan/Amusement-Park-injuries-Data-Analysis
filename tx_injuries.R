library(tidyverse)
library(tidytext)

safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")
#> Parsed with column specification:
#> cols(
#>   .default = col_character(),
#>   acc_id = col_double(),
#>   num_injured = col_double(),
#>   age_youngest = col_double(),
#>   mechanical = col_double(),
#>   op_error = col_double(),
#>   employee = col_double()
#> )
#> See spec(...) for full column specifications.

body_parts<- tolower(c("HEAD", "BACK", "EAR", "HIP", "ARM", "LEG"))

body_parts_freq <- safer_parks %>%
  unnest_tokens(output=word, input=injury_desc) %>% #
  anti_join(get_stopwords()) %>% #Remove stopwords
  filter(word %in% body_parts) %>% #Filter to rows with body part
  mutate(word = toupper(word)) %>%
  distinct(word, .keep_all = TRUE) %>% 
  mutate(word=as.factor(word)) %>%
  group_by(word) %>%
  summarise(total = sum(num_injured))
#> Joining, by = "word"

body_parts_freq
#> # A tibble: 6 x 2
#>   word  total
#>   <fct> <dbl>
#> 1 ARM       1
#> 2 BACK      8
#> 3 EAR       8
#> 4 HEAD      8
#> 5 HIP       1
#> 6 LEG       1