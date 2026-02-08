
# ================================= DATA =======================================

# read in adress excel
library(tidyverse)
library(magrittr)

path.address <- here::here("00_data", "Addresses.xlsx")
d.address <- readxl::read_excel(path.address, sheet = "Tabellenblatt1")

# separate street and place
d.address %<>% 
  separate(col = Address, into = c("Name", "Street", "Place", "Country"), sep = "\r\n")

# simulate data ================================================================

guests <- paste0('Guest_', 1:60)
n_guests <- length(guests)
questions <- c('Question A', 'Question B', 'Question C', 'Question D', 'Question E',
               'Question F', 'Question G', 'Question H', 'Question I', 'Question J')

# one data frame
d.cards.raw <- data.frame(
  Guest = rep(guests, each = length(questions)),
  Question = rep(questions, times = n_guests),
  Answer = sample(0:10, size = n_guests * length(questions), replace = TRUE)
)





# aggregate data to traits =====================================================
d.cards.traits <- d.cards.raw |> 
  pivot_wider(names_from = Question, values_from = Answer) |>
  mutate(trait_1 = (`Question A` + `Question B` + `Question C`)/3,
         trait_2 = (`Question D` + `Question E` + `Question F`)/3,
         trait_3 = (`Question G` + `Question H` + `Question I` + `Question J`)/4,
         trait_4 = (`Question A` + `Question D` + `Question G`)/3,
         trait_5 = (`Question B` + `Question E` + `Question H`)/3
         )





