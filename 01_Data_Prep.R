
# simulate data ================================================================

guests <- paste0('Guest_', 1:60)
n_guests <- length(guests)
questions <- c('Question A', 'Question B', 'Question C', 'Question D', 'Question E',
               'Question F', 'Question G', 'Question H', 'Question I', 'Question J')

# one data frame
data.raw <- data.frame(
  Guest = rep(guests, each = length(questions)),
  Question = rep(questions, times = n_guests),
  Answer = sample(0:10, size = n_guests * length(questions), replace = TRUE)
)

# aggregate data to traits =====================================================
data.traits <- data.raw |> 
  pivot_wider(names_from = Question, values_from = Answer) |>
  mutate(trait_1 = (`Question A` + `Question B` + `Question C`)/3,
         trait_2 = (`Question D` + `Question E` + `Question F`)/3,
         trait_3 = (`Question G` + `Question H` + `Question I` + `Question J`)/4
         )

# add Höhenunterschied

data <-



