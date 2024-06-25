library(bit64)
library(dplyr)
library(jsonlite)

input_data <- stream_in(file("Input/sdvx@asphyxia.db"))

data_clean <- input_data %>% 
  filter(collection == "music") %>%
  mutate(matchType = "sdvxInGameID") %>%
  select(score, lamp = clear, matchType, identifier = mid, difficulty = type, timeAchieved = 9) 

data_edited <- data_clean %>%
  mutate(
    lamp = case_when(
      lamp == 1 ~ "FAILED",
      lamp == 2 ~ "CLEAR",
      lamp == 3 ~ "EXCESSIVE CLEAR",
      lamp == 4 ~ "ULTIMATE CHAIN",
      lamp == 5 ~ "PERFECT ULTIMATE CHAIN"
    ),
    identifier = as.character(identifier),
    difficulty = case_when(
      difficulty == 0 ~ "NOV",
      difficulty == 1 ~ "ADV",
      difficulty == 2 ~ "EXH",
      difficulty == 3 ~ "ANY_INF",
      difficulty == 4 ~ "MXM"
    ),
    timeAchieved = as.integer64(unlist(timeAchieved))
  )

json_format <- fromJSON("Meta/format.json")
json_format[[2]] <- data_edited

json_output <- toJSON(json_format, auto_unbox = TRUE)

write(json_output,"Output/output.json")