# plumber.R
library(plumber)
library(tidyverse)
library(lubridate)
library(textclean)
library(httr)


#keys = jsonlite::read_json('keys.json')

OpenAIKey <- Sys.getenv("OpenAIKey")

detect_swearwords <- function(x){
  message <- tolower(x)
  curse_words <- c("fuck", "shit", "bitch", "gay", "retard", "whore", "bastard", "cunt", "pussy", "dick", "cock", "jack off", "homo")
  sum(map_int(curse_words, ~ str_count(message, .x)))
}


detect_hangouts <- function(x){
  message <- tolower(x)
  time_patterns <- c("at [0-9]{1}?", "[0-9]{1}:[0-9]{1,2}?", "boat quay", "reso", "beers", "scoops", "pints", "drinks", "anyone want to",
                     "meet up", "hang", "dinner", "lunch", "my place", "at mine", "reservation", "see you there", "on my way")
  max(map_int(time_patterns, ~ str_count(message, .x)))
}

detect_hangout_ai <- function(message){


  # Create the prompt
  prompt <- paste0(
    "The following is a message from a groupchat between friends. In this message, is the user proposing an in-person meetup between members of the chat?",
    " This could include casual hangouts, grabbing beers or a meal, attending an event or concert, or suggesting a group vacation. Respond either YES or NO with no additional commentary, formatting, context or pleasantries:",
    message)

  # Create the JSON payload
  payload <- list(
    model = "gpt-3.5-turbo", # Changed from gpt-4o-mini to gpt-4
    messages = list(
      list(role = "user", content = prompt)
    )
  )

  # Convert the payload to JSON
  payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE)

  # Send the POST request
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    body = payload_json,
    encode = "json",
    add_headers(Authorization = paste0("Bearer ", OpenAIKey),
                `Content-Type` = "application/json")
  )

  if (http_status(response)$category == "Success") {
    content <- content(response, as = "parsed")
    message <- content$choices[[1]]$message$content
    return(message)
}
}



summarize_day <- function(day_data){
  # Convert dataframe to text string, handling grouping
  day_data_string <- paste(
    capture.output(print(day_data, row.names = FALSE)),  # Convert dataframe to text
    collapse = "\n"  # Combine rows with newlines
  )

  # Clean the text to ensure it's JSON-safe
  day_data_string <- gsub("\n", " ", day_data_string)   # Replace newlines with spaces
  day_data_string <- gsub('"', '\\"', day_data_string)  # Escape quotes

  # Create the prompt
  prompt <- paste0(
    "Please analyze all of the messages in this chat transcript from a day in a groupchat ",
    "and return a brief summary of what happened that day, including a title. ",
    "The tone of the summary should be lighthearted and sarcastic if something funny happened, ",
    "and comforting if something less fun happened. Return only the requested summary and a title for it",
    "with no additional commentary, context, formatting, or pleasantries. Return each summary as a json with two keys: title and summary.",
    day_data_string
  ) %>% gsub("\n", " ", .)

  # Create the JSON payload
  payload <- list(
    model = "gpt-4o-mini", # Changed from gpt-4o-mini to gpt-4
    messages = list(
      list(role = "user", content = prompt)
    )
  )

  # Convert the payload to JSON
  payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE)

  # Send the POST request
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    body = payload_json,
    encode = "json",
    add_headers(Authorization = paste0("Bearer ", OpenAIKey),
                `Content-Type` = "application/json")
  )

  if (http_status(response)$category == "Success") {
    content <- content(response, as = "parsed")
    message <- content$choices[[1]]$message$content
    # Try to parse the JSON response
    tryCatch({
      parsed_message <- jsonlite::fromJSON(message) # Changed from 'summary' to 'message'
      return(parsed_message)
    }, error = function(e) {
      warning("Failed to parse JSON response: ", e$message)
      return(list(title = "Error", summary = "Failed to generate summary"))
    })
  } else {
    warning("API request failed: ", content(response, as = "text"))
    return(list(title = "Error", summary = "Failed to generate summary"))
  }
}

caption_handler <- function(caption_text, to_insert = ""){

  insert_at_position <- function(string, insert, position) {
    paste0(
      substr(string, 1, position - 1),
      insert,
      substr(string, position, nchar(string))
    )
  }

  find_left_boundary <- function(string, index) {
    # Get substring up to index
    substring_to_index <- substr(string, 1, index)
    # Find position of last space
    last_space <- max(gregexpr(" ", substring_to_index)[[1]])

    # Return the position (if no space found, returns beginning of string)
    if(last_space == -1) return(1)
    return(last_space)
  }

  if(nchar(caption_text) > 80){

    line_breaks <- floor(nchar(caption_text)/80)

    line_breaks <- c(1:line_breaks)*80


    for(line_break in line_breaks){

      insert_position <- find_left_boundary(caption_text, line_break)

      caption_text <- insert_at_position(caption_text, to_insert, insert_position)

    }

  }

  return(caption_text)


}


#* @apiTitle WhatsApp Analysis API
#* @apiDescription Accepts raw transcript and returns structured JSON data for visualization
#* @post /analyze
#* @param transcript:character The WhatsApp transcript as a single string (with newline chars)
#* @param year:character Indicates the year over which to perform analysis
function(transcript, year = '2024'){

  start_date <- as.Date(paste0(year,"-01-01"))
  end_date <- start_date + years(1)

  transcript <- read_lines(transcript)

  message_data <-
    tibble(lines = transcript) %>%
    mutate(date = trimws(str_extract(lines, "(?<=\\[\\s?).{8,}(?=\\s*\\])")),
      sender = ifelse(!is.na(date), trimws(str_extract(lines, "(?<=\\]\\s?).*?(?=\\s*:)")), NA),
           message = ifelse(!is.na(date), trimws(str_extract(lines, "(?<=[A-Za-z]{3}:\\s?).*$")), lines),
           first_digit = max(as.numeric(str_extract(date, "^[0-9]{1,2}")), na.rm=TRUE),
           first_hour = max(as.numeric(str_extract(date, "(?<=\\s?)[0-9]{1,2}(?=\\s*:)")), na.rm = TRUE)) %>%
    mutate(sender = trimws(sender),
           message = trimws(message),
         date = case_when(first_digit > 12 ~ case_when(first_hour <= 12 ~ parse_date_time(date, '%d/%m/%y, %I:%M:%S %p'), 
                                                       first_hour > 12 ~ parse_date_time(date, '%d/%m/%y, %I:%M:%S')),
                          first_digit <= 12 ~ case_when(first_hour <= 12 ~ parse_date_time(date, '%m/%d/%y, %I:%M:%S %p'), 
                                                        first_hour > 12 ~ parse_date_time(date, '%m/%d/%y, %I:%M:%S')) 
                          TRUE ~ parse_date_time(date, 'ymd'))) %>%
    select(date, sender, message) %>%
    fill(date, sender) %>%
    group_by(date, sender) %>%
    summarize(message = paste0(message, collapse = " ")) %>%
    ungroup() %>%
    filter(!sender %in% c('Meta AI', 'Enemies', 'America f')) %>%
    mutate(sender = case_when(
      sender == 'Cody' ~ 'Cody Anderson',
      sender %in% c("🗿 🗿🗿", "‎ 🗿 🗿🗿", " ~ Brett", "~ Brett", "~Brett") ~ "Brett Graham",
      TRUE ~ trimws(replace_non_ascii(sender, ""))
    )) %>%
    mutate(sender = case_when(
      sender == "~Brett" ~ "Brett Graham",
      sender == "~DD" ~ "Dan Dean",
      sender == "Cody" ~ "Cody Anderson",
      TRUE ~ sender
    )) %>%
    filter(sender != "Meta AI" & sender != "" & !str_detect(sender, "~"))


  # day_of_week_data
  day_of_week_data <- message_data %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(day_of_week = wday(date, label = TRUE, abbr = FALSE)) %>%
    group_by(date = floor_date(date, "day"), day_of_week) %>%
    summarize(message_count = n(), .groups="drop") %>%
    group_by(day_of_week) %>%
    summarize(avg_messages = mean(message_count), .groups="drop") %>%
    # Convert factor to character for JSON
    mutate(day_of_week = as.character(day_of_week)) %>%
    as.data.frame()

  top_day_of_week <- day_of_week_data$day_of_week[which(day_of_week_data$avg_messages == max(day_of_week_data$avg_messages, na.rm = TRUE))]
  avg_messages_top_day <- round(max(day_of_week_data$avg_messages, na.rm = TRUE))

  caption = paste0(top_day_of_week, "s are particularly lively for the groupchat. On average, y'all send ", avg_messages_top_day, " messages on this lovely day.") %>%
    caption_handler()

  day_of_week <- list(data = day_of_week_data, caption = c(caption))

  # monthly_messages_data: total messages per month
  monthly_messages_data <- message_data %>%
    filter(date >= start_date & date <= end_date) %>%
    group_by(date = floor_date(as.Date(date), "month")) %>%
    summarize(message_count = n(), .groups="drop") %>%
    as.data.frame()

  first_half_count <- sum(monthly_messages_data$message_count[c(1:floor(nrow(monthly_messages_data)/2))])
  last_half_count <- sum(monthly_messages_data$message_count[c(floor(nrow(monthly_messages_data)/2):length(monthly_messages_data))])
  ratio <- last_half_count/first_half_count - 1

  if(ratio > 0){
    caption <- "Here's how many messages you all posted in the chat each month. Looks like activity picked up over the year, great job!"
  } else{

    caption <- "Here's how many messages you all posted in the chat each month. Looks like activity fell off in the second half of the year. What happened!?"
  }

  caption <- caption %>% caption_handler()

  monthly_messages <- list(data = monthly_messages_data, caption = c(caption))

  # avg_messages_sent_data: Average messages sent per month (across senders)
  avg_messages_sent_data <- message_data %>%
    filter(date >= start_date & date <= end_date) %>%
    group_by(month = floor_date(as.Date(date), "month"), sender) %>%
    summarize(message_count = n(), .groups="drop") %>%
    group_by(month) %>%
    summarize(avg_messages_sent = mean(message_count), .groups="drop") %>%
    as.data.frame()

  caption <- "And here we can see the average messages sent per member per month..." %>% caption_handler()

  avg_messages_sent <- list(data = avg_messages_sent_data, caption = c(caption))

  # quarterly_contribution_data: per quarter by sender
  quarterly_contribution_data <- message_data %>%
    filter(date >= start_date & date <= end_date) %>%
    group_by(date = floor_date(as.Date(date), "quarter"), sender) %>%
    summarize(message_count = n(), .groups="drop") %>%
    as.data.frame()

  top_sender <- quarterly_contribution_data %>%
    filter(date >= start_date & date <= end_date) %>%
    group_by(sender) %>%
    reframe(sender_total = sum(as.numeric(message_count))) %>%
    ungroup() %>%
    mutate(percent = sender_total/sum(sender_total)) %>%
    filter(percent == max(percent))

  caption <- paste0("If you’ve muted this chat, blame ", top_sender$sender[1], " – they sent ", round(top_sender$percent[1]*100, 1), "% of all messages!") %>%
    caption_handler()


  quarterly_contribution <- list(data = quarterly_contribution_data,
                                 caption = c(caption))

  # year-over-year change

  this_year <- year(start_date)
  last_year <- this_year - 1

  yearly_comparison_data <- message_data %>%
    mutate(year = year(date)) %>%
    filter(year %in% c(last_year, this_year)) %>%
    mutate(year = ifelse(year == last_year, "last_year", "this_year")) %>%
    group_by(sender, year) %>%
    summarize(messages = n(), .groups="drop") %>%
    pivot_wider(names_from = year, values_from = messages) %>%
    mutate(percent_change = (this_year/last_year) - 1) %>%
    drop_na() %>%
    arrange(desc(percent_change)) %>%
    as.data.frame()

  year_over_year_changes <- yearly_comparison_data %>%
    filter(percent_change == max(percent_change) | percent_change == min(percent_change))

  caption = paste0("Wow, keep it up ", year_over_year_changes$sender[1], ", you really stepped up your game this year. ", year_over_year_changes$sender[2], ", this falloff will be studied." ) %>%
    caption_handler()

  yearly_comparison = list(data = yearly_comparison_data, caption = c(caption))

  # Top ten days
  top_ten_days_data <- message_data %>%
    filter(date >= start_date & date <= end_date) %>%
    group_by(day = floor_date(as.Date(date), "day")) %>%
    summarize(chats = n(), .groups="drop") %>%
    arrange(desc(chats)) %>%
    slice(1:10) %>%
    as.data.frame()


  top_three_days <- top_ten_days_data %>%
    slice(1:3) %>%
    pull(day)


  # top_three_days_summary <- list(list(date = "2024-03-17",
  #                                     content = list(title = "Summary #1", summary = "blah")),
  #                                list(date = "2024-01-07",
  #                                     content = list(title = "Summary #2", summary = "blah blah")),
  #                                list(date = "2024-06-19",
  #                                     content = list(title = "Summary #3", summary = "blah blah blah")))


  top_three_days_summary <- list()
  for (i in c(1:length(top_three_days))){

    day_summary <- message_data %>%
    filter(as.Date(date) == as.Date(top_three_days[i], origin = "1970-01-01")) %>%
      summarize_day()

    day_summary = list(date = strftime(as.Date(top_three_days[i], origin = "1970-01-01"), format = "%B %d, %Y"),
                       content = day_summary)

    top_three_days_summary[[i]] <- day_summary
  }


  caption <- "Here we see the most active days in the chat. Boy were they some big ones! What happened on those days?" %>%
    caption_handler()

  top_ten_days = list(data = top_ten_days_data, caption = c(caption), day_summaries = top_three_days_summary)

  # Manic data
  manic_data <- message_data %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(hour = hour(date)) %>%
    mutate(manic = ifelse(hour %in% c(22, 23, 24, 0, 1, 2, 3, 4), 1, 0)) %>%
    group_by(sender) %>%
    summarize(count_manic = sum(manic),
              messages = n(),
              percent_manic = count_manic / messages,
              .groups="drop") %>%
    as.data.frame()

  most_manic <- manic_data$sender[which(manic_data$percent_manic == max(manic_data$percent_manic))]
  manic_percent <- round(100*manic_data$percent_manic[which(manic_data$percent_manic == max(manic_data$percent_manic))], 1)
  caption <- paste0("This group seems pretty rowdy, but who's the wildest of all? ", most_manic, " leads the way, sending ", manic_percent, "% of all their messages between 10 pm and 4 am. Truly impressive!") %>%
    caption_handler()

  manic <- list(data = manic_data, caption = c(caption))

  # Most Ignored

  most_ignored_data <- message_data %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(last_message_in_chunk = ifelse(lead(sender) != sender, TRUE, FALSE),
           next_sender_timestamp = case_when(last_message_in_chunk ~ as.POSIXct(lead(date)),
                                             TRUE ~ NA),
           time_to_respond = as.numeric(next_sender_timestamp - date)/60) %>%
    group_by(sender) %>%
    summarize(average_time_to_respond = mean(time_to_respond, na.rm = TRUE)) %>%
    arrange(desc(average_time_to_respond))

  most_ignored <- most_ignored_data$sender[which(most_ignored_data$average_time_to_respond == max(most_ignored_data$average_time_to_respond))]
  average_time_to_respond <- round(most_ignored_data$average_time_to_respond[which(most_ignored_data$average_time_to_respond == max(most_ignored_data$average_time_to_respond))])

  caption <- paste0("Don't you hate the feeling when you make a fire joke in the groupchat and no one responds? ",
                    most_ignored,
                    " definitely knows the feeling - it takes ",
                    average_time_to_respond,
                    " minutes on average for someone to respond to one of their messages. Sorry bud, better luck next year.") %>%
    caption_handler()

  most_ignored <- list(data = most_ignored_data, caption = c(caption))


  # Wall of text award

  novelist_data <- message_data %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(message_length = nchar(message)) %>%
    group_by(sender) %>%
    summarize(average_message_length = mean(message_length, na.rm = TRUE)) %>%
    arrange(desc(average_message_length))

  best_novelist <- novelist_data$sender[which(novelist_data$average_message_length == max(novelist_data$average_message_length))]
  average_message_length <- round(novelist_data$average_message_length[which(novelist_data$average_message_length== max(novelist_data$average_message_length))])

  caption <- paste0("Looks like ",
                    best_novelist,
                    " has a bright career as an author! Your messages are ",
                    average_message_length,
                    " characters long on average. Someone get this guy a Pulitzer!") %>%
    caption_handler()

  novelist <- list(data = novelist_data, caption = c(caption))

  # Swear data
  swear_data <- message_data %>%
    filter(date >= start_date & date <= end_date) %>%
    rowwise() %>%
    mutate(count_swears = detect_swearwords(message)) %>%
    ungroup() %>%
    group_by(sender) %>%
    summarize(count_messages = n(),
              count_swears = sum(count_swears),
              swears_per_message = count_swears / count_messages,
              .groups="drop") %>%
    as.data.frame()

  most_swears <- swear_data$sender[which(swear_data$swears_per_message == max(swear_data$swears_per_message))]
  swears_per_message <- round(swear_data$swears_per_message[which(swear_data$swears_per_message == max(swear_data$swears_per_message))], 2)

  caption <- paste0("You all need to wash your mouths out with soap! This chat is absolutely not safe for work, and ",
                    most_swears, " is the biggest liability, using a naughty word ",
                    swears_per_message,
                    " times per message. Tsk tsk, do better.") %>%
    caption_handler()

  swears <- list(data = swear_data, caption = c(caption))

  # Hangout data
  hangout_data <- message_data %>%
    filter(date >= start_date & date <= end_date) %>%
    rowwise() %>%
    mutate(count_hangouts = detect_hangouts(message)) %>%
    ungroup() %>%
    group_by(sender) %>%
    summarize(count_messages = n(),
              count_hangouts = sum(count_hangouts),
              hangouts_per_message = count_hangouts/count_messages,
              .groups="drop") %>%
    as.data.frame()

  top_hangout <- hangout_data$sender[which(hangout_data$hangouts_per_message == max(hangout_data$hangouts_per_message, na.rm = T))]
  bottom_hangout <- hangout_data$sender[which(hangout_data$hangouts_per_message == min(hangout_data$hangouts_per_message, na.rm = T))]

  if(length(top_hangout) > 1){

    top_hangout <- paste0(top_hangout, collapse = ", ")

  }

  if(length(bottom_hangout) > 1){

    bottom_hangout <- paste0(bottom_hangout, collapse = ", ")

  }

  caption <- paste0("But who's really bringing the groupchat together? We've analyzed the data and looked for words and phrases that relate to organizing at meetup. ",
                    top_hangout,
                    " take a bow, when you're hitting the groupchat we know things are happening. ",
                    bottom_hangout,
                    ", we see you freeloading over there.") %>%
    caption_handler()

  hangout <- list(data = hangout_data, caption = c(caption))

  # Combine all into one list
  list(
    day_of_week = day_of_week,
    monthly_messages = monthly_messages,
    avg_messages_sent = avg_messages_sent,
    quarterly_contribution = quarterly_contribution,
    yearly_comparison = yearly_comparison,
    top_ten_days = top_ten_days,
    manic = manic,
    most_ignored = most_ignored,
    novelist = novelist,
    swears = swears,
    hangout = hangout
  )
}


#* @apiTitle WhatsApp Analysis API
#* @apiDescription Accepts raw transcript and returns structured JSON data for visualization
#* @post /available-years
#* @param transcript:character The WhatsApp transcript as a single string (with newline chars)
function(transcript){

  transcript <- read_lines(transcript)

  message_data <-
    tibble(lines = transcript) %>%
    mutate(date = trimws(str_extract(lines, "(?<=\\[\\s?).{8,}(?=\\s*\\])")),
           first_digit = max(as.numeric(str_extract(date, "^[0-9]{1,2}")), na.rm = TRUE),
           first_hour = max(as.numeric(str_extract(date, "(?<=\\s?)[0-9]{1,2}(?=\\s*:)")), na.rm = TRUE)) %>%
    mutate(date = case_when(first_digit > 12 ~ case_when(first_hour <= 12 ~ parse_date_time(date, '%d/%m/%y, %I:%M:%S %p'), 
                                                       first_hour > 12 ~ parse_date_time(date, '%d/%m/%y, %I:%M:%S')),
                          first_digit <= 12 ~ case_when(first_hour <= 12 ~ parse_date_time(date, '%m/%d/%y, %I:%M:%S %p'), 
                                                        first_hour > 12 ~ parse_date_time(date, '%m/%d/%y, %I:%M:%S')) 
                          TRUE ~ parse_date_time(date, 'ymd')))  %>%
    select(date) 

  available_years <- message_data %>%
    transmute(years = year(date)) %>%
    distinct() %>%
    pull()

  list(available_years = sort(available_years[-1], decreasing = TRUE))

}

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}
