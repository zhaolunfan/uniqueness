#' Create Year Windows
#'
#' This function creates year windows by grouping years into specified intervals.
#'
#' @param df A data frame containing a 'year' column.
#' @param window_size An integer specifying the size of the year window.
#' @return A data frame with an additional 'year_window' column indicating the year window for each row.
#' @import dplyr
#' @export
create_year_windows <- function(df, window_size) {
  df <- df %>% mutate(year_window = floor(year / window_size) * window_size)
  return(df)
}

#' Generate Bootstrap Samples
#'
#' This function generates bootstrap samples from the provided data.
#'
#' @param data A data frame containing the data to sample from.
#' @param I An integer specifying the number of bootstrap samples to generate. Default is 1000.
#' @param size An integer specifying the size of each bootstrap sample. Default is 200.
#' @return A data frame where each column is a bootstrap sample.
#' @import dplyr
#' @export
generate_bootstrap_samples <- function(data, I = 1000, size = 200) {
  # set seeds
  set.seed(30042024)

  # Initialize a list to store the bootstrap samples
  bootstrap_samples <- vector("list", I)

  data <- data %>% select(name)

  # Generate the bootstrap samples
  for (i in 1:I) {
    bootstrap_samples[[i]] <- data[sample(1:nrow(data), size, replace = TRUE), ]
  }

  # Convert the list to a data frame where each sample is a column
  bootstrap_data <- as.data.frame(do.call(cbind, bootstrap_samples))

  # Ensure column names are unique
  colnames(bootstrap_data) <- paste0("sample_", seq_len(ncol(bootstrap_data)))

  return(bootstrap_data)
}

#' Calculate Name Frequency
#'
#' This function calculates the frequency of names in the provided data.
#'
#' @param data A data frame containing the data with 'sample' columns.
#' @return A data frame with the frequency of each name.
#' @import dplyr
#' @import tidyr
#' @export
calculate_name_frequency <- function(data) {
  ### pivot to longer to make frequency
  data <- data %>%
    mutate(row_id = row_number()) %>% # add row id
    pivot_longer(cols = starts_with("sample"), names_to = "sample", values_to = "name")
  
  # make name count to avoid count() removing duplicates
  name_counts = data %>% count(sample, name) %>% rename(frequency = n)

  # join the name count back to the long data
  data <- data %>% left_join(name_counts, by = c("sample", "name"))

  # pivot it back to wider
  data <- data %>%
    pivot_wider(id_cols = row_id, names_from = sample, values_from = frequency) %>% 
    select(-row_id)

  return(data)
}

#' Calculate Uniqueness Score
#'
#' This function calculates the uniqueness score for each name in the data.
#'
#' @param data A data frame containing the frequency of names.
#' @return A data frame with the uniqueness score for each name.
#' @import dplyr
#' @export
calculate_uniqueness <- function(data) {
  # Calculate the number of rows in the data frame
  size <- nrow(data)

  # calculate the uniqueness score
  data <- data %>%
    mutate(across(everything(), ~-log10(. / size) + 0.0000001, .names = "name_uniqueness_{.col}")) %>%
    select(starts_with("name_uniqueness")) %>% 
    rename_with(~ str_remove(., "name_uniqueness_")) 

  return(data)
}

#' Calculate Mean Uniqueness
#'
#' This function calculates the mean uniqueness score for each name.
#'
#' @param data A data frame containing the uniqueness scores.
#' @return A data frame with the mean uniqueness score for each name.
#' @import dplyr
#' @export
calculate_mean_uniqueness <- function(data) {
  # this is 1000 boostrap result of the "uniquenss score" of your specific time window
  # calculate the mean of each column
  data <- summarise_all(data, mean)
  
  return(data)
}

#' Calculate Mean of Mean Uniqueness
#'
#' This function calculates the mean of the mean uniqueness scores.
#'
#' @param data A data frame containing the mean uniqueness scores.
#' @return A single numeric value representing the mean of the mean uniqueness scores.
#' @import dplyr
#' @export
calculate_mean_of_mean <- function(data) {
  # this is a mean of the 1000 bootstrap
  #taking the mean of the mean to form one value
  data <- mean(unlist(data %>%
  summarise(across(everything(), list(mean = mean), .names = "{.col}_{.fn}"))))

  return(data)
}

#' Adjusted Uniqueness
#'
#' This function calculates the adjusted uniqueness score using bootstrap samples.
#'
#' @param data A data frame containing the data to sample from.
#' @param I An integer specifying the number of bootstrap samples to generate.
#' @param size An integer specifying the size of each bootstrap sample.
#' @return A single numeric value representing the adjusted uniqueness score.
#' @import dplyr
#' @export
adjusted_uniqueness <- function(data, I = 1000, size = 200) {
  # Generate the bootstrap samples
  data = generate_bootstrap_samples(data, I = I, size = size)

  # Calculate the name frequency
  data = calculate_name_frequency(data)

  # Calculate the uniqueness score
  data = calculate_uniqueness(data)

  # Calculate the mean uniqueness score
  data = calculate_mean_uniqueness(data)

  # Calculate the mean of the mean
  data = calculate_mean_of_mean(data)

  return(data)
}