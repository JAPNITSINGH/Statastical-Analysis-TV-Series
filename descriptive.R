# Import required libraries
library(ggplot2)

# Import the dataset
tmdb_data <- read.csv("TMDB_tv_dataset_v3.csv", header = TRUE)
summary(tmdb_data)

expand_nested_data <- function(df, cat_col, n=20) 
  #---- Function Description ----#
  # Deconstruct a column to retrieve most frequent values and their frequency
  
  #---- Arguments ----#
  # df = dataframe
  # cat_col = specifies the column from where data is to be used
  # n = the number of unique values with maximum frequency


  { # Function definition starts
  
  # Split and trim the values
  cat_vals_list <- strsplit(trimws(df[[cat_col]]), ",")
  
  # Flatten the list of values
  all_vals <- unlist(cat_vals_list)
  
  # Calculate frequencies and sort
  freq_table <- table(all_vals)
  sorted_vals <- names(head(sort(freq_table, decreasing = TRUE), n))
  
  # Filter the data frame to include only the top n values
  exp_df <- data.frame(cat_col = sorted_vals, frequency = as.numeric(freq_table[sorted_vals]), stringsAsFactors = FALSE)
  
  return(exp_df)
} # Function definition ends



plot_histogram <- function(data, column_name, x_label="Data", y_label="Frequency", title="Histogram", type=1) 
  #---- Function Description ----#
  # Plot the Histogram of input data based on column
  
  #---- Arguments ----#
  # data = dataframe
  # column_name = specifies the column from where data is to be used
  # x_label = the label for x-axis of histogram
  # y_label = the label for y-axis of histogram
  # title = the title of histogram
  # type = the type of column data to be plotted, values can be :-
  #        "numeric"= 0, "nested_categorical"= 1, "categorical"= 2
  
  
  { # Function definition starts
  
  # Extract the specified column from the data frame
  column_data <- data[[column_name]]
  
  if (type == 0) {
    # For Numeric Data
    
    # Sturges' formula : No. of bins = [log2(n)] + 1
    length_of_seasons_data <- length(column_data)
    num_of_bins <- ceiling(log2(length_of_seasons_data)) + 1
    bin_width <- (max(column_data) - min(column_data)) / num_of_bins # Set the bin width
    
    # Plot the histogram
    histplot <- ggplot(data, aes(x = column_data)) + 
      geom_histogram(binwidth = bin_width, colour = "black", fill = "white", boundary = 0) +
      labs(title = title, x = x_label, y = y_label) +
      scale_x_continuous(breaks = seq(min(column_data), max(column_data), by = bin_width),
                         labels = function(x) round(x, 1))
    
    # Adding Mean
    mean_value <- mean(data[[column_name]])
    histplot <- histplot + geom_vline(aes(xintercept=mean_value) ,colour="red", show.legend = F) + annotate("text", x=mean_value, y= 0.9 * max(table(column_data)), 
                label=substitute(paste(bar(x),"=",m), list(m=sprintf("%.02f",mean_value))), colour="red")
    
    # Adding Median  
    median_value <- median(data[[column_name]])
#    max_freq <- max(table(column_data))
#    yv <- (0.7 * max_freq)
#    cat("Median: ", median_value, "\n")
#    cat("Y-value: ", str(yv), "\n")
    
    histplot <- histplot + geom_vline(aes(xintercept = median_value), colour = "blue", show.legend = FALSE) #+
#      annotate("text", x = median_value, y = 5000, 
#              label = substitute(paste(tilde(x), "=", m)), 
#               list(m = sprintf("%.02f", median_value)), colour = "blue")
  }
  
  else if(type == 1) {
    # For Nested Categorical Data
    
    # Modify the data
    modified_data = expand_nested_data(data, column_name, 20)
    
    histplot <- ggplot(modified_data, aes(x = cat_col, y = frequency)) + 
    geom_bar(stat = "identity", position = "dodge", color = "black", fill = "white") +
    labs(title = title, x = x_label, y = y_label) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  }
  
  else {
    # For Categorical Data
    histplot <- ggplot(data, aes(x = column_data)) + 
      geom_bar(stat="count", colour = "black", fill = "white") +
      labs(title = title, x = x_label, y = y_label)
  }
  
  # Check if "histograms/" directory exists, create if not
  if (!dir.exists("histograms")) {
    dir.create("histograms")
  }
  
  # Print and Save the plot as an image file
  print(histplot)
  file_name <- paste("histograms/", title, ".png")
  ggsave(file_name, plot = histplot, width = 8, height = 6, units = "in", dpi = 300)

} # Function definition ends



#  NUMBER OF SEASONS
plot_histogram(
  data = tmdb_data, 
  column_name = "number_of_seasons", 
  x_label = "Range of Season Count",
  y_label = "Number of TV Series",
  title = "Histogram of TV Series Season Count",
  type = 0)

# NUMBER OF EPISODES
plot_histogram(
  data = tmdb_data, 
  column_name = "number_of_episodes", 
  x_label = "Range of Episode Count",
  y_label = "Number of TV Series",
  title = "Histogram of TV Series Episode Count",
  type = 0)

# VOTE COUNT
plot_histogram(
  data = tmdb_data, 
  column_name = "vote_count", 
  x_label = "Vote Count",
  y_label = "Number of TV Series",
  title = "Histogram of TV Series Vote Count",
  type = 0)

# VOTE AVERAGE
plot_histogram(
  data = tmdb_data, 
  column_name = "vote_average", 
  x_label = "Vote Average",
  y_label = "Number of TV Series",
  title = "Histogram of TV Series Vote Average",
  type = 0)

# EPISODE RUN TIME
plot_histogram(
  data = tmdb_data, 
  column_name = "episode_run_time", 
  x_label = "Episode Run Time (in minutes)",
  y_label = "Number of TV Series",
  title = "Histogram of Runtime of TV Series Episodes",
  type = 0)

# PRODUCTION COMPANIES
plot_histogram(
  data = tmdb_data, 
  column_name = "production_companies", 
  x_label = "Production Company",
  y_label = "Number of TV Series",
  title = "Production Companies TV Series Portfolio Count",
  type = 1)

# GENRES
plot_histogram(
  data = tmdb_data, 
  column_name = "genres", 
  x_label = "Genre",
  y_label = "Number of TV Series",
  title = "Genres of TV Series",
  type = 1)

# LANGUAGES
plot_histogram(
  data = tmdb_data, 
  column_name = "languages", 
  x_label = "Language",
  y_label = "Number of TV Series",
  title = "Languages of TV Series",
  type = 1)

# NETWORKS
plot_histogram(
  data = tmdb_data, 
  column_name = "networks", 
  x_label = "Network",
  y_label = "Number of TV Series",
  title = "Dominant Networks of TV Series",
  type = 1)

# ORIGIN COUNTRY
plot_histogram(
  data = tmdb_data, 
  column_name = "origin_country", 
  x_label = "Origin Country",
  y_label = "Number of TV Series",
  title = "TV Series Origin Countries",
  type = 1)

# SPOKEN LANGUAGE
plot_histogram(
  data = tmdb_data, 
  column_name = "spoken_languages", 
  x_label = "Spoken Language",
  y_label = "Number of TV Series",
  title = "Spoken Languages of TV Series",
  type = 1)

# PRODUCTION COUNTRIES
plot_histogram(
  data = tmdb_data, 
  column_name = "production_countries", 
  x_label = "Production Country",
  y_label = "Number of TV Series",
  title = "TV Series Production Countries",
  type = 1)

# ORIGINAL LANGUAGE
plot_histogram(
  data = tmdb_data, 
  column_name = "original_language", 
  x_label = "Language",
  y_label = "Number of TV Series",
  title = "Original Language(ISO 639-1 Code) of TV Series",
  type = 2)

# STATUS
plot_histogram(
  data = tmdb_data, 
  column_name = "status", 
  x_label = "Status",
  y_label = "Number of TV Series",
  title = "Status of TV Series",
  type = 2)

# ADULT
plot_histogram(
  data = tmdb_data, 
  column_name = "adult", 
  x_label = "Adult Status",
  y_label = "Number of TV Series",
  title = "Adult Status of TV Series",
  type = 2)

# IN PRODUCTION
plot_histogram(
  data = tmdb_data, 
  column_name = "in_production", 
  x_label = "Production Status",
  y_label = "Number of TV Series",
  title = "Production Status of TV Series",
  type = 2)

# POPULARITY
plot_histogram(
  data = tmdb_data, 
  column_name = "popularity", 
  x_label = "Popularity",
  y_label = "Number of TV Series",
  title = "Popularity of TV Series",
  type = 2)

# TYPE
plot_histogram(
  data = tmdb_data, 
  column_name = "type", 
  x_label = "Series Type",
  y_label = "Number of TV Series",
  title = "Series Type of TV Series",
  type = 2)

