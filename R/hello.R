word_path <- file.path(getwd(), "words_alpha.txt")
huge_word_list <- readLines(word_path)

# file.path(getwd(), "./words_alpha.txt")
#' Random Word Generator
#'
#' @param n The random word
#' @param huge_word_list
#'
#' @return The output from \code{\link{print}}
#' @export
#'
#' @examples
generate_random_words <- function(n, huge_word_list) {
  sample(huge_word_list, n, replace = TRUE)
}

random_words <- generate_random_words(1, huge_word_list)
print(random_words)


text <- readLines("~/Library/CloudStorage/OneDrive-IndianaUniversity/Getting Started in R/Final Biostats Project/crime_and_punishment.txt")

file.path(getwd(), "./crime_and_punishment.txt")
library(tm)
library(wordcloud)
library(ngram)
library(RColorBrewer)

#' Wordcloud That Displays the Most Frequently Used Word
#'
#' @param x The input string we want to process, like removing punctuation
#'
#' @return The output from \code{\link{wordcloud()}}
#' @export
#'
#' @importFrom tm removeWords stopwords
#' @import wordcloud
#' @importFrom ngram get.phrasetable
#' @import RColorBrewer
#' @examples
remove_custom_punctuation <- function(x) {
  x <- gsub("'", "", x)  # Remove apostrophes
  x <- gsub("[[:punct:]]", "", x)  # Remove other punctuation
  return(x)
}

# Combine lines into a single string
text <- paste(text, collapse = "")

# Convert text to lowercase
text <- tolower(text)

# Apply the custom punctuation removal function
text <- remove_custom_punctuation(text)

# Remove stopwords
text <- removeWords(text, stopwords("en"))

# Generate n-grams
ng1 <- ngram(text, n = 1)
ng2 <- ngram(text, n = 2)

# Create frequency tables
freq_table_1gram <- get.phrasetable(ng1)
freq_table_2gram <- get.phrasetable(ng2)

# Combine frequency tables
combined_freq_table <- rbind(freq_table_1gram, freq_table_2gram)

# Create the word cloud
wordcloud(words = combined_freq_table$ngrams,
          freq = combined_freq_table$freq,
          scale = c(3, 0.7),
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Paired"))

#' Fun Game Using Words from Random Word Generator
#'
#' @return The output from \code{\link{play_word_game}}
#' @export
#'
#' @examples
play_word_game <- function() {
  # Generate rando words
  random_words <- generate_random_words(3, huge_word_list)

  # Show the rando words
  cat("Here are your random words:\n")
  print(random_words)

  # Create your own sentence or story
  cat("\nCreate a sentence or story using these words:\n")
  user_input <- readline(prompt = "Your sentence or story: ")

  # Simple scoring system (or, ength of the sentence)
  score <- nchar(user_input)
  cat("\nYour score:", score, "\n")

  # Play again?
  play_again <- readline(prompt = "Do you want to play again? (yes/no): ")
  if (tolower(play_again) == "yes") {
    play_word_game()
  } else {
    cat("Thanks for playing!\n")
  }
}

# Start the game
play_word_game()

