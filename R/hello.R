huge_word_list <- readLines("~/Library/CloudStorage/OneDrive-IndianaUniversity/Getting Started in R/Final Biostats Project/words_alpha.txt")

file.path(getwd(), "./words_alpha.txt")
#' Random Word Generator
#'
#' @param n The random word
#' @param huge_word_list
#'
#' @returns The output from \code(\link(print))
#' @export
#'
#' @examples
generate_random_words <- function(n, huge_word_list) {
  sample(huge_word_list, n, replace = TRUE)
}

random_words <- generate_random_words(1, huge_word_list)
print(random_words)


text <- readLines("~/Library/CloudStorage/OneDrive-IndianaUniversity/Getting Started in R/Final Biostats Project/crime_and_punishment.txt")

# Combine lines into a single string
text <- paste(text, collapse = "")

# Convert text to lowercase
text <- tolower(text)

# Custom function to remove specific punctuation
remove_custom_punctuation <- function(x) {
  x <- gsub("'", "", x)  # Remove apostrophes
  x <- gsub("[[:punct:]]", "", x)  # Remove other punctuation
  return(x)
}

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
          scale = c(4, 0.5),
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Paired"),
          family= "serif")

