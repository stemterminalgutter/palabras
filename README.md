
<!-- README.md is generated from README.Rmd. Please edit that file -->

# palabras

<!-- badges: start -->

[![R-CMD-check](https://github.com/stemterminalgutter/palabras/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stemterminalgutter/palabras/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of palabras is to be a random word generator. It also can
create a wordcloud based off frequently used words. Finally, a word
game. For fun. No one will probably use this but myself and my
professor.

## Installation

You can install the development version of palabras like so:

    git clone https://github.com/stemterminalgutter/palabras.git

## Example

See below :

``` r
library(palabras)

word_path <- file.path(getwd(), "words_alpha.txt")
huge_word_list <- readLines(word_path)

generate_random_words <- function(n, huge_word_list) {
  sample(huge_word_list, n, replace = TRUE)
}

random_words <- generate_random_words(1, huge_word_list)
```

What is special about this random word generator? Nothing in particular.
But at least you can play a game if you get bored with life – also,
isn’t it great to play around with words you’ve probably never heard of?

``` r
print(random_words)
#> [1] "spoils"
```
