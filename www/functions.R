library(shiny)
library(ggplot2)


jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'

getAllWords <- function(middleLetter, outerLetters, words) {
  returnList <- words[grepl(middleLetter, words)]
  excludeLetters <- setdiff(LETTERS, c(middleLetter, outerLetters))
  pattern <- paste0("[", paste(excludeLetters, collapse = ""), "]+")
  returnList <- returnList[!grepl(pattern, returnList)]
  return(returnList)
}

createWordPlot <- function(middleLetter, outerLetters, words) {
  letters <- c(middleLetter, outerLetters)
  longestWord <- max(nchar(words))
  letterDF <- data.frame(letter = letters)
  letterDF$totalWords <- NA
  for(i in 1:nrow(letterDF)) {
    letter <- letterDF$letter[i]
    letterWords <- words[grepl(paste0("^", letter), words)]
    letterDF$totalWords[i] <- length(letterWords)
  }
  
  letterDFLength <- expand.grid(letters, 4:longestWord)
  names(letterDFLength) <- c("letter", "wordLength")
  letterDFLength$totalWords <- NA
  for (i in 1:nrow(letterDFLength)){
    letter <- letterDFLength$letter[i]
    length <- letterDFLength$wordLength[i]
    letterWords <- words[grepl(paste0("^", letter), words)]
    lengthWords <- letterWords[nchar(letterWords) == length]
    if (length(lengthWords) == 0) {
      letterDFLength$totalWords[i] <- NA
    } else {
      letterDFLength$totalWords[i] <- length(lengthWords)
    }
  }
  
  p <- ggplot(letterDFLength)+
    geom_tile(aes(x = wordLength, y = letter,
                  fill = totalWords), width = 0.9,
              height = 0.9) +
    theme_bw(base_size = 30) +
    labs(x = "Number of Letters",
         y = element_blank()) +
    theme(panel.grid = element_blank(),
          legend.position = "none") +
    scale_x_continuous(breaks = 4:longestWord) +
    scale_fill_gradient2(low = "lightblue", mid = "gold", high = "red",
                         na.value = "green4") +
    geom_label(aes(x = wordLength, y = letter,
                   label = totalWords), size = 9, alpha = 0.5)
  
  return(p)
}

#createWordPlot(middleLetter, outerLetters, wordtemp)


