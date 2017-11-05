require(pdftools)
require(tiff)
require(png)
require(tesseract)
require(magrittr)
require(dplyr)
require(tidyr)
require(magick)
require(tm)
require(SnowballC)

import_ocr_doc <- function(full_pdf_url, page ){
  # given a PDF location (can be a local file or on hte web),
  # converts one page of the pdf to a bitmap and then performs Optical Character Recognition
  # using tesseract/magick libraries. Returns a text string
  
  bitmap <- pdftools::pdf_render_page(full_pdf_url, page=page, dpi = 400, numeric=TRUE) 
  
  ### bread-and-butter way
  # tiff::writeTIFF(bitmap, "C:/Users/Bonny/Documents/JFK/tiff/page.tiff")
  # png::writePNG(.,target = full_png_url, dpi=400) 
  # ocr_txt <- tesseract::ocr("C:/Users/Bonny/Documents/JFK/tiff/page.tiff")
  # cat(ocr_txt)
  
  # cleaning the bitmap with magick gives typically better results; 
  # magick::image_ocr is based on tesseract::ocr
  ocr_txt <- magick::image_read(bitmap) %>% #"C:/Users/Bonny/Documents/JFK/png/page.png") %>%
    magick::image_resize("2000") %>%
    magick::image_convert(colorspace = 'gray') %>% #assume all docs are black&white
    magick::image_trim() %>%
    magick::image_ocr() 

  return(ocr_txt)
}# end import_ocr_doc


remove_non_ascii <- function(raw_dat){
  # remove words with non-ASCII characters
  #
  # convert string to vector of words
  dat <- unlist( strsplit(raw_dat, " ") )
  # find indices of words with non-ASCII characters
  dat_nonascii_ind <- grep("NONASCII", iconv(dat, from="latin1", to="ASCII", sub="NONASCII"))
  # subset original vector of words to exclude words with non-ASCII char
  dat <- dat[-dat_nonascii_ind]
  # convert vector back to a string, ready to be converted to a TM corpus
  dat <- paste(dat, collapse = " ")
  
  return(dat)
  
}# end remove_non_ascii


remove_nonwords <- function(raw_dat){
  
  ### function called before creating the corpus to remove
  ### words that are clearly non-words like single-consonant words,
  ### consonant only words and similar gibberish.
  ### Main target are artifacts of the OCR that are still ASCII
  
  # convert string to vector of words
  dat <- unlist( strsplit(raw_dat, " ") )
  
  # find indices of words made by only one character; this character is a consonant
  dat_ind1 <- grep(pattern="^[^aeiouAEIOU]$", x=dat)
  #dat <- grep(pattern="\\b[^aeiouAEIOU]\\b", x=dat) ### this is equivalent to the previous one
  
  # indices of words made only by consonants and/or digits (2 or more letters)
  dat_ind2 <- grep(pattern="^[^aeiouAEIOU]{2,}$", x=dat)
  
  # remove elements matching the previous indexes
  dat <- dat[-c(dat_ind1, dat_ind2)]  
  # convert vector back to a string, ready to be converted to a TM corpus
  dat <- paste(dat, collapse = " ")
  
  return(dat)
}# end remove_nonwords



convert_special_to_space <- function(corpus){

  # function that converts a series of special characteers
  # to a space, using the generic content transformer of tm.
  # Notice that the conversion to a space is just a conveninece:
  # we could convert the spec characters to anything. 
  # However, using a blank is convenient because we can later
  # remove them with tm::stripWhiteSpace
  toSpace <- tm::content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  corpus <- tm_map(corpus, toSpace, "/")
  corpus <- tm_map(corpus, toSpace, "@")
  corpus <- tm_map(corpus, toSpace, "\\|")
  corpus <- tm_map(corpus, toSpace, "\\\\n") # "\n"
  
  return(corpus)
}


clean_corpus <- function( raw_corpus ){
  
  # Convert the text to lower case
  corpus <- tm::tm_map(raw_corpus, tm::content_transformer(tolower))
  # Remove numbers
  corpus <- tm::tm_map(corpus, tm::removeNumbers)
  
  #some special characters converted to blank white spaces
  corpus <- convert_special_to_space(corpus)
  
  # Remove punctuations
  corpus <- tm::tm_map(corpus, tm::removePunctuation)
  
  # Eliminate extra white spaces
  corpus <- tm::tm_map(corpus, tm::stripWhitespace)

  # Remove english common stopwords
  corpus <- tm_map(corpus, tm::removeWords, tm::stopwords("english"))
  
  # remove specific words 
  corpus <- tm_map(corpus, removeWords, c("dont", "say", "can", "just", "now"))  
  
  # Text stemming (reduces words to their root form)
  corpus <- tm::tm_map(corpus, stemDocument)
  
  
  return(corpus)
}# end clean_corpus





