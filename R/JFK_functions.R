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
  tesseract_options = list(  gapmap_no_isolated_quanta='1', #segment_debug='1',
                             #textord_heavy_nr='1', # not very effective
                             textord_words_default_nonspace='0.5', 
                             classify_min_slope='0.2', 
                             textord_xheight_error_margin='0.5',
                             textord_max_noise_size='7', #def = 7
                             words_default_fixed_limit='0.3')
  #textord_max_noise_size='4') #,tessedit_char_whitelist = "0123456789")  textord_heavy_nr='1'
  ocr_txt <- magick::image_read(bitmap) %>% #"C:/Users/Bonny/Documents/JFK/png/page.png") %>%
    magick::image_resize("2000") %>%
    magick::image_background(color="white", flatten = TRUE) %>%
    magick::image_convert(colorspace = 'gray',  antialias=TRUE) %>% #assume all docs are black&white
    # magick::image_despeckle(., times = 4) %>% # not very effective
    magick::image_modulate(saturation=200 , brightness = 80 ) %>%
    magick::image_contrast(sharpen = 10) %>%
    magick::image_convolve(kernel = "LoG:0x2", scaling="300%,100%") %>%
    #magick::image_convolve(kernel = "Diamond", scaling="100%!") %>%
    #magick::image_enhance(.) %>%# not very effective
    magick::image_trim() 
  
  #print(ocr_txt)
  
  ocr_txt <- ocr_txt %>%
    magick::image_ocr(options=tesseract_options) 
  
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
  
  # indices of words with 5 or more consonants in a row
  dat_ind3 <- grep(pattern="^[^aeiouAEIOU]{5,}[[:alnum:]]*", x=dat)
  dat_ind4 <- grep(pattern="^[[:alnum:]]*[^aeiouAEIOU]{5,}$", x=dat)
  
  # other weird letter patterns that are most likely incompatible with any meaningful
  # English word or name
  forbidden_patterns <- c("g[bcfvx]","z[cfgsvx]","t[bdvx]" )
  for (fp in forbidden_patterns){
    tmp_ind5 <- grep(pattern = paste0(fp,"+"), x=dat) 
    if(fp==forbidden_patterns[1]){
      dat_ind5 <- tmp_ind5
    }else{
      dat_ind5 <- c(dat_ind5, tmp_ind5)
    }
  }# end for loop
  
  
  # remove elements matching the previous indexes
  ind_remove <- sort( unique( c(dat_ind1, dat_ind2, dat_ind3, dat_ind4, dat_ind5) ) )
  dat <- dat[-ind_remove]  
  # convert vector back to a string, ready to be converted to a TM corpus
  dat <- paste(dat, collapse = " ")
  
  return(dat)
}# end remove_nonwords

replace_special_words <- function(raw_dat){
  
  ### function called in the first phase of pre-processing
  ### It replaces some words of text with others.
  ### Useful especially with names, uniforming them to a standard.
  ### Example: 'Harvey Lee Oswald' is replaced everywhere with 'Oswald'
  ### In this way, we don't spread into two different counts the occurrencies of
  ### 'Harvey Lee Oswald' and 'oswald', consolidating the frequency
  ### of referring to Oswald. 
  ###
  ### Notice that the order is important! First we replace 'Lee Oswald' with Oswald
  ### and then we replace 'Harvey Oswald' with Oswald. This will cover automatically
  ### the case of 'Harvey Lee Oswald'
  ###
  ### The regex [[:print:]] instead of spaces will cater for any blip in OCR
  ### that introduced spurious characters in the text 
  ### [[:print:]] ---> Printable characters ([[:alpha:]], [[:punct:]] and space)
  ### The usage of the wild card'*' in conjunction with [[:print:]] is deemed
  ### to be too risky, it can runcate wide portions of text. For safety, 
  ### limit to a max of 4 the number of characters in between the key words.
  ###
  
  dat <- gsub("lee[[:print:]]{1,4}oswald", "oswald", raw_dat)
  dat <- gsub("harvey[[:print:]]{1,4}oswald", "oswald", dat)
  dat <- gsub("[[:space::]]h[[:print:]]{1,2}l?[[:print:]]{1,3}oswald", "oswald", dat)
  
  dat <- gsub("gerald[[:print:]]{1,4}ford", "ford", dat)
  dat <- gsub("fitzgerald[[:print:]]{1,4}Kennedy", "kennedy", dat)
  dat <- gsub("john[[:print:]]{1,4}kennedy", "kennedy", dat)
  dat <- gsub("[[:space::]]j[[:print:]]{1,2}f?[[:print:]]{1,3}kennedy", "kennedy", dat)
  dat <- gsub("henry[[:print:]]{1,4}kissinger", "kissinger", dat)
  dat <- gsub("fidel[[:print:]]{1,4}castro", "castro", dat)
  
  dat <- gsub("robert[[:print:]]{1,4}edwards", "edwards", dat)
  dat <- gsub("[[:space::]]r[[:print:]]{1,4}edwards", "edwards", dat)
  dat <- gsub("robert[[:print:]]{1,2}e?[[:print:]]{1,3}edwards", "edwards", dat)
  
  dat <- gsub("federal[[:print:]]{1,4}bureau[[:print:]]{1,4}investigation", "fbi", dat)
  dat <- gsub("federal[[:print:]]{1,4}bureau[[:print:]]{1,4}of[[:print:]]{1,4}investigation", "fbi", dat)
  dat <- gsub("centtal[[:print:]]{1,4}intelligence[[:print:]]{1,4}agency", "cia", dat)
  
  ### other misspelled / poorly imported words
  dat <- gsub("cmmunicationshave", "communications have", dat)
  dat <- gsub("communicatiohshave", "communications have", dat)
  dat <- gsub("hayessecurty", "high security", dat)
  dat <- gsub("cancerning", "concerning", dat)
  dat <- gsub("joee", "joe", dat)
  dat <- gsub("dallasfile","dallas file",dat)
  dat <- gsub("dallasv","dallas",dat)
  dat <- gsub("francsco","francisco",dat)
  dat <- gsub("victimcivil","victim civil",dat)
  dat <- gsub("withthe", "with the", dat)
  dat <- gsub("leeoswald", "oswald", dat)
  dat <- gsub("naziparty", "nazi party", dat)
  
  return(dat)
  
}# end replace_special_words

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


clean_corpus <- function( raw_corpus , stemming=FALSE, excl_words=NA){
  
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
  words_blacklist <- c("dont", "say", "can", "just",  
                       "now", "made", "one", "said", "also")
  if(length(excl_words[!is.na(excl_words)])>0){
    if(is.character(excl_words)){
      words_blacklist <- c(words_blacklist, excl_words)
    }
    else{
      warning("WARNING from clean_corpus! Input argument excl_words is not of class 'character'. It will be ignored.")
    }
  }
  corpus <- tm_map(corpus, removeWords, words_blacklist)  
  
  # fix some words misspelled, / poorly imported
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "admieistrative", replacement = "administrative")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "sepaxate", replacement = "separate")
  
  # this is going to merge two words that we want to consider as one
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "new orleans", replacement = "new_orleans")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "new york", replacement = "new_york")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "los angeles", replacement = "los_angeles")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "san francisco", replacement = "san_francisco")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "san antonio", replacement = "san_antonio")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "attorney general", replacement = "attorney_general")
  
  
  
  
  if(stemming){
    # Text stemming (reduces words to their root form). Uses the SnowballC package
    corpus <- tm::tm_map(corpus, stemDocument)
  }
  
  return(corpus)
}# end clean_corpus


check_corpus_non_empty <- function(c){
  
  ### Noticed that after filtering on metadata, the corpus might be empty
  ### although of size >0, i.e. the length of the corpus is 1 but the length of the first
  ### (and only) element of the corpus is zero. This function flags cases like this.
  ### Input argument is an object of class Corpus
  
  non_empty <- NA
  
  if(length(c)<1){
    non_empty <- FALSE
  }else if(length(c)==1){
    if(length(c[[1]]) < 1 ){
      non_empty <- FALSE    
    }else{
      non_empty <- TRUE
    }
    
  }else{
    non_empty <- TRUE    
  }
  
  return(non_empty)  
}### end check_corpus_nonempty

##### MACHINE LEARNING SPECIFIC FUNCTIONS ######




tag_POS <-  function( untagged_sentences, POS_whitelist=NULL, POS_blacklist=NULL) {
  
  ### Performs Part-of-Speech tagging of sentences
  ### see https://en.wikipedia.org/wiki/Part-of-speech_tagging
  ###
  ### takes as input a character vector, each element is a whole untokenized sentence.
  ### Converts it to a NLP::String
  ### Runs openNLP functions to annotate each term 
  ### and add a POS tag (noun, adjective, verb...).
  ###
  ### 
  
  require(NLP)
  require(openNLP)
  
  n_sentences <- length(untagged_sentences)
  # add 3 because when we paste and collapse, we add the three characters "|. "
  nchar_sentences <- cumsum( nchar(untagged_sentences) ) +3
  start_sentences <- c(1, nchar_sentences[-n_sentences])
  end_sentences <- c( nchar_sentences)
  all_sentences <-  paste( paste( untagged_sentences ,collapse="|. "), "|. ")
  
  s <- NLP::as.String(all_sentences)
  rm(all_sentences)
  word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
  annotation1 <- NLP::Annotation(1:n_sentences, rep("sentence", n_sentences), start_sentences, end_sentences)
  annotation1 <- NLP::annotate(s, word_token_annotator, annotation1)
  annotation2 <- NLP::annotate(s, openNLP::Maxent_POS_Tag_Annotator(), annotation1)
  annotation2w <- annotation2[annotation2$type == "word"]
  POStags <- lapply(annotation2w$features, '[[', "POS")
  
  s <- s[annotation2w]
  if (!is.null(POS_whitelist) ){
    thisPOSindex <- integer()
    for(iP in POS_whitelist){
      tmpPOSindex <- grep(iP, POStags)
      thisPOSindex <- c(thisPOSindex , tmpPOSindex)
    }
    s <- s[thisPOSindex]
    POStags <- POStags[thisPOSindex]
  }

  if (!is.null(POS_blacklist) ){
    thisPOSindex <- integer()
    for(iP in POS_blacklist){
      tmpPOSindex <- grep(iP, POStags)
      thisPOSindex <- c(thisPOSindex , tmpPOSindex)
    }
    s <- s[-thisPOSindex]
    POStags <- POStags[-thisPOSindex]
  }
  
    
  POStagged <- paste(sprintf("%s/%s", s, POStags), collapse = " ")
  return( list(POStagged = POStagged, POStags = POStags) )
  
}### end tagPOS


filter_words_POS <- function(all_terms, filter){
  filtered <- all_terms
  return( filtered)
}


get_word_links <- function(all_words, position, window, min_links=2) {
  ### all_words is a tokenized set of word, i.e. a character vector with all the words 
  ### I want to add in the graph.  
  ### position is the index in all_terms of the word you are interested to.
  ### width_window is a symmetric window around position of how many words before/after
  ### position to consider.
  ###
  
  stopifnot(min_links>0)
  start_window <- ifelse(position-window < 1,1,position - window)
  end_window <- ifelse(position+window>length(all_terms),length(all_terms),position + window)
  links <- all_words[start_window, end_window]
  
  ### not yet implemented: keep as edges only certain types of words
  ### links <- filter_words_POS(links, my_filters)
  
  if (length(links)>=min_links) {
    links[min_links:length(links)]
  }
  else {
    links <- ""
  }
  return(links)
}### end get_word_links

create_text_graph <- function(all_words, search_window) { 
  
  ### all_words is a tokenized set of word, i.e. a character vector with all the words 
  ### I want to add in the graph.  
  
  require(igraph)
  word_graph <- igraph::graph.empty(0, directed=FALSE)
  i <- 1
  n_terms <- length(all_terms)

  ### we loop over all the words in the list 
  while (i < n_terms ) {
    
    if (i%%1000==1){
      print(paste("Creating vertexes and edges for word ",i," / ",n_terms))
    }
    ### First we collect the link words, with the definition of
    ### 'link' as a word within a window around the word under scrutiny.
    ### The width of the search window is defined by the user.
    links <- get_word_links(i,search_window)                                
    if (links[1] != "") {                                     
      #cat(i," ",words[i]," - ",paste(c(links),collapse=" "),"\n")
      
      ### If it is the first time we encounter the word under scrutiny, we add it
      ### to the list of vertices of the graph.
      if ( length(which(V(word_graph)$name==all_words[i]))==0  ) {     
        word_graph <- word_graph + igraph::vertices(all_words[i])
      }                                               
      
      ### for each link word, we:
      ###    - add the link to the list of vertices, if not already present
      ###    - if the link word is not in the list of edges departing 
      ###      from the node of the word under scrutiny, we add it to the 
      ###      list of edges and assign a weight=1 to that edge
      ###    - conversely, if the link word is already in the list of edges,
      ###      increment by 1 the weight for that edge
      for (j in 1:length(links)) {
        if ( length(which(V(word_graph)$name==links[j]))==0 ) {
          word_graph <- word_graph + igraph::vertices(links[j])
          word_graph <- word_graph + igraph::edges(c(words[i],links[j]),weight=1)
        }else {
          if ( igraph::are.connected(word_graph, all_words[i], links[j])) { 
            my_edge <- E(word_graph, c(all_words[i], links[j]) )
            prev_edge_weight <- as.numeric(edgeData(word_graph,words[i],links[j],"weight"))
            word_graph <- set_edge_attr(word_graph,name = 'weight',
                                        index = my_edge ,value= prev_edge_weight+1)
            
          } else {
            word_graph <- word_graph + igraph::edges(c(words[i],links[j]),weight=1)
          }
        } 
      }# end for loop over j (links)
    }
    i <- i+1
  }#end while loop over i (words)
  return( word_graph )
}#end create_text_graph






