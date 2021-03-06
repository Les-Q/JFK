library(pdftools)
library(ggplot2)
library(tesseract)
library(magrittr)
#library(xlsx)
library(dplyr)
library(tidyr)
library(reshape2)
library(tm)
library(SnowballC)
library(wordcloud)
library(NLP)
library(textrank)
library(igraph)

if(! require(magick)){
  library(devtools)
  devtools::install_github("ropensci/magick")
}

#import helper functions

################# USER INPUTS AND SETTINGS ########################
curr_dor <- getwd()
# get directory of this script and move up by one dir; works only if you are sourcing the script
tryCatch({
  work_dir <- strsplit(dirname(sys.frame(1)$ofile),split='/')[[1]] 
  work_dir <- paste(work_dir[-length(work_dir)],collapse = '/')
}, error=function(e){
  default_work_dir <- "C:/Users/Bonny/Documents/Projects/JFK/"
  print(paste("You are probably running this in interactive mode Setting working dir to ", default_work_dir) )
  work_dir <- default_work_dir 
}# end error
)# end tryCatch

source(paste0(work_dir,"/R/JFK_functions.R"))

tmp_dir  <- paste0(work_dir,"/tmp/")
if(!dir.exists(tmp_dir)){
  dir.create(tmp_dir,recursive = TRUE, showWarnings = FALSE)
}

log_file_stage1 <- paste0(work_dir,"/log_JFK_STAGE1_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".txt")

base_url <- "https://www.archives.gov/files/research/jfk/releases/"  # HTTP URL where all pdfs are accessible
# range of docs to process
min_id <- 1 # set to 0 for startign since first doc in list
max_id <- 400 # set to Inf to process till the end of doc list


sink(log_file_stage1)

# xlsx does not work really great, IMHO; example: stringAsFactors not working
#doc_list <- xlsx::read.xlsx2("C:/Users/Bonny/Documents/JFK/jfkrelease-2017-dce65d0ec70a54d5744de17d280f3ad2-nolinks.xlsx",
#                             sheetIndex=1, stringsFactors=FALSE) %>% tbl_df()

doc_list <- read.csv(paste0(work_dir,"/data/jfkrelease-2017-dce65d0ec70a54d5744de17d280f3ad2-nolinks.csv"),
                     stringsAsFactors = FALSE) %>% tbl_df()
doc_list <- doc_list %>% mutate(File.Name = tolower(File.Name),
                                Comments = toupper(Comments),
                                NARA.Release.Date = as.POSIXct(NARA.Release.Date, format="%m/%d/%Y"),
                                Doc.Date = as.POSIXct(Doc.Date, format="%m/%d/%Y")) %>%
  mutate(Doc.Date = as.POSIXct( ifelse(Doc.Date=="0000-01-01",NA, Doc.Date), origin='1970-01-01') ) %>%
  mutate(Doc.Type = gsub('[[:punct:]]+','',Doc.Type)) %>%
  mutate(Doc.Type = gsub('  +',' ',Doc.Type)) 

old_cols <- names(doc_list)
doc_list$Doc.Index <- seq(1, nrow(doc_list),1)
doc_list <- doc_list[,c('Doc.Index',old_cols)]
unique(doc_list$Doc.Type)

#add more columns with metadata and flags
doc_list$Conv.Flag <- 0
doc_list$Memo.Flag <- 0
doc_list$Report.Flag <- 0
doc_list$Imported.Pages <- as.integer(NA)
doc_list$Import.Time.Sec <- as.integer(NA) 

#do not process handwritten notes
handwritten_mask <- grepl('HANDWRITTEN', doc_list$Comments) # | grepl('NOTES', doc_list$Doc.Type) 


#### create tbl_df where to store outputs

doc_raw_txt <- data.frame(Doc.Index=doc_list$Doc.Index,
                          First.Page = as.character(NA),
                          Raw.Body.Text = as.character(NA),
                          Body.Text = as.character(NA),
                          stringsAsFactors = FALSE) %>% tbl_df()


doc_kw <- data.frame(Doc.Index=doc_list$Doc.Index,
                     Keyword = as.character(NA),
                     Count = as.integer(NA) ,stringsAsFactors = FALSE) %>% tbl_df()

doc_list_file <- paste0(work_dir,"/doc_list_tmp.rds")
doc_raw_file <- paste0(work_dir,"/doc_rawtext_tmp.rds")
####
####  STAGE 1: import and OCR ####
####  LOOP OVER ALL DOCS
print(paste("Starting to loop over documents at ",Sys.time()))

for (id in doc_list$Doc.Index[!handwritten_mask]){
  t0 <- Sys.time()
  ### process only docs in the range specified by user
  if( (id< min_id )|(id > max_id)){
    next
  }
  
  doc_list_tmp = doc_list %>% filter(Doc.Index==id)
  print(paste0("ID=",id,"  Accessing document ",doc_list_tmp$File.Name," of type ",doc_list_tmp$Doc.Type," , NumPages=",doc_list_tmp$Num.Pages," , dated as ",doc_list_tmp$Doc.Date))
  pdf_url <- paste0(base_url,doc_list$File.Name[id])
  
  ### download the original pdf, extract basic metadata
  local_pdf = paste0(tmp_dir,"/doc_tmp.pdf")
  download.file(pdf_url, destfile = local_pdf, method='auto', cacheOK = FALSE, mode='wb' )
  doc_info <- pdftools::pdf_info(local_pdf)
  n_pages <- doc_info$pages
  
  
  ###  import the text, save raw values to the appropriate tibble
  ### import_ocr_doc defined in helper functions file import_df_ocr_functions.R
  print("importing first page")
  first_page <- import_ocr_doc(local_pdf, 1) 
  doc_list[id, "Conv.Flag"]   <- grepl(pattern = "CONVERSATION",x = toupper(first_page) )
  doc_list[id, "Report.Flag"] <- grepl(pattern = "REPORT",x = toupper(first_page) )
  doc_list[id, "Memo.Flag"]   <- grepl(pattern = "MEMO",  x = toupper(first_page) )
  
  if(n_pages<2){
    warning(paste0("ID=",id,"   Document has ",n_pages," pages. Skipping to the next doc."))
  }
  
  ### loop through pages, OCR them one-by-one
  raw_body_text <- as.character(NA)
  for(iP in seq(2,n_pages,1) ){
    cat(paste("\rPage",iP,"of",n_pages,"             "))
    tmp_txt <- import_ocr_doc(local_pdf, iP) 
    raw_body_text <- paste(raw_body_text,tmp_txt, sep = " ")
  }# end loop on pages of pdf
  
  # save raw imported text to df
  doc_raw_txt[doc_raw_txt$Doc.Index==id,'First.Page'] <- first_page
  doc_raw_txt[doc_raw_txt$Doc.Index==id,'Raw.Body.Text'] <- raw_body_text

  ### keep track of processing time
  delta_t <- difftime(Sys.time(), t0, units="secs")
  writeLines(paste0("ID=",id," processed in ",sprintf("%.1f",as.numeric(delta_t))," seconds\n" ))
  doc_list[doc_list$Doc.Index==id, 'Imported.Pages'] <- n_pages
  doc_list[doc_list$Doc.Index==id, "Import.Time.Sec"] <- round(delta_t) 
  
  ### wrap up and move to next document
  print(paste("Saving outputs to tmp caches in ",work_dir) )
  saveRDS(doc_list, file = doc_list_file)
  saveRDS(doc_raw_txt, file = doc_raw_file)
  
}#end for loop over id (loop over list of docs)

print(paste("Finished to loop over documents at ",Sys.time()))
print(paste("All documents imported and saved to file ",doc_raw_file) )
print(paste("Imported a total of ", sum(doc_list$Imported.Pages, na.rm=T)," pages. Average # of pages per document: ", sprintf("%.3f",mean(doc_list$Imported.Pages, na.rm=T))))
####


sink()

### a chart for performance monitoring: showing the import time vs the number of pages

p <- ggplot(data = doc_list) +
     geom_point(aes(x=Imported.Pages, y=Import.Time.Sec), colour='royalblue', size=2)  +
     scale_x_continuous(limits = c(0.0,120.0))+ 
     labs(title="Summary of OCR import performances", x="No. of pages imported", y="Total time [sec]")
print(p)

stop("Terminating process after STAGE 1")

### some special ones, in particular no. 356 has 409 pages
sel_id <- c(70, 77, 78, 356)

doc_df <- readRDS(doc_raw_file)
doc_df <- doc_df %>% dplyr::filter(!is.na(Raw.Body.Text) )
####
#### STAGE 2: pre-processing ######
####

print("Pre-processing text")
### loop on raw body text and for each one clean up text 
for(id in dplyr::filter(doc_df, !is.na(Raw.Body.Text))$Doc.Index){
  raw_body_text <- tolower( (doc_df[id, ])$Raw.Body.Text )
  doc <- remove_non_ascii(raw_body_text)
  doc <- remove_nonwords(doc)
  doc <- replace_special_words( doc )
  doc_df[id, 'Body.Text'] <- doc
}
doc_txt_file <- paste0(work_dir,"/doc_text.rds")
saveRDS(doc_df, file = doc_txt_file)


### create a TM corpus from the pre-cleaned texts and then apply further processing 
### to get rid of unwanted/unnecessary features 
print("Creating a corpus from data.frame")
doc_df <- dplyr::inner_join( x = doc_list %>% dplyr::select(Doc.Index, Title, Doc.Date, Doc.Type, Record.Series, Originator) , 
                                  y = doc_df %>% dplyr::filter(!is.na(Body.Text)) %>% dplyr::select(Doc.Index, Body.Text) ,
                                  by= 'Doc.Index') %>% dplyr::mutate(Doc.Date= format(Doc.Date,"%Y-%m-%d"))
corpus_map <- list(content = 'Body.Text', heading='Title', date='Doc.Date', id='Doc.Index', origin='Originator' , series='Record.Series', type='Doc.Type')
df_doc_reader <- tm::readTabular(mapping = corpus_map)

doc_corpus_raw <- tm::VCorpus(DataframeSource(as.data.frame(doc_df) ), readerControl = list(reader = df_doc_reader))

### you can inspect the metadata for a given document in this way
#meta(doc_corpus[[65]])

### you can filter on metadata by creating an index and then applying it to the corpus
# sel_date_idx <- unlist( lapply( meta(doc_corpus, "date") , FUN=function(x){ return( as.POSIXct(x) > as.POSIXct("1974-01-01") ) }) )
# sel_orig_idx <- unlist( lapply( meta(doc_corpus, "origin"), FUN=function(x){return(  x== 'WH')}) ) 
# sel_corpus <- doc_corpus[sel_cod_idx & sel_orig_idx]

### exactly the same thing using the tm_filter function
# sel_corpus <- tm::tm_filter(doc_corpus, 
#                           FUN = function(x){
#                             tmp_date<- as.POSIXct(meta(x)[['date']])
#                             tmp_orig <- meta(x)[['origin']]
#                             return( (tmp_orig=='WH') & (tmp_date> as.POSIXct('1974-01-01')) ) })
# 
# if(! check_corpus_non_empty(sel_corpus) ){ #check_corpus_non_empty defined in JFK_functions.py
#   warning("Warning! the selected subset of the corpus is empty!")
# }


### be aware that some pre-defined words are removed automatically inside the clean_corpus functions,
### regardless of what you list in here
excluded_words <- c('page', 'docid', 'made','stated', 'case', 'general',
                    'plans','will','report','memorandum', 'date', 'time',
                    'state','library', 'may', 'two', 'yes', 'called','none',
                    'taken', 'name','individual', 'group', 'subject', 'office', 'know')

# clean up the corpus. Function clear_corpus in helper file JFK_functions.R
doc_corpus <- clean_corpus(doc_corpus_raw, stemming=FALSE, 
                           excl_words= excluded_words  )


#This tells R to treat your preprocessed documents as text documents.
doc_corpus <- tm::tm_map(doc_corpus, tm::PlainTextDocument)

# if you want you can print the text of the n-th document in the corpus
# writeLines(as.character(doc_corpus[19]))


####
#### STAGE 3: analysis ####
####

#tdm ia matrix with as many rows as distinct words (terms) and as many cols as documents in the corpus
tdm <- tm::TermDocumentMatrix(doc_corpus)
m <- as.matrix(tdm) 
colnames(m) <- doc_df$Doc.Index

#reorder matrix rows by frequency
wrdc <- sort(rowSums(m),decreasing=TRUE) 
d <- data.frame(word = names(wrdc),word_count=wrdc)
head(d, 20) # most frequent words
#tail(d, 20) #least frequent words

### create a word cloud chart out of the 50 most frequent terms 
set.seed(13522)
png(paste0(work_dir,"/JFK_wordcloud_top50.png"),840,680,"px")
wordcloud(words = d$word, freq = d$word_count, min.freq = 30,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()



#findAssocs(tdm, terms = c("sac", "espjhinosa"), corlimit = c(0.4,0.5))
#saveRDS(doc_kw, file = paste0(work_dir,"/doc_keywords.rds"))


### remove sparse terms from the tdm 
tdm_nosparse <- removeSparseTerms(tdm, 0.98)
m_nosparse <- as.matrix(tdm_nosparse) 
colnames(m_nosparse) <- doc_df$Doc.Index
wrdc_nosparse <- sort(rowSums(m_nosparse),decreasing=TRUE) 
d_nosparse <- data.frame(word = names(wrdc_nosparse),word_count=wrdc_nosparse)
head(d_nosparse, 20) # most frequent words
set.seed(13522)
png(paste0(work_dir,"/JFK_wordcloud_top50_nonsparse.png"),840,680,"px")
wordcloud(words = d_nosparse$word, freq = d_nosparse$word_count, min.freq = 30,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()


### do it again with a TF-IDF: the frequency of each distinct term is weighted by the inverse of document frequency
### (i.e., the log of the ratio btw the total number of documents and the number of documents containing that term)
###
### This emphasizes terms that appear frequently in only one or few documents, the idea being that these terms
### summarize better the entrie corpus. Because of this, I removed the cleaning of sparse terms, it does not
### make sense to run a TF-IDF and then remove sparse terms.
###
### However, I don't think this is the right approach in this case, for two reason:
###  -) we have gazillions of spurious words, as well interview trasncripts where the name of the 
###     of the witness is repeated gazillions of times
###  -) the aim of the analysis is not to look for the words that summarize the cirpus doc by doc,
###     rather to find connection and ideas across the entire corpus. If a term is central to the
###     entire corpus, TF-IDF will suppress it.
tdm2 <- tm::TermDocumentMatrix(doc_corpus, control=list(weighting= function(x){tm::weightTfIdf(x,normalize=TRUE)} ))
m2 <- as.matrix(tdm2) 
colnames(m2) <- doc_df$Doc.Index
wrdc2 <- sort(rowSums(m2),decreasing=TRUE) 
print(wrdc2[1:5])
d2 <- data.frame(word = names(wrdc2),word_count=wrdc2)
head(d2, 20) # most frequent words
set.seed(13522)
png(paste0(work_dir,"/JFK_wordcloud_top50_TFIDF.png"),840,680,"px")
wordcloud(words = d2$word, freq = d2$word_count, min.freq = 30,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

#findAssocs(tdm2, terms = c("sac", "pci"), corlimit = c(0.4,0.4))
#m2['espinosa', which(m2['espinosa',]>0.0)]


### here you run the proper ML analysis


### Approach #1: text graph
### starting example: https://rpubs.com/ivan_berlocher/79860


### start from raw vector of terms. reimport it from the doc_corpus
### in order to use the cleaning done in the Corpus creation
all_terms <- unname( unlist( lapply( lapply(doc_corpus, as.character) , FUN=strsplit, split=" " )) )
all_sentences <-  unname( unlist(lapply(doc_corpus, as.character)) )


### run NLP over the corpus in order to tag terms by synctatic role (noun, adjective, verb, etc.)
### compared to the example linked above, we will treat each document as it was a unique sentence.
print("Associating to every word in the corpus a Part-Of-Speech tag. Please be patient...")
list_pos_tag <- tag_POS(all_sentences, 
                        POS_whitelist = c('NN', 'JJ','VB','RB')) #keep only nouns, adjectives, verbs and adverbs
POS_tagged_docs <- list_pos_tag$POStagged
POS_tagged_terms <- list_pos_tag$POStags
rm(list_pos_tag)
print(paste("POS tagging completed successfully. ", nrow(POS_tagged_terms)," words remaining (after filters on POS tags)") )

#### word cloud again, but this time after inner join with the tagged terms.
### this will further get rid of crap and noise. Anyway, exactly because 
### poorly OCR'ed words and other noise is random, the sparse removal will
### already do most of the job. Likely to bring to very similar if not equal result.
m_tagged <- m_nosparse[intersect(rownames(m_nosparse), POS_tagged_terms$word), ]

#reorder matrix rows by frequency
wrdc_tagged <- sort(rowSums(m_tagged),decreasing=TRUE) 
d_tagged <- data.frame(word = names(wrdc_tagged),word_count=wrdc_tagged)
#head(d_tagged, 20) # most frequent words

### create a word cloud chart out of the 50 most frequent terms 
set.seed(13522)
png(paste0(work_dir,"/JFK_wordcloud_top50_tagged.png"),840,680,"px")
wordcloud(words = d_tagged$word, freq = d_tagged$word_count, min.freq = 30,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()




### nota: puoi fare un super graph con mille mila vertici e ancora di piu' bordi
### pero' forse sarebbe meglio un graph avente le connessioni solo per un gruppo
### interessante di parole, tipo le top 20 trovate precedentemente. 
word_graph <- create_text_graph(POS_tagged_terms$word, 
                  search_window=10, # for each vertex will add 20 edges (search_window*2)
                  key_words=as.character( head(d_tagged[,1], 20) ), key_words_edge_mod = 5.0) 


##doc_list%>%filter(Doc.Index==360)%>%print(width=Inf)

### Approach #2: key phrase extraction with textrank
