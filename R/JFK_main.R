library(pdftools)
library(tesseract)
library(magrittr)
#library(xlsx)
library(dplyr)
library(tidyr)
library(tm)
library(SnowballC)

if(! require(magick)){
  library(devtools)
  devtools::install_github("ropensci/magick")
}


################# USER INPUTS AND SETTINGS ########################

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

tmp_dir  <- paste0(work_dir,"/tmp/")
if(!dir.exists(tmp_dir)){
  dir.create(tmp_dir,recursive = TRUE, showWarnings = FALSE)
}

#import helper functions
source(paste0(work_dir,"/R/JFK_functions.R"))

base_url <- "https://www.archives.gov/files/research/jfk/releases/"  # HTTP URL where all pdfs are accessible
# range of docs to process
min_id <- 1 # set to 0 for startign since first doc in list
max_id <- 100 # set to Inf to process till the end of doc list


###################################################################

# xlsx does not work really great, IMHO; example: stringAsFactors not working
#doc_list <- xlsx::read.xlsx2("C:/Users/Bonny/Documents/JFK/jfkrelease-2017-dce65d0ec70a54d5744de17d280f3ad2-nolinks.xlsx",
#                             sheetIndex=1, stringsFactors=FALSE) %>% tbl_df()

doc_list <- read.csv(paste0(work_dir,"/data/jfkrelease-2017-dce65d0ec70a54d5744de17d280f3ad2-nolinks.csv"),
                     stringsAsFactors = FALSE) %>% tbl_df()
doc_list <- doc_list %>% mutate(File.Name = tolower(File.Name),
                                Comments = toupper(Comments),
                                NARA.Release.Date = as.POSIXct(NARA.Release.Date, format="%m/%d/%Y"),
                                Doc.Date = as.POSIXct(Doc.Date, format="%m/%d/%Y")) %>%
  mutate(Doc.Date = as.POSIXct( ifelse(Doc.Date=="0000-01-01",NA, Doc.Date)) ) %>%
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

##########################################
####  STAGE 1: import and OCR
####  LOOP OVER ALL DOCS
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
    print(paste("Page",iP,"of",n_pages))
    tmp_txt <- import_ocr_doc(local_pdf, iP) 
    raw_body_text <- paste(raw_body_text,tmp_txt, sep = " ")
  }# end loop on pages of pdf
  
  # save raw imported text to df
  doc_raw_txt[doc_raw_txt$Doc.Index==id,'First.Page'] <- first_page
  doc_raw_txt[doc_raw_txt$Doc.Index==id,'Raw.Body.Text'] <- raw_body_text

  ### keep track of processing time
  delta_t <- difftime(Sys.time(), t0, units="secs")
  writeLines(paste0("ID=",id," processed in ",sprintf("%.1f",as.numeric(delta_t))," seconds\n" ))
  doc_list[id, "Import.Time.Sec"] <- round(delta_t) 
  
  ### wrap up and move to next document
  print(paste("Saving outputs to tmp caches in ",work_dir) )
  saveRDS(doc_list, file = paste0(work_dir,"/doc_list_tmp.rds"))
  saveRDS(doc_raw_txt, file = paste0(work_dir,"/doc_rawtext_tmp.rds"))
  
}#end for loop over id (loop over list of docs)

print(paste("Finished to loop over documents at ",Sys.time()))
##############################

stop("Terminating process after STAGE 1")

###################################
#### STAGE 2: pre-processing

### loop on raw body text and for each one clean up text 

doc <- remove_non_ascii(raw_body_text)
doc <- remove_nonwords(doc)

saveRDS(doc_list, file = paste0(work_dir,"/doc_list.rds"))
saveRDS(doc_raw_txt, file = paste0(work_dir,"/doc_rawtext.rds"))


### create a TM corpus from the pre-cleaned texts and then apply further processing 
### to get rid of unwanted/unnecessary features 
doc <- tm::Corpus(tm::VectorSource(strsplit(doc, " ") ) )

# clean up the corpus. Function clear_corpus in helper file JFK_functions.R
doc <- clean_corpus(doc)


#This tells R to treat your preprocessed documents as text documents.
doc <- tm_map(doc, PlainTextDocument)


#################################
#### STAGE 3: analysis

dtm <- TermDocumentMatrix(doc)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)





 saveRDS(doc_kw, file = paste0(work_dir,"/doc_keywords.rds"))


### here you run the proper ML analysis






