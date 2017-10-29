library(pdftools)
library(tesseract)
library(magrittr)
#library(xlsx)
library(dplyr)
library(tidyr)
library(tm)

if(! require(magick)){
  library(devtools)
  devtools::install_github("ropensci/magick")
}


################# USER INPUTS AND SETTINGS ########################

# get directory of this script and move up by one dir; works only if you are sourcing the script
work_dir <- strsplit(dirname(sys.frame(1)$ofile),split='/')[[1]] 
work_dir <- paste(work_dir[-length(work_dir)],collapse = '/')
tmp_dir  <- paste0(work_dir,"/tmp/")
if(!dir.exists(tmp_dir)){
  dir.create(tmp_dir,recursive = TRUE, showWarnings = FALSE)
}

#import helper functions
source(paste0(work_dir,"/R/JFK_functions.R"))

base_url <- "https://www.archives.gov/files/research/jfk/releases/"  # HTTP URL where all pdfs are accessible
# range of docs to process
min_id <- 0 # set to 0 for startign since first doc in list
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
  mutate(ifelse(Doc.Date=="0000-01-01",NA, Doc.Date) ) %>%
  mutate(Doc.Type = gsub('[[:punct:]]+','',Doc.Type)) %>%
  mutate(Doc.Type = gsub('  +',' ',Doc.Type)) 


old_cols <- names(doc_list)
doc_list$Doc.Index <- seq(1, nrow(doc_list),1)
doc_list <- doc_list[,c('Doc.Index',old_cols)]
unique(doc_list$Doc.Type)
#remove handwritten notes
handwritten_mask <- grepl('HANDWRITTEN', doc_list$Comments) # | grepl('NOTES', doc_list$Doc.Type) 


#### create tbl_df where to store outputs

doc_raw_txt <- data.frame(Doc.Index=doc_list$Doc.Index,
                          First.Page = as.character(NA),
                          Body.Text = as.character(NA)) %>% tbl_df()


doc_kw <- data.frame(Doc.Index=doc_list$Doc.Index,
                     Keyword = as.character(NA),
                     Count = as.integer(NA) ) %>% tbl_df()

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
  
  if(n_pages<2){
    warning(paste0("ID=",id,"   Document has ",n_pages," pages. Skipping to the next doc."))
  }
  
  ###  import the text, save raw values to the appropriate tibble
  ### import_ocr_doc defined in helper functions file import_df_ocr_functions.R
  first_page <- import_ocr_doc(local_pdf, 1) 
  is_conv <- grep(pattern = "CONVERSATION",x = toupper(first_page) )
  is_repo <- grep(pattern = "REPORT",x = toupper(first_page) )
  is_memo <- grep(pattern = "MEMO",  x = toupper(first_page) )
  
  raw_body_text <- character()
  for(iP in seq(2,n_pages,1)){
    tmp_txt <- import_ocr_doc(local_pdf, iP) 
    raw_body_text <- paste(raw_body_text,tmp_txt, sep = " ")
  }# end loop on pages of pdf
  
  doc_raw_txt[doc_raw_txt$Doc.Index==id,'First.Page'] <- first_page
  doc_raw_txt[doc_raw_txt$Doc.Index==id,'Body.Text'] <- raw_body_text
  
  ### clean up text 
  
  
  ### do smtg smart to extract key words
  
  
  ### store key words frequency in tibble
  
  
  ### wrap up and move to next document
  print(paste("Saving outputs to tmp caches in ",work_dir) )
  saveRDS(doc_list, file = paste0(work_dir,"/doc_list_tmp.rds"))
  saveRDS(doc_raw_txt, file = paste0(work_dir,"/doc_rawtext_tmp.rds"))
  saveRDS(doc_kw, file = paste0(work_dir,"/doc_keywords_tmp.rds"))
  t1 <- Sys.time()
  writeLines(paste0("ID=",id," processed in ",sprintf("%.1f",as.numeric(t1-t0))," seconds\n"  ) )
  
}#end loop over docs

saveRDS(doc_list, file = paste0(work_dir,"/doc_list.rds"))
saveRDS(doc_raw_txt, file = paste0(work_dir,"/doc_rawtext.rds"))
saveRDS(doc_kw, file = paste0(work_dir,"/doc_keywords.rds"))


### here you run the proper ML analysis






