require(pdftools)
require(tiff)
require(tesseract)
require(magrittr)
require(dplyr)
require(tidyr)
require(magick)


import_ocr_doc <- function(full_pdf_url, page ){
  
  bitmap <- pdftools::pdf_render_page(local_pdf, page=page, dpi = 400, numeric=TRUE)
  
  # bread-and-butter way
  # tiff::writeTIFF(bitmap, "C:/Users/Bonny/Documents/JFK/tiff/page.tiff")
  # ocr_txt <- tesseract::ocr("C:/Users/Bonny/Documents/JFK/tiff/page.tiff")
  # cat(ocr_txt)
  
  # cleaning the bitmap with magick gives typically better results; 
  # magick::image_ocr is based on tesseract::ocr
  ocr_txt <- magick::image_read(bitmap) %>% #"C:/Users/Bonny/Documents/JFK/tiff/page.tiff") %>%
    magick::image_resize("2000") %>%
    magick::image_convert(colorspace = 'gray') %>%
    magick::image_trim() %>%
    magick::image_ocr()
  
  return(ocr_txt)
}

