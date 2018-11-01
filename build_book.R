file.remove("New_Stats.md")

### HTML

bookdown::render_book("_bookdown.yml", 
                      output_format = "bookdown::gitbook", 
                      clean = TRUE, 
                      new_session = T,
                      output_dir = "docs", 
                      encoding = "UTF-8")

### Epub

bookdown::render_book("_bookdown.yml",
                      output_format = "bookdown::epub_book", 
                      clean = TRUE, 
                      new_session = T,
                      output_dir = "docs", 
                      encoding = "UTF-8")

### DOC

bookdown::render_book("_bookdown.yml", 
                      output_format = "bookdown::word_document2", 
                      clean = F,
                      new_session = T,
                      output_dir = "docs",
                      encoding = "UTF-8")



### PDF

bookdown::render_book("_bookdown.yml", 
                      output_format = "bookdown::pdf_book", 
                      clean = F,
                      new_session = T,
                      output_dir = "docs/",
                      encoding = "UTF-8")


