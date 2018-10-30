file.remove("_main.Rmd")

### HTML

bookdown::render_book("_bookdown.yml", 
                      output_format = "bookdown::gitbook", 
                      clean = TRUE, 
                      new_session = T,
                      output_dir = "docs/", 
                      encoding = "UTF-8")

### Epub

bookdown::render_book("_bookdown.yml",
                      output_format = "bookdown::epub_book", 
                      clean = TRUE, 
                      new_session = T,
                      output_dir = "docs/", 
                      encoding = "UTF-8")

### PDF

bookdown::render_book("_bookdown.yml", 
                      output_format = "bookdown::pdf_book", 
                      clean = T,
                      new_session = T,
                      output_dir = "docs/",
                      encoding = "UTF-8")

