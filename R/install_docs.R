#remotes::install_github("mcfanda/mcdocs")
library(mcdocs)

bookdown::render_book("gitdocs/",output_dir = "../docs/book",clean = T)

"bookdown::pdf_book"
bookdown::render_book("gitdocs/",output_dir = "../docs/book",clean = T, output_format = "bookdown::pdf_book")

pkgdown::build_site(preview = FALSE)

## this renders the website
#render_mcdocs()

## this update your github site
#update_git()
