#remotes::install_github("mcfanda/mcdocs")
library(mcdocs)
mcdocs_init()

## this renders the website
render_mcdocs()
bookdown::render_book("gitdocs/",output_dir = "../docs/book",clean = T)

## this update your github osite
update_git()

