#remotes::install_github("mcfanda/mcdocs")
library(mcdocs)


bookdown::render_book("gitdocs/",output_dir = "../docs/book",clean = T)

 pkgdown::build_site(preview = FALSE)
mcdocs_init()

str(knitr::knit_engines$get()) 
## this renders the website
#render_mcdocs()

## this update your github site
#update_git()


# old tag G-2YGWBQCQK3
.30^2
