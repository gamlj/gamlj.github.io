  library(rmarkdown)
  source("R/constants.R")
  source("R/functions.R")
  HERE<-getwd()
  copy_vignettes()
  msg<-"updates"
  render_site("docssource/",output_format = "html_document")
  system("git add .")
  system(paste('git commit -m "',msg,'"'))
  system("git push origin master")
  
