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
  
  # ### the following is needed for organization pages
  # source("R/constants.R")
  # cmd<-paste("cp -R ",paste0(SOURCE_FOLDER,"/*"),paste0(TARGET_FOLDER,"/"))
  # system(cmd)
  # setwd(TARGET_FOLDER)
  # system("git add .")
  # system(paste('git commit -m "',msg,'"'))
  # system("git push origin master")
  # setwd(HERE)
  # 
