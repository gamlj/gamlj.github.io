library(yaml)
library(rmarkdown)
library(Rsearchable)
library(gh)

jamovi<-function() {
  paste0('<span class="jamovi">jamovi</span>')
}

gamlj<-paste0('<span class="gamlj">GAMLj</span>')


gamlj<-function() {
  paste0('<span class="gamlj">GAMLj</span>')
}

opt<-function(opt) {
  paste0('<span class="option">',opt,'</span>')
}

datafile<-function(name,file) {
  if (length(grep(":/",file,fixed = T))==0)
    file<-paste0(DATALINK,"/",file)
  paste0('[',name,'](',file,')')
}

keywords<-function(key) {
  span<-'<span class="keywords"> <span class="keytitle"> keywords </span>'
  paste(span,key,"</span>")
}

version<-function(ver) {
    paste('<span class="version"> <span class="versiontitle"> GALMj version ≥ </span> ',ver,' </span>')
}

draft<-'<span class="draft"> Draft version, mistakes may be around </span>'

incomplete<-'<span class="incomplete"> Work in progress: incomplete version </span>'

pic<-function(name) paste('<img src="',name,'" class="img-responsive" alt="">')


get_files<-function(path=".",pattern=".Rmd") {
  lf<-list.files(path=path,pattern = pattern,full.names = F)
  files<-list()
  for (f in lf) {
   name<-gsub(".Rmd","",f)
   record<-yaml_front_matter(f)
   record$filename<-name
   files[[name]]<-record
  }
  files
}

get_pages<-function(nickname=NULL,topic=NULL,category=NULL) {
  
  criteria=c()
  if (!is.null(topic))
    criteria["topic"]<-topic
  if (!is.null(category))
    criteria["category"]<-category
  if (!is.null(nickname))
    criteria["nickname"]<-nickname
  
  files<-get_files()
  sfiles<-searchable(files)  
  res<-lookup.searchable(sfiles,criteria)
  res
}

link_pages<-function(nickname=NULL,topic=NULL,category=NULL) {
  
 pages<-get_pages(nickname,topic,category)
 a<-""
 for (p in pages) {
   link<-paste0(p$filename,".html")
   a<-paste(a,paste0('<a href="',link,'">',p$title,'</a>'))
 }
 return(a)  
}
 

list_pages<-function(nickname=NULL,topic=NULL,category=NULL) {
  pages<-get_pages(nickname,topic,category)
  ul<-'<ul>\n'
  a<-""
  for (p in pages) {
    link<-paste0(p$filename,".html")
    b<-paste0('<li><a href="',link,'">',p$title,'</a></li>\n')
    a<-paste(a,b)
  }
  a<-paste(ul,a,'</ul>\n')

  return(a)
}

include_examples<-function(topic)  {
  return(list_pages(topic=topic,category = "example"))
}

include_details<-function(topic)  {
  return(list_pages(topic=topic,category = "details"))
}


issues<-function() {
  a<-'<h1>Comments?</h1>\n'
  a<-paste(a,'<p>Got comments, issues or spotted a bug? Please open an issue on
      <a href="https://github.com/gamlj/gamlj/issues">
      GAMLj at github“</a> or <a href="mailto:mcfanda@gmail.com">send me an email</a></p>
  ')
  return(a)
  
}

backto<-function(...) {
  a<-'<div class="return"> Return to : ' 

  b<-lapply(list(...), function(topic) {
      p<-get_pages(topic=topic,category = "help")[[1]]
      link<-paste0(p$filename,".html")
      paste0('<span class="little"><a href="',link,'">',p$title,'</a></span>')
  })
  b<-paste(b,collapse = ", ")
  a<-paste(a,b,"</div>")
  return(a)
}

get_current<-function() {
  
  query<-"/repos/:owner/:repo/pulls?state=closed"
  pulls<-gh::gh(query,
                owner = MODULE_REPO_OWNER, 
                repo = MODULE_REPO,
                .limit=Inf,
                .token=API_TOKEN)
  cv<-pulls[[1]]
  cv$head$ref
}

get_all_commits<-function() {
    query<-"/repos/:owner/:repo/pulls?state=closed"
    pulls<-gh::gh(query,
              owner = MODULE_REPO_OWNER, 
              repo = MODULE_REPO,
              .limit=Inf,
              .token=API_TOKEN)
    vers<-sapply(pulls, function(p) if(p$head$ref!="develop") p$head$ref else NULL) 
    vers<-vers[!sapply(vers,is.null)]
    nvers<-as.numeric(unlist(sapply(vers,function(v) 
    gsub("version","",
         gsub(".","",
              gsub("Version","",v,fixed = T),fixed=T)))))

    nfirst<-as.numeric(gsub(".","",gsub("Version","",FIRST_VERSION,fixed=T),fixed=T))
    sel<-nvers>=nfirst
    nvers<-nvers[sel]
    vers<-vers[sel]

    vers<-rev(vers[order(nvers)])
    pulls<-pulls[sel]
    pulls<-rev(pulls[order(nvers)])

    commits<-list()

      for (pull in pulls) {
          cu<-pull$commits_url
          query<-gsub("https://api.github.com","",cu,fixed = T)
  
          coms<-gh::gh(query,
                 .limit=Inf,
                 .token=API_TOKEN)
          for (com in coms) {
              if (length(com)==0)
                  next
          one<-list(sha=com$sha,msg=com$commit$message,version=pull$head$ref)
          commits[[length(commits)+1]]<-one
        }
    }
   commits<-as.data.frame(do.call(rbind,commits))
   commits
}
save_commits<-function() {

  test<-file.exists("../resources/commitsdata.Rda")
  if (test) {
    load("../resources/commitsdata.Rda")
    print(commits)  
  } else {
    commits<-get_all_commits()
    save(commits,file="../resources/commitsdata.Rda")
  }
  
}

get_commits<-function() {
  
  load(file="../resources/commitsdata.Rda")
  present<-unique(unlist(commits$version))

  query<-"/repos/:owner/:repo/pulls?state=closed"
  pulls<-gh::gh(query,
                owner = MODULE_REPO_OWNER, 
                repo = MODULE_REPO,
                .limit=Inf,
                .token=API_TOKEN)
  vers<-sapply(pulls, function(p) if(p$head$ref!="develop") p$head$ref else NULL) 
  vers<-vers[!sapply(vers,is.null)]
  nvers<-as.numeric(unlist(sapply(vers,function(v) 
    gsub("version","",
         gsub(".","",
              gsub("Version","",v,fixed = T),fixed=T)))))
  
  nfirst<-as.numeric(gsub(".","",gsub("Version","",FIRST_VERSION,fixed=T),fixed=T))
  sel<-nvers>=nfirst
  nvers<-nvers[sel]
  vers<-vers[sel]

  new<-setdiff(unlist(vers),present)
  sel<-sapply(pulls, function(p) (p$head$ref %in% new)) 
  pulls<-pulls[sel]

  for (pull in pulls) {
    cu<-pull$commits_url
    query<-gsub("https://api.github.com","",cu,fixed = T)
    
    coms<-gh::gh(query,
                 .limit=Inf,
                 .token=API_TOKEN)
    for (com in coms) {
      if (length(com)==0)
        next
      one<-c(sha=com$sha,msg=com$commit$message,version=pull$head$ref)
      commits[length(commits)+1,]<-one
    }
  }
  commits
    
}

write_commits<-function(commits, current=NULL) {
  
  sel<-list()
  j<-1
  for (i in 1:dim(commits)[1]) {
    msg<-trimws(commits[i,"msg"])
    gonext=FALSE
    try({
      if (!is.null(BANNED_COMMITS))
        for (rule in BANNED_COMMITS) {
          if (msg==rule)
            gonext=TRUE
        }
    })
    for (rule in BANNED_COMMITS_GREP) {
      if (length(grep(rule,msg)))
        gonext=TRUE
    }
    
    if (gonext)
      next()
    test<-grep("§",msg,fixed=T)
    if (length(test)>0) msg<-paste("<b>",msg,"</b>")
    sel[[j]]<-c(msg=msg,version=commits[i,"version"])
    j<-j+1
  }
  sel<-rev(sel)
  coms<-as.data.frame(do.call("rbind",sel))
  versions<-unlist(rev(unique(coms$version)))
  cv<-ifelse(!is.null(current),current,".x.x.x.")
  
  
  for (i in seq_along(versions)) {
    rel<-""
    if (versions[i]==cv)
        rel<-"(Current)"
    cat(paste("#",versions[i],rel,"\n\n"))
    cs<-coms[coms[,2]==versions[i],1]
    for (j in cs)
      cat(paste("*",j,"\n\n"))
  }
  #coms
}



jtable<-function(jobject,digits=3) {
  snames<-sapply(jobject$columns,function(a) a$title)
  asDF<-jobject$asDF
  tnames<-unlist(lapply(names(asDF) ,function(n) snames[[n]]))
  names(asDF)<-tnames
  kableExtra::kable(asDF,"html",
                    table.attr='class="jmv-results-table-table"',
                    row.names = F,
                    digits=3)
}

copy_vignettes<-function() {
  files<-list.files(path=VIGNETTES_FOLDER,pattern = "*.Rmd")
  cpcommand<-paste0("cp ",VIGNETTES_FOLDER,"*.Rmd", "  docssource")
  system(cpcommand)
  
}

copy_rhelp<-function() {
  folder<-paste0(MODULE_FOLDER,"/man/")
  files<-list.files(path=folder,pattern = "*.Rd")
  cpcommand<-paste0("cp ",folder,"*.Rd", "  docssource/rhelp/")
  system(cpcommand)
  
}


get_vignettes<-function() {
  files<-get_files(path=VIGNETTES_FOLDER,pattern = "*.Rmd")
  return(files)
}

link_vignettes<-function() {
  pages<-get_files(path=VIGNETTES_FOLDER,pattern = "*.Rmd")
  ul<-'<ul>\n'
  a<-""
  for (p in pages) {
    link<-paste0(p$filename,".html")
    b<-paste0('<li><h2 class="vignettes"><a href="',link,'">',p$title,'</a></h2></li>\n')
    a<-paste(a,b)
  }
  a<-paste(ul,a,'</ul>\n')
  
  return(a)
}


fixRd<-function(rd) {
  print(val<-Rdpack::Rdo_locate_core_section(rdo = rd,sec = "\\value"))
  val<-Rdpack::Rdo_locate_core_section(rdo = rd,sec = "\\value")[[1]]$pos
  value<-rd[[val]]
  rvalue<-Rdpack::Rdapply(value,function(r) {
    if(length(grep("$",r,fixed = T))>0)
      return(paste0("`",r,"`"))
    else return(r)
  })
  rdvalue<-Rdpack::char2Rdpiece(value,name = "value")
  Rdpack::Rdo_replace_section(rd,rdvalue)
}


foldable_title<-function(title,comment=NULL) {
 a<-'<button type="button" class="collapsible">'
 a<- paste(a,fa("expand-alt", fill = "steelblue"))
 a<- paste(a,'<span class="colltitle">',title,"</span>")
 a<- paste(a,'<span class="collinfo">click to read</span>')
 if (!is.null(comment))
    a<-paste(a,' <span class="collinfo">',comment,'</span>')
  a<-paste(a,'</button>')
  a<-paste(a,'<div class="collapsiblecontent">')
  a
}
