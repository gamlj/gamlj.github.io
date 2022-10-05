#remotes::install_github("mcfanda/mcdocs")
library(mcdocs)
mcdocs_init()
## this renders the website
render_mcdocs()
## this update your github site
update_git()

mcdocs::get_github_pulls()

query<-"/repos/:owner/:repo/pulls?state=closed"
gh::gh(query, owner = MODULE_REPO_OWNER, repo = MODULE_REPO, 
       .limit = Inf, .token = API_TOKEN)
