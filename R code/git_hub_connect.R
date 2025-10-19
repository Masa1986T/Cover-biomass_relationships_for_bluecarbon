library(usethis)
use_git_config(user.name="Masa1986T", )#

use_git() 
# The use_git() function will add a Git repository (often referred to as a “repo”) to an existing RStudio project.

create_github_token()
#When github_token is expired, you should create new token


use_github()
#Creat github repo from this project
#Sometimes you already have a project locally and you want to get it on GitHub. To do this, 
#you’ll need to first use the use_git() function from usethis, as we did above. 
#Then, you can use the use_github() function, which will create a GitHub repo and connect it to your current RStudio project.
#Finally, created github repo 
