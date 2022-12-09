# How to edit .RProfile                                                         # http://tinyurl.com/y5kref79
####################################################################################################

options(prompt="R> ", digits=4, show.signif.stars=FALSE)

if(interactive()) 
  try(fortunes::fortune(), silent=TRUE)

Sys.setenv("plotly_username" = "UTexas80")
Sys.setenv("plotly_api_key" = "4BfkAzu0M0QTEajYFYdH")

####################################################################################################
# To create a project-specific start-up script, simply create a .Rprofile file in the project’s root directory and start adding R code, e.g. via file.edit(".Rprofile"). Remember that this will make .Rprofile in the home directory be ignored. The following commands will open your .Rprofile from within an R editor:
file.edit(file.path("~", ".Rprofile")) # edit .Rprofile in HOME
file.edit(".Rprofile") # edit project specific .Rprofile

# By default R looks for and runs .Rprofile files in the three locations described above, in a specific order. .Rprofile files are simply R scripts that run each time R runs and they can be found within R_HOME, HOME and the project’s home directory, found with getwd(). To check if you have a site-wide .Rprofile, which will run for all users on start-up, run:
site_path = R.home(component = "home")
fname = file.path(site_path, "etc", "Rprofile.site")
file.exists(fname)

# The above code checks for the presence of Rprofile.site in that directory. As outlined above, the .Rprofile located in your home directory is user-specific. Again, we can test whether this file exists using
file.exists("~/.Rprofile")
# We can use R to create and edit .Rprofile (warning: do not overwrite your previous .Rprofile - we suggest you try project-specific .Rprofile first):
if(!file.exists("~/.Rprofile")) # only create if not already there
  file.create("~/.Rprofile")    # (don't overwrite it)
file.edit("~/.Rprofile")

# 3.3.2 Example .Rprofile settings
# An .Rprofile file is just an R script that is run at start-up. The examples at the bottom of the .Rprofile help file
help("Rprofile")

# 3.3.2.1 Setting options
# The function options is a list that contains a number of default options. See help("options") or simply type options() to get an idea of what we can configure. In my .Rprofile file, we have the line
options(prompt="R> ", digits=4, show.signif.stars=FALSE)
####################################################################################################