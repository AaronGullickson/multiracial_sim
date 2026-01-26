## check_packages.R

#Run this script to check for packages that the other R scripts will use. If missing, try to install.
#code borrowed from here:
#http://www.vikram-baliga.com/blog/2015/7/19/a-hassle-free-way-to-verify-that-r-packages-are-installed-and-loaded

#add new packages to the chain here
packages = c(
  "here", # absolute requirement always
  "knitr", # for processing quarto
  "readr","haven", # I/O
  "tidyverse","lubridate","broom", #tidyverse and friends
  "modelsummary","gt", # for table output
  "devtools", # for installing RSOCSIM
  "fs", # for filesystem interaction
  "future", # for how SOCSIM process is run
  "quarto", # for automating parameterized reports
  "googlesheets4", # for using google sheets to track simulation parameters
  "nnet" # for multinomial models
)

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# install rsocsim - installing a particular version that works. The newer versions
# are completely breaking the sim, due to some substantial changes in file paths. 
# At some point, I should try to figure out if I can get them working right with 
# the architecture we have but the old sims were working so its not clear its 
# worth it.
if(!require(rsocsim)) {
  devtools::install_github("MPIDR/rsocsim")
}
