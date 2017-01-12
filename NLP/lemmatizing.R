# lemmatization with korPus and treetagger

# installation > first time only

  #  install treetagger https://www.youtube.com/watch?v=SYMc2SllI0c
  #  download "http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/dutch-par-linux-3.2-utf8.bin.gz"
  #  put the file dutch-utf8.par in the treetagger/lib folder
  
  #  library(devtools)
  #  install_github("unDocUMeantIt/koRpus", ref="develop", force = TRUE)
  #  install.packages("https://reaktanz.de/R/src/contrib/koRpus.lang.nl_0.01-3.tar.gz")

library("koRpus")
library("koRpus.lang.nl")

# Options (set treetagger folder and language)
set.kRp.env(TT.cmd="manual", TT.options=list(
        path="c://treetagger", 
        preset="nl"), 
        lang="nl")

# lemma fucntion (set treetagger folder and language)
lemmatize <- function(text){
        tagged.txt <- treetag(text, format="obj", TT.options=list(path="c://TreeTagger", preset="nl"))
        tagged.txt
}

#lemmatize
lemmatize(txtobject)

# related functions
taggedText(lemmatize(txtobject))     # gives a data.frame of the tagged text
description(lemmatize(txtobject))    # gives a report on some basic text analytics

# more information: https://cran.r-project.org/web/packages/koRpus/vignettes/koRpus_vignette.pdf
