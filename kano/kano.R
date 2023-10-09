# Source:
# This is a scipt to calculate KANO scores
# After: Noriaki Kano et al.(1984)
# R Script by Bas Baccarne

# Arguments:
# The function kano takes functional and non-functional as arguments
# These are 2 integers with value 1 (dislike) to 5 (like)

# Output:
# The function returns an character with the KANO category ("required", "desired", "delightful" or "indifferent")
# questionable = combination is not valid

kano <- function(functional, disfunctional){
  
  kano_cat =NA
  
  for(i in 1:length(functional)){
    
    if((functional[i] == 5 & disfunctional[i] == 5) |
       (functional[i] == 1 & disfunctional[i] == 1)){
      kano_cat[i] <- "questionable"
    }
    if((functional[i] == 5 & disfunctional[i] == 4) |
       (functional[i] == 5 & disfunctional[i] == 3) |
       (functional[i] == 5 & disfunctional[i] == 2)){
      kano_cat[i] <- "delightful"
    }
    if(functional[i] == 5 & disfunctional[i] == 1){
      kano_cat[i] <- "desired"
    }
    if((functional[i] == 4 & disfunctional[i] == 5) |
       (functional[i] == 3 & disfunctional[i] == 5) |
       (functional[i] == 2 & disfunctional[i] == 5) |
       (functional[i] == 1 & disfunctional[i] == 5) |
       (functional[i] == 1 & disfunctional[i] == 4) |
       (functional[i] == 1 & disfunctional[i] == 3) |
       (functional[i] == 1 & disfunctional[i] == 2)){
      kano_cat[i] <- "anti-feature"
    }
    if((functional[i] == 4 & disfunctional[i] == 4) |
       (functional[i] == 4 & disfunctional[i] == 3) |
       (functional[i] == 4 & disfunctional[i] == 2) |
       (functional[i] == 3 & disfunctional[i] == 4) |
       (functional[i] == 3 & disfunctional[i] == 3) |
       (functional[i] == 3 & disfunctional[i] == 2) |
       (functional[i] == 2 & disfunctional[i] == 4) |
       (functional[i] == 2 & disfunctional[i] == 3) |
       (functional[i] == 2 & disfunctional[i] == 2)){
      kano_cat[i] <- "indifferent"
    }
    if((functional[i] == 4 & disfunctional[i] == 1) |
       (functional[i] == 3 & disfunctional[i] == 1) |
       (functional[i] == 2 & disfunctional[i] == 1)){
      kano_cat[i] <- "required"
    }
  }
  
  kano_cat
}
