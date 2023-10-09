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
  if((functional == 5 & disfunctional == 5) |
     (functional == 1 & disfunctional == 1)){
    kano_cat <- "questionable"
  }
  if((functional == 5 & disfunctional == 4) |
     (functional == 5 & disfunctional == 3) |
     (functional == 5 & disfunctional == 2)){
    kano_cat <- "delightful"
  }
  if(functional == 5 & disfunctional == 1){
    kano_cat <- "desired"
  }
  if((functional == 4 & disfunctional == 5) |
     (functional == 3 & disfunctional == 5) |
     (functional == 2 & disfunctional == 5) |
     (functional == 1 & disfunctional == 5) |
     (functional == 1 & disfunctional == 4) |
     (functional == 1 & disfunctional == 3) |
     (functional == 1 & disfunctional == 2)){
    kano_cat <- "anti-feature"
  }
  if((functional == 4 & disfunctional == 4) |
     (functional == 4 & disfunctional == 3) |
     (functional == 4 & disfunctional == 2) |
     (functional == 3 & disfunctional == 4) |
     (functional == 3 & disfunctional == 3) |
     (functional == 3 & disfunctional == 2) |
     (functional == 2 & disfunctional == 4) |
     (functional == 2 & disfunctional == 3) |
     (functional == 2 & disfunctional == 2)){
    kano_cat <- "indifferent"
  }
  if((functional == 4 & disfunctional == 1) |
     (functional == 3 & disfunctional == 1) |
     (functional == 2 & disfunctional == 1)){
    kano_cat <- "required"
  }
  kano_cat
}
