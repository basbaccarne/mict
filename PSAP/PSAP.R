# Source:
# This is the PSAP syntax as developed by  De Marez, L. (2006)
# Cite as: De Marez, L. (2006). Diffusie van ICT-innovaties: accurater gebruikersinzicht voor betere introductiestrategieën [doctoral dissertation]. UGent, Vakgroep Communicatiewetenschappen.
# R Script by Bas Baccarne

# Arguments:
# The function PSAP takes PSAP1, PSAP2 & PSAP3 as arguments
# These are 3 integers with value 1 (innovator) to 5 (laggard)

# Output:
# The function returns an factor vector with labeled adoption profiles
# NA = combination is not valid

PSAP <- function(PSAP1, PSAP2, PSAP3) {
  
  # Matrix to store the theoretical PSAP mappings
  mapping_matrix <- array(NA, dim = c(5, 5, 5))
  
  # Fill in the matrix with the mappings
  mapping_matrix[1,1,1] <- 1
  mapping_matrix[1,1,2] <- 1
  mapping_matrix[1,1,3] <- 2
  mapping_matrix[1,1,4] <- 3
  mapping_matrix[1,1,5] <- 4
  mapping_matrix[1,2,1] <- NA
  mapping_matrix[1,2,2] <- 2
  mapping_matrix[1,2,3] <- 2
  mapping_matrix[1,2,4] <- 3
  mapping_matrix[1,2,5] <- 4
  mapping_matrix[1,3,1] <- NA
  mapping_matrix[1,3,2] <- NA
  mapping_matrix[1,3,3] <- 3
  mapping_matrix[1,3,4] <- 4
  mapping_matrix[1,3,5] <- 4
  mapping_matrix[1,4,1] <- NA
  mapping_matrix[1,4,2] <- NA
  mapping_matrix[1,4,3] <- NA
  mapping_matrix[1,4,4] <- 4
  mapping_matrix[1,4,5] <- 5
  mapping_matrix[1,5,1] <- NA
  mapping_matrix[1,5,2] <- NA
  mapping_matrix[1,5,3] <- NA
  mapping_matrix[1,5,4] <- NA
  mapping_matrix[1,5,5] <- 5
  mapping_matrix[2,1,1] <- 1
  mapping_matrix[2,1,2] <- 1
  mapping_matrix[2,1,3] <- 2
  mapping_matrix[2,1,4] <- 3
  mapping_matrix[2,1,5] <- 4
  mapping_matrix[2,2,1] <- NA
  mapping_matrix[2,2,2] <- 2
  mapping_matrix[2,2,3] <- 2
  mapping_matrix[2,2,4] <- 3
  mapping_matrix[2,2,5] <- 4
  mapping_matrix[2,3,1] <- NA
  mapping_matrix[2,3,2] <- NA
  mapping_matrix[2,3,3] <- 3
  mapping_matrix[2,3,4] <- 4
  mapping_matrix[2,3,5] <- 4
  mapping_matrix[2,4,1] <- NA
  mapping_matrix[2,4,2] <- NA
  mapping_matrix[2,4,3] <- NA
  mapping_matrix[2,4,4] <- 4
  mapping_matrix[2,4,5] <- 4
  mapping_matrix[2,5,1] <- NA
  mapping_matrix[2,5,2] <- NA
  mapping_matrix[2,5,3] <- NA
  mapping_matrix[2,5,4] <- NA
  mapping_matrix[2,5,5] <- 4
  mapping_matrix[3,1,1] <- 2
  mapping_matrix[3,1,2] <- 2
  mapping_matrix[3,1,3] <- 3
  mapping_matrix[3,1,4] <- 3
  mapping_matrix[3,1,5] <- 3
  mapping_matrix[3,2,1] <- NA
  mapping_matrix[3,2,2] <- 2
  mapping_matrix[3,2,3] <- 3
  mapping_matrix[3,2,4] <- 3
  mapping_matrix[3,2,5] <- 4
  mapping_matrix[3,3,1] <- NA
  mapping_matrix[3,3,2] <- NA
  mapping_matrix[3,3,3] <- 4
  mapping_matrix[3,3,4] <- 4
  mapping_matrix[3,3,5] <- 4
  mapping_matrix[3,4,1] <- NA
  mapping_matrix[3,4,2] <- NA
  mapping_matrix[3,4,3] <- NA
  mapping_matrix[3,4,4] <- 4
  mapping_matrix[3,4,5] <- 5
  mapping_matrix[3,5,1] <- NA
  mapping_matrix[3,5,2] <- NA
  mapping_matrix[3,5,3] <- NA
  mapping_matrix[3,5,4] <- NA
  mapping_matrix[3,5,5] <- 5
  mapping_matrix[4,1,1] <- 2
  mapping_matrix[4,1,2] <- 2
  mapping_matrix[4,1,3] <- 3
  mapping_matrix[4,1,4] <- 4
  mapping_matrix[4,1,5] <- 4
  mapping_matrix[4,2,1] <- NA
  mapping_matrix[4,2,2] <- 3
  mapping_matrix[4,2,3] <- 3
  mapping_matrix[4,2,4] <- 4
  mapping_matrix[4,2,5] <- 5
  mapping_matrix[4,3,1] <- NA
  mapping_matrix[4,3,2] <- NA
  mapping_matrix[4,3,3] <- 4
  mapping_matrix[4,3,4] <- 4
  mapping_matrix[4,3,5] <- 5
  mapping_matrix[4,4,1] <- NA
  mapping_matrix[4,4,2] <- NA
  mapping_matrix[4,4,3] <- NA
  mapping_matrix[4,4,4] <- 5
  mapping_matrix[4,4,5] <- 5
  mapping_matrix[4,5,1] <- NA
  mapping_matrix[4,5,2] <- NA
  mapping_matrix[4,5,3] <- NA
  mapping_matrix[4,5,4] <- NA
  mapping_matrix[4,5,5] <- 5
  mapping_matrix[5,1,1] <- 4
  mapping_matrix[5,1,2] <- 4
  mapping_matrix[5,1,3] <- 4
  mapping_matrix[5,1,4] <- 4
  mapping_matrix[5,1,5] <- 5
  mapping_matrix[5,2,1] <- NA
  mapping_matrix[5,2,2] <- 3
  mapping_matrix[5,2,3] <- 3
  mapping_matrix[5,2,4] <- 4
  mapping_matrix[5,2,5] <- 5
  mapping_matrix[5,3,1] <- NA
  mapping_matrix[5,3,2] <- NA
  mapping_matrix[5,3,3] <- 4
  mapping_matrix[5,3,4] <- 4
  mapping_matrix[5,3,5] <- 5
  mapping_matrix[5,4,1] <- NA
  mapping_matrix[5,4,2] <- NA
  mapping_matrix[5,4,3] <- NA
  mapping_matrix[5,4,4] <- 5
  mapping_matrix[5,4,5] <- 5
  mapping_matrix[5,5,1] <- NA
  mapping_matrix[5,5,2] <- NA
  mapping_matrix[5,5,3] <- NA
  mapping_matrix[5,5,4] <- NA
  mapping_matrix[5,5,5] <- 5

  adoption_profile <- NA
  # Look up the value in the matrix, based in actual PSAP1, PSAP2 & PSAP3 values
  for(i in 1:length(PSAP1)){
    adoption_profile[i] <- mapping_matrix[PSAP1[i], PSAP2[i],PSAP3[i]]
  }

  # Define the labels of the factor
  labels <- c("innovator","early adopter","early majority","late majority","laggard")
  
  # Create a labeled factor variable
  adoption_profile_labeled <- factor(adoption_profile, levels = 1:5, labels = labels)
  
  # Return the labeled factor variable
  return(adoption_profile_labeled)
}
