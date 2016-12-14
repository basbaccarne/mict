# Select relational data
data_relations <- data.frame(
        from = data$from,
        to = data$to
)
# make sure you have no missing values

# Convert relational data to a vector format
relations_vector <- vector()
  for(i in 1:nrow(data_relations)){
        relations_vector <- c(
                relations_vector,
                as.character(data$from[i]), 
                as.character(data$to[i]))
}
# "&" character might cause errors. If so, remove from data

# Convert vector to igraph object
# If nessecary, install ipgraph package first
library(igraph)
igraph <- graph(relations_vector)

# Optional: Take a look at this igraph object in R
plotNetwork <- function(igraph){
        plot(igraph, 
             edge.arrow.size=.5, 
             vertex.color="gold", 
             vertex.size=15, 
             vertex.frame.color="gray", 
             vertex.label.color="black", 
             vertex.label.cex=0.8, 
             vertex.label.dist=2, 
             edge.curved=0.2) 
}
plotNetwork(igraph)

# Convert igraph object to a .gexf file (Gephi)
# If nessecary, install rgexf package first
library(rgexf)
gexf <- igraph.to.gexf(igraph)

# Save the .gexf file on your hard disk
# Make sure to set the appropiate working directory first
capture.output(print(gexf),
               file = "gephi_data.gexf")

# The file "gephi_data.gexf" will be stored in you working directory
# you can now analyse this with Gephi (https://gephi.org/)
