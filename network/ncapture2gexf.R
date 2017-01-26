# function to transform facebook pagedata collected with ncapture to a gephi file and igraph file
# NCAPTURE > https://chrome.google.com/webstore/detail/ncapture/lgomjifbpjfhpodjhihemafahhmegbek
# required packages: openxlsx, dplyr, igraph, rgexf (install first)
# check if first sheet is names "Table1-1", as is the NCAPTURE default naming

# call function with: ncapture2gexf(ncapturefile,output.filename.gexf)

# load required packages
library(openxlsx)
library(dplyr)
library(igraph)
library(rgexf)

ncapture2gexf <- function(ncapturefile, output.filename.gexf){
        
        # import data
        print("starting import ...")
        ncapturedata <- read.xlsx(ncapturefile, sheet = "Table1-1", colNames = TRUE)
        print(paste("file -", ncapturefile, "- succesfuly imported"))
        print("processing data ...")
              
        # filter data
        myvars <- names(ncapturedata) %in% c("Row.ID",
                                             "Post.ID",
                                             "Posted.By.Username",
                                             "Comment.ID",
                                             "Commenter.Username",
                                             "In.Reply.To.ID")
        interactions <- ncapturedata[myvars]
        
        # retreive poster data
        posts <- interactions[,1:3]
        posts <- posts[complete.cases(posts$Posted.By.Username),]
        
        # retreive commenter data
        comments <- interactions[,c(1,2,4,5,6)]
        comments <- comments[complete.cases(comments$Comment.ID),]
        for (i in 1:nrow(comments)){
                if(is.na(comments$In.Reply.To.ID[i])){
                        comments$In.Reply.To.ID[i] <- comments$Post.ID[i]
                }
        }
        
        # create from > to data.frame
        fromto <- left_join(comments,posts,by=c("In.Reply.To.ID" = "Post.ID"))
        fromto <- fromto[,c(4,7,5,1,3)]
        fromto <- left_join(fromto,comments,by=c("In.Reply.To.ID" = "Comment.ID"))
        for (i in 1:nrow(fromto)){
                if(is.na(fromto$Posted.By.Username [i])){
                        fromto$Posted.By.Username [i] <- fromto$Commenter.Username.y[i]
                }
        }
        
        fromto <- fromto[,c(4,1,2)]
        names(fromto) <- c("row", "from", "to")
        
        # convert from > to data.frame to relational vector
        fromto_vector <- vector()
        for(i in 1:nrow(fromto)){
                fromto_vector <- c(
                        fromto_vector,
                        as.character(fromto$from[i]), 
                        as.character(fromto$to[i]))
        }
        
        # convert relational vector to igraph object & push this object to environment
        igraph <- graph(fromto_vector)
        ncapture.igraph <<- igraph
        print("ncapture.igraph object pushed to global envrvionment")
        print("converting to Gephi file ...")
        
        # convert igraph object to gexf file and save in working dir
        gexf <- igraph.to.gexf(igraph)
        capture.output(print(gexf),
                       file = output.filename.gexf)
        print(paste("the file", output.filename.gexf,"was saved here:", getwd()))
        print("you can now open this file in Gephi")
        
}
