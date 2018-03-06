# R script for the visualisation of time series in bars from a "start timestamp" to an "end timestamp"
# an example of this visualisation can be found in the folder above

gg_timebar_df <- function(dataframe, id.vars, timestamp.start, timestamp.end){
    
    # arguments: dataframe = subset of the data you want to visualize
    # arguments: id.vars = variable, or array of variables to want to use for a meltdown
    # arguments: timestamp.start = which variable contains this information
    # arguments: timestamp.end = which variable contains this information
    
    require(reshape)
    require(ggplot2)
    
    # rename timestamp variables for easier processing
    names(dataframe)[names(dataframe)==timestamp.start] <- "start.TS"
    names(dataframe)[names(dataframe)==timestamp.end] <- "end.TS"
    
    # add event.id
    dataframe$event.id. <- 1:nrow(dataframe)
    
    # melt data to long form
    # (keep id.vars as variables)
    df <- melt.data.frame(dataframe, 
            id.vars = c(id.vars,"event.id."), 
            measure.vars = c("start.TS","end.TS")
    )
    
    # rename variables
    names(df) <- c(id.vars,"event.id.","TS.type", "TS")
    
    # rerurn this dataframe
    df
}

# run the gg_timebar_df function (edit the four arguments below)
gg_df <- gg_timebar_df(source_df,
                       id.vars = c("id","name","description", "duration"),
                       timestamp.start = "startTS",
                       timestamp.end = "endTS")

# define the plot x & y axis + group on event.id. (required for bars from start to end)
g <- ggplot(gg_df, aes(
    x = TS, 
    y = description, 
    group = event.id., color = description))

# generate a plot in which a bar is drawn from start TS to end TS
g + xlab("time") + ylab("activity name") +
            geom_line(size=3.5)
