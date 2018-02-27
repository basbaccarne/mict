################################################
### script voor het converteren van een KML  ###
### file export uit Google location history  ###
### naar een data.frame in R                 ###
################################################

# packages
require("dplyr")
require("xml2")
require("plyr")
require("tidykml")
# devtools::install_github("briatte/tidykml")


################################################
##  benadering 1: via het KML package voor R  ##
##  nadeel: neemt verplaatsingen niet mee     ##
################################################

# filename
filename <- "google location history 20180210.kml"

# inlezen file
kml <- kml_read(filename)

    # what's in the data?
    kml_info(kml) 
    
    # what are the binding box ranges?
    kml_bounds(kml)

    # location points
    kml_points(kml)


################################################
##  benadering 2: parsen van het KML bestand  ##
##                als xml                     ##
################################################
    
# functie met arguments filename en optioneel id
# geeft een dataframe terug met locaties en verplaatsingen

kml2df <- function(filename, id = sample(1:10000, 1)){
    
    # parsen van de kml file als xml file
    xml <- xmlParse(filename,useInternalNodes = TRUE)
    rootNode <- xmlRoot(xml)
    
    # relevante waarden uit de datafile halen
    values <- xmlSApply(rootNode[[1]],function(x) xmlSApply(x, xmlValue))
    
    # welke ids bevatten info over de placemarks?
    data_ids <- which(names(values)=="Placemark")
    
    # extraheer enkel de placemark data
    df_transposed <- as.data.frame(values[data_ids])
    
    # dit dataframe moet nog worden getransponeerd
    df <- t(df_transposed)
    df <- as.data.frame(df)

    # anonimiseren van deze data (verwijderen account e-mail en toevoegen (random) ID)
    df <- df[ , -which(names(df)=="ExtendedData")]
    df$id <- id
    
    # clean descriptions
    df$description <- gsub(" from ..*","",df$description)
    df$description <- tolower(substring(df$description, 2))
    
    # covert df$Point to triplets of longitude + langitude + altitude
    df$Point <- gsub("clampToGround","",df$Point)
    df$long_lat_alt <- strsplit(as.character(df$Point), "\\, |\\,| ")
    df <- df[ , -which(names(df)=="Point")]
    
    # defining timestamp start, end and duration
    df$timespan <- strsplit(as.character(df$TimeSpan), "Z")
    for(i in 1:nrow(df)){
        df$startTS[i] <- df$timespan[[i]][1]
        df$endTS[i] <- df$timespan[[i]][2]
    }
    df$startTS<- strptime(df$startTS, "%Y-%m-%dT%H:%M:%S") 
    df$endTS<- strptime(df$endTS, "%Y-%m-%dT%H:%M:%S") 
    df$duration <- round(df$endTS - df$startTS,0)
    
    # cleaning dataframe
    df <- df[c("id", "name", "description", "address", "long_lat_alt", "startTS", "endTS", "duration")]
    
    # provide df as response
    df
}        

# filename
filename <- "google location history 20180210.kml"

# laat functie lopen over deze file, met ID "respondent1"
df <- kml2df(filename, "respondent1")

# toepassing: hoe lang gefietst, gereden, boodschappen?
tapply(df$duration,df$description, sum)


