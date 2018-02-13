##########################################################
# script voot de anotatie van apps op basis van app ids  # 
#        een key laat 100 calls per dag toe              #
# vraag aan Bas, of vraag aan via https://appmonsta.com/ #
##########################################################

###########################
## settings & libraries ###
###########################

library(httr)
library(plyr)
library(jsonlite)

key <- ###                                        # max 100 calls per dag
store <- "android"                                # android or itunes
countrycode <- "all"                              # tweeletterige landencodes (all = alle landen)

###########################
###### inlezen data #######
###########################

# inlezen logdata
logs <- read.csv2("data.csv", header=TRUE, stringsAsFactors=FALSE)

# unieke applicatie ids (packid) opslaan als een data.frame
packids <- as.data.frame(unique(logs$applicatie),stringsAsFactors=FALSE)
names(packids) <- "packid"

# sommige variabelen kunnen niet teruggevonden in de app store
# deze kan je hier excluden, eventueel kunnen deze handmatig worden gecategoriseerd
excluded <- c("com.android.systemui",
              "com.android.settings",
              "com.oneplus.filemanager",
              "com.android.mms",
              "com.android.contacts",
              "com.android.incallui",
              "android",
              "com.oneplus.camera",
              "com.android.documentsui",
              "com.android.captiveportallogin",
              "com.android.dialer",
              "com.android.dialer")
packids <- packids[!(packids$packid %in% excluded),]
packids <- as.data.frame(packids,stringsAsFactors=FALSE)

# gezien het beperkte API volume worden calls lokaal opgeslagen
# laad lokale database met app info
if (file.exists("appid_localdatabase.csv")) {local_database <- read.csv2("appid_localdatabase.csv")}

################################
###### annotatiefuncties #######
################################

# primaire API functie 
# input = packid, output = list met informatie
get_appinfo <- function(packid){                                
    url <- paste(                                          # request URL opstellen
        "https://api.appmonsta.com/v1/stores/", store,
        "/details/", packid,
        ".json?country=", countrycode, sep = "")
    response <- GET(url, authenticate(key, "X"))           # API call zelf
    fromJSON(content(response), flatten=T)                 # haal de informatie uit de API response (class=list)
}

# secundaire functie: list data filteren en omvormen naar een dataframe
annotate <- function(packid){
    appinfo <- get_appinfo(packid)                         # roept de primaire functie op 
    selection <- c(                                        # selectie van de variabelen die we interessant vinden
        "id","app_name","genres","genre",
        "app_type","price","content_rating",
        "all_rating","downloads")
    as.data.frame(appinfo[selection])                      # transformeer deze subset naar een dataframe
}

# tertiaire functie: haal data uit de lokale database of sprek de API aan + merge met eerder verkregen data
merge_appdata <- function(i){
    # scenario 1: indien we de info al lokaal hebben
    if (any(packids$packid[i]==local_database$id)){
        index <- which(local_database$id==packids$packid[i])
        new <- local_database[index,]
        annotated.df <<- rbind(annotated.df, new)
    }
    # scenario 2: indien we de info nog niet lokaal hebben staan
    else{
        new <- annotate(packids$packid[i])
        local_database <<- rbind(local_database, new)                        # update de lokale database met deze nieuwe data
        write.csv2(local_database, "local_database.csv", row.names=FALSE)   # sla deze update van de lokale database op als csv
        annotated.df <<- rbind(annotated.df, new)
    }
}

#######################################
## annoteer de df met unieke packids ##
#######################################

# initiatliseer een leeg dataframe waar later lijn per lijn de annotaties worden toegevoegd
annotated.df <- data.frame()

# maak een data.frame met alle geannoteerde packids
# (inclusief error handling wanneer de API geen correcte respons geeft)
for (i in 1:nrow(packids)){
    tryCatch(merge_appdata(i), 
             error=function(e) cat(
                 "de app", packids$packid[i], "komt niet voor in de store (voeg toe aan aan excluded), of je overschreed het maximum van 100 calls per dag.\n"))
}

# converteer vector naar character
annotated.df$id <- as.character(annotated.df$id) 

# voeg deze data toe aan het originele data.frame met logdata
merged <- merge(logs, annotated.df, 
      by.x = "applicatie", by.y = "id",
      all=T)

# sla het resultaat op als csv
write.csv2(merged, "data_annotated_appinfo.csv")
