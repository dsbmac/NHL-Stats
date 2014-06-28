# scraping the game ids from page to then scrape the play by plays for each game

setwd("C:/Users/dsbmac/Documents/NHL-Stats")

library(XML)

siteNHL <- "http://www.nhl.com/ice/schedulebyseason.htm?season=20132014&gameType=2&team=&network=&venue="

#Parsing the site code
parseNHL <- htmlParse(siteNHL)

#The XPath expression to get the id #'s
getId <- "//a[contains(concat(' ',@href,' '),'/gamecenter')]"

attrNHL <- xpathSApply(parseNHL,getId,xmlAttrs) #loops over all the relevant nodes and returns the                                                         #attribute values

attrNHL <- unlist(attrNHL) #Turns the attribute list into a vector

idVec <- gsub(pattern = "(\\D)",replacement = "",x = attrNHL) #Gets rid of everything that isn't a number

idVec <- idVec[-which(names(idVec) %in% c("shape","class"))] #Removes the shape and class attributes
idVec = idVec[!(is.na(idVec) | idVec == "")]

write(idVec, file="gameids")
