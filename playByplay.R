# NHL stat analysis
# we're trying to scrape play by play data from nhl.com to analyze the data

setwd("C:/Users/dsbmac/Documents/NHL")

# libraries
library(XML)


# get the page html for schwdules

# scrape all the game id codes
url <- "http://www.nhl.com/ice/schedulebyseason.htm"
doc <- htmlTreeParse(url, useInternal=TRUE)
doc

# Parse the html tree, ignoring errors on the page
# pagetree <- htmlTreeParse(url, error=function(...){})

# get node that contains the play by play
# get game id codes from schedule page then get play by play from gamecode urls
# root node
rootNode = xmlRoot(doc)
rootNode

print(rootNode)[1:2]

# obtain ids for all games in a season
#dataTables <-getNodeSet(doc,"//table[@class='data schedTbl']")
dataTables <-getNodeSet(rootNode,"//table[@class='data schedTbl']")

# get relevant table and button links with the word recap
links <- getNodeSet(doc, "//table[@class='data schedTbl']
                          //a[@class='btn' and contains(.,'RECAP')]") 
links
links[1]


# code ex. 2013030217
id = "2013030217"

# extract play by plays
# http://www.nhl.com/scores/htmlreports/20132014/PL030217.HTM
year = as.numeric(substr(id, 0, 4))

#create season string
season = paste(as.character(year),as.character(year+1), sep="")
season

playByplayURLFrag = "http://www.nhl.com/scores/htmlreports/"
playByplayURL = paste(playByplayURLFrag, season, "/PL", substr(id,5,nchar(id)), ".HTM", sep="")
#playByplayURL == "http://www.nhl.com/scores/htmlreports/20132014/PL030217.HTM"
playByplayURL 

# retrieve play By play stat page 
docPbp <- htmlTreeParse(playByplayURL, useInternal=TRUE)
docPbp

# root node
rootNode = xmlRoot(docPbp)
rootNode

pbpTables = getNodeSet(rootNode,"//table")
pbpTables[[1]]

#this is the main stat table
playByplay <- readHTMLTable(pbpTables[[1]])
playByplay[4970,]



# stat rows for the play by play
pbpRows = getNodeSet(docPbp,"//tr[@class='evenColor']")
n = xmlSize(pbpRows)
n
pbpRows
pbpRows[[17]]
head(pbpRows)

#get table cells td
tds = getNodeSet(pbpRows[[3]],"td")
tds
tds[[1]]
tds[[7]]
td7 = tds[[7]]
td7[[2]]
class(td7)
td7Cells = getNodeSet(td7[[2]],"td")
oniceTables = getNodeSet(td7,"table")
td7Cells[[1]]
oniceTables[[1]]
xmlSize(oniceTables)
readHTMLTable(td7)
onice

# main stat extract function
# Use preallocated vectors
f4 <- function(n) {
  playId <- numeric(n)
  period <- numeric(n)
  strength <- character(n)
  time     <- character(n)
  event    <- character(n)
  description <- character(n)

  for (i in 1:n) {
    # get set of all table cells for the row
    tds = getNodeSet(pbpRows[[i]], "td")

    # extract values
    playId[i] <- as.numeric(xmlValue(tds[[1]]))
    period[i] <- as.numeric(xmlValue(tds[[2]]))
    strength[i] <-          xmlValue(tds[[3]])
    time[i]     <-          xmlValue(tds[[4]])
    event[i]     <-          xmlValue(tds[[5]])
    description[i]     <-          xmlValue(tds[[6]])


  }

  data.frame(playId, period, strength, time, event, description, stringsAsFactors=FALSE)
}

df = f4(n)
str(df)
df

# this is an individual stat row in the play by play table
# if you parse all the rows then you have all the data
x = pbpRows[[1]]

xmlSApply(x, xmlValue)

# TODO figure out how to parse the on-ice table