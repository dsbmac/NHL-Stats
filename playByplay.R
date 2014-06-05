# NHL stat analysis
# we're trying to scrape play by play data from nhl.com to analyze the data

setwd("C:/Users/dsbmac/Documents/NHL-Stats")

# libraries
library(XML)


gameids <- scan(file="gameids")
gameids

# code ex. 2013030217

gameid = "2013030217"

# extract play by plays
# http://www.nhl.com/scores/htmlreports/20132014/PL030217.HTM
year = as.numeric(substr(gameid, 0, 4))

#create season string
season = paste(as.character(year),as.character(year+1), sep="")
season

playByplayURLFrag = "http://www.nhl.com/scores/htmlreports/"
playByplayURL = paste(playByplayURLFrag, season, "/PL", substr(gameid,5,nchar(gameid)), ".HTM", sep="")
#playByplayURL == "http://www.nhl.com/scores/htmlreports/20132014/PL030217.HTM"
playByplayURL 

# retrieve play By play stat page 
docPbp <- htmlTreeParse(playByplayURL, useInternal=TRUE)
docPbp

# root node
rootNode = xmlRoot(docPbp)
rootNode

pbpTables = getNodeSet(rootNode,"//table")

#this is the main stat table
playByplay <- readHTMLTable(pbpTables[[1]])


# stat rows for the play by play
pbpRows = getNodeSet(docPbp,"//tr[@class='evenColor']")
n = xmlSize(pbpRows)


#get table cells td
tds = getNodeSet(pbpRows[[6]],"td")
tds
tds[[1]]
tds[[7]]
td7 = tds[[7]]
td7[[2]]
class(td7)
td7Cells = getNodeSet(td7[[2]],"//td")
oniceTables = getNodeSet(td7,"table")
td7Cells[[1]]
oniceTables[[1]]
xmlSize(oniceTables)
onice

tn = readHTMLTable(td7[[2]])
tn
as.numeric(as.character((tn$V1[1])))
as.character((tn$V1[2]))
as.character((tn$V1[12]))

kids = xmlChildren(td7)
kids
td7
xmlSize(td7Cells)

time1 = tds[4]/text()[1]
time2 = /html/body/table[1]/tbody/tr[6]/td[4]/text()[1]

y <- htmlParse(playByplayURL)
x <- xpathApply(y, "/html/body/table[1]/tbody/tr[6]/td[4]/text()[1]")
x <- xpathApply(docPbp, "/html/body/table[1]/tbody/tr[6]/td[4]/text()[1]")
t = sapply(x,xmlValue)   
t[[1]]
length(t)
x = xmlValue("/html/body/table[1]/tbody/tr[6]/td[4]/text()[1]")

x = xpathSApply(y, string(/html/body/table[1]/tbody/tr[6]/td[4]/text()[1]@content))
string(/*/head/meta[@name='description']/@content)
x

# /html/body/table[1]/tbody/tr[6]/td[4]/text()[1]
# /html/body/table[1]/tbody/tr[6]/td[4]/text()[2]

# maybe use lapply for performance
# main stat extract function
# Use preallocated vectors
extract <- function(n) {
  gameid <- numeric(n)
  playid <- numeric(n)
  period <- numeric(n)
  strength <- character(n)
  time     <- character(n)
  event    <- character(n)
  description <- character(n)
  playersAway <- matrix(data=NA,nrow=n, ncol=6)
  playerPosAway <- matrix(data=NA,nrow=n, ncol=6)
  playersHome <- matrix(data=NA,nrow=n, ncol=6)
  playerPosHome <- matrix(data=NA,nrow=n, ncol=6)
  
  for (i in 1:n) {
    # get set of all table cells for the row
    tds = getNodeSet(pbpRows[[i]], "td")

    # extract values
    gameid[i] <- gameid
    playid[i] <- as.numeric(xmlValue(tds[[1]]))
    period[i] <- as.numeric(xmlValue(tds[[2]]))
    strength[i] <-          xmlValue(tds[[3]])
    time[i]     <-          xmlValue(tds[[4]])
    event[i]     <-          xmlValue(tds[[5]])
    description[i]     <-          xmlValue(tds[[6]])
    
    # process the on ice players    
    onIceTable = readHTMLTable(tds[[7]][[2]])
    
    #alternative way, must pad missing values
    a = as.character(onIceTable[seq(1, nrow(onIceTable), 2),])
    b = as.character(onIceTable[seq(2, nrow(onIceTable), 2),])

    playersAway[i,1:length(a)] <- a
    playerPosAway[i,1:length(b)] <- b
    
    # process the home on ice players    
    onIceTable = readHTMLTable(tds[[8]][[2]])
    
    #alternative way, must pad missing values
    a = as.character(onIceTable[seq(1, nrow(onIceTable), 2),])
    b = as.character(onIceTable[seq(2, nrow(onIceTable), 2),])
    
    playersHome[i,1:length(a)] <- a
    playerPosHome[i,1:length(b)] <- b
    
    
    #for (j in seq(1, nrow(onIceTable), 2)) {
     # playersAway[i,(j+1)/2] <- as.numeric(as.character(onIceTable[j,]))
      #playerPosAway[i,j/2+1] <- as.character(onIceTable[j+1,])
    #}    
  }
  
  #playersAway = as.numeric(as.character(playersAway))
  #playerPosAway <- as.character(playerPosAway)
  playersAway = as.data.frame(playersAway)
  playersHome = as.data.frame(playersHome)
  playerPosAway = as.data.frame(playerPosAway)
  playerPosHome = as.data.frame(playerPosHome)
  
  colnames(playersAway) = c("player1visitor", "player2visitor", "player3visitor", "player4visitor", "player5visitor", "player6visitor") 
  colnames(playersHome) = c("player1home", "player2home", "player3home", "player4home", "player5home", "player6home") 
  colnames(playerPosAway) = c("player1posvisitor", "player2posvisitor", "player3posvisitor", "player4posvisitor", "player5posvisitor", "player6posvisitor") 
  colnames(playerPosHome) = c("player1poshome", "player2poshome", "player3poshome", "player4poshome", "player5poshome", "player6poshome") 
  
  result = data.frame(playid, period, strength, time, event, description, 
             stringsAsFactors=FALSE)
  result = cbind(result, playersAway, playerPosAway, playersHome, playerPosHome)
}

data = extract(n)
str(data)
data[65,]

# this is an individual stat row in the play by play table
# if you parse all the rows then you have all the data
x = pbpRows[[1]]

xmlSApply(x, xmlValue)

# write file to output
write.table(data, file="playByplay.txt", sep = "\t", append=F)
