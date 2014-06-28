# NHL stat analysis
# extracts play by play data. requires gameids file
# data from nhl.com

setwd("C:/Users/dsbmac/Documents/NHL-Stats")

# libraries
library(XML)


# extract play by plays

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


extract_pbp <- function(gameid) {
  
  print("extracting pbp")  
  
  # format of the url
  # http://www.nhl.com/scores/htmlreports/20132014/PL030217.HTM
  year = as.numeric(substr(gameid, 0, 4))
  
  #create season string
  season = paste(as.character(year),as.character(year+1), sep="")
  
  playByplayURLFrag = "http://www.nhl.com/scores/htmlreports/"
  playByplayURL = paste(playByplayURLFrag, season, "/PL", substr(gameid,5,nchar(gameid)), ".HTM", sep="")
  
  # retrieve play By play stat page 
  docPbp <- htmlTreeParse(playByplayURL, useInternal=TRUE)
  
  # root node
  rootNode = xmlRoot(docPbp)
  
  pbpTables = getNodeSet(rootNode,"//table")
  
  #this is the main stat table
  playByplay <- readHTMLTable(pbpTables[[1]])
  
  
  # stat rows for the play by play
  pbpRows = getNodeSet(docPbp,"//tr[@class='evenColor']")
    
}

  
# maybe use lapply for performance
# main stat extract function
# Use preallocated vectors
parse_play_by_play <- function(gid, pbp) {
  
  print("parsing play by play")
  
  n = xmlSize(pbp)
  print(n)
  
  #get table cells td
  tds = getNodeSet(pbp[[6]],"td")
  
  gameid         <- character(n)
  playid         <- numeric(n)
  period         <- numeric(n)
  strength       <- character(n)
  time           <- character(n)
  event          <- character(n)
  description    <- character(n)
  playersAway    <- character(n)
  playersPosAway <- character(n)
  playersHome    <- character(n)
  playersPosHome <- character(n)
  
  for (i in 1:n) {
    
    # get set of all table cells for the row
    tds = getNodeSet(pbp[[i]], "td")

    # extract values
    gameid[i] <- gid
    playid[i] <- as.numeric(xmlValue(tds[[1]]))
    period[i] <- as.numeric(xmlValue(tds[[2]]))
    strength[i] <-          xmlValue(tds[[3]])
    time[i]     <-          xmlValue(tds[[4]])
    event[i]     <-          xmlValue(tds[[5]])
    description[i]     <-          xmlValue(tds[[6]])
    
    # process the on ice players for away team    
    
    if(!is.null(tds[[7]][[2]])) {
      
      #print("Away")
      
      onIceTable = readHTMLTable(tds[[7]][[2]])
        
      #alternative way, must pad missing values
      a = as.character(onIceTable[seq(1, nrow(onIceTable), 2),])
      b = as.character(onIceTable[seq(2, nrow(onIceTable), 2),])
  
      playersAway[i]  <- paste(a, collapse=" ") 
      playersPosAway[i] <- paste(b, collapse=" ") 
      
    }
    
    # process the home on ice players    
    
    if(!is.null(tds[[8]][[2]])) {
      
      #print("Home")
      onIceTable = readHTMLTable(tds[[8]][[2]])    

      #alternative way, must pad missing values
      a = as.character(onIceTable[seq(1, nrow(onIceTable), 2),])
      b = as.character(onIceTable[seq(2, nrow(onIceTable), 2),])
      
      playersHome[i]   <- paste(a, collapse=" ") 
      playersPosHome[i] <- paste(a, collapse=" ") 
      
    }  
  }
    
  result = data.frame(gameid, playid, period, strength, time, event, description, 
                      playersAway, playersPosAway, playersHome, playersPosAway,                      
             stringsAsFactors=FALSE)
}

# extraction routine for all games

process_games <- function() {
  print("processing games...")
  
  result = data.frame()
  result$gameid = as.factor(result$gameid)
  
  if(file.exists("./playbyplays.csv")) {
    result <- read.csv("./playbyplays.csv")
  } 
  
  # load gameids from file
  gameids <- scan(file="gameids")

  # process games
  for (i in 1:10) {    
    # avoid reprocessing old files
    if (!gameids[i] %in% levels(result$gameid)) {
      gameid = as.character(gameids[i])
      print(gameid)
      
      pbp = extract_pbp(gameid)
      data = parse_play_by_play(gameid, pbp)
      
      # append processed data to result
      result = rbind(result, data)  
    }
  }
  
  #convert some columns to factors to save space
  result$gameid = as.factor(result$gameid)
  result$strength = as.factor(result$strength)
  result$event = as.factor(result$event)
  
  return(result)
}

gameid = gameids[1]

pbp = extract_pbp(gameid)
d = parse_play_by_play(gameid, p)

# scrape play by play data
data = process_games()
write.csv(data, file="playbyplays.csv", row.names = FALSE)
