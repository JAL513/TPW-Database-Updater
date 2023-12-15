#Packages#####
require(googlesheets4)
require(googledrive)

#Functions#####
pilotEmailtoName <- function(fileURL){
  officialPilotNames <- readRDS('C:/Users/josha/OneDrive/Documents/GitHub/TPW-Database-Updater/OfficialPilotNames.RDS')
  tmp <- data.frame(read_sheet(ss = fileURL), check.names = FALSE)
  tmp2 <- data.frame('email' = tmp$Username,
                     'pilotName' = tmp$`What's your pilot name?`, 
                     check.names = FALSE)
  
  newNames <- tmp2[!tmp2$email %in% officialPilotNames$email,]
  if(nrow(newNames) >= 1){
    officialPilotNames <- rbind(officialPilotNames, newNames)
    saveRDS(officialPilotNames, 'C:/Users/josha/OneDrive/Documents/GitHub/TPW-Database-Updater/OfficialPilotNames.RDS')
  }
  repeatPilotNames <- table(officialPilotNames$pilotName)
  repeatPilotNames <- names(repeatPilotNames[repeatPilotNames >= 2])
  if(length(repeatPilotNames) != 0){
    print(repeatPilotNames)
  }
}

TPWdatabaseUpdate <- function(fileURL, updateDate, eventName){
  TPWdatabase <- readRDS('D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')
  #TPWdatabase <- readRDS('C:/Users/josha/OneDrive/Documents/GitHub/TPWShinyAppFullServerVersion/TyrantProWhooper/app/Leaderboard/TPWDatabase.RDS')
  officialPilotNames <- readRDS('C:/Users/josha/OneDrive/Documents/GitHub/TPW-Database-Updater/OfficialPilotNames.RDS')
  
  tmp <- data.frame(read_sheet(ss = fileURL), check.names = FALSE)
  repeateUploads <- table(tmp$`Email Address`)
  repeateUploads <- repeateUploads[repeateUploads >= 2]
  
  if(length(repeateUploads) != 0){
    for(j in 1:length(repeateUploads)){
      tmpDeduped <- tail(tmp[tmp$Username == names(repeateUploads[j]),], 1)
      tmp <- tmp[tmp$Username != names(repeateUploads[j]), ]
      tmp <- rbind(tmp, tmpDeduped)
    }
  }

  for(i in 1:length(tmp$`Please upload your Google Sheets calculator scorecard here.`)){
    #i = 3
    tmpCalc <- tmp$`Please upload your Google Sheets calculator scorecard here.`[i]
    tmpYTlink <- tmp$`Please share your public/unlisted YouTube video link here`[i]
    tmpEmail <- tmp$`Email Address`[i]
    
    tmp2 <- data.frame(read_sheet(ss = tmpCalc, sheet = 'Calculator - Outdoor'), check.names = FALSE)
      
    if(tmp2[11,'...2'] == 'Click here to start'){
      tmp2 <- data.frame(read_sheet(ss = tmpCalc, sheet = 'Calculator - Indoor'), check.names = FALSE)
      baseTricks <- na.omit(tmp2[ ,'...36', drop = FALSE])
      baseTricks <- setNames(tail(baseTricks, -1), 'BaseTricks')
    } else {
      baseTricks <- tmp2[11:40,'...2']
      baseTricks <- as.character(na.omit(baseTricks))
      
      # baseTricks <- head(baseTricks, -6)
      # baseTricks <- tail(baseTricks, -7)
      baseTricks <- setNames(baseTricks, 'BaseTricks')
    }
    
    pilotName <- officialPilotNames[officialPilotNames$email == tmpEmail, 'pilotName']
    mapSelection <- tmp2[[2,3]]
    trickScore <- tmp2[[4,3]]
    kwadScore <- tmp2[[5,3]]
    mapScore <- tmp2[[6,3]]
    totalScore <- tmp2[[7,3]]
    
    K. <- tmp2[[4,'...35']]
    W. <- tmp2[[5,'...35']]
    A. <- tmp2[[6,'...35']]
    D. <- tmp2[[7,'...35']]
    
    maxStreak <- tmp2[[4,10]]
    maxCombo <- tmp2[[5,10]]
    highPoint <- tmp2[[6,10]]
    totwCompleted <- tmp2[[7,10]]
    
    tmp3 <- tail(tmp2, -10)
    tmp3 <- tmp3[-c(31:nrow(tmp3)),]
    tmp3 <- tmp3[,-c(6:ncol(tmp3))]
    tmp3 <- setNames(tmp3, c('#', 'TRICK', 'POINTS', 'EXECUTION', 'COMBO'))
    tmp3 <- tmp3[, c('#', 'TRICK', 'EXECUTION', 'COMBO', 'POINTS')]
    tmp3 <- na.omit(tmp3)
    baseTricks <- data.frame('BaseTricks' = baseTricks, 'EXECUTION' = tmp3[,'EXECUTION'])
    
    tmpList <- list('PILOT NAME' = pilotName,
                    'MAP SELECTION' = mapSelection,
                    'TRICK SCORE' = trickScore,
                    # 'KWAD SCORE' = list('SCORE' = kwadScore,
                    #                     'K' = K.,
                    #                     'W' = W.,
                    #                     'A' = A.,
                    #                     'D' = D.),
                    'KWAD SCORE' = kwadScore,
                    'MAP SCORE' = mapScore,
                    'TOTAL SCORE' = totalScore,
                    'MAX STREAK' = maxStreak,
                    'MAX COMBO' = maxCombo,
                    'HIGHEST POINT TRICK' = highPoint,
                    'TRICK OF THE WEEK COUNT' = totwCompleted,
                    'SCORE CARD' = tmp3,
                    'EVENT DATE' = updateDate,
                    'BASE TRICKS' = baseTricks,
                    'YOUTUBE LINK' = tmpYTlink
                    )
    
    TPWdatabase[[pilotName]][[eventName]] <- tmpList
    saveRDS(TPWdatabase, 'D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')
    #saveRDS(TPWdatabase, 'C:/Users/josha/OneDrive/Documents/GitHub/TPWShinyAppFullServerVersion/TyrantProWhooper/app/Leaderboard/TPWDatabase.RDS')
  }
  return(TPWdatabase)
}

#Update Database####
 # folderURL. <- 'https://drive.google.com/drive/folders/1nAecmg0ccWO3p8EHmsD3Q5xh06Sdr5Jg'
 # folderURL. <- 'https://drive.google.com/drive/folders/1NJCvRyrZcszw3MdThqSVVx5AcUY88tIx'
# fileURL. <- 'https://docs.google.com/spreadsheets/d/1G7NemlbABou_bNX7FS9Km7fsuSZO-Aqg9STy_oCUgYM/edit#gid=467078288'
# fileURL. <- 'https://docs.google.com/spreadsheets/d/1x1bNbe298HK6PxcNER84KiJecNMTkfM2Yw5hQasjIoM/edit#gid=467078288'
fileURL. <- 'https://docs.google.com/spreadsheets/d/1OInd9C5pqpR04O2Q9gUTj746OL78_66rrG57tQVqF50/edit?resourcekey#gid=664964755'

updateDate. <- '2023-12-16'
eventName. <- 'Preseason Event 2'

# TPWdatabase. <- TPWdatabaseUpdate(fileURL = fileURL., updateDate = updateDate., eventName = eventName.)

#RESET DATABASE#####
# TPWdatabase <- list()
# saveRDS(TPWdatabase, 'D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')

#CLEAR EVENT FROM ALL PLAYERS####
# TPWdatabase <- readRDS('D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')
# for(i in 1:length(TPWdatabase)){
#   TPWdatabase[[i]] <- TPWdatabase[[i]][names(TPWdatabase[[i]]) != "Preseason Event 2"]
# }
# saveRDS(TPWdatabase, 'D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')

