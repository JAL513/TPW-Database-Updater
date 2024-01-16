#Packages#####
require(googlesheets4)
require(googledrive)

#Functions#####
pilotEmailtoName <- function(fileURL){
  officialPilotNames <- readRDS('C:/Users/josha/OneDrive/Documents/GitHub/TPW-Database-Updater/OfficialPilotNames.RDS')
  tmp <- data.frame(read_sheet(ss = fileURL), check.names = FALSE)
  tmp2 <- data.frame('email' = tmp$`Email Address`,
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
    #i = 4
    tmpCalc <- tmp$`Please upload your Google Sheets calculator scorecard here.`[i]
    tmpYTlink <- tmp$`Please share your public/unlisted YouTube video link here`[i]
    tmpEmail <- tmp$`Email Address`[i]
    
    tmp2 <- data.frame(read_sheet(ss = tmpCalc, sheet = 'Calculator - Outdoor'), check.names = FALSE)
      
    if(tmp2[10,'Pilot'] == 'Click here to start'){
      tmp2 <- data.frame(read_sheet(ss = tmpCalc, sheet = 'Calculator - Indoor'), check.names = FALSE)
      baseTricks <- na.omit(tmp2[ , 43])
      baseTricks <- setNames(tail(baseTricks, -1), 'BaseTricks')
    } else {
      baseTricks <- tmp2[10:39,2]
      baseTricks <- as.character(na.omit(baseTricks))
      # baseTricks <- head(baseTricks, -6)
      # baseTricks <- tail(baseTricks, -7)
      baseTricks <- setNames(baseTricks, 'BaseTricks')
    }
    
    pilotName <- officialPilotNames[officialPilotNames$email == tmpEmail, 'pilotName']
    mapSelection <- tmp2[[1,3]]
    trickScore <- tmp2[[3,3]]
    kwadScore <- tmp2[[4,3]]
    mapScore <- tmp2[[5,3]]
    totalScore <- tmp2[[6,3]]
    
    K. <- tmp2[[12,32]]
    W. <- tmp2[[13,32]]
    A. <- tmp2[[14,32]]
    D. <- tmp2[[15,32]]
    
    maxStreak <- tmp2[[24,30]]
    maxCombo <- tmp2[[25,30]]
    highPoint <- tmp2[[26,30]]
    totwCompleted <- tmp2[[27,30]]
    adjRawScore <- tmp2[[28,30]]

    tmp3 <- tail(tmp2, -9)
    tmp3 <- tmp3[-c(31:nrow(tmp3)),]
    tmp3 <- tmp3[,-c(6:ncol(tmp3))]
    tmp3 <- setNames(tmp3, c('#', 'TRICK', 'EXECUTION', 'COMBO', 'POINTS'))
    tmp3 <- tmp3[, c('#', 'TRICK', 'EXECUTION', 'COMBO', 'POINTS')]
    tmp3 <- na.omit(tmp3)
    baseTricks <- data.frame('BaseTricks' = baseTricks, 'EXECUTION' = unlist(tmp3[,'EXECUTION']))
    
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
                    'ADJ RAW SCORE' = adjRawScore,
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

# fileURL. <- 'https://docs.google.com/spreadsheets/d/1OInd9C5pqpR04O2Q9gUTj746OL78_66rrG57tQVqF50/edit?resourcekey#gid=664964755'
# fileURL. <- 'https://docs.google.com/spreadsheets/d/1K8XLMirSLHBb_MM2DKU5GS5po19JOghp3O6KvFLmhos/edit#gid=664964755'
# fileURL. <- 'https://docs.google.com/spreadsheets/d/1R8FH46FJAtQt_E1Uxe3bYbL7fZTrqIMgYZS0ddkmZV8/edit#gid=664964755'
fileURL. <- 'https://docs.google.com/spreadsheets/d/1Z8vdYcD7gFWiVRFp_LZMJI3vlimiEN6280Yiu1U_hdw/edit#gid=664964755'

#Update Pilot emails####
# pilotEmailtoName(fileURL = fileURL.)

#Update Database####
updateDate. <- '2024-01-16'
eventName. <- 'Event 1'

# TPWdatabase. <- TPWdatabaseUpdate(fileURL = fileURL., updateDate = updateDate., eventName = eventName.)

#RESET DATABASE#####
# TPWdatabase <- list()
# saveRDS(TPWdatabase, 'D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')

#CLEAR EVENT FROM ALL PLAYERS####
# TPWdatabase <- readRDS('D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')
# for(i in 1:length(TPWdatabase)){
#   TPWdatabase[[i]] <- TPWdatabase[[i]][names(TPWdatabase[[i]]) != "Event 1"]
# }
# saveRDS(TPWdatabase, 'D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')

