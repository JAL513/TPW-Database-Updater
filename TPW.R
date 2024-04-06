#Packages#####
require(googlesheets4)
require(googledrive)

#Functions#####
pilotEmailtoName <- function(fileURL){
  officialPilotNames <- readRDS('C:/Users/josha/OneDrive/Documents/GitHub/TPW-Database-Updater/OfficialPilotNames.RDS')
  tmp <- data.frame(read_sheet(ss = fileURL), check.names = FALSE)
  tmp2 <- data.frame('email' = tolower(tmp$`Email Address`),
                     'pilotName' = tmp$`What's your pilot name?`, 
                     check.names = FALSE)
  
  newNames <- tmp2[!tmp2$email %in% officialPilotNames$email,]
  if(nrow(newNames) >= 1){
    
    officialPilotNames <- rbind(officialPilotNames, newNames)
    
    if(length(table(officialPilotNames$pilotName)[table(officialPilotNames$pilotName) == 2]) != 0 ){
      repeatePilotNames <- table(officialPilotNames$pilotName)[table(officialPilotNames$pilotName) == 2]
      print(repeatePilotNames)
      stop('Pilot Name Repeat')
    }
    saveRDS(officialPilotNames, 'C:/Users/josha/OneDrive/Documents/GitHub/TPW-Database-Updater/OfficialPilotNames.RDS')
  }
  
  repeatPilotNames <- table(officialPilotNames$pilotName)
  repeatPilotNames <- names(repeatPilotNames[repeatPilotNames >= 2])
  if(length(repeatPilotNames) != 0){
    print(repeatPilotNames)
  }
}

TPWdatabaseUpdate <- function(fileURL, updateDate, eventName, specificSubmission = NULL){
  #TPWdatabase <- readRDS('D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')
  TPWdatabase <- readRDS('C:/Users/josha/OneDrive/Documents/GitHub/TPWShinyAppFullServerVersion/TyrantProWhooper/app/Leaderboard/TPWDatabase.RDS')
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
  
  if(!is.null(specificSubmission)){
    tmp <- tmp[specificSubmission,]
  }

  for(i in 1:length(tmp$`Please upload your Google Sheets calculator scorecard here.`)){
    #i = 2
    tmpCalc <- tmp$`Please upload your Google Sheets calculator scorecard here.`[i]
    tmpYTlink <- tmp$`Please share your public/unlisted YouTube video link here`[i]
    tmpEmail <- tolower(tmp$`Email Address`[i])
    tmpLocation <- tmp$`Did you fly indoors or outdoors?`[i]
    
    if(tmpLocation == 'Indoors'){
      tmp2 <- data.frame(read_sheet(ss = tmpCalc, sheet = 'Calculator - Indoor'), check.names = FALSE)
      baseTricks <- na.omit(tmp2[ , 43])
      baseTricks <- setNames(tail(baseTricks, -1), 'BaseTricks')
    } else {
      tmp2 <- data.frame(read_sheet(ss = tmpCalc, sheet = 'Calculator - Outdoor'), check.names = FALSE)
      baseTricks <- tmp2[10:39,2]
      baseTricks <- as.character(na.omit(baseTricks))
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
    baseTricks <- data.frame('BaseTricks' = as.character(baseTricks), 'EXECUTION' = unlist(tmp3[,'EXECUTION']))
    
    tmpList <- list('PILOT NAME' = pilotName,
                    'MAP SELECTION' = mapSelection,
                    'TRICK SCORE' = trickScore,
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
    #saveRDS(TPWdatabase, 'D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')
    saveRDS(TPWdatabase, 'C:/Users/josha/OneDrive/Documents/GitHub/TPWShinyAppFullServerVersion/TyrantProWhooper/app/Leaderboard/TPWDatabase.RDS')
  }
  return(TPWdatabase)
}

#Submission file URL####
fileURL. <- 'https://docs.google.com/spreadsheets/d/1dDPRNRBkmy6CmA_7iJ_IyYDLS22wndFvfAWypuTRkx4/edit#gid=664964755'

#Update Pilot emails####
pilotEmailtoName(fileURL = fileURL.)
officialPilotNames <- readRDS('C:/Users/josha/OneDrive/Documents/GitHub/TPW-Database-Updater/OfficialPilotNames.RDS')
officialPilotNames

#Update Database####
updateDate. <- '2024-04-07'
eventName. <- 'Event 6'
specificSubmission. <- NULL

TPWdatabase. <- TPWdatabaseUpdate(fileURL = fileURL., updateDate = updateDate., eventName = eventName., specificSubmission = specificSubmission.)

#Update website####

#database update
{
  check_minutes <- function() {
    current_time <- as.POSIXlt(Sys.time())
    minutes <- current_time$min
    return(minutes %% 10 == 0 || minutes %% 10 == 5)
  }
  
  while (check_minutes()) {
    Sys.sleep(15)
  }  

  drive_upload(media = 'C:/Users/josha/OneDrive/Documents/GitHub/TPWShinyAppFullServerVersion/TyrantProWhooper/app/Leaderboard/TPWDatabase.RDS',
               path = 'https://drive.google.com/drive/folders/1p36S4t7MaJjBnEZn0T5UQZqIXDsVdMe-',
               name = 'TPWDatabase.RDS')
  
  while (!check_minutes()) {
    Sys.sleep(60)
  }  
  
  drive_trash(drive_ls('https://drive.google.com/drive/folders/1p36S4t7MaJjBnEZn0T5UQZqIXDsVdMe-'))
}

#app update
{
  check_minutes <- function() {
    current_time <- as.POSIXlt(Sys.time())
    minutes <- current_time$min
    return(minutes %% 10 == 0 || minutes %% 10 == 5)
  }
  
  while (check_minutes()) {
    Sys.sleep(15)
  }  
  
  drive_upload(media = 'C:/Users/josha/OneDrive/Documents/GitHub/TPWShinyAppFullServerVersion/TyrantProWhooper/app/Leaderboard/app.R',
               path = 'https://drive.google.com/drive/folders/1p36S4t7MaJjBnEZn0T5UQZqIXDsVdMe-',
               name = 'app.R')
  drive_upload(media = 'C:/Users/josha/OneDrive/Documents/GitHub/TPWShinyAppFullServerVersion/TyrantProWhooper/app/Leaderboard/TrickList.txt',
               path = 'https://drive.google.com/drive/folders/1p36S4t7MaJjBnEZn0T5UQZqIXDsVdMe-',
               name = 'TrickList.txt')
  
  while (!check_minutes()) {
    Sys.sleep(60)
  }  
  
  drive_trash(drive_ls('https://drive.google.com/drive/folders/1p36S4t7MaJjBnEZn0T5UQZqIXDsVdMe-'))
}

#
#CLEAR EVENT FROM ALL PLAYERS####
# ############TPWdatabase <- readRDS('D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')
# TPWdatabase <- readRDS('C:/Users/josha/OneDrive/Documents/GitHub/TPWShinyAppFullServerVersion/TyrantProWhooper/app/Leaderboard/TPWDatabase.RDS')
# 
# for(i in 1:length(TPWdatabase)){
#   TPWdatabase[[i]] <- TPWdatabase[[i]][names(TPWdatabase[[i]]) != "Event 6"]
# }
# ###########saveRDS(TPWdatabase, 'D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')
# saveRDS(TPWdatabase, 'C:/Users/josha/OneDrive/Documents/GitHub/TPWShinyAppFullServerVersion/TyrantProWhooper/app/Leaderboard/TPWDatabase.RDS')
# 
#CLEAR PLAYER FROM DATABASE####
# TPWdatabase <- readRDS('D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')
# TPWdatabase <- TPWdatabase[!names(TPWdatabase) %in% c('FPVDev', 'CMYK FPV')]
# saveRDS(TPWdatabase, 'D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')

#RESET DATABASE#####
# TPWdatabase <- list()
# saveRDS(TPWdatabase, 'D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')