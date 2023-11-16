#Functions#####
TPWdatabaseUpdate <- function(folderURL, updateDate, eventName){
  calcFileList <- drive_ls(folderURL)
  TPWdatabase <- readRDS('D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')
  
  for(i in 1:length(calcFileList$id)){
    tmpCalc <- calcFileList$drive_resource[[i]]$webViewLink
    
    tmp2 <- data.frame(read_sheet(ss = tmpCalc, sheet = 'Calculator'), check.names = FALSE)
    
    pilotName <- tmp2[[1,3]]
    mapSelection <- tmp2[[2,3]]
    trickScore <- tmp2[[4,3]]
    kwadScore <- tmp2[[5,3]]
    mapScore <- tmp2[[6,3]]
    totalScore <- tmp2[[7,3]]
    
    K. <- tmp2[[4,6]]
    W. <- tmp2[[5,6]]
    A. <- tmp2[[6,6]]
    D. <- tmp2[[7,6]]
    
    maxStreak <- tmp2[[4,10]]
    maxCombo <- tmp2[[5,10]]
    highPoint <- tmp2[[6,10]]
    totwCompleted <- tmp2[[7,10]]
    
    tmp3 <- tail(tmp2, -10)
    tmp3 <- tmp3[-c(31:nrow(tmp3)),]
    tmp3 <- tmp3[,-c(6:ncol(tmp3))]
    tmp3 <- setNames(tmp3, c('#', 'TRICK', 'POINTS', 'EXECUTION', 'COMBO'))
    tmp3 <- tmp3[, c('#', 'TRICK', 'EXECUTION', 'COMBO', 'POINTS')]
    
    tmpList <- list('PILOT NAME' = pilotName,
                    'MAP SELECTION' = mapSelection,
                    'TRICK SCORE' = trickScore,
                    'KWAD SCORE' = list('SCORE' = kwadScore,
                                        'K' = K.,
                                        'W' = W.,
                                        'A' = A.,
                                        'D' = D.),
                    'MAP SCORE' = mapScore,
                    'TOTAL SCORE' = totalScore,
                    'MAX STREAK' = maxStreak,
                    'MAX COMBO' = maxCombo,
                    'HIGHEST POINT TRICK' = highPoint,
                    'TRICK OF THE WEEK COUNT' = totwCompleted,
                    'SCORE CARD' = tmp3,
                    'EVENT DATE' = updateDate
                    )
    
    #TPWdatabase <- list()
    TPWdatabase[[pilotName]][[eventName]] <- tmpList
    saveRDS(TPWdatabase, 'D:/Documents/R/Scripts/TPWApp/TPWDatabase.RDS')
  }
  return(TPWdatabase)
}

#Update Database####
# folderURL. <- 'https://drive.google.com/drive/folders/1nAecmg0ccWO3p8EHmsD3Q5xh06Sdr5Jg'
# folderURL. <- 'https://drive.google.com/drive/folders/1NJCvRyrZcszw3MdThqSVVx5AcUY88tIx'

updateDate. <- '2024-01-31'
eventName. <- 'Event 7'

# TPWdatabase. <- TPWdatabaseUpdate(folderURL = folderURL., updateDate = updateDate., eventName = eventName.)


# TPWdatabase <- list()
# saveRDS(TPWdatabase, 'D:/Documents/R/Scripts/TPW App/TPWDatabase.RDS')
