#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Obtain sudden gains and losses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)


# load dataset ####
#' @format The dataset includes the following variables:
#' encounter_id: session ID
#' messageInd: message index (e.g., first message of a chat = 1, second = 2)
#' neural_prob: probability of high expressed risk in a message

dt <- fread('<dataset/location>')


# criterion 1: absolute change ####

## calculate difference of two consecutive datapoints
diffs <- dt[, c(NA, diff(neural_prob)), by=encounter_id][, V1]
## set cutoff at 1.96 (5% alpha)
cri1 <- 1.96*sd(diffs, na.rm=T)
## add absolute differences to original data
dt[, absDiff := diffs]
dt[is.na(absDiff), absDiff := 0]


# critieron 2: relative change ####

dt[, absDiff_ := shift(absDiff, type='lead'), by=encounter_id]
dt[, relDiff := shift(absDiff_ / neural_prob, type='lag'), by=encounter_id]
dt[, absDiff_ := NULL]
dt[is.na(relDiff), relDiff := 0]


# criterion 3: stable change ####

## define a windowing function for t test with window size = 3
window_t_test <- function(TS, windowSize=3) {
  p.list <- c()
  if(length(TS) >= windowSize*2) {
    for(i in 1:(length(TS)-windowSize*2+1)) {
      segment1 <- TS[i : (i+windowSize-1)]
      segment2 <- TS[(i+windowSize) : (i+windowSize*2-1)]
      p <- t.test(segment1, segment2)$p.value
      p.list <- c(p.list, p)
    }
    p.list <- c(rep(1, windowSize), p.list, rep(1, windowSize-1))
  }
  return(p.list)
}
## apply the function to entire data
dt[, p.diff := window_t_test(neural_prob), by=encounter_id]


# select datapoints that match all criteria ####

## create a new dataframe to save sudden change points
dt.cpt <- dt[, .(cpts=paste(messageInd, collapse=',')), encounter_id]
## remove any chat with less than 5 messages (criterion 3 can't apply)
id_tobe_removed <- dt[, .N, encounter_id][N<=5, encounter_id]
dt.cpt[encounter_id %in% id_tobe_removed, cpts := '']
## filter changepoints that fit the criteria
filter.cpt <- function(DT, DT.cpt) {
  progress_total <- DT.cpt[, .N]
  
  for(i in DT.cpt$encounter_id) {
    progress_now <- which(i==DT.cpt$encounter_id)
    print(sprintf('Now processing %d (%d/%d)', i, progress_now, progress_total))
    
    trueGains <- c()
    trueLosses <- c()
    
    if(DT.cpt[encounter_id==i, cpts] == '') next
    
    changepoints <- DT.cpt[
      encounter_id==i, 
      as.numeric(str_split(cpts, ',', simplify=T))
    ]
    
    for(j in changepoints[-length(changepoints)]) {
      encounter <- DT[encounter_id==i & messageInd==(j+1)]
      if(encounter$absDiff <= -cri1
         & encounter$relDiff <= -.25
         & encounter$p.diff < .05) {
        trueGains <- c(trueGains, j)
      }
      else if(encounter$absDiff >= cri1
              & encounter$relDiff >= .25
              & encounter$p.diff < .05) {
        trueLosses <- c(trueLosses, j)
        }
      }
    DT.cpt[encounter_id==i, suddGains := paste(trueGains, collapse=',')]
    DT.cpt[encounter_id==i, suddLosses := paste(trueLosses, collapse=',')]
  }
  DT.cpt[, cpts := NULL]
}
## apply function
filter.cpt(dt, dt.cpt)


# save the dataset
#' @format This dataset indexes the locations of sudden gains and losses in each 
#' session. It contains these variables:
#' encounter_id: session ID
#' suddGains: message indexes of sudden gain
#' suddLosses: message indexes of sudden loss
fwrite(dt.cpt, 'sudden_changes.csv')
