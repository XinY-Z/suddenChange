#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyze distribution of sudden gains and losses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(ggplot2)
library(effectsize)


# load dataset
dt <- fread('<dataset/location>')
dt.cpt <- fread('sudden_changes.csv')


# distribution of the length of sessions for entire dataset
dt[, max(messageInd), by=encounter_id][, median(V1)]
dt[, max(messageInd), by=encounter_id][, mean(V1)]
dt[, max(messageInd), by=encounter_id][, sd(V1)]
dt[, max(messageInd), by=encounter_id][, range(V1)]
dt[, max(messageInd), by=encounter_id][, quantile(V1, c(0.025, 0.975))]


# prevalence ####

## sudden gains
dt.cpt[suddGains!='' & !is.na(suddGains), .N]/dt.cpt[, .N]
dt.cpt[suddGains!='' & !is.na(suddGains), median(str_count(suddGains, ','))+1]
dt.cpt[suddGains!='' & !is.na(suddGains), max(str_count(suddGains, ','))+1]

## sudden losses
dt.cpt[suddLosses!='' & !is.na(suddLosses), .N]/dt.cpt[, .N]
dt.cpt[suddLosses!='' & !is.na(suddLosses), median(str_count(suddLosses, ','))+1]
dt.cpt[suddLosses!='' & !is.na(suddLosses), max(str_count(suddLosses, ','))+1]

## chats with gains and losses
dt.cpt[suddGains!='' & !is.na(suddGains) & suddLosses!='' & !is.na(suddLosses), .N]


# timing ####

## sudden gains
temp <- dt.cpt[
  suddGains!='' & !is.na(suddGains), 
  as.numeric(str_split(suddGains, ',', simplify=T))
]
mean(temp, na.rm=T)
median(temp, na.rm=T)
sd(temp, na.rm=T)
quantile(temp, c(0.025, 0.975), na.rm=T)

## sudden losses
temp <- dt.cpt[
  suddLosses!='' & !is.na(suddLosses), 
  as.numeric(str_split(suddLosses, ',', simplify=T))
]
mean(temp, na.rm=T)
median(temp, na.rm=T)
sd(temp, na.rm=T)
quantile(temp, c(0.025, 0.975), na.rm=T)


# reversal rate ####

## choose gains or losses to view
VAR <- 'suddGains'
if(VAR=='suddGains') {
  sel <- dt.cpt[suddGains!='' & !is.na(suddGains), .(encounter_id, suddGains)]
} else if(VAR=='suddLosses') {
  sel <- dt.cpt[suddLosses!='' & !is.na(suddLosses), .(encounter_id, suddLosses)]
}
temp <- dt[
  encounter_id %in% sel$encounter_id, 
  .(encounter_id, messageInd, neural_prob)
]

rev_encounters <- data.table()
for(i in sel$encounter_id) {
  
  cutoffs <- matrix(nrow=0, ncol=2)
  
  sudd_chg <- sel[
    encounter_id==i, 
    as.numeric(str_split(.SD, ',', simplify=T), .SDcols=VAR)
  ]
  
  ## find cutoff level of each sudden change at 50%
  for(j in sudd_chg) {
    cutoff <- mean(c(
      temp[encounter_id==i & messageInd==j, neural_prob], 
      temp[encounter_id==i & messageInd==(j+1), neural_prob]
    ))
    cutoffs <- rbind(cutoffs, c(j, cutoff))
  }
  
  ## check if a session has reversal
  for(k in cutoffs[, 2]) {
    judge <- c()
    rev_points <- temp[encounter_id==i & neural_prob <= k, messageInd]
    if(any(rev_points > cutoffs[which(cutoffs==k, arr.ind=T)[1], 1])) {
      judge <- c(judge, T)
    }
    else judge <- c(judge, F)
  }
  
  if(any(judge)) {
    rev_encounters <- rbind(rev_encounters, list(encounter_id=i, eval=T))
  }
  else rev_encounters <- rbind(rev_encounters, list(encounter_id=i, eval=F))
}

## number and proportion of sessions with reversal
rev_encounters[, sum(eval)]
rev_encounters[, mean(eval)]


# magnitudes of sudden changes ####

temp <- dt[, .(pre=mean(first(neural_prob, 3))), by=encounter_id]

all_mag <- c()
for(i in sel$encounter_id) {
  sudd_chg <- sel[
    encounter_id==i, 
    as.numeric(str_split(.SD, ',', simplify=T), .SDcols=VAR)
  ]
  for(j in sudd_chg) {
    mag <- dt[encounter_id==i & messageInd==(j+1), abs(absDiff)]
    all_mag <- c(all_mag, mag)
  }
}

## calculate magnitude relative to baseline SD
mean(all_mag/sd(temp$pre))


# prevalence of chats with both gains and losses ####
temp <- dt.cpt[suddGains!='' & !is.na(suddGains) & suddLosses!='' & !is.na(suddLosses)]
temp[, .N]/dt.cpt[, .N]
temp_gain <- temp[, as.numeric(str_split(suddGains, ',', simplify=T)), by=1:nrow(temp)]
temp_loss <- temp[, as.numeric(str_split(suddLosses, ',', simplify=T)), by=1:nrow(temp)]

## loss following gain
gain_loss <- c()
for(i in temp_gain[, unique(nrow)]) {
  gain_loss <- c(
    gain_loss, 
    all(temp_loss[nrow==i, max(V1)] - temp_gain[nrow==i, V1] > 0)
  )
}
sum(gain_loss)
mean(gain_loss)

## gain following loss
loss_gain <- c()
for(i in temp_gain[, unique(nrow)]) {
  loss_gain <- c(
    loss_gain, 
    all(temp_gain[nrow==i, max(V1)] - temp_loss[nrow==i, V1] > 0)
  )
}
sum(loss_gain)
mean(loss_gain)


# difference in risk change ####
temp <- dt[, 
           .(chg_prob=mean(last(neural_prob, 3))-mean(first(neural_prob, 3)),
             first_prob=mean(first(neural_prob, 3)),
             last_prob=mean(last(neural_prob, 3))
           ), 
           encounter_id
]
dt.cpt <- dt.cpt[temp, on='encounter_id']
dt.cpt[, group := ifelse(
  suddGains!='' & !is.na(suddGains), 
  'suddGain', 
  ifelse(
    suddLosses!='' & !is.na(suddLosses),
    'suddLoss',
    'No sudd'
  )
)]
dt.cpt[, group := factor(group, levels=c('suddGain', 'suddLoss', 'No sudd'))]

## mean and sd
dt.cpt[group=='suddGain', c(mean(chg_prob), sd(chg_prob))]
dt.cpt[group=='suddLoss', c(mean(chg_prob), sd(chg_prob))]
dt.cpt[group!='No sudd', c(mean(chg_prob), sd(chg_prob))]

## anova
kruskal.test(chg_prob ~ group, dt.cpt)
pairwise.wilcox.test(
  dt.cpt$chg_prob, 
  dt.cpt$group, 
  p.adjust.method='holm'
)

## effect size
hedges_g(dt.cpt[group=='suddGain', chg_prob], dt.cpt[group=='suddLoss', chg_prob], pooled_sd=F)
hedges_g(dt.cpt[group=='suddGain', chg_prob], dt.cpt[group=='No sudd', chg_prob], pooled_sd=F)
hedges_g(dt.cpt[group=='suddLoss', chg_prob], dt.cpt[group=='No sudd', chg_prob], pooled_sd=F)

## visualization
ggplot(dt.cpt, aes(y=chg_prob, x=group)) +
  geom_hline(yintercept=0, linetype='solid', color='grey', size=0.8) +
  geom_violin(fill='#fbe9d2', color='#fbe9d2', alpha=0.7) +
  geom_boxplot(alpha=0.7, outlier.shape=NA) +
  scale_x_discrete(labels=c('Sudden gains', 'Sudden losses', 'Others')) +
  labs(x='', y='Overall Risk Change') +
  theme_minimal()
