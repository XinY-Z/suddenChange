#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyze distribution of sudden gains and losses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(ggplot2)
library(ggrepel)
library(gridExtra)
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
dt.cpt[suddGains!='' & !is.na(suddGains), .N]/dt.cpt[, .N]
dt.cpt[suddGains!='' & !is.na(suddGains), median(str_count(suddGains, ','))+1]
dt.cpt[suddGains!='' & !is.na(suddGains), max(str_count(suddGains, ','))+1]

dt.cpt[suddLosses!='' & !is.na(suddLosses), .N]/dt.cpt[, .N]
dt.cpt[suddLosses!='' & !is.na(suddLosses), median(str_count(suddLosses, ','))+1]
dt.cpt[suddLosses!='' & !is.na(suddLosses), max(str_count(suddLosses, ','))+1]

dt.cpt[suddGains!='' & !is.na(suddGains) & suddLosses!='' & !is.na(suddLosses), .N]


# timing ####
temp <- dt.cpt[
  suddGains!='' & !is.na(suddGains), 
  as.numeric(str_split(suddGains, ',', simplify=T))
]
mean(temp, na.rm=T)
median(temp, na.rm=T)
sd(temp, na.rm=T)
quantile(temp, c(0.025, 0.975), na.rm=T)

temp <- dt.cpt[
  suddLosses!='' & !is.na(suddLosses), 
  as.numeric(str_split(suddLosses, ',', simplify=T))
]
mean(temp, na.rm=T)
median(temp, na.rm=T)
sd(temp, na.rm=T)
quantile(temp, c(0.025, 0.975), na.rm=T)


# reversal ####
gain_id <- dt.cpt[suddGains!='' & !is.na(suddGains), .(encounter_id, suddGains)]
#gain_id <- dt.cpt[suddLosses!='' & !is.na(suddLosses), .(encounter_id, suddLosses)]
temp <- dt[
  encounter_id %in% gain_id$encounter_id, 
  .(encounter_id, messageInd, neural_prob)
]

rev_encounters <- data.table()
for(i in gain_id$encounter_id) {
  
  #if(i > 500) break
  
  cutoffs <- matrix(nrow=0, ncol=2)
  
  gain_points <- gain_id[
    encounter_id==i, 
    as.numeric(str_split(suddLosses, ',', simplify=T))
  ]
  
  for(j in gain_points) {
    cutoff <- mean(c(
      temp[encounter_id==i & messageInd==j, neural_prob], 
      temp[encounter_id==i & messageInd==(j+1), neural_prob]
    )
    )
    cutoffs <- rbind(cutoffs, c(j, cutoff))
  }
  
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


# magnitudes ####
temp <- dt[, .(pre=mean(first(neural_prob, 3))), by=encounter_id]

all_mag_gain <- c()
for(i in gain_id$encounter_id) {
  gain_points <- gain_id[
    encounter_id==i, 
    as.numeric(str_split(suddGains, ',', simplify=T))
  ]
  for(j in gain_points) {
    mag <- dt[encounter_id==i & messageInd==(j+1), abs(absDiff)]
    all_mag_gain <- c(all_mag_gain, mag)
  }
}

all_mag_loss <- c()
loss_id <- dt.cpt[suddLosses!='' & !is.na(suddLosses), .(encounter_id, suddLosses)]
for(i in loss_id$encounter_id) {
  loss_points <- loss_id[
    encounter_id==i, 
    as.numeric(str_split(suddLosses, ',', simplify=T))
  ]
  for(j in loss_points) {
    mag <- dt[encounter_id==i & messageInd==(j+1), abs(absDiff)]
    all_mag_loss <- c(all_mag_loss, mag)
  }
}

mean(all_mag_gain/sd(temp$pre))
mean(all_mag_loss/sd(temp$pre))


# both gains and losses ####
temp <- dt.cpt[suddGains!='' & !is.na(suddGains) & suddLosses!='' & !is.na(suddLosses)]
temp[, .N]/dt.cpt[, .N]
temp_gain <- temp[, as.numeric(str_split(suddGains, ',', simplify=T)), by=1:nrow(temp)]
temp_loss <- temp[, as.numeric(str_split(suddLosses, ',', simplify=T)), by=1:nrow(temp)]

gain_loss <- c()
for(i in temp_gain[, unique(nrow)]) {
  gain_loss <- c(
    gain_loss, 
    all(temp_loss[nrow==i, max(V1)] - temp_gain[nrow==i, V1] > 0)
  )
}
sum(gain_loss)
mean(gain_loss)

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
)
]
dt.cpt[, group := factor(group, levels=c('suddGain', 'suddLoss', 'No sudd'))]

dt.cpt[group=='suddGain', c(mean(chg_prob), sd(chg_prob))]
dt.cpt[group=='suddLoss', c(mean(chg_prob), sd(chg_prob))]
dt.cpt[group!='No sudd', c(mean(chg_prob), sd(chg_prob))]

kruskal.test(chg_prob ~ group, dt.cpt)
pairwise.wilcox.test(
  dt.cpt$chg_prob, 
  dt.cpt$group, 
  p.adjust.method='holm'
)

hedges_g(dt.cpt[group=='suddGain', chg_prob], dt.cpt[group=='suddLoss', chg_prob], pooled_sd=F)
hedges_g(dt.cpt[group=='suddGain', chg_prob], dt.cpt[group=='No sudd', chg_prob], pooled_sd=F)
hedges_g(dt.cpt[group=='suddLoss', chg_prob], dt.cpt[group=='No sudd', chg_prob], pooled_sd=F)

# visualization
ggplot(dt.cpt, aes(y=chg_prob, x=group)) +
  geom_hline(yintercept=0, linetype='solid', color='grey', size=0.8) +
  geom_violin(fill='#fbe9d2', color='#fbe9d2', alpha=0.7) +
  geom_boxplot(alpha=0.7, outlier.shape=NA) +
  scale_x_discrete(labels=c('Sudden gains', 'Sudden losses', 'Others')) +
  labs(x='', y='Overall Risk Change') +
  theme_minimal()
