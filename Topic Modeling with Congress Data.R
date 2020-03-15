# Fit K-means to the speech text of the members, comprising of the 1000 phrases, for K in
# 5,10,15,20,25

#load dataset
load("congress.RData")
#scaling method : (x - mean) / sd
#the frenquency of terms = count of each term for each speaker / count of all terms for each speaker
congress <- scale(as.matrix(congress109Counts/rowSums(congress109Counts)))
#k means model
km5_25 = list()
for (k in 5*(1:5)){
  km5_25[[k]] = kmeans(congress,k)
}

# Use BIC to choose the K and interpret the selected model. Also use the elbow curve method to
# identify the most optimal value of K. 

kic <- function(fit, rule=c("A","B")){
  df <- length(fit$centers)
  n <- sum(fit$size)
  D <- fit$tot.withinss
  rule=match.arg(rule)
  if(rule=="A")
    return(D + 2*df*n/(n-df-1))
  else
    return(D + log(n)*df)
}
kfit <- lapply(5*(1:5), function(k) kmeans(congress,k))

#AIC
AICs <- sapply(kfit, kic, "A")
plot(x=5*(1:5), y = AICs, xlab="K", ylab="AICc", bty="n", type="l", lwd=2)

#BIC
BICs <- sapply(kfit, kic, "B")
plot(x=5*(1:5), y = BICs, xlab="K", ylab="BIC", bty="n", type="l", lwd=2)

#the elbow method
#k = 5
deviance <- lapply(5*(1:5), function(k) kmeans(congress, k)$tot.withinss)
plot(5*(1:5), deviance, xlab="K", ylab="Deviance")

# Fit a topic model for the speech counts. Use Bayes factors to choose the number of topics.

# Convert from a Matrix to a `slam' simple_triplet_matrix
x <- as.simple_triplet_matrix(congress109Counts)

# choosing the number of topics
tpcs <- topics(x, K=(5:30), verb=10)
summary(tpcs, n=10) # Choose 13 as the optimal num of topics.

# Alternatively, look at words ordered by simple in-topic prob
# Output the 13 topics.
for (i in 1:13){
  cat(i,"topic","\n")
  print(rownames(tpcs$theta)[order(tpcs$theta[,i], decreasing=TRUE)[1:10]])
}
  
# Connect the unsupervised clusters to partisanship. Tabulate party membership by K-means
# cluster. Fit topic regressions for each of party and repshare.
# Compare to regression onto phrase percentages: x <- 100 * congress109Counts /
# rowSums(congress109Counts)

# Tabulate party membership by K-means cluster.
km_25 = kmeans(congress, 25,nstart = 10)
table(party = congress109Ideology[,"party"],cluster = km_25$cluster)

#----------------------------------------------------
# topics regression for party
library(glmnet)
library(gamlr)

# Phrase percentage
x <- 100*congress109Counts/rowSums(congress109Counts)

# Progress the x and y variables
partisanship <- congress109Ideology[,"party"]
partisanship_char = as.character(partisanship)
topics_partyD <- tpcs$omega[c(which(partisanship_char %in% c('D'))),]
topics_partyR <- tpcs$omega[c(which(partisanship_char %in% c('R'))),]
x_partyD <- x[c(which(partisanship_char %in% c('D'))),]
x_partyR <- x[c(which(partisanship_char %in% c('R'))),]
repshare <- congress109Ideology[,"repshare"]
repshare_D <- repshare[c(which(partisanship_char %in% c('D')))]
repshare_R <- repshare[c(which(partisanship_char %in% c('R')))]

# set seed
set.seed(101)

# Regress topics model
tpcreg <- gamlr(topics_partyD, repshare_D)
drop(coef(tpcreg))*0.1

tpcreg <- gamlr(topics_partyR, repshare_R)
drop(coef(tpcreg))*0.1


# Regress topics clusters and phrase percentage
regtopics_partyD.cv <- cv.glmnet(topics_partyD, repshare_D)
regwords_partyD.cv <- cv.glmnet(x_partyD,repshare_D)
regtopics_partyR.cv <- cv.glmnet(topics_partyR, repshare_R)
regwords_partyR.cv <- cv.glmnet(x_partyR, repshare_R)

# Plot the deviance and lamnda figure to compare
par(mfrow=c(2,2))
plot(regtopics_partyD.cv)
mtext("topic regression party D", font=2, line=2)
plot(regwords_partyD.cv)
mtext("bigram regression party D", font=2, line=2)
plot(regtopics_partyR.cv)
mtext("topic regression party R", font=2, line=2)
plot(regwords_partyR.cv)
mtext("bigram regression party R", font=2, line=2)

# max OOS R^2s
(r2_tpcs_D <- max(1-regtopics_partyD.cv$cvm/regtopics_partyD.cv$cvm[1]))
(r2_words_D <- max(1-regwords_partyD.cv$cvm/regwords_partyD.cv$cvm[1]))
(r2_tpcs_R <- max(1-regtopics_partyR.cv$cvm/regtopics_partyR.cv$cvm[1]))
(r2_words_R <- max(1-regwords_partyR.cv$cvm/regwords_partyR.cv$cvm[1]))




