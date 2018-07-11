rm(list = ls())
gc()
?rm

cup98 <- read.csv("/Users/yaroina-kente/Desktop/Мои_Штукензии/PhD/Data_Mining/Project/dataset-Q2/training.txt")
dim(cup98)
(response.percentage <- round(100 * prop.table(table(cup98$TARGET_B)), digits=1))
mylabels <- paste("TARGET_B=", names(response.percentage), "\n", 
                  response.percentage, "%", sep="")
pie(response.percentage, labels=mylabels)

cup98pos <- cup98[cup98$TARGET_D>0, ]
targetPos <- cup98pos$TARGET_D
summary(targetPos)
boxplot(targetPos)

length(targetPos)
sum(!(targetPos %in% 1:200))
targetPos <- round(targetPos)
barplot(table(targetPos), las=2)

cup98$TARGET_D2 <- cut(cup98$TARGET_D, right=F,
                       breaks=c(0, 0.1, 10, 15, 20, 25, 30, 50, max(cup98$TARGET_D)))
table(cup98$TARGET_D2)
cup98pos$TARGET_D2 <- cut(cup98pos$TARGET_D, right=F,
                          breaks=c(0, 0.1, 10, 15, 20, 25, 30, 50, max(cup98pos$TARGET_D)))

table(cup98$RFA_2R)
round(100 * prop.table(table(cup98$NOEXCH)), digits=3)

varSet <- c(
  # demographics
  "ODATEDW", "OSOURCE", "STATE", "ZIP", "PVASTATE", "DOB", "RECINHSE",
  "MDMAUD", "DOMAIN", "CLUSTER", "AGE", "HOMEOWNR", "CHILD03", "CHILD07",
  "CHILD12", "CHILD18", "NUMCHLD", "INCOME", "GENDER", "WEALTH1", "HIT", 
  # donor interests
  "COLLECT1", "VETERANS", "BIBLE", "CATLG", "HOMEE", "PETS", "CDPLAY",
  "STEREO", "PCOWNERS", "PHOTO", "CRAFTS", "FISHER", "GARDENIN", "BOATS",
  "WALKER", "KIDSTUFF", "CARDS", "PLATES",
  # PEP star RFA status
  "PEPSTRFL",
  # summary variables of promotion history
  "CARDPROM", "MAXADATE", "NUMPROM", "CARDPM12", "NUMPRM12",
  # summary variables of giving history
  "RAMNTALL", "NGIFTALL", "CARDGIFT", "MINRAMNT", "MAXRAMNT", "LASTGIFT",
  "LASTDATE", "FISTDATE", "TIMELAG", "AVGGIFT",
  # ID & targets
  "CONTROLN", "TARGET_B", "TARGET_D", "TARGET_D2", "HPHONE_D",
  # RFA (Recency/Frequency/Donation Amount)
  "RFA_2F", "RFA_2A", "MDMAUD_R", "MDMAUD_F", "MDMAUD_A",
  #others
  "CLUSTER2", "GEOCODE2")
cup98 <- cup98[, varSet]

idx.num <- which(sapply(cup98, is.numeric))
layout(matrix(c(1,2), 1, 2)) 
myHist <- function(x) {
  hist(cup98[,x], main=NULL, xlab=x)
}
sapply(names(idx.num), myHist)

layout(matrix(c(1,2),1,2)) 
boxplot(cup98$HIT)
cup98$HIT[cup98$HIT>200]
boxplot(cup98$HIT[cup98$HIT<200])
layout(matrix(1))

AGE2 <- cut(cup98pos$AGE, right=F, breaks=seq(0, 100, by=5))
boxplot(cup98pos$TARGET_D ~ AGE2, ylim=c(0,40), las=3)

attach(cup98pos)
layout(matrix(c(1,2),1,2)) # 2 graphs per page
boxplot(TARGET_D ~ GENDER, ylim=c(0,80))

plot(density(TARGET_D[GENDER=="F"]), xlim=c(0,60), col=1, lty=1)
lines(density(TARGET_D[GENDER=="M"]), col=2, lty=2)
lines(density(TARGET_D[GENDER=="J"]), col=3, lty=3)
legend("topright", c("Female", "Male", "Joint account"), col=1:3, lty=1:3)
layout(matrix(1))
detach(cup98pos)



correlation <- cor(cup98$TARGET_D, cup98[,idx.num], use="pairwise.complete.obs")
correlation <- abs(correlation)
(correlation <- correlation[,order(correlation, decreasing=T)])

write.csv(correlation, "absolute_correlation.csv")

color <- ifelse(cup98$TARGET_D>0, "blue", "black")
pch <- ifelse(cup98$TARGET_D>0, "+", ".")
plot(jitter(cup98$AGE), jitter(cup98$HIT), pch=pch, col=color, cex=0.7, 
     ylim=c(0,70), xlab="AGE", ylab="HIT")
legend("topleft", c("TARGET_D>0", "TARGET_D=0"), col=c("blue", "black"), 
       pch=c("+", "."))

myChisqTest <- function(x) {
  t1 <- table(cup98pos[,x], cup98pos$TARGET_D2)
  plot(t1, main=x, las=1)
  print(x)
  print(chisq.test(t1))
}
myChisqTest("GENDER")

idx.cat <- which(sapply(cup98pos, is.factor))
categ <- sapply(names(idx.cat), myChisqTest)
write.csv(categ, "chi_correlation.csv")

nRec <- dim(cup98)[1]
trainSize <- round(nRec * 0.7)
testSize <- nRec - trainSize
# ctree parameters   
MinSplit <- 1000
MinBucket <- 400
MaxSurrogate <- 4
MaxDepth <- 10  
(strParameters <- paste(MinSplit, MinBucket, MaxSurrogate, MaxDepth, sep="-"))
LoopNum <- 9
cost <- 0.68
varSet2 <- c("AGE", "AVGGIFT", "CARDGIFT", "CARDPM12", "CARDPROM", "CLUSTER2",
             "DOMAIN", "GENDER", "GEOCODE2", "HIT", "HOMEOWNR", "HPHONE_D", "INCOME",
             "LASTGIFT", "MAXRAMNT", "MDMAUD_F", "MDMAUD_R", "MINRAMNT", "NGIFTALL",
             "NUMPRM12", "PCOWNERS", "PEPSTRFL", "PETS", "RAMNTALL", "RECINHSE", 
             "RFA_2A", "RFA_2F", "STATE", "TIMELAG")
cup98 <- cup98[, c("TARGET_D", varSet2)]
install.packages("party")
library(party)

pdf(paste("evaluation-tree-", strParameters, ".pdf", sep=""), 
width=12, height=9, paper="a4r", pointsize=6)
cat(date(), "\n")
cat(" trainSize=", trainSize, ", testSize=", testSize, "\n")
cat(" MinSplit=", MinSplit, ", MinBucket=", MinBucket, 
", MaxSurrogate=", MaxSurrogate, ", MaxDepth=", MaxDepth, "\n\n")
allTotalDonation <- matrix(0, nrow=testSize, ncol=LoopNum)
allAvgDonation <- matrix(0, nrow=testSize, ncol=LoopNum)
allDonationPercentile <- matrix(0, nrow=testSize, ncol=LoopNum)
for (loopCnt in 1:LoopNum) {
cat(date(), ":  iteration = ", loopCnt, "\n")
trainIdx <- sample(1:nRec, trainSize)
trainData <- cup98[trainIdx,]
testData <- cup98[-trainIdx,]
myCtree <- ctree(TARGET_D ~ ., data=trainData,
controls=ctree_control(minsplit=MinSplit, minbucket=MinBucket,
                                maxsurrogate=MaxSurrogate, maxdepth=MaxDepth))

print(object.size(myCtree), units="auto")
save(myCtree, file=paste("cup98-ctree-", strParameters, "-run-", 
                            loopCnt, ".rdata", sep=""))
figTitle <- paste("Tree", loopCnt)
plot(myCtree, main=figTitle, type="simple", ip_args=list(pval=FALSE), 
        ep_args=list(digits=0,abbreviate=TRUE), tp_args=list(digits=2))
print(myCtree)

pred <- predict(myCtree, newdata=testData)
plot(pred, testData$TARGET_D)
print(sum(testData$TARGET_D[pred > cost] - cost))

s1 <- sort(pred, decreasing=TRUE, method = "quick", index.return=TRUE)
totalDonation <- cumsum(testData$TARGET_D[s1$ix]) # cumulative sum
avgDonation <- totalDonation / (1:testSize)
donationPercentile <- 100 * totalDonation / sum(testData$TARGET_D)
allTotalDonation[,loopCnt] <- totalDonation
allAvgDonation[,loopCnt] <- avgDonation
allDonationPercentile[,loopCnt] <- donationPercentile
plot(totalDonation, type="l")
grid()

}

graphics.off()
cat(date(), ":  Loop completed.\n\n\n")
fnlTotalDonation <- rowMeans(allTotalDonation)
fnlAvgDonation <- rowMeans(allAvgDonation)
fnlDonationPercentile <- rowMeans(allDonationPercentile)
rm(trainData, testData, pred)

results <- data.frame(cbind(allTotalDonation,fnlTotalDonation))
names(results) <- c(paste("run",1:LoopNum), "Average")
write.csv(results, paste("evaluation-TotalDonation-", strParameters, ".csv",
                          sep=""))  

result <- read.csv("evaluation-TotalDonation-1000-400-4-10.csv")
head(result)
result[,2:12] <- result[,2:12] - cost * (1:testSize)
idx.pos <- c(seq(1, nrow(result), by=10), nrow(result))
plot(result[idx.pos,12], type="l", lty=1, col=1, ylim=c(0,4500), 
     xlab="Number of Mails", ylab="Amount of Donations ($)")
for (fCnt in 1:LoopNum) {
  lines(result[idx.pos,fCnt+1], pty=".", type="l", lty=1+fCnt, col=1+fCnt)
}
legend("bottomright", col=1:(LoopNum+1), lty=1:(LoopNum+1), 
       legend=c("Average", paste("Run",1:LoopNum)))

donationPercentile <- sapply(2:12, function(i) 
  100 *  result[,i] / result[testSize,i])
percentile <- 100 * (1:testSize)/testSize
plot(percentile[idx.pos], donationPercentile[idx.pos,11], pty=".", type="l",
     lty=1, col=1, ylim=c(0,170), xlab="Contact Percentile (%)",
     ylab="Donation Percentile (%)")
grid(col = "gray", lty = "dotted")
for (fCnt in 1:LoopNum) {
  lines(percentile[idx.pos], donationPercentile[idx.pos,fCnt], pty=".",
        type="l", lty=1+fCnt, col=1+fCnt)
}
legend("bottomright", col=1:(LoopNum+1), lty=1:(LoopNum+1), 
       legend=c("Average", paste("Run",1:LoopNum)))

avgDonation <- sapply(2:12, function(i) result[,i] / (1:testSize))
yTitle = c("Total Donation Amount Percentile (%)",
           "Average Donation Amount per Contact ($)")
par(mar=c(5,4,4,5)+.1)
plot(percentile[idx.pos], donationPercentile[idx.pos,7], pty=".", type="l",
     lty="solid", col="red", ylab=yTitle[1], xlab="Contact Percentile (%)")
grid(col = "gray", lty = "dotted")
par(new=TRUE)
plot(percentile[idx.pos], avgDonation[idx.pos,7], type="l", lty="dashed",
     col="blue", xaxt="n", yaxt="n", xlab="", ylab="",
     ylim=c(0,max(avgDonation[,7])))
axis(4)     
mtext(yTitle[2], side=4, line=2)
legend("right", col=c("red","blue"), lty=c("solid","dashed"),
       legend=yTitle)

parameters <- c("1000-400-4-5", "1000-400-4-6", "1000-400-4-8", "1000-400-4-10")
# parameters <- c("1000-400-4-10", "700-200-4-10", "200-50-4-10")
paraNum <- length(parameters)
percentile <- 100 * (1:testSize)/testSize

results <- read.csv(paste("evaluation-TotalDonation-", parameters[1], ".csv",
                          sep=""))
avgResult <- results$Average - cost * (1:testSize)
plot(percentile, avgResult, pty=1, type="l", lty=1, col=1, #ylim=c(0,4000),
    ylab="Amount of Donation", xlab="Contact Percentile (%)", 
    main="Parameters: MinSplit, MinBucket, MaxSurrogate, MaxDepth")
grid(col = "gray", lty = "dotted")     

for (i in 2:paraNum) {
results <- read.csv(paste("evaluation-TotalDonation-", parameters[i], 
                            ".csv", sep=""))
avgResult <- results$Average - cost * (1:testSize)
lines(percentile, avgResult, type="l", lty=i, col=i)
}
legend("bottomright", col=1:paraNum, lty=1:paraNum, legend=parameters)

cup98val <- read.csv("/Users/yaroina-kente/Desktop/Мои_Штукензии/PhD/Data_Mining/Project/dataset-Q2/validation.txt")

cup98val <- cup98val[, c("CONTROLN", varSet2)]
trainNames <- names(cup98)
scoreNames <- names(cup98val)

idx <- which(trainNames %in% scoreNames)
print(trainNames[-idx])

scoreData <- cup98val
vars <- intersect(trainNames, scoreNames)
for (i in 1:length(vars)) {
  varname <- vars[i]
  trainLevels <- levels(cup98[,varname])
  scoreLevels <- levels(scoreData[,varname])
  if (is.factor(cup98[,varname]) & setequal(trainLevels, scoreLevels)==F) {
    cat("Warning: new values found in score data, and they will be changed to NA!\n")
    cat(varname, "\n")
    cat("train: ", length(trainLevels), ", ", trainLevels, "\n")
    cat("score: ", length(scoreLevels), ", ", scoreLevels, "\n\n")
    scoreData[,varname] <- factor(scoreData[,varname], levels=trainLevels)
  }
}
rm(cup98val)

load("cup98-ctree-1000-400-4-10-run-7.Rdata")

pred <- predict(myCtree, newdata = scoreData)
pred <- round(pred, digits=3)
table(pred, useNA="ifany")
result <- data.frame(scoreData$CONTROLN, pred)
names(result) <- c("CONTROLN", "pred")

load("response-result.rdata")

valTarget <- read.csv("./data/KDDCup1998/valtargt.txt")
merged <- merge(result, valTarget, by="CONTROLN")

sum(valTarget$TARGET_D - cost)

idx <- (merged$pred > cost)
sum(merged$TARGET_D[idx] - cost)


merged <- merged[order(merged$pred, decreasing=T),]
x <- 100 * (1:nrow(merged)) / nrow(merged)
y <- cumsum(merged$TARGET_D) - cost*(1:nrow(valTarget))

idx.pos <- c(seq(1, length(x), by=10), length(x))
plot(x[idx.pos], y[idx.pos], type="l", xlab="Contact Percentile (%)",
     ylab="Amount of Donation")
grid()
