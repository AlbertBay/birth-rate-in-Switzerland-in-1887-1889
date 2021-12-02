#-------------------------1-----------------------------------------------------
group1 = swiss[1:10,]
group2 = swiss[11:47,]
group1
group2

d = rep(0,37)
res = rep(0,10)

group1_ec = group1[,2:5]
group2_ec = group2[,2:5]
group1_ec
group2_ec

group1_fert = group1[,1]
group2_fert = group2[,1]
group1_fert
group2_fert

for (i in 1:10){
  x = group1_ec[i,]
  for (j in 1:37){
    y = group2_ec[j,]
    d[j] = dist(rbind(x,y))
  }
  res[i] = group2_fert[which.min(d)]
}
wilcox.test(group1_fert, res, paired = T)

#-------------------------2-----------------------------------------------------
C = swiss[which(swiss$Catholic>80),1]
P = swiss[which(swiss$Catholic<20),1]
M = swiss[which(swiss$Catholic<80 & swiss$Catholic>20),1]
boxplot(cbind(C,M,P), col='blue')
kruskal.test(list(C,M,P))

wilcox.test(C,M,alternative = "greater")
wilcox.test(M,P,alternative = "less")
wilcox.test(C,P,alternative = "greater")

#-------------------------3-----------------------------------------------------
q_25 = quantile(swiss$Infant.Mortality, name=F)[2]

C = swiss[which(swiss$Catholic>80),]
P = swiss[which(swiss$Catholic<20),]
M = swiss[which(swiss$Catholic<80 & swiss$Catholic>20),]

C1 = mean(C[which(C$Agriculture>50 & C$Infant.Mortality<q_25),1])
C2 = mean(C[which(C$Agriculture<50 & C$Infant.Mortality<q_25),1])
C3 = mean(C[which(C$Agriculture>50 & C$Infant.Mortality>q_25),1])
C4 = mean(C[which(C$Agriculture<50 & C$Infant.Mortality>q_25),1])
C1
C2
C3
C4
# Пустота в С2
C2 = median(swiss[which(swiss$Agriculture<50 & swiss$Infant.Mortality<q_25),1])
C2

P1 = mean(P[which(P$Agriculture>50 & P$Infant.Mortality<q_25),1])
P2 = mean(P[which(P$Agriculture<50 & P$Infant.Mortality<q_25),1])
P3 = mean(P[which(P$Agriculture>50 & P$Infant.Mortality>q_25),1])
P4 = mean(P[which(P$Agriculture<50 & P$Infant.Mortality>q_25),1])
P1
P2
P3
P4

M1 = mean(M[which(M$Agriculture>50 & M$Infant.Mortality<q_25),1])
M2 = mean(M[which(M$Agriculture<50 & M$Infant.Mortality<q_25),1])
M3 = mean(M[which(M$Agriculture>50 & M$Infant.Mortality>q_25),1])
M4 = mean(M[which(M$Agriculture<50 & M$Infant.Mortality>q_25),1])
M1
M2
M3
M4
# Пустота в M1
M1 = median(swiss[which(swiss$Agriculture>50 & swiss$Infant.Mortality<q_25),1])
M1

res = rbind(c(C1,C2,C3,C4), c(P1,P2,P3,P4), c(M1,M2,M3,M4))
friedman.test(res)
о