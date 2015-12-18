setwd("D:/Google Drive/Work/Experiences/experiment-globaleffect/img")
title_text = ""

result.D <- read.csv("DFCAd-toR3.csv") 
result.D$DCA <- result.D$DFCAd
result.T <- read.csv("DFCAt-toR3.csv") 
result.T$DCA = result.T$DFCAt

list_results = list(result.T, result.D)

#["DFCAd/t", "Participant", "Condition", "Distance"]
for (result in list_results){ 
  print ("Quick look at the dataframe:")
  result <- as.data.frame(result, stringsAsFactors=TRUE)
  result[1:10,]
  
  
  ## Data conversion for analysis
  result$DCA <- as.numeric(result$DCA)
  
  #result$Participant <- as.numeric(result$Participant)
  #result$Distance <- as.factor(result$Distance)
  result$TypeDistractor <- as.factor(result$TypeDistractor)
  result$Frequence <- as.factor(result$Frequence)
  result$Participant <- as.factor(result$Participant)
  #for (p in unique(result$Participant)){
  #  result$DCA[result$Participant == p] <- result$DCA[result$Participant == p] - mean(result$DCA[result$Participant == p]) + mean(result$DCA)
  #}
  print (result[1:10,])
}

## Pairwise u.test:
# we use the non parametric independent 2-group Mann-Whitney U Test; i.e. wilcox.test(y,x):
library(gtools)
library(orddom)

factors <- list(c("TypeDistractor","Td", "Ts"), c("Frequence","F8", "F2"))
df_model = data.frame(Name= character(0),
                Distance = numeric(0),
                p.value= numeric(0),
                u.stat= numeric(0),
                #mean.diff= numeric(0),
                #mean.same= numeric(0),
                n.diff= numeric(0),
                n.same= numeric(0),
                #sd.diff = numeric(0),
                #sd.same = numeric(0),
                CLES = numeric(0),
                pmedian = numeric(0),
                conf.int1 = numeric(0),
                conf.int2 = numeric(0),
                stringsAsFactors=FALSE)
list_dfs <- list(df.T.Type = data.frame(df_model), 
                 df.T.Freq = data.frame(df_model),
                 df.D.Type = data.frame(df_model),
                 df.D.Freq = data.frame(df_model))

icondition = 1
for (result in list_results){ #  Target / Distractor results
  #r1 <- result
  #r1 <- result[result$Participant != "RI" & result$Participant != "BD",] ### WARNING!!!
  #r1 <- result[result$Participant != "RI",] ### WARNING!!!
  groups = with(r1, interaction(Distance))
  #groups = with(r1, interaction(Participant))
  list_groups = unique(groups)
  for (factor in factors){ # TypeDistractor / Frequence
    i=1
    df = list_dfs[[icondition]]
    print(factor)
    colnames(df)[5:6] <- c( paste0("n.", factor[2]), paste0("n.", factor[3]) )
    for (group in list_groups) { # Distances
      #print(group)
      #print(factor[1])
      ydiff = r1$DCA[groups == group & r1[factor[1]] == factor[2]]
      ysame = r1$DCA[groups == group & r1[factor[1]] == factor[3]]
      #print("break")
      test = wilcox.test(ydiff, ysame, conf.int =TRUE)
      #print("break")
      #df[i,]$Distance = as.numeric(group)
      df[i,]$Distance = group
      #print("break")
      df[i,]$p.value = test$p.value
      #print("break")
      df[i,]$u.stat = test$statistic
      #print("break")
      #df[i,]$fd = as.numeric(test$parameter)
      #df[i,]$mean.same = mean(ysame)
      #df[i,]$mean.diff = mean(ydiff)
      df[i,5] = length(ydiff)
      df[i,6] = length(ysame)
      #df[i,]$sd.same = sd(ysame)
      #df[i,]$sd.diff = sd(ydiff)
      #es.all = dmes(ydiff,ysame)
      #df[i,]$CLES = es.all$PSc
      df[i,]$CLES = 1 - df[i,]$u.stat / (df[i,5]*df[i,6]) ## WARNING, access n.diff and n.same with index
      df[i,]$pmedian = test$estimate
      df[i,]$conf.int1 = test$conf.int[1]
      df[i,]$conf.int2 = test$conf.int[2]
      #df[i,]$AUC = es.all$Ac
      i = i + 1
      #print("Yeah!")
    }
    ## Once there is rows, we mark them with a name:
    if (icondition <= 2){
      df$Name <- paste0("Target.", factor[1])
    } else {
      df$Name <- paste0("Distractor.", factor[1])
    }
    options(scipen=1)
    #format(df, scientific = FALSE)
    df$p.value <- p.adjust(df$p.value, "bonferroni")
    df$stars <- stars.pval(df$p.value)
    #print("break")
    #print(df)
    ## significance level: 
    sl = 0.05 # there is 5 comparison
    print("Significant Distances:")
    print(df$Name[df$p.value < sl])
    list_dfs[[icondition]] = df
    icondition= icondition + 1    
    #print("break")
  }
}

list_dfs
all_df = df_model[,-(5:6)]
for (df in list_dfs){
  all_df = rbind(all_df, df[-(5:6)])
}
all_df
write.csv(all_df, "analysis_DFCA_results-controlperblock.csv")

par(mfrow = c(2,1))
plot(0, 0, type="n", ylim=c(-0.6, 0.6), xlim=c(10, 91.25))
abline(h=0, lty="dashed") 
i = 0
inverse1 =1; inverse2 =1
for (df in list_dfs){  
  distance = seq(11.25, 90, 11.25) +i*0.8
  avg = df$CLES
  if (i > 1){      # Distractor
    color = "orange"
    #inverse1 = -1
  } else {         # Target
    color = "red"
    #inverse = 1
  }
  if ( (i %% 2) == 1){ # Frequence
    lstyle = "dashed"
    inverse2 = 1
  } else {        # Type
    lstyle = "solid"
    inverse2 = -1
  }
  lines(distance, inverse2*inverse1*df$pmedian, type="o", lty=lstyle, col=color, lw=2.0) #, ylim=c(.4, .6)) 
  arrows(distance, inverse2*inverse1*df$conf.int1, 
         distance, inverse2*inverse1*df$conf.int2, 
         length=0.05, angle=90, code=3, lty=lstyle, col=color, lw=2.0)
  
  i = i + 1
}

plot(0, 0, type="n",  ylim=c(0.40, 0.60), xlim=c(10, 91.25))
abline(h=0.50, lty="dashed")
i = 0
for (df in list_dfs){  
  distance = seq(11.25, 90, 11.25) +i*0.8
  avg = df$CLES
  if (i > 1){      # Distractor
    color = "orange"
    #inverse1 = -1
  } else {         # Target
    color = "red"
    #inverse = 1
  }
  if ( (i %% 2) == 1){ # Frequence
    lstyle = "dashed"
    avg <- 1- df$CLES
  } else {        # Type
    lstyle = "solid"
    avg <- df$CLES
  }
  lines(distance, avg, type="o", lty=lstyle, col=color, lw=2.0) #, ylim=c(.4, .6))
  i = i + 1
}


# 
# distance = seq(11.25, 90, 11.25)
# avg <- - df$mean.same + df$mean.diff
# plot(distance, avg)
# sdev = 1.96 * (df$sd.same + df$sd.diff) / sqrt(df$n.diff + df$n.same)
# arrows(distance, avg-sdev, distance, avg+sdev, length=0.05, angle=90, code=3)



## Bayesian Anova on DCA for one distance:
library(BayesFactor)
bfis <- c()
for (result in list_results){ 
  r1 <- result
  r1$Distance <- as.factor(r1$Distance)
  bfi <- generalTestBF(DCA ~ TypeDistractor*Frequence*Distance + Participant, 
                     data=r1, whichRandom="Participant", whichModels="top")
  bfis <- c(bfis, bfi)
}
# Target side
bfis[1]
# Distractor side:
bfis[2]

Tside <- list_results[[1]]
Tside <-Tside[,!names(Tside) == "DFCAt"]
Tside$Side <- "Target"
Dside <- list_results[[2]]
Dside <-Dside[,!names(Dside) == "DFCAd"]
Dside$Side <- "Distractor"
Dside$DCA <- (-Dside$DCA) ## Clever !

allSide <- rbind(Dside, Tside)
allSide$Side <- as.factor(allSide$Side)

bfi <- generalTestBF(DCA ~ TypeDistractor*Side + Frequence*Side + Distance*Side + Participant, 
                     data=allSide, whichRandom="Participant", whichModels="top")

bfi <- generalTestBF(DCA ~ TypeDistractor*Side + Frequence*Distance*Side + Participant, 
                     data=allSide, whichRandom="Participant", whichModels="top")

bfi <- generalTestBF(DCA ~ TypeDistractor + Side + Frequence + Distance + Participant + 
                       TypeDistractor:Side + 
                       Frequence:Distance + 
                       Side:Frequence +
                        TypeDistractor:Frequence,
                     data=allSide, whichRandom="Participant", whichModels="top")


# Bayes factor top-down analysis
# --------------
#   When effect is omitted from TypeDistractor + Frequence + Distance + Participant + TypeDistractor:Frequence + TypeDistractor:Distance + Frequence:Distance + TypeDistractor:Frequence:Distance , BF is...
# [1] Omit Distance:Frequence:TypeDistractor : 3028.094      ±6.19%
# [2] Omit Distance:Frequence                : 0.003001699   ±6.69%
# [3] Omit Distance:TypeDistractor           : 115390.4      ±6.49%
# [4] Omit Frequence:TypeDistractor          : 31.19554      ±6.37%
# [5] Omit Participant                       : 3.860878e-784 ±7.33%
# [6] Omit Distance                          : 1.080168e-100 ±6.47%
# [7] Omit Frequence                         : 3.519445e-13  ±6.53%
# [8] Omit TypeDistractor                    : 0.004880289   ±6.77%
# 
# Against denominator:
#   DCA ~ TypeDistractor + Frequence + Distance + Participant + TypeDistractor:Frequence + TypeDistractor:Distance + Frequence:Distance + TypeDistractor:Frequence:Distance 
# ---
#   Bayes factor type: BFlinearModel, JZS
max(bfi)
ordered_bfi = sort(bfi, decreasing=TRUE)
nbfi = ordered_bfi / max(bfi)
nbfi
