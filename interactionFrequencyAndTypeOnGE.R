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
r1 <- result
for (result in list_results){ #  Target / Distractor results
  #r1 <- result
  #r1 <- result[result$Participant != "RI" & result$Participant != "BD",] ### WARNING!!!
  #r1 <- result[result$Participant != "RI"& result$Participant != "BD",] ### WARNING!!!
  groups = with(r1, interaction(Distance))
  #groups = with(r1, interaction(Participant))
  list_groups = unique(groups)
  for (DType in c("Ts", "Td")){ # TypeDistractor / Frequence
    i=1
    df = list_dfs[[icondition]]
    print(factor)
    colnames(df)[5:6] <- c( paste0("n.", factor[2]), paste0("n.", factor[3]) )
    for (group in list_groups) { # Distances
      #print(group)
      #print(factor[1])
      yF80 = r1$DCA[groups == group & r1$Frequence == "F8" & r1$TypeDistractor == DType]
      yF20 = r1$DCA[groups == group & r1$Frequence == "F2" & r1$TypeDistractor == DType]
      #print("break")
      test = wilcox.test(yF80, yF20, conf.int =TRUE)
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
      df[i,5] = length(yF80)
      df[i,6] = length(yF20)
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
      df$Name <- paste0("Target.", DType)
    } else {
      df$Name <- paste0("Distractor.", DType)
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
write.csv(all_df, "analysis_DFCA_interaction-controlperblock.csv")