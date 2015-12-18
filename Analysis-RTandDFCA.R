library(reshape2)
library(car)
library(moments)

setwd("D:/Google Drive/Work/Experiences/experiment-globaleffect/img")
double_stim = TRUE
title_text = ""


result <- read.csv("all_RTwithDFCA_blocks.csv")
print ("Quick look at the dataframe:")
result <- as.data.frame(result)
result[1:10,]


## Data conversion for analysis
result$RT <- as.numeric(result$RT)
result$iRT <- 1/result$RT
result$logRT <- log10(result$RT)
result$DFCA <- as.numeric(result$DFCA)
#result$Participant <- as.numeric(result$Participant)
result$Distance <- as.factor(result$Distance)
result$Block <- as.factor(result$Block)
result$TypeDistractor <- as.factor(result$TypeDistractor)
result$Frequence <- as.factor(result$Frequence)
result$Side[result$Side == 1] <- "Target"
result$Side[result$Side == -1] <- "Distractor"
result$Side <- as.factor(result$Side)
result$Participant <- as.factor(result$Participant)
result[1:10,]

skewness(result$RT)
skewness(result$iRT)
skewness(result$logRT)

#select = abs( (result$iRT - mean(result$iRT))/sd(result$iRT) ) < 2.5
#result = result[select,]

## Data selection:
r1 <- result
#r1 <- result[result$Distance == 5.625,] 
#r1 <- result[result$Distance == 5.625 | result$Distance == 11.250,] 
#r1 <- droplevels(r1)
wilcox.test(r1$RT[r1$Frequence=="F2"], r1$RT[r1$Frequence=="F8"], conf.int =TRUE)
wilcox.test(r1$RT[r1$TypeDistractor=="Td"], r1$RT[r1$TypeDistractor=="Ts"], conf.int =TRUE)

# Effect of Block (Training Effect) ------
r1$Conditions = with(r1, interaction(TypeDistractor, Frequence, Block))
r1$Conditions = factor(r1$Conditions, c("Ts.F2.1","Td.F2.1","Ts.F8.1","Td.F8.1",
                                        "Td.F8.2","Ts.F8.2","Td.F2.2","Ts.F2.2"))
par(mfrow=c(1,1))
title = sprintf("Training Effect (%s Stim.)", title_text)
with(r1,
     boxplot(DFCA ~ Conditions*Participant, col=rep(c("gold","pink"), each=8), notch=TRUE,
             main= title, #ylim=c(0, 0.010),
             ylab=expression(paste("Global Effect (visual degrees)")), las=2
             #names= c("ShortS1.ShortGap", "ShortS1.LongGap", "LongS1.ShortGap", "LongS1.LongGap")
     )
)
# Training Effect, in passing order ------------------
par(mfrow=c(1,1))
title = sprintf("Training Effect (%s Stim.)", title_text)
with(r1,
     boxplot(DFCA ~ Block, col=rep(c("gold","pink"), each=8), notch=TRUE,
             main= title, #ylim=c(0, 0.010),
             ylab=expression(paste("Global Effect (visual degrees)")), las=2
             #names= c("ShortS1.ShortGap", "ShortS1.LongGap", "LongS1.ShortGap", "LongS1.LongGap")
     )
)



# Data Plot ---------
par(mfrow=c(1,1))
title = sprintf("Inverse Reaction Times According to Conditions (%s Stim.)", title_text)
with(r1,
     boxplot(RT ~ Frequence*Participant, col=c("gold","pink"), notch=TRUE,
             main= title, #ylim=c(0, 0.010),
             ylab=expression(paste("Reaction Time Inverse (ms"^"-1",")")), las=2
             #names= c("ShortS1.ShortGap", "ShortS1.LongGap", "LongS1.ShortGap", "LongS1.LongGap")
     )
)
# Data Plot --------
par(mfrow=c(2,1))
title = sprintf("Inverse Reaction Times According to Conditions (%s Stim.)", title_text)
## Plot the condition Different:
select = r1$TypeDistractor == "Td" & r1$Side=="Target" ## in RED
plot(r1$RT[select], r1$DFCA[select], col=rgb(1, 0, 0, 0.1), main="Different", xlim=c(0,800), ylim=c(-5, 10))
select = r1$TypeDistractor == "Td" & r1$Side=="Distractor" ## in BLUE
points(r1$RT[select], r1$DFCA[select], col=rgb(0, 0, 1, 0.1) )
## Plot the condition Similar:
select = r1$TypeDistractor == "Ts" & r1$Side=="Target" ## in RED
plot(r1$RT[select], r1$DFCA[select], col=rgb(1, 0, 0, 0.1), main="Similar", xlim=c(0,800), ylim=c(-5, 10))
select = r1$TypeDistractor == "Ts" & r1$Side=="Distractor" ## in BLUE
points(r1$RT[select], r1$DFCA[select], col=rgb(0, 0, 1, 0.1) )

# Data Plot of RT -----
par(mfrow=c(2,1))
title = sprintf("Inverse Reaction Times According to Conditions (%s Stim.)", title_text)
## Plot the condition Different:
colTarget = rgb(1, 0, 0, 1) # RED
colDistractor = rgb(0, 0, 1, 1) # BLUE
select = r1$TypeDistractor == "Td" & r1$Side=="Target"
plot(density(r1$RT[select]), col=colTarget , main="Different", xlim=c(0,800), ylim=c(0,0.015))
select = r1$TypeDistractor == "Td" & r1$Side=="Distractor"
lines(density(r1$RT[select]), col=colDistractor )
legend(700,0.009, legend=c("Target","Distractor"), col = c(colTarget, colDistractor))
## Plot the condition Similar:
select = r1$TypeDistractor == "Ts" & r1$Side=="Target"
plot(density(r1$RT[select]), col=colTarget, main="Similar", xlim=c(0,800), ylim=c(0,0.015) )
select = r1$TypeDistractor == "Ts" & r1$Side=="Distractor"
lines(density(r1$RT[select]), col=colDistractor )
legend(700,0.009, legend=c("Target","Distractor"), col = c(colTarget, colDistractor))

# Data Plot of difference RT centered -----------
par(mfrow=c(2,1))
participants = unique(r1$Participant)
r1$centeredRT = 0
for (i in 1:length(participants)){ 
  participant = participants[i]
  select = r1$TypeDistractor == "Td"
  r1$cRT[r1$Participant == participant & select] = 
    r1$RT[r1$Participant == participant & select] - mean(r1$RT[r1$Participant == participant & select]) + mean(r1$RT[select])
  select = r1$TypeDistractor == "Ts"
  r1$cRT[r1$Participant == participant & select] = 
    r1$RT[r1$Participant == participant & select] - mean(r1$RT[r1$Participant == participant & select]) + mean(r1$RT[select])
  
}
plot(0,0, main="Different", xlim=c(0,800), ylim=c(-0.010,0.015))
colTarget = rgb(1, 0, 0, 1) # RED
colDistractor = rgb(0, 0, 1, 1) # BLUE
selectT = r1$TypeDistractor == "Td" & r1$Side=="Target"
selectD = r1$TypeDistractor == "Td" & r1$Side=="Distractor"
Tdensity = density(r1$cRT[selectT], from= range(c(r1$cRT[selectT], r1$cRT[selectD]))[1], 
                   to=range(c(r1$cRT[selectT], r1$cRT[selectD]))[2])
Ddensity = density(r1$cRT[selectD], from= range(c(r1$cRT[selectT], r1$cRT[selectD]))[1], 
                   to=range(c(r1$cRT[selectT], r1$cRT[selectD]))[2])
lines(Ddensity$y-Tdensity$y, col=colDistractor ,lty=2, lw=2)
legend(700,0.009, legend=c("Target","Distractor"), col = c(colTarget, colDistractor))

plot(0,0, main="Similar", xlim=c(0,800), ylim=c(-0.010,0.015))
selectT = r1$TypeDistractor == "Ts" & r1$Side=="Target"
selectD = r1$TypeDistractor == "Ts" & r1$Side=="Distractor"
Tdensity = density(r1$cRT[selectT], from= range(c(r1$cRT[selectT], r1$cRT[selectD]))[1], 
                   to=range(c(r1$cRT[selectT], r1$cRT[selectD]))[2])
Ddensity = density(r1$cRT[selectD], from= range(c(r1$cRT[selectT], r1$cRT[selectD]))[1], 
                   to=range(c(r1$cRT[selectT], r1$cRT[selectD]))[2])
lines(Ddensity$y-Tdensity$y, col=colDistractor ,lty=2, lw=2)
legend(700,0.009, legend=c("Target","Distractor"), col = c(colTarget, colDistractor))

# density per participants -----------
# Data Plot:
par(mfrow=c(2,1))
participants = unique(r1$Participant)
color <- rainbow(length(participants))
plot(0,0, main="Different", xlim=c(0,600), ylim=c(0,0.025))
for (i in 1:length(participants)){ 
  participant = participants[i]
  title = sprintf("Inverse Reaction Times According to Conditions (%s Stim.)", title_text)
  ## Color:
  colTarget = color[i] # solid
  colDistractor = color[i] # # Dashed
  ## Plot the condition Different:
  ##  - Target:
  select = r1$TypeDistractor == "Td" & r1$Side=="Target" & r1$Participant == participant
  lines(density(r1$RT[select]), col=colTarget,lty=1, lw=2) ## solid
  ##  - Distractor:
  select = r1$TypeDistractor == "Td" & r1$Side=="Distractor" & r1$Participant == participant
  lines(density(r1$RT[select]), col=colDistractor ,lty=2, lw=2) ## dashed

}
legend(500,0.025, legend=participants, col = color, lty=1, lw=2)

plot(0,0, main="Similar", xlim=c(0,600), ylim=c(0,0.025))
for (i in 1:length(participants)){ 
  participant = participants[i]
  title = sprintf("Inverse Reaction Times According to Conditions (%s Stim.)", title_text)
  ## Color:
  colTarget = color[i] # solid
  colDistractor = color[i] # # Dashed
  ## Plot the condition Different:
  ##  - Target:
  select = r1$TypeDistractor == "Ts" & r1$Side=="Target" & r1$Participant == participant
  lines(density(r1$RT[select]), col=colTarget,lty=1, lw=2)
  ##  - Distractor:
  select = r1$TypeDistractor == "Ts" & r1$Side=="Distractor" & r1$Participant == participant
  lines(density(r1$RT[select]), col=colDistractor ,lty=2, lw=2)

}
legend(500,0.025, legend=participants, col = color, lty=1, lw=2)

# difference of density per participants -----------
# Data Plot:
par(mfrow=c(2,1))
participants = unique(r1$Participant)
color <- rainbow(length(participants))
plot(0,0, main="Different", xlim=c(0,800), ylim=c(-0.015,0.015))
for (i in 1:length(participants)){ 
  participant = participants[i]
  title = sprintf("Inverse Reaction Times According to Conditions (%s Stim.)", title_text)
  ## Color:
  colTarget = color[i] # solid
  colDistractor = color[i] # # Dashed
  ## Plot the condition Different:
  ##  - Target:
  selectT = r1$TypeDistractor == "Td" & r1$Side=="Target" & r1$Participant == participant
  selectD = r1$TypeDistractor == "Td" & r1$Side=="Distractor" & r1$Participant == participant
  Tdensity = density(r1$RT[selectT], from= range(c(r1$RT[selectT], r1$RT[selectD]))[1], 
                     to=range(c(r1$RT[selectT], r1$RT[selectD]))[2])
  Ddensity = density(r1$RT[selectD], from= range(c(r1$RT[selectT], r1$RT[selectD]))[1], 
                     to=range(c(r1$RT[selectT], r1$RT[selectD]))[2])
  lines(Ddensity$y-Tdensity$y, col=colDistractor ,lty=2, lw=2)
}
legend(700,0.009, legend=participants, col = color, lty=1, lw=2)

plot(0,0, main="Similar", xlim=c(0,800), ylim=c(-0.015,0.015))
for (i in 1:length(participants)){ 
  participant = participants[i]
  title = sprintf("Inverse Reaction Times According to Conditions (%s Stim.)", title_text)
  ## Color:
  colTarget = color[i] #
  colDistractor = color[i] #
  ## Plot the condition Similar:
  ##  - Target:
  selectT = r1$TypeDistractor == "Ts" & r1$Side=="Target" & r1$Participant == participant
  selectD = r1$TypeDistractor == "Ts" & r1$Side=="Distractor" & r1$Participant == participant
  Tdensity = density(r1$RT[selectT], from= range(c(r1$RT[selectT], r1$RT[selectD]))[1], 
                     to=range(c(r1$RT[selectT], r1$RT[selectD]))[2])
  Ddensity = density(r1$RT[selectD], from= range(c(r1$RT[selectT], r1$RT[selectD]))[1], 
                     to=range(c(r1$RT[selectT], r1$RT[selectD]))[2])
  lines(Ddensity$y-Tdensity$y, col=colDistractor ,lty=2, lw=2)
}
legend(700,0.009, legend=participants, col = color, lty=1, lw=2)



