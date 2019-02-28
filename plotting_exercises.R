
df_states<-read.csv('../df_states.csv', sep=';')


hist(df_states$Income)
boxplot(df_states$Income~df_states$region, col=1:4)

with(df_states, hist(Income))

unique(levels(df_states$region))

par(mfrow=c(2,2))
boxplot(df_states[df_states$region=="North Central",]$Income)
boxplot(df_states[df_states$region=="Northeast",]$Income)
boxplot(df_states[df_states$region=="South",]$Income)
boxplot(df_states[df_states$region=="West",]$Income)


str(df_states)


for(i in colnames(df_states)){
  if(class(df_states[, i])=='numeric'){
    hist(df_states[, i], main=i)
  }
}






