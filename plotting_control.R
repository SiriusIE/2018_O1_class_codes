df_states<-read.csv('../df_states.csv', sep=';')


for(i in seq_along(df_states)){
  if(is.numeric(df_states[, i])){
    png(paste0('plot_',colnames(df_states)[i],'.png'))
    hist(df_states[, i], main=colnames(df_states)[i])
    dev.off()
  }
}
