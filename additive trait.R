# is trait additive in PushxPull double mutant?


 
library(dplyr)


# Mid-Parent Heteroisis Ratio
# calculate mid-parent ratio
# MP = mean(pushParent, pullParent)
# MPHR = mean((PushPull - MP)/MP)


# pushLineSimple, pullLineSimple, push.pullLineSimple arguments in quotes
# var argument without quotes
midParentHeterosis <- function(df, pushLineSimple, pullLineSimple, push.pullLineSimple, var){
  
  df1 <- df %>% 
    filter_(.dots = lazyeval::lazy(lineSimple == pushLineSimple)) %>% 
    select_(.dots = lazyeval::lazy(var))
  push.mean <- mean(df1[[1]])
  
  df2 <- df %>% 
    filter_(.dots = lazyeval::lazy(lineSimple == pullLineSimple)) %>% 
    select_(.dots = lazyeval::lazy(var))
  pull.mean <- mean(df2[[1]])
  
  MP <- mean(c(push.mean, pull.mean))
  
  df3 <- df %>% 
    filter_(.dots = lazyeval::lazy(lineSimple == push.pullLineSimple)) %>% 
    select_(.dots = lazyeval::lazy(var))
  push.pull.MPHR <- (df3[[1]]-MP)/MP
  push.pull.mean <- mean(df3[[1]])
  
  MPHR <- (push.pull.mean - MP)/MP
  
  TTest <- t.test(push.pull.MPHR, mu = 0)
  
  results <- list(pushParent_samples = df1[[1]], pushParent_mean = push.mean, pullParent_samples = df2[[1]], pullParent_mean = pull.mean, midParent = MP, pushPull_samples = df3[[1]], pushPull_mean = push.pull.mean, pushPullMPHR_samples = push.pull.MPHR, MPHR_mean = MPHR, ttest_pval = TTest$p.value)
  
  return(results)
}
