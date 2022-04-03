alloc <- function(num_panels = 100000, equity = 0, region = "all"){
  library(readxl)
  final_mev <- read_excel("final_mev.xlsx", col_names=TRUE)
  lambda1 = 72.93
  lambda2 = 0.774463
  lambda3 = 30862
  lambda = 54.2
  num_panels = as.integer(num_panels)
  if(region == "all"){
    data = data.frame(final_mev)
  }
  else{
    data = data.frame(final_mev[final_mev$region == region])
  }
  i = num_panels
  df = data
  if(equity == 1){
    i = (num_panels)/2
    df$panels = i/nrow(df)
    while(i > 0) {
      df = df[order(-df$MEV),]
      #print(df)
      df$MEV[1] = df$MEV[1] - lambda
      df$panels[1] = df$panels[1] + 1
      i = i - 1
    }
    return(df)
  }
  else if(equity == 0){
    df$panels = 0
    while(i > 0) {
      df = df[order(-df$MEV),]
      #print(df)
      df$MEV[1] = df$MEV[1] - lambda
      df$panels[1] = df$panels[1] + 1
      i = i - 1
    }
    return(df)
  }
}
print(alloc())
