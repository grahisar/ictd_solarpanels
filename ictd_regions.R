library(readxl)
alloc <- function(num_panels = 100000, equity = 0, region = "all"){
  final_mev <- read_excel("final_mev_india.xlsx", col_names=TRUE)
  lambda1 = 72.93
  lambda2 = 0.774463
  lambda3 = 30862
  lambda = 54.2
  num_panels = as.integer(num_panels)
  if(region == "all"){
    data = data.frame(final_mev)
  }
  else{
    print(region)
    data = data.frame(final_mev[(final_mev$region %in% region) == TRUE,])
  }
  i = num_panels
  df = data
  if(equity == "Equitable"){
    print("Allocating equitably...")
    i = (num_panels)/2
    df$panels = i/nrow(df)
    while(i > 0) {
      df = df[order(-df$MEV),]
      #print(df)
      df$MEV[1] = df$MEV[1] - lambda
      df$panels[1] = df$panels[1] + 1
      i = i - 1
    }
  }
  else if(equity == "Efficient"){
    print("Allocating efficiently...")
    df$panels = 0
    while(i > 0) {
      df = df[order(-df$MEV),]
      #print(df)
      df$MEV[1] = df$MEV[1] - lambda
      df$panels[1] = df$panels[1] + 1
      i = i - 1
    }
  }
  print("Allocation complete!")
  return(df)
}

x = alloc(10000, 1, c("WR", "NR"))
