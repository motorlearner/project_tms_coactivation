library(here)
library(rstan)
library(tidyverse)
library(stringr)

# ordered muscles
muscles <- c("APB", "FPB", "FDI", "ADM", "FDS", "FCR", "EDC", "ECR")

# ordered pairs
pairs <- combn(names(df.m), 2, simplify=TRUE) |> 
  t() |> as.data.frame() |> 
  
  rowwise() |> 
  transmute(pair = case_when(
    which(muscles==V1) < which(muscles==V2) ~ paste(V1,V2,sep="_"),
    which(muscles==V2) < which(muscles==V1) ~ paste(V2,V1,sep="_")
  )) |> 
  
  t() |> as.vector() 

# get individual muscle responses
df.m <- lapply(1:20,
  function(i) read.csv(paste("data/binary", toString(i), ".csv", sep=""))) |> 
  bind_rows() |> 
  select(all_of(muscles))

# get joint muscle responses
df.j <- lapply(
  1:28, function(i){
    col1 <- which(muscles==substr(pairs[i],1,3))
    col2 <- which(muscles==substr(pairs[i],5,7))
    df.m[ ,col1] * df.m[ ,col2]
   }) |> 
  bind_cols() |> 
  data.table::setnames(old = 1:28, new = pairs)

# matrix of hand joint muscle contrasts
chand <- pairs |> 
  as.data.frame() |> 
  filter(substr(pairs,1,3) %in% muscles[1:4] &
         substr(pairs,5,7) %in% muscles[1:4]) |> 
  t() |> as.vector() |> 
  combn(2, simplify=TRUE) |> 
  t() |> as.data.frame() |> 
  
  rowwise() |> 
  transmute(
    c1 = which(pairs==V1),
    c2 = which(pairs==V2)
  ) |> 
  
  as.matrix()

# matrix of forearm joint muscle contrasts
cfarm <- pairs |> 
  as.data.frame() |> 
  filter(substr(pairs,1,3) %in% muscles[5:8] &
           substr(pairs,5,7) %in% muscles[5:8]) |> 
  t() |> as.vector() |> 
  combn(2, simplify=TRUE) |> 
  t() |> as.data.frame() |> 
  
  rowwise() |> 
  transmute(
    c1 = which(pairs==V1),
    c2 = which(pairs==V2)
  ) |> 
  
  as.matrix()

# prep data
datlist <- list(
  # n
  n = nrow(df.m),
  # muscle responses
  xm = df.m |> as.matrix(),
  # joint responses
  xj = df.j |> as.matrix(),
  # pair muscle indices
  m1 = sapply(substr(pairs,1,3), function(x) which(muscles==x)) |> as.vector(),
  m2 = sapply(substr(pairs,5,7), function(x) which(muscles==x)) |> as.vector(),
  # contrast pair indices
  chand = chand,
  cfarm = cfarm
)

stancode <- "
data{
  int n;
  int xm[n,8];
  int xj[n,28];
  int m1[28];
  int m2[28];
  int chand[15,2];
  int cfarm[15,2];
}
parameters{
  vector<lower=0, upper=1>[8]  pm;
  vector<lower=0, upper=1>[28] pj;
}
transformed parameters{
  vector[28] pmi;
  vector[15] pmi_chand;
  vector[15] pmi_cfarm;
  for (i in 1:28) pmi[i] = log(pj[i] / (pm[m1[i]] * pm[m2[i]]));
  for (i in 1:15) {
    pmi_chand[i] = pmi[chand[i,1]] - pmi[chand[i,2]];
    pmi_cfarm[i] = pmi[cfarm[i,1]] - pmi[cfarm[i,2]];
  }
}
model{
  pm ~ uniform(0,1);
  pj ~ uniform(0,1);
  for (i in 1:8)  xm[:,i] ~ bernoulli(pm[i]);
  for (i in 1:28) xj[:,i] ~ bernoulli(pj[i]);
}
"

stanfit <- stan(
  data = datlist,
  model_code = stancode,
  chains = 4,
  cores = 4
)

percs <- c(0.05, 0.95)

df.params <- summary(stanfit, probs=c(0.05,0.95))$summary |> 
  as.data.frame() |> 
  rownames_to_column(var = "param")|> 
  select("param", "mean", contains("%")) |> 
  rowwise() |> 
  mutate(ind = readr::parse_number(param, na="lp__")) |> 
  mutate(paramname = case_when(
    substr(param,1,3) == "pm[" ~ paste("p(", muscles[ind], ")", sep=""),
    substr(param,1,3) == "pj[" ~ paste("p(", pairs[ind], ")", sep=""),
    substr(param,1,4) == "pmi[" ~ paste("pmi(", pairs[ind], ")", sep=""),
    substr(param,1,9) == "pmi_chand" ~ paste("pmi(", pairs[as.vector(chand)[ind]], " - ", 
                                             pairs[as.vector(chand)[nrow(chand)+ind]], ")", sep=""),
    substr(param,1,9) == "pmi_cfarm" ~ paste("pmi(", pairs[as.vector(cfarm)[ind]], " - ", 
                                             pairs[as.vector(cfarm)[nrow(cfarm)+ind]], ")", sep=""),
    TRUE ~ param
  )) |> 
  data.table::setnames(old = paste(percs*100, "%", sep=""), new = paste("p", percs*100, sep=""))

df.params |> 
  filter(grepl("pmi[", param, fixed=T)) |> 
  pivot_longer(
    cols=c(3,4),
    names_to = "p",
    values_to = "x"
  ) |> 
  ggplot() +
  geom_line(aes(y=paramname, x=x, group=paramname)) +
  geom_vline(xintercept=0, linetype="dashed")




