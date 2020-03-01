library(sparklyr)
library(dplyr)
library(visNetwork)

#install spark from tar
#spark_install_tar('C:\\Users\\alexf\\Downloads\\spark-2.4.3-bin-hadoop2.7.tgz')

#### spark connect #########################################
conf <- spark_config()
conf$`sparklyr.cores.local` <- 4
conf$`sparklyr.shell.driver-memory` <- "6G"
conf$spark.memory.fraction <- 0.9
sc <- spark_connect(master = "local", config = conf)

# colvec <- c("InvoiceNo","Description")
# 
# trans <- arules::read.transactions(
#   file = "../data/online_retail_II_combined_clean.csv",
#   format = "single",
#   header = TRUE,
#   sep = ",",
#   cols=colvec,
#   rm.duplicates = T
# )
data<-read.csv("../data/online_retail_II_combined.csv")
data_cols_removed <- data[ -c(1, 3, 5, 6:9) ]
train <- sapply(data_cols_removed, as.factor)
train <- data.frame(train, check.names=FALSE)

#remove duplicates from train
train_no_duplicates = train[!duplicated(train), ]


#### upload to spark #########################################  
trx_tbl  = copy_to(sc, train_no_duplicates, overwrite = TRUE)

# data needs to be aggregated by id, the items need to be in a list
trx_agg = trx_tbl %>% 
  group_by(InvoiceNo) %>% 
  summarise(
    items = collect_list(Description)
  )


uid = sparklyr:::random_string("fpgrowth_")
jobj = invoke_new(sc, "org.apache.spark.ml.fpm.FPGrowth", uid)

FPGmodel = jobj %>% 
  invoke("setItemsCol", "items") %>%
  invoke("setMinConfidence", 0.5) %>%
  invoke("setMinSupport", 0.01)  %>%
  invoke("fit", spark_dataframe(trx_agg))

# FPGmodel = ml_fpgrowth(
#   x = trx_agg, 
#   items_col = 'items', 
#   min_support = 0.01, 
#   min_confidence = 0.5,
#   uid = uid
# )

ml_fpgrowth_extract_rules = function(FPGmodel, nLHS = 2, nRHS = 1)
{
  rules = FPGmodel %>% invoke("associationRules")
  sdf_register(rules, "rules")
  
  exprs1 <- lapply(
    0:(nLHS - 1), 
    function(i) paste("CAST(antecedent[", i, "] AS string) AS LHSitem", i, sep="")
  )
  exprs2 <- lapply(
    0:(nRHS - 1), 
    function(i) paste("CAST(consequent[", i, "] AS string) AS RHSitem", i, sep="")
  )
  
  splittedLHS = rules %>% invoke("selectExpr", exprs1) 
  splittedRHS = rules %>% invoke("selectExpr", exprs2) 
  p1 = sdf_register(splittedLHS, "tmp1")
  p2 = sdf_register(splittedRHS, "tmp2")
  
  ## collecting output rules to R should be OK and not flooding R
  bind_cols(
    sdf_bind_cols(p1, p2) %>% collect(),
    rules %>% collect() %>% select(confidence)
  )
}

#### Plot resulting rules in a networkgraph

plot_rules = function(rules, LHS = "LHSitem0", RHS = "RHSitem0", cf = 0.5)
{
  rules = rules %>% filter(confidence > cf)
  nds = unique(
    c(
      rules[,LHS][[1]],
      rules[,RHS][[1]]
    )
  )
  
  nodes = data.frame(id = nds, label = nds, title = nds) %>% arrange(id)
  
  edges = data.frame(
    from =  rules[,LHS][[1]],
    to = rules[,RHS][[1]]
  )
  visNetwork(nodes, edges, main = "Online Purchases Network", size=1) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visEdges(smooth = FALSE) %>%
    visPhysics(
      solver = "barnesHut", 
      forceAtlas2Based = list(gravitationalConstant = -20, maxVelocity = 1)
    )
}

#memory.limit(size = 56000)

GroceryRules = FPGmodel %>% ml_fpgrowth_extract_rules()

plot_rules(GroceryRules)
