calcRelation = function(data, measure, family=FriedmanTestPaircomp, significance=0.05) {
  stopifnot(length(unique(data$dim)) == 1)
  stopifnot(length(unique(data$prob)) == 1)
  data2 = data[, c("prob", "algo", "repl")]
  data2$measure = measure
  data2$val = data[[measure]]
  data.4d = cast(repl~algo~measure~prob, data=data2, value="val")
  data.wh = as.warehouse.array4dim(data.4d)
  data.ap = data.wh$viewAlgorithmPerformance()
  pc = paircomp(data.ap, family = family, significance = significance)
  rel = as.relation(pc)
  return(rel)
}
