calcConsensus = function(data, measure, family=FriedmanTestPaircomp, significance=0.05, method="SD/O") {
  rels = dlply(data, "prob", calcRelation, measure=measure, family=family, significance=significance)
  rels.ens = relation_ensemble(list=rels)
  cons=relation_consensus(rels.ens, method=method)
  list(rels=rels, cons=cons)
}

