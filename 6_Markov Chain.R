install.packages("markovchain")
library(markovchain)

statesNames=c("Serba","Ada")
mcA<-new("markovchain",
         transitionMatrix=matrix(c(0.667,0.333,0.625,0.375),
                                byrow=TRUE, nrow=2, dimnames=list(statesNames,statesNames)))

mcA^5
steadyStates(mcA)
absorbingStates(mcA)
markovchainSequence(n=20,markovchain=mcA,include=TRUE)