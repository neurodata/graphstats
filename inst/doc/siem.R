## ---- warning=FALSE, echo=FALSE------------------------------------------
require(graphstats)
require(ggplot2)

## ---- fig.width=5, fig.height=3.5----------------------------------------
nv <- 20  # number of vertices in simulated graph
X <- array(0, dim=c(nv, nv))  # simulated graph initialized
com <- array(0, dim=c(nv, nv))  # probability at each edge
Es <- list()
split <- sample(nv^2, nv^2, replace=FALSE)  # sample edges in random order
pedge <- data.frame(community=c(), result=c(), p=c())

Es[[1]] <- split[1:(nv^2/2)]  # first half in edge-community 1
Es[[2]] <- split[(nv^2/2+1):(nv^2)]  # second half in edge-community 2

p <- c(.3, .7)  # probabilities between community 1 and 2 are vastly different

for (i in 1:length(Es)) {
  X[Es[[i]]] <- rbinom(n=length(Es[[i]]), prob=p[i], size=1)
  com[Es[[i]]] <- i
  pedge <- rbind(pedge, data.frame(community=c(i), result=c("true"), p=c(p[i])))
  pedge <- rbind(pedge, data.frame(community=c(i), result=c("sampled"), p=c(sum(X[Es[[i]]])/length(Es[[i]]))))
}

pedge$community <- factor(pedge$community)
ggplot(pedge, aes(x=community, y=p, fill=result)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  ggtitle("Comparing True vs. Sim Edge Community, Sample 1")
gs.plot.plot_matrix(com, legend.name = "community", xlabel = "vertex",
                     ylabel = "vertex", title = "Community Each Edge is from, Sample 1", ffactor=TRUE)
gs.plot.plot_matrix(X, legend.name = "connection", xlabel = "vertex",
                     ylabel = "vertex", title = "Graph Simulated from SIEM, Sample 1")

## ---- fig.width=5, fig.height=3.5----------------------------------------
model <- gs.siem.fit(X, Es)  # fit siem model
os_pval <- array(0, dim=c(2, 2))
for (i in 1:length(Es)) {
  for (j in 1:length(Es)) {
    os_pval[i, j] <- graphstats:::gs.siem.sample.test(model$pr[i], model$pr[j], model$var[i],
                                                      model$var[j], 1, alt='greater')$p
  }
}

gs.plot.plot_matrix(os_pval, legend.name = "p-value", xlabel = "c_i",
                     ylabel = "c_j", title = "p-value that p_{c_i} > p_{c_j}, Sample 1",
                     limits=c(0, 1), vfactor=TRUE)

## ---- fig.height=3.5, fig.width=5----------------------------------------
X2 <- array(0, dim=c(nv, nv))  # simulated graph initialized
com2 <- array(0, dim=c(nv, nv))  # probability at each edge
pedge <- data.frame(community=c(), result=c(), p=c())

p2 <- c(.5, .6)  # probabilities between community 1 and 2 are vastly different

for (i in 1:length(Es)) {
  X2[Es[[i]]] <- rbinom(n=length(Es[[i]]), prob=p2[i], size=1)
  com[Es[[i]]] <- i
  pedge <- rbind(pedge, data.frame(community=c(i), result=c("true"), p=c(p2[i])))
  pedge <- rbind(pedge, data.frame(community=c(i), result=c("sampled"), p=c(sum(X[Es[[i]]])/length(Es[[i]]))))
}

pedge$community <- factor(pedge$community)
ggplot(pedge, aes(x=community, y=p, fill=result)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  ggtitle("Comparing True vs. Sim Edge Community, Sample 2")
gs.plot.plot_matrix(com, legend.name = "community", xlabel = "vertex",
                     ylabel = "vertex", title = "Community Each Edge is from, Sample 2", ffactor=TRUE)
gs.plot.plot_matrix(X, legend.name = "connection", xlabel = "vertex",
                     ylabel = "vertex", title = "Graph Simulated from SIEM, Sample 2")

## ------------------------------------------------------------------------
model2 <- gs.siem.fit(X2, Es)  # fit siem model
graphstats:::gs.siem.sample.test(model$dpr[2, 1], model2$dpr[2, 1], model2$dvar[2, 1],
                                 model2$dvar[2, 1], 2, alt='greater')$p

