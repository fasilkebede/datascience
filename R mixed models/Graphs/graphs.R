# Different causal relationships involving a genetic locus and 2 phenotypic traits

par(mfrow=c(2,2))
library(Rgraphviz)

net<-rbind(c(0,1,0), c(0,0,1), c(0,0,0))
rownames(net)<-c("G", "Y1", "Y2")
colnames(net)<-c("G", "Y1", "Y2")
netgraph<-new("graphAM", adjMat=net, edgemode="directed")
plot(main="Graph 1", netgraph, attrs= list(node=list(fontsize=9), edge=list(arrowsize=0.8, lwd = 2)))

net<-rbind(c(0,1,1), c(0,0,0), c(0,0,0))
rownames(net)<-c("G", "Y1", "Y2")
colnames(net)<-c("G", "Y1", "Y2")
netgraph<-new("graphAM", adjMat=net, edgemode="directed")
plot(main="Graph 2", netgraph, attrs= list(node=list(fontsize=9), edge=list(arrowsize=0.8, lwd = 2)))

net<-rbind(c(0,0,1), c(0,0,1), c(0,0,0))
rownames(net)<-c("G", "Y1", "Y2")
colnames(net)<-c("G", "Y1", "Y2")
netgraph<-new("graphAM", adjMat=net, edgemode="directed")
plot(main="Graph 3", netgraph, attrs= list(node=list(fontsize=9), edge=list(arrowsize=0.8, lwd = 2)))

net<-rbind(c(0,1,1), c(0,0,1), c(0,0,0))
rownames(net)<-c("G", "Y1", "Y2")
colnames(net)<-c("G", "Y1", "Y2")
netgraph<-new("graphAM", adjMat=net, edgemode="directed")
plot(main="Graph 4", netgraph, attrs= list(node=list(fontsize=9), edge=list(arrowsize=0.8, lwd = 2)))