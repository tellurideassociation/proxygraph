# Takes a file of the format:
# Person A, Proxy B, Proxy C
# Person B, Proxy C, Proxy D
#
# And outputs:
# just_edges.csv:
# source target weight
# 1      2      1
#
# And nodes.csv:
# id    label
# 1     Fred Frederickson
# 2     adam hogan
# 
# Together, these can be loaded into gephi,
# which makes handsome network graphs
# Additional Network Analysis forthcoming
library(igraph)
library(sna)
library(network)
library(intergraph)
library(ggplot2)

headnames <- c('Last_Name','First_Name','Salutation','Updated','Rank_1','Rank_2','Rank_3','Rank_4','Rank_5','Rank_6','Rank_7','Rank_8','Rank_9','Rank_10')
data.raw <- read.csv('proxies.csv', header=F, col.names=headnames)
data.test <- data.raw[,-2:-4]
data.test2 <- cbind(seq(1,dim(data.test)[1]),data.test)
names(data.test2)[1] <- "ID"
d <- data.test2
d2 <- reshape(d, direction="long", varying=list(names(d)[3:12]), v.names="Destination", idvar="ID", timevar="Weight")
d2<- d2[order(d2$ID),]

d3 <- cbind(d2,d2$ID[match(d2$Destination,d2$Last_Name)])

names(d3)[5] <- "DestID"

d4 <- d3[d3$Destination!="",]

trid_names <- subset(d3, select=c(ID, Last_Name))
trid_names <- subset(trid_names[duplicated(trid_names)==FALSE,],select=c(ID, Last_Name))
disp_names <- trid_names
names(disp_names) <- c('ID','Label')

errors <- d4[is.na(d4$DestID),]
numerrors <- dim(errors)[1]
print(paste('There were ', numerrors, ' matching errors in this sample. \n These are usually caused by bracket problems.'))

# This is language you would use to pull out an individual:
# Gives me May's ID: d3[d3$Last_Name=="May",1][1]
# d3[d3$Last_Name=="May",][1,1]

d5 <- subset(d4, select=c(ID,Weight,DestID))
names(d5) <- c('Source', 'Rank', 'Destination')

d5$Weight <- (11 - d5$Rank)/10
d5$WW <- d5$Weight^2
d5$WSqrt <- round(sqrt(d5$Weight), digits=2)
d5$WInv <- round(1/d5$Rank, digits=2)

d6 <- subset(d5, select=c(Source,Destination, WInv))
names(d6) <- c('Source', 'Target', 'Weight')

write.csv(d6,file="Just_edges.csv", quote=F, row.names=F)
write.csv(disp_names,file="nodes.csv", quote=F, row.names=F)

# THEN IN GEPHI
# NEW PROJECT
# Import Spreadsheet -> Nodes First
# Import Spreadsheet -> Edges Second
# TADA!

# HERE IS THE EXPERIMENTAL SECTION SHOWING SOME NONESSENTIAL
# NETWORK ANALYSIS:

# REATTACH THE LABELS
# NAMED R VISUALIZATIONS

tha_network <- network(d6)

tframe <- merge(d6,trid_names, by.x="Source", by.y="ID")
names(tframe)[4] <- "Label"

htell <- graph.data.frame(tframe, directed=TRUE)

# HERE ARE SOME STATISTICS
g <- as.network(d6, format="edgelist")
summary(g)
cent <- data.frame(bet=betweenness(g), eig=evcent(g))
row.names(cent) <- disp_names$Last_Name
summary(cent)
res<-lm(eig~bet, data=cent)$residuals
cent<-transform(cent,res=res)
p<-ggplot(cent,aes(x=bet,y=eig, label=rownames(cent),colour=res,size=abs(res)))+xlab("Betweenness Centrality")+ylab("Eigenvector Centrality")
p <- p+geom_text()+opts(title="Let's see")
p

ig <- as.igraph(g)
nodes <- as.vector(disp_names$Last_Name)

# FURTHER STATISTICS
library(igraph)

V(ig)$name <- as.vector(disp_names$Label)
E(ig)$weight <- as.vector(d6$Weight)
V(ig)$label <- as.vector(disp_names$Label)
leading.eigenvector.community(ig)
comps <- leading.eigenvector.community(ig)$membership
colbar <- rainbow(max(comps)+1)
V(ig)$color <- colbar[comps+1]
igsize <- (page.rank(ig, vids=V(ig), directed=TRUE)$vector)
V(ig)$size <- igsize*200

# HERE IS ONE WAY OF PLOTTING IN R
tkplot(ig, layout=layout.fruchterman.reingold, edge.curved=TRUE, edge.arrow.size=0.5, 
edge.arrow.width=0.5, vertex.frame.color="#F1F1F1", vertex.label.font=1, 
vertex.label.family="sans", vertex.label.color="black", vertex.label.cex=0.5)

# HERE IS ANOTHER WAY OF PLOTTING IN R
plot.igraph(ig, layout=layout.fruchterman.reingold, edge.curved=TRUE, edge.arrow.size=0.2, 
edge.arrow.width=0.2, vertex.frame.color="#F1F1F1", vertex.label.font=1, 
vertex.label.family="sans", vertex.label.color="black", vertex.label.cex=0.5)

