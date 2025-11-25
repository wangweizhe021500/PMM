#————————————————————————微生物分类分组可视化-----————————————————————————————————————————-----
library(tidyverse)
library(Biostrings)
library(igraph)
library(conflicted)
library(phyloseq)
library(igraph)
library(network)
library(sna)
library(tidyverse)
library(ggClusterNet)

getwd()
#先将主要文件读入
CK1<-read.table("F:/RData/2021-Microeukaryotes/group_M_RM加了NO.txt",  header=T,row.names=1,  sep="\t", comment.char="", stringsAsFactors = F)
CK0 = read.table("F:/RData/2021-Microeukaryotes/Metazoa_fungi_protist_RM加了NO.txt", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
CKM = read.table("F:/RData/2021-Microeukaryotes/Metazoa.txt", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
CKF = read.table("F:/RData/2021-Microeukaryotes/fungi.txt", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
CK0 = read.table("F:/RData/2021-Microeukaryotes/Protist.txt", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)


#——————————————ALL_NETWORK_网络性质分析———————————-----
#工作目录准备好以上文件
library(phyloseq)
library(ggClusterNet)
library(tidyverse)
library(Biostrings)
library(igraph)
library(conflicted)
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\跨域网络图\\Metozoa_Fungi_Protist_network\\Protist_网络性质分析_0.6\\Protist_时空分析_0.6")

#METAZOA: c(1:3084)；FUNGI：c(3085:8650);PROTIST：c(8651:34171)；
###METAZOA----
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\跨域网络图\\Metozoa_Fungi_Protist_network\\Metazoa_网络性质分析_0.6")

otutab1 <- CK0[c(1:3084),c(c(1:120),c(133:144))]#otu
sample1<- CK1[c(c(1:120),c(133:144)),]#otu

taxonomy1 <-  CK0[c(1:3084),c(145:153)]

#METAZOA: c(1:3084)；FUNGI：c(3085:8650);PROTIST：c(8651:34171)；
###FUNGI----
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\跨域网络图\\Metozoa_Fungi_Protist_network\\Fungi_网络性质分析_0.6")
otutab1 <- CK0[c(3085:8650),c(c(1:120),c(133:144))]#otu
sample1<- CK1[c(c(1:120),c(133:144)),]#otu

taxonomy1 <-  CK0[c(3085:8650),c(145:153)]

#METAZOA: c(1:3084)；FUNGI：c(3085:8650);PROTIST：c(8651:34171)；
###PROTIST----
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\跨域网络图\\Metozoa_Fungi_Protist_network\\Protist_网络性质分析_0.6")
otutab1 <- CK0[c(8651:34171),c(c(1:120),c(133:144))]#otu
sample1<- CK1[c(c(1:120),c(133:144)),]#otu
taxonomy1 <-  CK0[c(8651:34171),c(145:153)]

###CM----
otutab1 <- CK0[,c(1:48)]#otu
sample1<- CK1[c(1:48),]#otu

taxonomy1 <-  CK0[,c(145:153)]


#去除所有NO----#METAZOA: c(1:3084)；FUNGI：c(3085:8650);PROTIST：c(8651:34171)----
otutab1 <- CK0[8651:34171,c(c(1:24),c(37:72),c(85:120),c(133:144))]#otu
sample1<- CK1[c(c(1:24),c(37:72),c(85:120),c(133:144)),]#otu

taxonomy1 <-  CK0[8651:34171,c(145:153)]


###——————————————ALL_NETWORK—原核-真核网络--考虑所有连接—————————————————————----
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\跨域网络图\\16S_18S(Metozoa_Fungi_Protist)_network\\考虑所有连接和共现\\gephi总网络-20240419")
#先将主要文件读入
CK1<-read.table("F:/RData/2021-Microeukaryotes/group_M_RM加了NO.txt",  header=T,row.names=1,  sep="\t", comment.char="", stringsAsFactors = F)
CK0 = read.table("F:/RData/2021-Microeukaryotes/Metazoa_fungi_protist_RM加了NO.txt", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)

#真核----
otutab1 <- CK0[,c(c(1:11),c(13:47),c(49:119),c(133:144))]#otu
sample1<- CK1[c(c(1:11),c(13:47),c(49:119),c(133:144)),]#otu

taxonomy1 <-  CK0[,c(145:153)]
# 提取两个表中共有的ID
# 对OTU和taxonomy表中共同出现的OTU进行判断，出现判断为TRUE 
idx1 = rownames(otutab1) %in% rownames(taxonomy1)
# 依据idx 分别从OTU和TAX表中筛选

otutab1 = otutab1[idx1,]
taxonomy1= taxonomy1[rownames(otutab1),]

#读取发育树需要ape包直接使用install.packages(“ape”) 命令
#library("ape")
#tree = read.tree("rooted_tree.tre")

# 构建phyloseq(ps)对象
psEuk = phyloseq(sample_data(sample1),otu_table(as.matrix(otutab1), taxa_are_rows=TRUE), tax_table(as.matrix(taxonomy1)))
#ps=merge_phyloseq(ps, tree)
psEuk=psEuk
#data(ps)
psEuk

#原核----
CKB = read.table("F:/RData/2021-Microeukaryotes/BW.txt", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
CK1<-read.table("F:/RData/2021-Microeukaryotes/group_M_RM加了NO.txt",  header=T,row.names=1,  sep="\t", comment.char="", stringsAsFactors = F)
# 引入dplyr包
library(dplyr)


otutab2 <- CKB[,c(1:129)]#otu
sample1<- CK1[c(c(1:11),c(13:47),c(49:119),c(133:144)),]#otu

taxonomy2 <-  CKB[,c(131:139)]

otutab2=otutab2[which(rowSums(otutab2) > 0),]

OTU2<-otutab2
TAX2<-taxonomy2
# 提取两个表中共有的ID
# 对OTU和taxonomy表中共同出现的OTU进行判断，出现判断为TRUE 
idx2 = rownames(OTU2) %in% rownames(TAX2)
# 依据idx 分别从OTU和TAX表中筛选

OTU2 = OTU2[idx2,]
TAX2 = TAX2[rownames(OTU2),]

#读取发育树需要ape包直接使用install.packages(“ape”) 命令
#library("ape")
#tree = read.tree("rooted_tree.tre")

# 构建phyloseq(ps)对象
psPro = phyloseq(sample_data(sample1),otu_table(as.matrix(OTU2), taxa_are_rows=TRUE), tax_table(as.matrix(TAX2)))
#ps=merge_phyloseq(ps, tree)
psPro=psPro
#data(ps)
psPro

#--细菌和真菌ps对象中的map文件要一样
ps.merge <- ggClusterNet::merge16S_ITS(ps16s = psPro,
                                       psITS = psEuk,
                                       N16s = 10,
                                       NITS = 10
)


ps = ps.merge
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 1000 taxa and 18 samples ]
#> sample_data() Sample Data:       [ 18 samples by 2 sample variables ]
#> tax_table()   Taxonomy Table:    [ 1000 taxa by 8 taxonomic ranks ]
map =  phyloseq::sample_data(ps)

#  
#map$Group = "one"
phyloseq::sample_data(ps) <- map







ps =  ps %>%
  scale_micro("rela") %>%
  phyloseq::subset_samples(Group %in% c("CM","RCI","RM")) %>%
  filter_OTU_ps(20)

result = cor_Big_micro(ps = ps,
                       N = 0,
                       r.threshold=0.6,
                       p.threshold=0.05,
                       method = "spearman"
)

#--提取相关矩阵
cor = result[[1]]
dim(cor)
conflict_prefer("simplify", "igraph")
conflict_scout()
result2 <- model_igraph(cor = cor,
                        method = "cluster_fast_greedy",
                        seed = 12
)
node = result2[[1]]
head(node)

dat = result2[[2]]
head(dat)
tem = data.frame(mod = dat$model,col = dat$color) %>%  
  dplyr::distinct( mod, .keep_all = TRUE)  
col = tem$col
names(col) = tem$mod

#---node节点注释#-----------
otu_table = as.data.frame(t(vegan_otu(ps)))
tax_table = as.data.frame(vegan_tax(ps))
nodes = nodeadd(plotcord =node,otu_table = otu_table,tax_table = tax_table)
head(nodes)
#-----计算边#--------
edge = edgeBuild(cor = cor,node = node)
colnames(edge)[8] = "cor"
head(edge)

tem2 = dat %>% 
  dplyr::select(OTU,model,color) %>%
  dplyr::right_join(edge,by =c("OTU" = "OTU_1" ) ) %>%
  dplyr::rename(OTU_1 = OTU,model1 = model,color1 = color)
head(tem2)

tem3 = dat %>% 
  dplyr::select(OTU,model,color) %>%
  dplyr::right_join(edge,by =c("OTU" = "OTU_2" ) ) %>%
  dplyr::rename(OTU_2 = OTU,model2 = model,color2 = color)
head(tem3)

tem4 = tem2 %>%inner_join(tem3)
head(tem4)

edge2 = tem4 %>% mutate(color = ifelse(model1 == model2,as.character(model1),"across"),
                        manual = ifelse(model1 == model2,as.character(color1),"#C1C1C1")
)
head(edge2)
#具体解决方案：
#dplyr包经常和其他包的函数有冲突，需要选择一下优先级：
library(conflicted)
conflict_prefer('filter','dplyr')
conflict_prefer('select','dplyr')
conflict_scout()
#结果顺利运行
col_edge = edge2 %>% dplyr::distinct(color, .keep_all = TRUE)  %>% 
  select(color,manual)
col0 = col_edge$manual
names(col0) = col_edge$color

library(ggnewscale)

p1 <- ggplot() + geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2,color = color),
                              data = edge2, size = 1) +
  scale_colour_manual(values = col0) 
p1
ggsave("./_cs1.pdf",p1,width = 10,height = 6)
p2 = p1 +
  new_scale_color() +
  geom_point(aes(X1, X2,color =model), data = dat,size = 4) +
  scale_colour_manual(values = col) +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  theme(panel.background = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.background = element_rect(colour = NA)) +
  theme(panel.background = element_rect(fill = "white",  colour = NA)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
p2
ggsave("./_cs2.pdf",p2,width = 14,height = 12)


#——————————节点模块化计算和可视化————————————————————----
result4 = nodeEdge(cor = cor)
#提取变文件
edge = result4[[1]]
#--提取节点文件
node = result4[[2]]
igraph  = igraph::graph_from_data_frame(edge, directed = FALSE, vertices = node)
res = ZiPiPlot(igraph = igraph,method = "cluster_fast_greedy")
p0 <- res[[1]]
p0

ggsave("Z_score.pdf",p0,width = 7.5,height = 5.5)
#————————网络性质计算——————————————————----
#22年6月升级后版本包括了16项网络属性，
#包括周集中老师21年NCC文章中全部属性
dat = net_properties(igraph)
head(dat)
# 报错————升级后包含的网络属性更多
dat = net_properties.2(igraph,n.hub = T)
head(dat,n = 16)
write.csv(dat,file="网络属性.csv")
#——————————节点性质计算——————————----
nodepro = node_properties(igraph)
head(nodepro)
write.csv(nodepro,file="节点性质.csv")

#——————————扩展-关键OTU挑选——————————----
#Hub微生物就是与其他微生物联系较为紧密的微生物，
#可以称之为关键微生物（keystone）
hub = hub_score(igraph)$vector %>%
  sort(decreasing = TRUE) %>%
  head(80) %>%
  as.data.frame()

colnames(hub) = "hub_sca"

p1<-ggplot(hub) +
  geom_bar(aes(x = hub_sca,y = reorder(row.names(hub),hub_sca)),stat = "identity",fill = "#4DAF4A")
p1
ggsave("hub_sca.pdf",p1,width = 7.5,height = 20)
write.csv(hub,file="关键OTU挑80.csv")

#——————————对应随机网络构建和网络参数比对——————————----
result = random_Net_compate(igraph = igraph, type = "gnm", step = 100, netName = layout)
p2 = result[[1]]
sum_net = result[[4]]
p2
ggsave("network_ERnetwork.pdf",p2,width = 7.5,height = 5.5)
head(sum_net)

#——————————微生物组网络pipeline分析————————————----
#使用network函数运行微生物网络全套分析：

#使用OTU数量建议少于250个，如果OTU数量为250个，
#同时计算zipi，整个运算过程为3-5min。

#先将主要文件读入
CK1<-read.table("F:/RData/2021-Microeukaryotes/Group_BW.txt",  header=T,row.names=1,  sep="\t", comment.char="", stringsAsFactors = F)
CK0 = read.table("F:/RData/2021-Microeukaryotes/BW_PW考虑所有连接.txt", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)

#真核----
otutab1 <- CK0[,c(1:129)]#otu
sample1<- CK1#otu

taxonomy1 <-  CK0[,c(130:138)]
# 提取两个表中共有的ID
# 对OTU和taxonomy表中共同出现的OTU进行判断，出现判断为TRUE 
idx1 = rownames(otutab1) %in% rownames(taxonomy1)
# 依据idx 分别从OTU和TAX表中筛选

otutab1 = otutab1[idx1,]
taxonomy1= taxonomy1[rownames(otutab1),]

#读取发育树需要ape包直接使用install.packages(“ape”) 命令
#library("ape")
#tree = read.tree("rooted_tree.tre")

# 构建phyloseq(ps)对象
ps = phyloseq(sample_data(sample1),otu_table(as.matrix(otutab1), taxa_are_rows=TRUE), tax_table(as.matrix(taxonomy1)))
#ps=merge_phyloseq(ps, tree)
ps
#data(ps)


#原核----
CKB = read.table("F:/RData/2021-Microeukaryotes/BW.txt", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
CK1<-read.table("F:/RData/2021-Microeukaryotes/Group_BW.txt",  header=T,row.names=1,  sep="\t", comment.char="", stringsAsFactors = F)
# 引入dplyr包
library(dplyr)


otutab2 <- CKB[,c(1:129)]#otu
sample1<- CK1#otu

taxonomy2 <-  CKB[,c(130:138)]

otutab2=otutab2[which(rowSums(otutab2) > 0),]

OTU2<-otutab2
TAX2<-taxonomy2
# 提取两个表中共有的ID
# 对OTU和taxonomy表中共同出现的OTU进行判断，出现判断为TRUE 
idx2 = rownames(OTU2) %in% rownames(TAX2)
# 依据idx 分别从OTU和TAX表中筛选

OTU2 = OTU2[idx2,]
TAX2 = TAX2[rownames(OTU2),]

#读取发育树需要ape包直接使用install.packages(“ape”) 命令
#library("ape")
#tree = read.tree("rooted_tree.tre")

# 构建phyloseq(ps)对象
psPro = phyloseq(sample_data(sample1),otu_table(as.matrix(OTU2), taxa_are_rows=TRUE), tax_table(as.matrix(TAX2)))
#ps=merge_phyloseq(ps, tree)
psPro=psPro
#data(ps)
psPro

#--细菌和真菌ps对象中的map文件要一样
ps.merge <- ggClusterNet::merge16S_ITS(ps16s = psPro,
                                       psITS = psEuk,
                                       N16s = 10,
                                       NITS = 10
)


ps = ps.merge
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 1000 taxa and 18 samples ]
#> sample_data() Sample Data:       [ 18 samples by 2 sample variables ]
#> tax_table()   Taxonomy Table:    [ 1000 taxa by 8 taxonomic ranks ]
map =  phyloseq::sample_data(ps)

#  
#map$Group = "one"
phyloseq::sample_data(ps) <- map


ps =  ps %>%
  scale_micro("rela") %>%
  phyloseq::subset_samples(Group %in% c("CM","RCI","RM")) %>%
  filter_OTU_ps(20)

result = cor_Big_micro(ps = psEuk,
                       N = 10,
                       r.threshold=0.6,
                       p.threshold=0.05,
                       method = "spearman"
)




ps
path = "./result_micro_20"
dir.create(path)
conflicts_prefer(ggClusterNet::network)
conflicted::conflicts_prefer(igraph::path)
result = network(ps = ps,
                 N =10 ,
                 layout_net = "model_Gephi.2",
                 r.threshold=0.6,
                 p.threshold=0.05,
                 fill= "Domain",
                 label = FALSE,
                 path = path,
                 zipi = TRUE)

# 不同组网络绘制到一个面板
p = result[[1]]

p
# 全部样本网络参数比对
data = result[[2]]
library(grid)
ggsave("网络性质分析_CM_RCI_RM.pdf", p,width =45,height = 14.5)



#plotname1 = paste(path,"/network_all.pdf",sep = "")
#ggsave(plotname1, p,width = 7.5,height = 5.5)

tablename <- paste(path,"/co-occurrence_Grobel_net",".csv",sep = "")
write.csv(data,tablename)
tablename <- paste(path,"/Total_co-occurrence_Grobel_net",".csv",sep = "")
write.csv(dat,tablename)


#--随即取出任意比例节点-网络鲁棒性#---------
res = Robustness.Random.removal(ps = ps,
                                Top = 1000,
                                r.threshold= 0.6,
                                p.threshold=0.05,
                                method = "spearman"
)
p = res[[1]]
p
dat = res[[2]]

dir.create("./Robustness_Random_removal/")
write.csv(dat,
          paste("./Robustness_Random_removal/","_random_removal_network.csv",sep = ""))
ggsave(paste("./Robustness_Random_removal/","_random_removal_network.pdf",sep = ""),
       p,width = 8,height = 4)

#---去除关键节点-网络鲁棒性#------
conflicted::conflicts_prefer(dplyr::filter)
res= Robustness.Targeted.removal(ps = ps,
                                 Top = 1000,
                                 degree = TRUE,
                                 zipi = FALSE,
                                 r.threshold= 0.6,
                                 p.threshold=0.05,
                                 method = "spearman")

p = res[[1]]
p
dat = res[[2]]

dir.create(".//Robustness_Targeted_removal/")
write.csv(dat,
          paste(".//Robustness_Targeted_removal/","_random_removal_network.csv",sep = ""))
ggsave(paste(".//Robustness_Targeted_removal/",
             "_Targeted_removal_network.pdf",sep = ""),
       p,width = 8,height = 4
)

#---网络易损性#------

res = Vulnerability.micro(ps = ps,
                          Top = 500,
                          degree = TRUE,
                          zipi = FALSE,
                          r.threshold= 0.6,
                          p.threshold=0.05,
                          method = "spearman")

p = res[[1]]
p
dat = res[[2]]

dir.create(".//Vulnerability/")
write.csv(dat,
          paste("./Vulnerability//","_Vulnerability_network.csv",sep = ""))
ggsave(paste(".//Vulnerability/",
             "_Vulnerability_network.pdf",sep = ""),
       p,width = 8,height = 4
)

#--计算负相关的比例#----
res = negative.correlation.ratio(ps = ps,
                                 Top = 1000,
                                 degree = TRUE,
                                 zipi = FALSE,
                                 r.threshold= 0.6,
                                 p.threshold=0.05,
                                 method = "spearman")
#报错：At core/community/walktrap/walktrap.cpp:149 : Weight vector must be non-negative. Invalid value
p = res[[1]]
p
dat = res[[2]]

dir.create("./negative_correlation_ratio/")
write.csv(dat,
          paste("./negative_correlation_ratio/","_negative_ratio_network.csv",sep = ""))
ggsave(paste(".//negative_correlation_ratio/",
             "_negative_ratio_network.pdf",sep = ""),
       p,width = 8,height = 4
)

#--组成稳定性#----
treat = ps %>% sample_data()
treat$pair = paste( "A",c(rep(1:33,3)),sep = "")
head(treat)
sample_data(ps) = treat
res = community.stability( ps = ps,time = FALSE)
p = res[[1]]
p
dat = res[[2]]
dir.create("./community_stability/")
write.csv(dat,
          paste("./community_stability/","community_stability.csv",sep = ""))
ggsave(paste(".//community_stability/",
             "community_stability_network.pdf",sep = ""),
       p,width = 8,height = 4
)


#--网络抗毁性#----
#网址：https://mp.weixin.qq.com/s/6ujvLiXqvw6iywsZHAmErg
library(tidyfst)
library("pulsar")
library(ggClusterNet)
library(phyloseq)
library(tidyverse)
#报错：Error in `diag<-`(`*tmp*`, value = 0) : 只能替换矩阵的对角----
res = natural.con.microp (
  ps = ps,
  Top = 500,
  r.threshold= 0.6,
  p.threshold=0.05,
  method = "spearman",
  norm = F,
  end = 450,# 小于网络包含的节点数量
  start = 0,
  con.method = "pulsar"
)

p = res[[1]]
p
dat  = res[[2]]

dir.create("./网络抗毁性/")
write.csv(dat,
          paste("./网络抗毁性/","Natural_connectivity.csv",sep = ""))
ggsave(paste(".//网络抗毁性/",
             "Natural_connectivity.pdf",sep = ""),
       p,width = 8,height = 4
)





#————————CM————————----
Envnetplot<- paste("./ALL_NETWORK_CM",sep = "")
dir.create(Envnetplot)


otutab1 <- CK0[,c(1:132)]#otu FZ CM
sample1<- CK1[c(1:132),]#otu CM


taxonomy1 <-  CK0[,c(133:141)]


OTU1<-otutab1
TAX1<-taxonomy1
# 提取两个表中共有的ID
# 对OTU和taxonomy表中共同出现的OTU进行判断，出现判断为TRUE 
idx1 = rownames(OTU1) %in% rownames(TAX1)
# 依据idx 分别从OTU和TAX表中筛选

OTU1 = OTU1[idx1,]
TAX1 = TAX1[rownames(OTU1),]

#读取发育树需要ape包直接使用install.packages(“ape”) 命令
#library("ape")
#tree = read.tree("rooted_tree.tre")

# 构建phyloseq(ps)对象
ps1 = phyloseq(sample_data(sample1),otu_table(as.matrix(OTU1), taxa_are_rows=TRUE), tax_table(as.matrix(TAX1)))
#ps=merge_phyloseq(ps, tree)
ps1=ps1
#data(ps)
ps1




#----------计算相关#----
result = corMicro (ps = ps1,
                   N = 500,
                   method.scale = "TMM",
                   r.threshold=0.8,
                   p.threshold=0.05,
                   method = "spearman"
)
#--提取相关矩阵
cor = result[[1]]
# head(cor)
#-网络中包含的OTU的phyloseq文件提取
ps_net = result[[3]]
#-导出otu表格
otu_table = ps_net %>% 
  vegan_otu() %>%
  t() %>%
  as.data.frame()
tax = ps_net %>% vegan_tax() %>%
  as.data.frame()
tax$filed = tax$TAX
group2 <- data.frame(ID = row.names(tax),group = tax$TAX)
group2$group  =as.factor(group2$group)
result2 = PolygonClusterG (cor = cor,nodeGroup =group2 )
node = result2[[1]]
# ---node节点注释#-----------
nodes = nodeadd(plotcord =node,otu_table = otu_table,tax_table = taxonomy1)
#-----计算边#--------
edge = edgeBuild(cor = cor,node = node)
### 出图
pnet <- ggplot() + geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2,color = as.factor(cor)),
                                data = edge, size = 0.5) +
  geom_point(aes(X1, X2,fill = TAX,size = mean),pch = 21, data = nodes) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  # labs( title = paste(layout,"network",sep = "_"))+
  # geom_text_repel(aes(X1, X2,label=Phylum),size=4, data = plotcord)+
  # discard default grid + titles in ggplot2
  theme(panel.background = element_blank()) +
  # theme(legend.position = "none") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.background = element_rect(colour = NA)) +
  theme(panel.background = element_rect(fill = "white",  colour = NA)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
pnet
ggsave("./CM_Metazoa_fungi_protist.pdf",pnet,width =10,height = 8)

plotname1 = paste(Envnetplot,"/NETWORK_CM_.pdf",sep = "")
ggsave(plotname1,pnet,width = 10,height = 8)
tablename <- paste(Envnetplot,"/NETWORK_CM__edge",".csv",sep = "")
write.csv(edge,tablename)
tablename <- paste(Envnetplot,"/NETWORK_CM__node_imformation",".csv",sep = "")
write.csv(nodes,tablename)
tablename <- paste(Envnetplot,"/NETWORK_CM__cor_imformation",".csv",sep = "")
write.csv(cor,tablename)

#————————RCI————————----
Envnetplot<- paste("./ALL_NETWORK_RCI",sep = "")
dir.create(Envnetplot)

otutab1 <- CK0[,c(49:96)]#otu FZ RCI
sample1<- CK1[c(49:96),]#otu 

taxonomy1 <-  CK0[,c(133:141)]


OTU1<-otutab1
TAX1<-taxonomy1
# 提取两个表中共有的ID
# 对OTU和taxonomy表中共同出现的OTU进行判断，出现判断为TRUE 
idx1 = rownames(OTU1) %in% rownames(TAX1)
# 依据idx 分别从OTU和TAX表中筛选

OTU1 = OTU1[idx1,]
TAX1 = TAX1[rownames(OTU1),]

#读取发育树需要ape包直接使用install.packages(“ape”) 命令
#library("ape")
#tree = read.tree("rooted_tree.tre")

# 构建phyloseq(ps)对象
ps1 = phyloseq(sample_data(sample1),otu_table(as.matrix(OTU1), taxa_are_rows=TRUE), tax_table(as.matrix(TAX1)))
#ps=merge_phyloseq(ps, tree)
ps1=ps1
#data(ps)
ps1




#----------计算相关#----
result = corMicro (ps = ps1,
                   N = 500,
                   method.scale = "TMM",
                   r.threshold=0.8,
                   p.threshold=0.05,
                   method = "spearman"
)
#--提取相关矩阵
cor = result[[1]]
# head(cor)
#-网络中包含的OTU的phyloseq文件提取
ps_net = result[[3]]
#-导出otu表格
otu_table = ps_net %>% 
  vegan_otu() %>%
  t() %>%
  as.data.frame()
tax = ps_net %>% vegan_tax() %>%
  as.data.frame()
tax$filed = tax$TAX
group2 <- data.frame(ID = row.names(tax),group = tax$TAX)
group2$group  =as.factor(group2$group)
result2 = PolygonClusterG (cor = cor,nodeGroup =group2 )
node = result2[[1]]
# ---node节点注释#-----------
nodes = nodeadd(plotcord =node,otu_table = otu_table,tax_table = taxonomy1)
#-----计算边#--------
edge = edgeBuild(cor = cor,node = node)
### 出图
pnet <- ggplot() + geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2,color = as.factor(cor)),
                                data = edge, size = 0.5) +
  geom_point(aes(X1, X2,fill = TAX,size = mean),pch = 21, data = nodes) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  # labs( title = paste(layout,"network",sep = "_"))+
  # geom_text_repel(aes(X1, X2,label=Phylum),size=4, data = plotcord)+
  # discard default grid + titles in ggplot2
  theme(panel.background = element_blank()) +
  # theme(legend.position = "none") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.background = element_rect(colour = NA)) +
  theme(panel.background = element_rect(fill = "white",  colour = NA)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
pnet
ggsave("./RCI_Metazoa_fungi_protist.pdf",pnet,width =10,height = 8)

plotname1 = paste(Envnetplot,"/NETWORK_RCI_.pdf",sep = "")
ggsave(plotname1,pnet,width = 10,height = 8)
tablename <- paste(Envnetplot,"/NETWORK_RCI__edge",".csv",sep = "")
write.csv(edge,tablename)
tablename <- paste(Envnetplot,"/NETWORK_RCI__node_imformation",".csv",sep = "")
write.csv(nodes,tablename)
tablename <- paste(Envnetplot,"/NETWORK_RCI__cor_imformation",".csv",sep = "")
write.csv(cor,tablename)

#————————RM————————----
otutab1 <- CK0[,c(97:132)]#otu RM
sample1<- CK1[c(97:132),]#otu RM



taxonomy1 <-  CK0[,c(133:141)]


OTU1<-otutab1
TAX1<-taxonomy1
# 提取两个表中共有的ID
# 对OTU和taxonomy表中共同出现的OTU进行判断，出现判断为TRUE 
idx1 = rownames(OTU1) %in% rownames(TAX1)
# 依据idx 分别从OTU和TAX表中筛选

OTU1 = OTU1[idx1,]
TAX1 = TAX1[rownames(OTU1),]

#读取发育树需要ape包直接使用install.packages(“ape”) 命令
#library("ape")
#tree = read.tree("rooted_tree.tre")

# 构建phyloseq(ps)对象
ps1 = phyloseq(sample_data(sample1),otu_table(as.matrix(OTU1), taxa_are_rows=TRUE), tax_table(as.matrix(TAX1)))
#ps=merge_phyloseq(ps, tree)
ps1=ps1
#data(ps)
ps1




#----------计算相关#----
result = corMicro (ps = ps1,
                   N = 500,
                   method.scale = "TMM",
                   r.threshold=0.8,
                   p.threshold=0.05,
                   method = "spearman"
)
#--提取相关矩阵
cor = result[[1]]
# head(cor)
#-网络中包含的OTU的phyloseq文件提取
ps_net = result[[3]]
#-导出otu表格
otu_table = ps_net %>% 
  vegan_otu() %>%
  t() %>%
  as.data.frame()
tax = ps_net %>% vegan_tax() %>%
  as.data.frame()
tax$filed = tax$TAX
group2 <- data.frame(ID = row.names(tax),group = tax$TAX)
group2$group  =as.factor(group2$group)
result2 = PolygonClusterG (cor = cor,nodeGroup =group2 )
node = result2[[1]]
# ---node节点注释#-----------
nodes = nodeadd(plotcord =node,otu_table = otu_table,tax_table = taxonomy1)
#-----计算边#--------
edge = edgeBuild(cor = cor,node = node)
### 出图
pnet <- ggplot() + geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2,color = as.factor(cor)),
                                data = edge, size = 0.5) +
  geom_point(aes(X1, X2,fill = TAX,size = mean),pch = 21, data = nodes) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  # labs( title = paste(layout,"network",sep = "_"))+
  # geom_text_repel(aes(X1, X2,label=Phylum),size=4, data = plotcord)+
  # discard default grid + titles in ggplot2
  theme(panel.background = element_blank()) +
  # theme(legend.position = "none") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.background = element_rect(colour = NA)) +
  theme(panel.background = element_rect(fill = "white",  colour = NA)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
pnet
ggsave("./RM_Metazoa_fungi_protist.pdf",pnet,width =10,height = 8)

plotname1 = paste(Envnetplot,"/NETWORK_RM_.pdf",sep = "")
ggsave(plotname1,pnet,width = 10,height = 8)
tablename <- paste(Envnetplot,"/NETWORK_RM__edge",".csv",sep = "")
write.csv(edge,tablename)
tablename <- paste(Envnetplot,"/NETWORK_RM__node_imformation",".csv",sep = "")
write.csv(nodes,tablename)
tablename <- paste(Envnetplot,"/NETWORK_RM__cor_imformation",".csv",sep = "")
write.csv(cor,tablename)



#——————————————————Bacteria:______Archaea——————————————————————————————--------------------------


getwd()
CKB = read.table("F:/RData/2021-Microeukaryotes/BW.txt", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
CK1<-read.table("F:/RData/2021-Microeukaryotes/group_M_RM加了NO.txt",  header=T,row.names=1,  sep="\t", comment.char="", stringsAsFactors = F)
# 引入dplyr包
library(dplyr)

###Prokaryotic----
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\原核生物网络\\原核生物")
# 使用filter函数筛选Name列中含有"Bacteria"的行


otutab1 <- CKB[,c(1:129)]#otu
sample1<- CK1[c(c(1:11),c(13:47),c(49:119),c(133:144)),]#otu

taxonomy1 <-  CKB[,c(130:136)]


###Bacteria----
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\原核生物网络\\细菌")
# 使用filter函数筛选Name列中含有"Bacteria"的行
conflicted::conflicts_prefer(dplyr::filter)
CKB_Bacteria <- filter(CKB, Domain == "d__Bacteria")

otutab1 <- CKB_Bacteria[,c(1:129)]#otu
sample1<- CK1[c(c(1:11),c(13:47),c(49:119),c(133:144)),]#otu

taxonomy1 <-  CKB_Bacteria[,c(130:136)]


###Archaea----
# 使用filter函数筛选Name列中含有"Archaea"的行
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\原核生物网络\\古菌")
conflicted::conflicts_prefer(dplyr::filter)
CKB_Archaea <- filter(CKB, Domain == "d__Archaea")
otutab1 <- CKB_Archaea[,c(1:129)]#otu
sample1<- CK1[c(c(1:11),c(13:47),c(49:119),c(133:144)),]#otu

taxonomy1 <- CKB_Archaea[,c(130:136)]




# 提取两个表中共有的ID
# 对OTU和taxonomy表中共同出现的OTU进行判断，出现判断为TRUE 
idx1 = rownames(otutab1) %in% rownames(taxonomy1)
# 依据idx 分别从OTU和TAX表中筛选

otutab1 = otutab1[idx1,]
taxonomy1= taxonomy1[rownames(otutab1),]

#读取发育树需要ape包直接使用install.packages(“ape”) 命令
#library("ape")
#tree = read.tree("rooted_tree.tre")

# 构建phyloseq(ps)对象
ps1 = phyloseq(sample_data(sample1),otu_table(as.matrix(otutab1), taxa_are_rows=TRUE), tax_table(as.matrix(taxonomy1)))
#ps=merge_phyloseq(ps, tree)
ps=ps1
#data(ps)
ps

ps =  ps %>%
  scale_micro("rela") %>%
  phyloseq::subset_samples(Group %in% c("CM","RCI","RM")) %>%
  filter_OTU_ps(500)

result = cor_Big_micro(ps = ps,
                       N = 0,
                       r.threshold=0.6,
                       p.threshold=0.05,
                       method = "spearman"
)

#--提取相关矩阵
cor = result[[1]]
dim(cor)
conflict_prefer("simplify", "igraph")
conflict_scout()
result2 <- model_igraph(cor = cor,
                        method = "cluster_fast_greedy",
                        seed = 12
)
node = result2[[1]]
head(node)

dat = result2[[2]]
head(dat)
tem = data.frame(mod = dat$model,col = dat$color) %>%  
  dplyr::distinct( mod, .keep_all = TRUE)  
col = tem$col
names(col) = tem$mod

#---node节点注释#-----------
otu_table = as.data.frame(t(vegan_otu(ps)))
tax_table = as.data.frame(vegan_tax(ps))
nodes = nodeadd(plotcord =node,otu_table = otu_table,tax_table = tax_table)
head(nodes)
#-----计算边#--------
edge = edgeBuild(cor = cor,node = node)
colnames(edge)[8] = "cor"
head(edge)

tem2 = dat %>% 
  dplyr::select(OTU,model,color) %>%
  dplyr::right_join(edge,by =c("OTU" = "OTU_1" ) ) %>%
  dplyr::rename(OTU_1 = OTU,model1 = model,color1 = color)
head(tem2)

tem3 = dat %>% 
  dplyr::select(OTU,model,color) %>%
  dplyr::right_join(edge,by =c("OTU" = "OTU_2" ) ) %>%
  dplyr::rename(OTU_2 = OTU,model2 = model,color2 = color)
head(tem3)

tem4 = tem2 %>%inner_join(tem3)
head(tem4)

edge2 = tem4 %>% mutate(color = ifelse(model1 == model2,as.character(model1),"across"),
                        manual = ifelse(model1 == model2,as.character(color1),"#C1C1C1")
)
head(edge2)
#具体解决方案：
#dplyr包经常和其他包的函数有冲突，需要选择一下优先级：
library(conflicted)
conflict_prefer('filter','dplyr')
conflict_prefer('select','dplyr')
conflict_scout()
#结果顺利运行
col_edge = edge2 %>% dplyr::distinct(color, .keep_all = TRUE)  %>% 
  select(color,manual)
col0 = col_edge$manual
names(col0) = col_edge$color

library(ggnewscale)

p1 <- ggplot() + geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2,color = color),
                              data = edge2, size = 1) +
  scale_colour_manual(values = col0) 
p1
ggsave("./_cs1.pdf",p1,width = 10,height = 6)
p2 = p1 +
  new_scale_color() +
  geom_point(aes(X1, X2,color =model), data = dat,size = 4) +
  scale_colour_manual(values = col) +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  theme(panel.background = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.background = element_rect(colour = NA)) +
  theme(panel.background = element_rect(fill = "white",  colour = NA)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
p2
ggsave("./_cs2.pdf",p2,width = 14,height = 12)


#——————————节点模块化计算和可视化————————————————————----
result4 = nodeEdge(cor = cor)
#提取变文件
edge = result4[[1]]
#--提取节点文件
node = result4[[2]]
igraph  = igraph::graph_from_data_frame(edge, directed = FALSE, vertices = node)
res = ZiPiPlot(igraph = igraph,method = "cluster_fast_greedy")
p0 <- res[[1]]
p0

ggsave("Z_score.pdf",p0,width = 7.5,height = 5.5)
#————————网络性质计算——————————————————----
#22年6月升级后版本包括了16项网络属性，
#包括周集中老师21年NCC文章中全部属性
dat = net_properties(igraph)
head(dat)
# 报错————升级后包含的网络属性更多
dat = net_properties.2(igraph,n.hub = T)
head(dat,n = 16)
write.csv(dat,file="网络属性.csv")
#——————————节点性质计算——————————----
nodepro = node_properties(igraph)
head(nodepro)
write.csv(nodepro,file="节点性质.csv")

#——————————扩展-关键OTU挑选——————————----
#Hub微生物就是与其他微生物联系较为紧密的微生物，
#可以称之为关键微生物（keystone）
hub = hub_score(igraph)$vector %>%
  sort(decreasing = TRUE) %>%
  head(80) %>%
  as.data.frame()

colnames(hub) = "hub_sca"

p1<-ggplot(hub) +
  geom_bar(aes(x = hub_sca,y = reorder(row.names(hub),hub_sca)),stat = "identity",fill = "#4DAF4A")
p1
ggsave("hub_sca.pdf",p1,width = 8,height = 20)
write.csv(hub,file="关键OTU挑80.csv")

#——————————对应随机网络构建和网络参数比对——————————----
result = random_Net_compate(igraph = igraph, type = "gnm", step = 100, netName = layout)
p2 = result[[1]]
sum_net = result[[4]]
p2
ggsave("network_ERnetwork.pdf",p2,width = 7.5,height = 5.5)
head(sum_net)

#——————————微生物组网络pipeline分析————————————----
#使用network函数运行微生物网络全套分析：

#使用OTU数量建议少于250个，如果OTU数量为250个，
#同时计算zipi，整个运算过程为3-5min。

ps
path = "./result_micro_500"
dir.create(path)
conflicts_prefer(ggClusterNet::network)
conflicted::conflicts_prefer(igraph::path)
result = network(ps = ps,
                 N =10 ,
                 layout_net = "model_Gephi.2",
                 r.threshold=0.6,
                 p.threshold=0.05,
                 fill= "Class",
                 label = FALSE,
                 path = path,
                 zipi = TRUE)

# 不同组网络绘制到一个面板
p = result[[1]]

p
# 全部样本网络参数比对
data = result[[2]]
library(grid)
ggsave("网络性质分析_CM_RCI_RM.pdf", p,width =45,height = 14.5)



#plotname1 = paste(path,"/network_all.pdf",sep = "")
#ggsave(plotname1, p,width = 7.5,height = 5.5)

tablename <- paste(path,"/co-occurrence_Grobel_net",".csv",sep = "")
write.csv(data,tablename)
tablename <- paste(path,"/Total_co-occurrence_Grobel_net",".csv",sep = "")
write.csv(dat,tablename)


#--随即取出任意比例节点-网络鲁棒性#---------
res = Robustness.Random.removal(ps = ps,
                                Top = 500,
                                r.threshold= 0.6,
                                p.threshold=0.05,
                                method = "spearman"
)
p = res[[1]]
p
dat = res[[2]]

dir.create("./Robustness_Random_removal/")
write.csv(dat,
          paste("./Robustness_Random_removal/","_random_removal_network.csv",sep = ""))
ggsave(paste("./Robustness_Random_removal/","_random_removal_network.pdf",sep = ""),
       p,width = 8,height = 4)

#---去除关键节点-网络鲁棒性#------
conflicted::conflicts_prefer(dplyr::filter)
res= Robustness.Targeted.removal(ps = ps,
                                 Top = 500,
                                 degree = TRUE,
                                 zipi = FALSE,
                                 r.threshold= 0.6,
                                 p.threshold=0.05,
                                 method = "spearman")

p = res[[1]]
p
dat = res[[2]]

dir.create(".//Robustness_Targeted_removal/")
write.csv(dat,
          paste(".//Robustness_Targeted_removal/","_random_removal_network.csv",sep = ""))
ggsave(paste(".//Robustness_Targeted_removal/",
             "_Targeted_removal_network.pdf",sep = ""),
       p,width = 8,height = 4
)

#---网络易损性#------

res = Vulnerability.micro(ps = ps,
                          Top = 500,
                          degree = TRUE,
                          zipi = FALSE,
                          r.threshold= 0.6,
                          p.threshold=0.05,
                          method = "spearman")

p = res[[1]]
p
dat = res[[2]]

dir.create(".//Vulnerability/")
write.csv(dat,
          paste("./Vulnerability//","_Vulnerability_network.csv",sep = ""))
ggsave(paste(".//Vulnerability/",
             "_Vulnerability_network.pdf",sep = ""),
       p,width = 8,height = 4
)

#--计算负相关的比例#----
res = negative.correlation.ratio(ps = ps,
                                 Top = 500,
                                 degree = TRUE,
                                 zipi = FALSE,
                                 r.threshold= 0.6,
                                 p.threshold=0.05,
                                 method = "spearman")
#报错：At core/community/walktrap/walktrap.cpp:149 : Weight vector must be non-negative. Invalid value
p = res[[1]]
p
dat = res[[2]]

dir.create("./negative_correlation_ratio/")
write.csv(dat,
          paste("./negative_correlation_ratio/","_negative_ratio_network.csv",sep = ""))
ggsave(paste(".//negative_correlation_ratio/",
             "_negative_ratio_network.pdf",sep = ""),
       p,width = 8,height = 4
)

#--组成稳定性#----
treat = ps %>% sample_data()
treat$pair = paste( "A",c(rep(1:33,3)),sep = "")
head(treat)
sample_data(ps) = treat
res = community.stability( ps = ps,time = FALSE)
p = res[[1]]
p
dat = res[[2]]
dir.create("./community_stability/")
write.csv(dat,
          paste("./community_stability/","community_stability.csv",sep = ""))
ggsave(paste(".//community_stability/",
             "community_stability_network.pdf",sep = ""),
       p,width = 8,height = 4
)


#--网络抗毁性#----
#网址：https://mp.weixin.qq.com/s/6ujvLiXqvw6iywsZHAmErg
library(tidyfst)
library("pulsar")
library(ggClusterNet)
library(phyloseq)
library(tidyverse)
#报错：Error in `diag<-`(`*tmp*`, value = 0) : 只能替换矩阵的对角----
res = natural.con.microp (
  ps = ps,
  Top = 500,
  r.threshold= 0.6,
  p.threshold=0.05,
  method = "spearman",
  norm = F,
  end = 450,# 小于网络包含的节点数量
  start = 0,
  con.method = "pulsar"
)

p = res[[1]]
p
dat  = res[[2]]

dir.create("./网络抗毁性/")
write.csv(dat,
          paste("./网络抗毁性/","Natural_connectivity.csv",sep = ""))
ggsave(paste(".//网络抗毁性/",
             "Natural_connectivity.pdf",sep = ""),
       p,width = 8,height = 4
)





#————————CM————————----
Envnetplot<- paste("./ALL_NETWORK_CM",sep = "")
dir.create(Envnetplot)


otutab1 <- CK0[,c(1:132)]#otu FZ CM
sample1<- CK1[c(1:132),]#otu CM


taxonomy1 <-  CK0[,c(133:141)]


OTU1<-otutab1
TAX1<-taxonomy1
# 提取两个表中共有的ID
# 对OTU和taxonomy表中共同出现的OTU进行判断，出现判断为TRUE 
idx1 = rownames(OTU1) %in% rownames(TAX1)
# 依据idx 分别从OTU和TAX表中筛选

OTU1 = OTU1[idx1,]
TAX1 = TAX1[rownames(OTU1),]

#读取发育树需要ape包直接使用install.packages(“ape”) 命令
#library("ape")
#tree = read.tree("rooted_tree.tre")

# 构建phyloseq(ps)对象
ps1 = phyloseq(sample_data(sample1),otu_table(as.matrix(OTU1), taxa_are_rows=TRUE), tax_table(as.matrix(TAX1)))
#ps=merge_phyloseq(ps, tree)
ps1=ps1
#data(ps)
ps1




#----------计算相关#----
result = corMicro (ps = ps1,
                   N = 500,
                   method.scale = "TMM",
                   r.threshold=0.8,
                   p.threshold=0.05,
                   method = "spearman"
)
#--提取相关矩阵
cor = result[[1]]
# head(cor)
#-网络中包含的OTU的phyloseq文件提取
ps_net = result[[3]]
#-导出otu表格
otu_table = ps_net %>% 
  vegan_otu() %>%
  t() %>%
  as.data.frame()
tax = ps_net %>% vegan_tax() %>%
  as.data.frame()
tax$filed = tax$TAX
group2 <- data.frame(ID = row.names(tax),group = tax$TAX)
group2$group  =as.factor(group2$group)
result2 = PolygonClusterG (cor = cor,nodeGroup =group2 )
node = result2[[1]]
# ---node节点注释#-----------
nodes = nodeadd(plotcord =node,otu_table = otu_table,tax_table = taxonomy1)
#-----计算边#--------
edge = edgeBuild(cor = cor,node = node)
### 出图
pnet <- ggplot() + geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2,color = as.factor(cor)),
                                data = edge, size = 0.5) +
  geom_point(aes(X1, X2,fill = TAX,size = mean),pch = 21, data = nodes) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  # labs( title = paste(layout,"network",sep = "_"))+
  # geom_text_repel(aes(X1, X2,label=Phylum),size=4, data = plotcord)+
  # discard default grid + titles in ggplot2
  theme(panel.background = element_blank()) +
  # theme(legend.position = "none") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.background = element_rect(colour = NA)) +
  theme(panel.background = element_rect(fill = "white",  colour = NA)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
pnet
ggsave("./CM_Metazoa_fungi_protist.pdf",pnet,width =10,height = 8)

plotname1 = paste(Envnetplot,"/NETWORK_CM_.pdf",sep = "")
ggsave(plotname1,pnet,width = 10,height = 8)
tablename <- paste(Envnetplot,"/NETWORK_CM__edge",".csv",sep = "")
write.csv(edge,tablename)
tablename <- paste(Envnetplot,"/NETWORK_CM__node_imformation",".csv",sep = "")
write.csv(nodes,tablename)
tablename <- paste(Envnetplot,"/NETWORK_CM__cor_imformation",".csv",sep = "")
write.csv(cor,tablename)

#————————RCI————————----
Envnetplot<- paste("./ALL_NETWORK_RCI",sep = "")
dir.create(Envnetplot)

otutab1 <- CK0[,c(49:96)]#otu FZ RCI
sample1<- CK1[c(49:96),]#otu 

taxonomy1 <-  CK0[,c(133:141)]


OTU1<-otutab1
TAX1<-taxonomy1
# 提取两个表中共有的ID
# 对OTU和taxonomy表中共同出现的OTU进行判断，出现判断为TRUE 
idx1 = rownames(OTU1) %in% rownames(TAX1)
# 依据idx 分别从OTU和TAX表中筛选

OTU1 = OTU1[idx1,]
TAX1 = TAX1[rownames(OTU1),]

#读取发育树需要ape包直接使用install.packages(“ape”) 命令
#library("ape")
#tree = read.tree("rooted_tree.tre")

# 构建phyloseq(ps)对象
ps1 = phyloseq(sample_data(sample1),otu_table(as.matrix(OTU1), taxa_are_rows=TRUE), tax_table(as.matrix(TAX1)))
#ps=merge_phyloseq(ps, tree)
ps1=ps1
#data(ps)
ps1




#----------计算相关#----
result = corMicro (ps = ps1,
                   N = 500,
                   method.scale = "TMM",
                   r.threshold=0.8,
                   p.threshold=0.05,
                   method = "spearman"
)
#--提取相关矩阵
cor = result[[1]]
# head(cor)
#-网络中包含的OTU的phyloseq文件提取
ps_net = result[[3]]
#-导出otu表格
otu_table = ps_net %>% 
  vegan_otu() %>%
  t() %>%
  as.data.frame()
tax = ps_net %>% vegan_tax() %>%
  as.data.frame()
tax$filed = tax$TAX
group2 <- data.frame(ID = row.names(tax),group = tax$TAX)
group2$group  =as.factor(group2$group)
result2 = PolygonClusterG (cor = cor,nodeGroup =group2 )
node = result2[[1]]
# ---node节点注释#-----------
nodes = nodeadd(plotcord =node,otu_table = otu_table,tax_table = taxonomy1)
#-----计算边#--------
edge = edgeBuild(cor = cor,node = node)
### 出图
pnet <- ggplot() + geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2,color = as.factor(cor)),
                                data = edge, size = 0.5) +
  geom_point(aes(X1, X2,fill = TAX,size = mean),pch = 21, data = nodes) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  # labs( title = paste(layout,"network",sep = "_"))+
  # geom_text_repel(aes(X1, X2,label=Phylum),size=4, data = plotcord)+
  # discard default grid + titles in ggplot2
  theme(panel.background = element_blank()) +
  # theme(legend.position = "none") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.background = element_rect(colour = NA)) +
  theme(panel.background = element_rect(fill = "white",  colour = NA)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
pnet
ggsave("./RCI_Metazoa_fungi_protist.pdf",pnet,width =10,height = 8)

plotname1 = paste(Envnetplot,"/NETWORK_RCI_.pdf",sep = "")
ggsave(plotname1,pnet,width = 10,height = 8)
tablename <- paste(Envnetplot,"/NETWORK_RCI__edge",".csv",sep = "")
write.csv(edge,tablename)
tablename <- paste(Envnetplot,"/NETWORK_RCI__node_imformation",".csv",sep = "")
write.csv(nodes,tablename)
tablename <- paste(Envnetplot,"/NETWORK_RCI__cor_imformation",".csv",sep = "")
write.csv(cor,tablename)

#————————RM————————----
otutab1 <- CK0[,c(97:132)]#otu RM
sample1<- CK1[c(97:132),]#otu RM



taxonomy1 <-  CK0[,c(133:141)]


OTU1<-otutab1
TAX1<-taxonomy1
# 提取两个表中共有的ID
# 对OTU和taxonomy表中共同出现的OTU进行判断，出现判断为TRUE 
idx1 = rownames(OTU1) %in% rownames(TAX1)
# 依据idx 分别从OTU和TAX表中筛选

OTU1 = OTU1[idx1,]
TAX1 = TAX1[rownames(OTU1),]

#读取发育树需要ape包直接使用install.packages(“ape”) 命令
#library("ape")
#tree = read.tree("rooted_tree.tre")

# 构建phyloseq(ps)对象
ps1 = phyloseq(sample_data(sample1),otu_table(as.matrix(OTU1), taxa_are_rows=TRUE), tax_table(as.matrix(TAX1)))
#ps=merge_phyloseq(ps, tree)
ps1=ps1
#data(ps)
ps1




#----------计算相关#----
result = corMicro (ps = ps1,
                   N = 500,
                   method.scale = "TMM",
                   r.threshold=0.8,
                   p.threshold=0.05,
                   method = "spearman"
)
#--提取相关矩阵
cor = result[[1]]
# head(cor)
#-网络中包含的OTU的phyloseq文件提取
ps_net = result[[3]]
#-导出otu表格
otu_table = ps_net %>% 
  vegan_otu() %>%
  t() %>%
  as.data.frame()
tax = ps_net %>% vegan_tax() %>%
  as.data.frame()
tax$filed = tax$TAX
group2 <- data.frame(ID = row.names(tax),group = tax$TAX)
group2$group  =as.factor(group2$group)
result2 = PolygonClusterG (cor = cor,nodeGroup =group2 )
node = result2[[1]]
# ---node节点注释#-----------
nodes = nodeadd(plotcord =node,otu_table = otu_table,tax_table = taxonomy1)
#-----计算边#--------
edge = edgeBuild(cor = cor,node = node)
### 出图
pnet <- ggplot() + geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2,color = as.factor(cor)),
                                data = edge, size = 0.5) +
  geom_point(aes(X1, X2,fill = TAX,size = mean),pch = 21, data = nodes) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  # labs( title = paste(layout,"network",sep = "_"))+
  # geom_text_repel(aes(X1, X2,label=Phylum),size=4, data = plotcord)+
  # discard default grid + titles in ggplot2
  theme(panel.background = element_blank()) +
  # theme(legend.position = "none") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.background = element_rect(colour = NA)) +
  theme(panel.background = element_rect(fill = "white",  colour = NA)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
pnet
ggsave("./RM_Metazoa_fungi_protist.pdf",pnet,width =10,height = 8)

plotname1 = paste(Envnetplot,"/NETWORK_RM_.pdf",sep = "")
ggsave(plotname1,pnet,width = 10,height = 8)
tablename <- paste(Envnetplot,"/NETWORK_RM__edge",".csv",sep = "")
write.csv(edge,tablename)
tablename <- paste(Envnetplot,"/NETWORK_RM__node_imformation",".csv",sep = "")
write.csv(nodes,tablename)
tablename <- paste(Envnetplot,"/NETWORK_RM__cor_imformation",".csv",sep = "")
write.csv(cor,tablename)


#——————————————————————时空组:多网络单独绘制却填充相同颜色——————————————————————#-----
CK1<-read.table("F:/RData/2021-Microeukaryotes/group_M_RM加了NO.txt",  header=T,row.names=1,  sep="\t", comment.char="", stringsAsFactors = F)
CK0 = read.table("F:/RData/2021-Microeukaryotes/Metazoa_fungi_protist_RM加了NO.txt", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\跨域网络图\\Metozoa_Fungi_Protist_network\\时空分析")
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\跨域网络图\\Metozoa_Fungi_Protist_network\\Protist_网络性质分析_0.6\\Protist_时空分析_0.6")
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\跨域网络图\\Metozoa_Fungi_Protist_network\\Fungi_网络性质分析_0.6\\Fungi_时空分析_0.6")
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\跨域网络图\\Metozoa_Fungi_Protist_network\\Metazoa_网络性质分析_0.6")

###ALL_NETWORK
#otutab1 <- CK0[,c(c(1:24),c(37:72),c(85:132))]#otu
#sample1<- CK1[c(c(1:24),c(37:72),c(85:132)),]#otu


otutab1 <- CK0[,c(1:144)]#otu
sample1<- CK1[c(1:144),]#otu
taxonomy1 <-  CK0[,c(145:153)]

###时空分析----
#MEETAZOA：c(1:3084)；
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\跨域网络图\\Metozoa_Fungi_Protist_network\\Metazoa_网络性质分析_0.6\\Metazoa_时空分析_0.6")
otutab1 <- CK0[c(1:3084),c(1:144)]#otu
sample1<- CK1[c(1:144),]#otu
taxonomy1 <-  CK0[c(1:3084),c(145:153)]

#FUNGI：c(3085:8650);
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\跨域网络图\\Metozoa_Fungi_Protist_network\\Fungi_网络性质分析_0.6\\Fungi_时空分析_0.6")
otutab1 <- CK0[c(3085:8650),c(1:144)]#otu
sample1<- CK1[c(1:144),]#otu
taxonomy1 <-  CK0[c(3085:8650),c(145:153)]

#PROTIST：c(8651:34171)；
setwd("F:\\RData\\2021-Microeukaryotes\\NET_ALL\\跨域网络图\\Metozoa_Fungi_Protist_network\\Protist_网络性质分析_0.6\\Protist_时空分析_0.6")
otutab1 <- CK0[c(8651:34171),c(1:144)]#otu
sample1<- CK1[c(1:144),]#otu
taxonomy1 <-  CK0[c(8651:34171),c(145:153)]

# 提取两个表中共有的ID
# 对OTU和taxonomy表中共同出现的OTU进行判断，出现判断为TRUE 
idx1 = rownames(otutab1) %in% rownames(taxonomy1)
# 依据idx 分别从OTU和TAX表中筛选

otutab1 = otutab1[idx1,]
taxonomy1 = taxonomy1[rownames(otutab1),]

#读取发育树需要ape包直接使用install.packages(“ape”) 命令
#library("ape")
#tree = read.tree("rooted_tree.tre")

# 构建phyloseq(ps)对象
ps1 = phyloseq(sample_data(sample1),otu_table(as.matrix(otutab1), taxa_are_rows=TRUE), tax_table(as.matrix(taxonomy1)))
#ps=merge_phyloseq(ps, tree)
ps.st=ps1
#data(ps)

ps.st


#---时空组双网络手动填充相同颜色#-------
library(tidyverse)
library(ggClusterNet)
library(phyloseq)
library(igraph)

#ps.st = readRDS("./ps_TS.rds")
#ps.st
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 2432 taxa and 108 samples ]
#> sample_data() Sample Data:       [ 108 samples by 4 sample variables ]
#> tax_table()   Taxonomy Table:    [ 2432 taxa by 7 taxonomic ranks ]
#> phy_tree()    Phylogenetic Tree: [ 2432 tips and 2431 internal nodes ]
#> refseq()      DNAStringSet:      [ 2432 reference sequences ]



#-----第一组网络绘制

res = Facet.network (
  ps.st= ps.st,# phyloseq对象
  N = 500,
  g1 = "Group",# 分组1
  g2 =  NULL,# 分组2
  g3 = "time",# 分组3
  ord.g1 = c("CM","RCI","RM"),# 排序顺序
  ord.g2 = NULL ,# 排序顺序
  ord.g3 = c("T1","T2","T3","T4","T5","T6","T7","T8") ,# 排序顺序
  order = "time", # 出图每行代表的变量
  fill = "Class",
  size = "igraph.degree",
  layout_net = "model_maptree2",
  r.threshold=0.6,
  p.threshold=0.05,
  method = "spearman",
  select_layout = TRUE,
  clu_method = "cluster_fast_greedy",
  maxnode = 5
)
#> [1] "B" "R"
#> [1] "T1" "T2" "T3"
#> [1] "WT" "KO" "OE"

p = res[[1]]
p
ggsave("./时空网络——Class.pdf",p,width =24,height = 49)
ggsave("./时空网络——TAX3.pdf",p,width =20,height = 45)


#--指定颜色映射，为了多个图使用同一个颜色#-------



#--设定的参数一致
fill = "Class"
size = "igraph.degree"
maxnode = 5
row.num = 6

#-从结果中提取网络节点和边数据
node  = res[[2]][[2]]
head(node)

edge = res[[2]][[3]]
head(edge)

cb_palette <- c("#aed4e9","#4593c3","#7149af" ,"#1a918f","#5b910f","#af2934", "#b0b9b8","#2baeb5", "#f0eedf",
                "#3ba889","#f4a69a","#023f75","#cebb10","#57e559" ,
                "#262a35","#9ed84e","#a2a7ab","#6373ed",  "#83ba9e","#d66551",   
                "#1a918f", "#edd05e", "#02567e" ,"#ce2523","#373bbf","#931635",  "#db5e92" )

cb_palette <- c("#ed1299", "#09f9f5", "#246b93", "#cc8e12", "#d561dd", "#c93f00", "#ddd53e",
                "#4aef7b", "#e86502", "#9ed84e", "#39ba30", "#6ad157", "#8249aa", "#99db27", "#e07233", "#ff523f",
                "#ce2523", "#f7aa5d",  "#03827f", "#931635", "#373bbf", "#a1ce4c", "#ef3bb6", "#d66551",
                "#1a918f", "#ff66fc", "#2927c4", "#7149af" ,"#57e559" ,"#8e3af4" ,"#f9a270" ,"#22547f", "#db5e92",
                "#edd05e", "#6f25e8", "#0dbc21", "#280f7a",  "#5b910f" ,"#7b34c1" ,"#0cf29a" ,"#d80fc1",
                "#dd27ce", "#07a301", "#167275", "#391c82", "#2baeb5","#925bea", "#63ff4f")
tem1 = node[,fill] %>% as.matrix() %>% as.vector() %>% unique()
tabf = data.frame(id = tem1,color =cb_palette[1:length(tem1)] )
head(tabf)

colnames(tabf)[1] = fill
node1 = node %>% left_join(tabf,by = fill)

p2 <- ggplot() + geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2,
                                  color = cor),
                              data = edge, size = 0.03,alpha = 0.5) +
  geom_point(aes(X1, X2,
                 size = !!sym(size),
                 fill = color ),
             pch = 21, data = node1,color = "gray40") +
  facet_wrap(.~ label,scales="free_y",ncol = row.num ) +
  # geom_text_repel(aes(X1, X2,label = elements),pch = 21, data = nodeG) +
  # geom_text(aes(X1, X2,label = elements),pch = 21, data = nodeG) +
  scale_colour_manual(values = c("#6D98B5","#D48852")) +
  scale_fill_manual(values  = tabf$color,labels = tabf[,fill]) +
  scale_size(range = c(1, maxnode)) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)
  ) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()
  ) +
  theme(legend.background = element_rect(colour = NA)) +
  theme(panel.background = element_rect(fill = "white",  colour = NA)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
p2
ggsave("./时空网络——Class_红色1.pdf",p2,width =35,height = 20)
ggsave("./时空网络——TAX_绿色.pdf",p2,width =30,height = 18)
ggsave("./时空网络——TAX_绿色1.pdf",p2,width =25,height = 15)

Envnetplot<- paste("./时空分析-500",sep = "")
dir.create(Envnetplot)
plotname1 = paste(Envnetplot,"/时空动态_NETWORK_TAX.pdf",sep = "")
ggsave(plotname1,p2,width = 30,height = 18)
tablename <- paste(Envnetplot,"/时空动态_NETWORK_TAX__edge",".csv",sep = "")
write.csv(edge,tablename)
tablename <- paste(Envnetplot,"/时空动态_NETWORK_TAX__node_imformation",".csv",sep = "")
write.csv(node,tablename)
tablename <- paste(Envnetplot,"/时空动态_NETWORK_TAX__cor_imformation",".csv",sep = "")
write.csv(cor,tablename)


#——————————————————————时空组跨域网络：不同分类等级跨域网络————————————————————-----
res = rank.network(
  ps.st= ps.st,# phyloseq对象
  g1 = "Group",# 分组1
  g2 = NULL,# 分组2
  g3 = "time",# 分组3
  ord.g1 =  c("CM","RCI","RM"), # 排序顺序
  ord.g2 = NULL, # 排序顺序
  ord.g3 =  c("T1","T2","T3","T4","T5","T6","T7","T8"), # 排序顺序
  order = "time", # 出图每行代表的变量
  jj = "TAX",
  fill = "TAX",
  method = "spearman",
  clu_method = "cluster_fast_greedy",
  select_layout = TRUE,
  r.threshold=0.6,
  p.threshold=0.05,
  N= 500)

p = res[[1]]
p


ggsave("./时空网络——TAX_绿色.pdf",p,width =15,height = 25)
ggsave("./时空网络——TAX_绿色1.pdf",p,width =25,height = 15)


#--提取相关数据
#a<-res$network.data$cortab$R.T1.WT
Envnetplot<- paste("./时空分析-不同分类间",sep = "")
dir.create(Envnetplot)
plotname1 = paste(Envnetplot,"/时空动态_NETWORK_不同分类间_TAX.pdf",sep = "")
ggsave(plotname1,p2,width = 30,height = 18)
tablename <- paste(Envnetplot,"/时空动态_NETWORK_不同分类间_TAX__edge",".csv",sep = "")
write.csv(res$network.data$edge,tablename)
tablename <- paste(Envnetplot,"/时空动态_NETWORK_不同分类间_node_imformation",".csv",sep = "")
write.csv(res$network.data$node,tablename)
tablename <- paste(Envnetplot,"/时空动态_NETWORK_不同分类间_TAX__cortab_imformation",".csv",sep = "")
write.csv(res$network.data$cortab,tablename)

