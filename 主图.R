setwd("D:/data/ciliate/扩增子-主图")
getwd()
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(ggprism)
library(vegan)
library(picante)
library(dplyr)
library(RColorBrewer)
library(UpSetR)
library(reshape2)
##导入数据，所需是数据行名为样本名、列名为OTUxxx的数据表
df <- read.table("./2021-营养指数1023.txt",header = T, row.names = 1, sep='\t',check.names = F)


#读入分组文件groups <- read.delim('F:/RData/2021-Microeukaryotes/养分循环指数/group.txt',header = T, row.names = 1, sep='\t',check.names = F)



###multinutrient-----
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p1=ggplot(data=df,aes(x=Group,
                      y=multinutrient,
                      color=Group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(0.75,1,0.85),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p1
###TN----
p2=ggplot(data=df,aes(x=Group,
                      y=TN,
                      color=Group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(3.075,3.35,3.15),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p2
###TP----
p3=ggplot(data=df,aes(x=Group,
                      y=TP,
                      color=Group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.12,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=1.1,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(0.465,0.51,0.555),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p3
###TN----
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p4=ggplot(data=df,aes(x=Group,
                      y=NO3,
                      color=Group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(0.3,0.35,0.32),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p4
###NO2----
p5=ggplot(data=df,aes(x=Group,
                      y=NO2.,
                      color=Group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.08,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=1.25,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(0.065,0.08,0.07),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p5
###NH4----
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p6=ggplot(data=df,aes(x=Group,
                      y=NH4,
                      color=Group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(0.85,0.95,1.05),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p6
###PO4----
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p7=ggplot(data=df,aes(x=Group,
                      y=PO4,
                      color=Group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(0.17,0.2,0.18),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p7
###TOC----
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p8=ggplot(data=df,aes(x=Group,
                      y=TOC,
                      color=Group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(61,67,73),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p8
###COD----
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p9=ggplot(data=df,aes(x=Group,
                      y=COD,
                      color=Group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(19,22,20),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p9
###水温-----
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p10=ggplot(data=df,aes(x=Group,
                       y=WaterTemperature,
                       color=Group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(35,43,38),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p10
###DO-----
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p11=ggplot(data=df,aes(x=Group,
                       y=DO,
                       color=Group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(13.6,16,14.5),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p11
###pH-----
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p12=ggplot(data=df,aes(x=Group,
                       y=pH,
                       color=Group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(9.4,10.3,9.8),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p12


pp <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,ncol = 3,nrow=4)
pp


ggsave("./去G-水体理化因子1023-养分循环指数箱线图.pdf", pp,width =15,height = 16)


#####不同模式alpha多样性比较-----
df <- read.table("./otu.txt",header = T, row.names = 1, sep='\t',check.names = F)

df<-df[,c(1:132)]
#使用vegan包计算多样性指数
Shannon <- diversity(df, index = "shannon", MARGIN = 2, base = exp(1))
invSimpson <- diversity(df, index = "invsimpson", MARGIN = 2, base =  exp(1))
Richness <- specnumber(df, MARGIN = 2)#spe.rich =sobs
###将以上多样性指数统计成表格
index <- as.data.frame(cbind(Shannon, invSimpson, Richness))
tdf <- t(df)#转置表格
tdf<-ceiling(as.data.frame(t(df)))
#计算obs，chao，ace指数
obs_chao_ace <- t(estimateR(tdf))
obs_chao_ace <- obs_chao_ace[rownames(index),]#统一行名
#将obs，chao，ace指数与前面指数计算结果进行合并
index$Chao <- obs_chao_ace[,2]
index$Ace <- obs_chao_ace[,4]
index$obs <- obs_chao_ace[,1]
#计算Pielou及覆盖度
index$Pielou <- Shannon / log(Richness, 2)
index$Goods_coverage <- 1 - colSums(df ==1) / colSums(df)
#导出表格
write.table(cbind(sample=c(rownames(index)),index),'./diversity.index.txt', row.names = F, sep = '\t', quote = F)


#读入文件
index <- read.delim('./diversity.index.txt', header = T, row.names = 1)
##figure:take shannon for example
index$samples <- rownames(index)#将样本名写到文件中
#读入分组文件
groups <- read.delim('./group.txt',header = T, stringsAsFactors = F)
colnames(groups)[1:2] <- c('samples','group')#改列名
#合并分组信息与多样性指数
df2 <- merge(index,groups,by = 'samples')

###香农威纳
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p1=ggplot(data=df2,aes(x=group,
                       y=Shannon,
                       color=group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.25,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(5,5.5,5.25),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))


####辛普森
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p2=ggplot(data=df2,aes(x=group,
                       y=invSimpson,
                       color=group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.1,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(40,45,42.5),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))

####ACE指数
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p3=ggplot(data=df2,aes(x=group,
                       y=Ace,
                       color=group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(4500,5000,4750),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))

#####Chao指数
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p4=ggplot(data=df2,aes(x=group,
                       y=Chao,
                       color=group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(4000,4500,4250),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))

#####丰富度
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p5=ggplot(data=df2,aes(x=group,
                       y=Richness,
                       color=group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(2500,2750,3000),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))

####均匀度
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999","#0072B2","#D55E00")
p6=ggplot(data=df2,aes(x=group,
                       y=Pielou,
                       color=group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.15,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(0.5,0.55,0.6),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))

ggarrange(p1,p2,p3,p4,p5,p6,ncol = 3,nrow=2)
#####OTU丰度-----
otu<-read.table("./BW.txt",header = T,row.names = 1,sep = "\t")
df <- read.table("./otu.txt",header = T, row.names = 1, sep='\t',check.names = F)
df_row_indices <- df$CMBPW11 >=0  
new_row_names <- rownames(df)  

new_row_names[df_row_indices] <- paste0("new_name_prefix_", new_row_names[df_row_indices])  


rownames(df) <- new_row_names


df1<-df[,c(1:11,13:47,49:119,121:132)]
otu1<-otu[,c(1:129)]
x<-rbind(otu1,df1)

x$CM <- apply(x[,1:46], 1, sum) 
x$RCI <- apply(x[,47:94], 1, sum) 
x$RM <- apply(x[,95:129], 1, sum) 


x2<-x[,c(130:132)]
x1=x2[which(rowSums(x2) > 0),]

x1[x1>=1]<-1

upset(x1,nsets=3,order.by="freq",sets=colnames(x1),
      queries=list(
        list(query=intersects,params=list(c("CM","RCI","RM")),color="#af2934",active=TRUE),
        list(query=intersects,params=list(c("CM","RM")),color="#ffe327",active=TRUE),
        list(query=intersects,params=list(c("RCI","RM")),color="#2f4e87",active=TRUE),
        list(query=intersects,params=list(c("CM","RCI")),color="#1C86EE",active=TRUE)
      ),
      number.angles=0,
      point.size=4,
      line.size=1,
      mainbar.y.label="Intersection size",
      main.bar.color='#363636',
      sets.bar.color=c("#266b69","#eb4601","#f6c619"),
      sets.x.label="set size",
      mb.ratio=c(0.7,0.3),
      text.scale=c(1.5,1.5,1.5,1.5,1.5,2.0),
      shade.color="#b0b9b8"#图中阴影部分的颜色
)

#######细菌香农维纳指数-----
df <- read.table("./BW_PW考虑所有连接.txt",header = T, row.names = 1, sep='\t',check.names = F)
df_filtered_bacteria <- df %>%filter(TAX == "Bacteria")
df_filtered_bacteria1<-df_filtered_bacteria[,c(1:129)]
Shannon <- diversity(df_filtered_bacteria1, index = "shannon", MARGIN = 2, base = exp(1))
index <- as.data.frame(Shannon)

write.table(cbind(sample=c(rownames(index)),index),'./diversity.index.txt', row.names = F, sep = '\t', quote = F)
index <- read.delim('./diversity.index.txt', header = T, row.names = 1)


##figure:take shannon for example
index$samples <- rownames(index)#将样本名写到文件中
#读入分组文件
groups <- read.delim('./group.txt',header = T, stringsAsFactors = F)
colnames(groups)[1:2] <- c('samples','group')#改列名
#合并分组信息与多样性指数
df2 <- merge(index,groups,by = 'samples')

###香农威纳
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9")
p1=ggplot(data=df2,aes(x=group,
                       y=Shannon,
                       color=group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.25,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(5.5,6,5.75),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p1


######古菌香浓指数---------

df_filtered_Archaea <- df %>%filter(TAX == "Archaea")
df_filtered_Archaea1<-df_filtered_Archaea[,c(1:129)]
Shannon <- diversity(df_filtered_Archaea1, index = "shannon", MARGIN = 2, base = exp(1))
index <- as.data.frame(Shannon)

write.table(cbind(sample=c(rownames(index)),index),'./古菌diversity.index.txt', row.names = F, sep = '\t', quote = F)
index <- read.delim('./古菌diversity.index.txt', header = T, row.names = 1)


##figure:take shannon for example
index$samples <- rownames(index)#将样本名写到文件中
#读入分组文件
groups <- read.delim('./group.txt',header = T, stringsAsFactors = F)
colnames(groups)[1:2] <- c('samples','group')#改列名
#合并分组信息与多样性指数
df2 <- merge(index,groups,by = 'samples')

###香农威纳
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9")
p1=ggplot(data=df2,aes(x=group,
                       y=Shannon,
                       color=group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.25,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("CM","RM"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(4,4.75,4.5),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p1
#####真菌香浓-------

df_filtered_fungi <- df %>%filter(TAX == "Fungi")
df_filtered_fungi1<-df_filtered_fungi[,c(1:129)]
Shannon <- diversity(df_filtered_fungi1, index = "shannon", MARGIN = 2, base = exp(1))
index <- as.data.frame(Shannon)

write.table(cbind(sample=c(rownames(index)),index),'./真菌diversity.index.txt', row.names = F, sep = '\t', quote = F)
index <- read.delim('./真菌diversity.index.txt', header = T, row.names = 1)


##figure:take shannon for example
index$samples <- rownames(index)#将样本名写到文件中
#读入分组文件
groups <- read.delim('./group.txt',header = T, stringsAsFactors = F)
colnames(groups)[1:2] <- c('samples','group')#改列名
#合并分组信息与多样性指数
df2 <- merge(index,groups,by = 'samples')

###香农威纳
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9")
p1=ggplot(data=df2,aes(x=group,
                       y=Shannon,
                       color=group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.25,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(4,4.75,4.5),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p1

#####后生动物香浓----
df_filtered_metazoa <- df %>%filter(TAX == "Metazoa")
df_filtered_metazoa1<-df_filtered_metazoa[,c(1:129)]
Shannon <- diversity(df_filtered_metazoa1, index = "shannon", MARGIN = 2, base = exp(1))
index <- as.data.frame(Shannon)

write.table(cbind(sample=c(rownames(index)),index),'./后生动物diversity.index.txt', row.names = F, sep = '\t', quote = F)
index <- read.delim('./后生动物diversity.index.txt', header = T, row.names = 1)


##figure:take shannon for example
index$samples <- rownames(index)#将样本名写到文件中
#读入分组文件
groups <- read.delim('./group.txt',header = T, stringsAsFactors = F)
colnames(groups)[1:2] <- c('samples','group')#改列名
#合并分组信息与多样性指数
df2 <- merge(index,groups,by = 'samples')

###香农威纳
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9")
p1=ggplot(data=df2,aes(x=group,
                       y=Shannon,
                       color=group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.25,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(2.5,3,2.75),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p1

######原生生物香浓------
df_filtered_protist <- df %>%filter(TAX == "Protist")
df_filtered_protist1<-df_filtered_protist[,c(1:129)]
Shannon <- diversity(df_filtered_protist1, index = "shannon", MARGIN = 2, base = exp(1))
index <- as.data.frame(Shannon)

write.table(cbind(sample=c(rownames(index)),index),'./原生动物diversity.index.txt', row.names = F, sep = '\t', quote = F)
index <- read.delim('./原生动物diversity.index.txt', header = T, row.names = 1)


##figure:take shannon for example
index$samples <- rownames(index)#将样本名写到文件中
#读入分组文件
groups <- read.delim('./group.txt',header = T, stringsAsFactors = F)
colnames(groups)[1:2] <- c('samples','group')#改列名
#合并分组信息与多样性指数
df2 <- merge(index,groups,by = 'samples')

###香农威纳
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9")
p1=ggplot(data=df2,aes(x=group,
                       y=Shannon,
                       color=group))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.25,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  geom_signif(comparisons = list(c("CM","RCI"),
                                 c("RCI","RM")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(5.5,6,5.75),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size =0.8,color="black")+
  theme_classic()  + 
  theme(text = element_text(size=16))+
  theme(panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_text(angle=60,hjust = 1))
p1

#pcoa分析------
otu<-read.table("./BW.txt",header = T,row.names = 1,sep = "\t")
df <- read.table("./otu.txt",header = T, row.names = 1, sep='\t',check.names = F)
df_row_indices <- df$CMBPW11 >=0  
new_row_names <- rownames(df)  

new_row_names[df_row_indices] <- paste0("new_name_prefix_", new_row_names[df_row_indices])  


rownames(df) <- new_row_names


df1<-df[,c(1:11,13:47,49:119,121:132)]
otu1<-otu[,c(1:129)]
x<-rbind(otu1,df1)
df<-t(x)

otu.distance <- vegdist(df)

pcoa <- cmdscale (otu.distance,eig=TRUE)
pc12 <- pcoa$points[,1:2]
pc <- round(pcoa$eig/sum(pcoa$eig)*100,digits=2)#解释度
#pc12原来是matrix,转化为data.frame
pc12 <- as.data.frame(pc12)
#给pc12添加samp1es变量
pc12$samples <- row.names(pc12)

df <- merge(pc12,groups,by="samples")

#组间差异性分析
df_anosim <- anosim(otu.distance,df$group,permutations = 999)

b1<-ggplot(data=df,aes(x=V1,y=V2,color=group))+#指定数据、X轴、Y轴，颜色
  theme_bw()+#主题设置
  geom_point(size=3)+#绘制点图并设定大小
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,lty="dashed",size=0.8)+
  geom_hline(yintercept = 0,lty="dashed",size=0.8)+#图中虚线
  labs(x=paste0("PC1 (",pc[1],"%)"),
       y=paste0("PC2 (",pc[2],"%)"))+#将x、y轴标题改为贡献度
  geom_text(aes(x=0.4,y=-0.2),label=paste("R=",round(df_anosim$statistic,3)),color="black",size=4)+
  geom_text(aes(x=0.4,y=-0.23),label=paste("p=", round(df_anosim$signif,3)),color="black",size=4)+
  scale_color_manual(values = c(brewer.pal(7,"Set3")[c(1,3,4)])) +#点的颜色设置
  theme(axis.title.x=element_text(size=12),#修改X轴标题文本
        axis.title.y=element_text(size=12,angle=90),#修改y轴标题文本
        axis.text.y=element_text(size=10),#修改x轴刻度标签文本
        axis.text.x=element_text(size=10),#修改y轴刻度标签文本
        panel.grid=element_blank())#隐藏网格线
b1
b1+stat_ellipse(data=df,
                geom = "polygon",level=0.9,
                linetype = 1,size=0.8,
                aes(fill=group),
                alpha=0.1,
                show.legend = T)+
  scale_fill_manual(values = c(brewer.pal(7,"Set3")[c(1,3,4)]))



####RCI
otu<-read.table(file="./otu-alpha-env.txt",header = T,sep = "\t",row.names = 1)

otu1<-otu[c(47:94),c(42:57,74)]

#读入分组文件
groups <- read.delim('./group.txt',header = T, stringsAsFactors = F)
colnames(groups)[1:2] <- c('samples','group')#改列名
groups1<-groups[c(49:96),]

df3RCI<-cbind(otu1,groups1)

df3RCI1<-df3RCI[c(7:12,19:48),]
#####RCI细菌-----
p7<-ggplot(df3RCI1, aes(Richness.bac, multinutrient, color = group))+  
  geom_point(size = 2)+  stat_smooth(method = 'lm', formula = 'y~x',level = 0.99)+  
  stat_cor(method = "pearson")+  scale_color_manual(values = c("#206864"))+ 
  theme_bw() +  theme(legend.position ="none")+ labs(x="Richness",y="Nutrient")

p8<-ggplot(df3RCI1, aes(Ace.bac, multinutrient, color = group))+  
  geom_point(size = 2)+  stat_smooth(method = 'lm', formula = 'y~x',level = 0.99)+  
  stat_cor(method = "pearson")+  scale_color_manual(values = c("#206864"))+ 
  theme_bw() +  theme(legend.position ="none")+ labs(x="Ace",y="Nutrient")

p9<-ggplot(df3RCI1, aes(Chao.bac, multinutrient, color = group))+  
  geom_point(size = 2)+  stat_smooth(method = 'lm', formula = 'y~x',level = 0.99)+  
  stat_cor(method = "pearson")+  scale_color_manual(values = c("#206864"))+ 
  theme_bw() +  theme(legend.position ="none")+ labs(x="Chao",y="Nutrient")
######RCIprotist-----

p10<-ggplot(df3RCI, aes(Richness.protist, multinutrient, color = group))+  
  geom_point(size = 2)+  stat_smooth(method = 'lm', formula = 'y~x',level = 0.99)+  
  stat_cor(method = "pearson")+  scale_color_manual(values = c("#206864"))+ 
  theme_bw() +  theme(legend.position ="none")+ labs(x="Richness",y="Nutrient")
p10
p11<-ggplot(df3RCI, aes(Ace.protist, multinutrient, color = group))+  
  geom_point(size = 2)+  stat_smooth(method = 'lm', formula = 'y~x',level = 0.99)+  
  stat_cor(method = "pearson")+  scale_color_manual(values = c("#206864"))+ 
  theme_bw() +  theme(legend.position ="none")+ labs(x="Ace",y="Nutrient")
p11
p12<-ggplot(df3RCI, aes(Chao.protist, multinutrient, color = group))+  
  geom_point(size = 2)+  stat_smooth(method = 'lm', formula = 'y~x',level = 0.99)+  
  stat_cor(method = "pearson")+  scale_color_manual(values = c("#206864"))+ 
  theme_bw() +  theme(legend.position ="none")+ labs(x="Chao",y="Nutrient")
p12

#####水体营养循环指数-----
x<-read.table("D:/data/ciliate/otu-alpha-env.txt",row.names = 1,header = T,sep = "\t")
bc<-x[,c(5:10,12,13)]
library(missForest)
bc<-missForest(bc)
ba<-bc$ximp
ba[ba<=0]<-0
normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) } #定义函数
data.norm <- apply(ba, 2, normalization) #应用apply()函数
ldata4 <- log(data.norm+1) #加1进行变换

be<-scale(ldata4)

write.csv(be,"D:/data/peritrich/扩增子养分循环指数.csv")

#####计算环境因子和物种多样性相关性-----
library(tidyverse)
library(ggcor)
library(ggplot2)
x<-read.table("D:/data/ciliate/otu-alpha-env.txt",row.names = 1,header = T,sep = "\t")
RM<-x[c(95:129),c(1,4,14:74)]

quickcor(RM, cor.test =T)+#计算相关性
  geom_square(data =get_data(type ="lower", show.diag =F))+
  geom_mark(data =get_data(type ="upper", show.diag =F), size =3)+
  guides(fill = guide_legend(title = "p_value", order = 1))+#图例
  geom_mark(r = NA,sig.thres = 0.05, size = 3, colour = "red")


###plspm-----
library(plspm)
library(missForest)
library(ggplot2)
otu<-read.table(file="./otu-alpha-env.txt",header = T,sep = "\t",row.names = 1)

####CM
otuCM<-otu[c(1:46),c(1,4,14:74)]
otuCM<-missForest(otuCM)  #用随机森林迭代弥补缺失值

otuCM<-otuCM$ximp
otuCM <- scale(otuCM)

dat_blocks <- list(MI=c("FeedN",'FeedP','KHSO5','Organic.acid','Bacillus',
                        "Feed","Management","labor"),
                   watertrait = c('WaterTemperature',"pH"),
                   bac=c('Shannon.bac','invSimpson.bac','Richness.bac',
                         'Chao.bac','Ace.bac','obs.bac','Pielou.bac',
                         'Goods_coverage.bac'),
                   arc=c('Shannon.arc','invSimpson.arc','Richness.arc',
                         'Chao.arc','Ace.arc','obs.arc','Pielou.arc',
                         'Goods_coverage.arc'),
                   fungi = c('Shannon.fungi','invSimpson.fungi','Richness.fungi', 
                             'Chao.fungi','Ace.fungi','obs.fungi','Pielou.fungi'),
                   protist= c( 'Shannon.protist','invSimpson.protist','Richness.protist',
                               'Chao.protist','Ace.protist','obs.protist','Pielou.protist',
                               'Goods_coverage.protist'), 
                   metazoa=c('Richness.metazoa','Chao.metazoa',
                             'Ace.metazoa','obs.metazoa','Pielou.metazoa','Goods_coverage.metazoa'),
                   multinutrient='multinutrient')


dat_blocks
#通过 0-1 矩阵描述潜变量之间的关联，其中 0 代表变量间没有关联，1 代表有关联
MI<-c(0,0,0,0,0,0,0,0)
watertrait <- c(0,0,0,0,0,0,0,0)
bac<-c(1,1,0,0,0,0,0,0)
arc<-c(1,1,1,0,0,0,0,0)
fungi <- c(1,1,1,1,0,0,0,0)
protist <- c(1,1,1,1,1,0,0,0)
metazoa<-c(1,1,1,1,1,1,0,0)
multinutrient<-c(1,1,1,1,1,1,1,0)




dat_path <- rbind(MI,watertrait,bac,arc,fungi,protist,metazoa,multinutrient)
colnames(dat_path) <- rownames(dat_path)
dat_path

#指定因果关系，可选 A（代表列是行的因） 或 B（代表行是列的因）
dat_modes <- rep('A',8)
dat_modes

dat_pls <- plspm(otuCM, dat_path, dat_blocks, modes = dat_modes)
dat_pls
summary(dat_pls)

#查看路径系数的参数估计值，以及相关的统计信息
dat_pls$path_coefs
dat_pls$inner_model

#查看因果关系的路径图，详情 ?innerplot
innerplot(dat_pls, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray', box.lwd = 0)

dat_pls$outer_model
outerplot(dat_pls, what = 'loadings', arr.width = 0.1, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray')
outerplot(dat_pls, what = 'weights', arr.width = 0.1, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray')

dat_pls$gof



####RCI
otuRCI<-otu[c(47:94),c(1,4,14:74)]
otuRCI<-missForest(otuRCI)  #用随机森林迭代弥补缺失值

otuRCI<-otuRCI$ximp

dat_blocks <- list(MI=c("Juvenile.crayfish","Animal.health.products",
                        "Feed","Management","labor","Bacillus","Organic.acid",
                        "KHSO5","FeedP","FeedN","FertilizerP","FertilizerN","Total.biomass.of.crayfish",
                        "Shrimp.catch","Aquatic.plants.biomass","Types.of.aquatic.plants",
                        "Rice.period"),
                   watertrait = c('WaterTemperature',"pH"),
                   bac=c('Shannon.bac','invSimpson.bac','Richness.bac',
                         'Chao.bac','Ace.bac','obs.bac','Pielou.bac',
                         'Goods_coverage.bac'),
                   arc=c('Shannon.arc','invSimpson.arc','Richness.arc',
                         'Chao.arc','Ace.arc','obs.arc','Pielou.arc',
                         'Goods_coverage.arc'),
                   fungi = c('Shannon.fungi','invSimpson.fungi','Richness.fungi', 
                             'Chao.fungi','Ace.fungi','obs.fungi','Pielou.fungi',
                             'Goods_coverage.fungi'),
                   protist= c( 'Shannon.protist','invSimpson.protist','Richness.protist',
                               'Chao.protist','Ace.protist','obs.protist','Pielou.protist',
                               'Goods_coverage.protist'), 
                   metazoa=c('Shannon.metazoa','invSimpson.metazoa','Richness.metazoa','Chao.metazoa',
                             'Ace.metazoa','obs.metazoa','Pielou.metazoa','Goods_coverage.metazoa'),
                   multinutrient='multinutrient')

dat_blocks
#通过 0-1 矩阵描述潜变量之间的关联，其中 0 代表变量间没有关联，1 代表有关联
MI<-c(0,0,0,0,0,0,0,0)
watertrait <- c(0,0,0,0,0,0,0,0)
bac<-c(1,1,0,0,0,0,0,0)
arc<-c(1,1,1,0,0,0,0,0)
fungi <- c(1,1,1,1,0,0,0,0)
protist <- c(1,1,1,1,1,0,0,0)
metazoa<-c(1,1,1,1,1,1,0,0)
multinutrient<-c(1,1,1,1,1,1,1,0)




dat_path <- rbind(MI,watertrait,bac,arc,fungi,protist,metazoa,multinutrient)
colnames(dat_path) <- rownames(dat_path)
dat_path

#指定因果关系，可选 A（代表列是行的因） 或 B（代表行是列的因）
dat_modes <- rep('A', 8)
dat_modes

dat_pls1 <- plspm(otuRCI, dat_path, dat_blocks, modes = dat_modes)
dat_pls1
summary(dat_pls1)

#查看路径系数的参数估计值，以及相关的统计信息
dat_pls1$path_coefs
dat_pls1$inner_model

#查看因果关系的路径图，详情 ?innerplot
innerplot(dat_pls1, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray', box.lwd = 0)

dat_pls1$outer_model
outerplot(dat_pls1, what = 'loadings', arr.width = 0.1, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray')
outerplot(dat_pls1, what = 'weights', arr.width = 0.1, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray')

dat_pls1$gof


####RM
otuRM<-otu[c(95:129),c(1,4,14:74)]
otuRM<-missForest(otuRM)  #用随机森林迭代弥补缺失值

otuRM<-otuRM$ximp

dat_blocks <- list(MI=c("Management","labor","FertilizerN","FertilizerP"),
                   watertrait = c('WaterTemperature',"pH"),
                   bac=c('Shannon.bac','invSimpson.bac','Richness.bac',
                         'obs.bac','Pielou.bac',
                         'Goods_coverage.bac'),
                   arc=c('Shannon.arc','invSimpson.arc','Richness.arc',
                         'Chao.arc','obs.arc','Pielou.arc',
                         'Goods_coverage.arc'),
                   fungi = c('Shannon.fungi','invSimpson.fungi','Richness.fungi', 
                             'Chao.fungi','Ace.fungi','obs.fungi','Pielou.fungi'),
                   protist= c( 'Shannon.protist','invSimpson.protist','Richness.protist',
                               'Chao.protist','Ace.protist','obs.protist','Pielou.protist',
                               'Goods_coverage.protist'), 
                   metazoa=c('Shannon.metazoa','invSimpson.metazoa','Richness.metazoa','Chao.metazoa',
                             'Ace.metazoa','obs.metazoa','Pielou.metazoa','Goods_coverage.metazoa'),
                   multinutrient='multinutrient')

dat_blocks
#通过 0-1 矩阵描述潜变量之间的关联，其中 0 代表变量间没有关联，1 代表有关联
MI<-c(0,0,0,0,0,0,0,0)
watertrait <- c(0,0,0,0,0,0,0,0)
bac<-c(1,1,0,0,0,0,0,0)
arc<-c(1,1,1,0,0,0,0,0)
fungi <- c(1,1,1,1,0,0,0,0)
protist <- c(1,1,1,1,1,0,0,0)
metazoa<-c(1,1,1,1,1,1,0,0)
multinutrient<-c(1,1,1,1,1,1,1,0)




dat_path <- rbind(MI,watertrait,bac,arc,fungi,protist,metazoa,multinutrient)
colnames(dat_path) <- rownames(dat_path)
dat_path

#指定因果关系，可选 A（代表列是行的因） 或 B（代表行是列的因）
dat_modes <- rep('A', 8)
dat_modes


dat_pls2 <- plspm(otuRM, dat_path, dat_blocks, modes = dat_modes)
dat_pls2
summary(dat_pls2)

#查看路径系数的参数估计值，以及相关的统计信息
dat_pls2$path_coefs
dat_pls2$inner_model

#查看因果关系的路径图，详情 ?innerplot
innerplot(dat_pls2, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray', box.lwd = 0)

dat_pls2$outer_model
outerplot(dat_pls2, what = 'loadings', arr.width = 0.1, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray')
outerplot(dat_pls2, what = 'weights', arr.width = 0.1, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray')

dat_pls2$gof

library(reshape2)
library(magrittr)
b<-dat_pls$effects
ba<-b[c(7,13,18,22,25,27,28),]
ba1 <- melt(ba, id = 'relationships')

ba1$relationships<-factor(ba1$relationships,
                          levels = ba1$relationships %>% unique())

p1<-ggplot(data=ba1,aes(x=relationships,y=value,fill=variable))+
  geom_bar(stat="identity",position = "dodge")+
  theme_classic()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=60,hjust = 1,vjust = 1),
        legend.title = element_blank())+
  labs(x=NULL,y="Effect(%)")+
  scale_fill_manual(values = c("#936eaa","#401f51",
                               "#de63aa"))+
  scale_y_continuous(labels = function(x){x*100}) -> p1

p1

x<-dat_pls1$effects
ca<-x[c(7,13,18,22,25,27,28),]
ca1 <- melt(ca, id = 'relationships')

ca1$relationships<-factor(ca1$relationships,
                          levels = ca1$relationships %>% unique())

p2<-ggplot(data=ca1,aes(x=relationships,y=value,fill=variable))+
  geom_bar(stat="identity",position = "dodge")+
  theme_classic()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=60,hjust = 1,vjust = 1),
        legend.title = element_blank())+
  labs(x=NULL,y="Effect(%)")+
  scale_fill_manual(values = c("#936eaa","#401f51",
                               "#de63aa"))+
  scale_y_continuous(labels = function(x){x*100}) -> p1

p2

m<-dat_pls2$effects
cc<-m[c(7,13,18,22,25,27,28),]
cc1 <- melt(cc, id = 'relationships')

cc1$relationships<-factor(cc1$relationships,
                          levels = cc1$relationships %>% unique())

p3<-ggplot(data=cc1,aes(x=relationships,y=value,fill=variable))+
  geom_bar(stat="identity",position = "dodge")+
  theme_classic()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=60,hjust = 1,vjust = 1),
        legend.title = element_blank())+
  labs(x=NULL,y="Effect(%)")+
  scale_fill_manual(values = c("#936eaa","#401f51",
                               "#de63aa"))+
  scale_y_continuous(labels = function(x){x*100}) -> p1

p3
library(ggpubr)
ggarrange(p1,p2,p3,nrow = 1,ncol = 3)


#####VPA分析------
library(rdacca.hp)
library(UpSetVP)
library(vegan)
library(ggplot2)
#—————一、CM系统——————————------
x<-read.table(file="F:/RData/2021-Microeukaryotes/Keystone-biodiversity的驱动/CM-keystone abundance.txt",
              header = T,sep = "\t",row.names = 1)
###1. keystone类群对T_Diversity的VPA分析----
spec1<-x[,c(92:131)]#所有多样性指数

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:90)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+BWOTU39+BWOTU41+
              BWOTU72+BWOTU76+
              BWOTU123+BWOTU143+BWOTU156+
              BWOTU234+
              BWOTU251+BWOTU284+BWOTU297+BWOTU302+
              BWOTU310+BWOTU324+BWOTU333+
              BWOTU416+BWOTU462+
              BWOTU628+BWOTU728+BWOTU752+BWOTU1635+BWOTU3480+
              BWOTU5198+PWOTU14+PWOTU26+PWOTU62+PWOTU20+PWOTU183+PWOTU184+
              PWOTU286+PWOTU309+PWOTU325+PWOTU721+PWOTU741+
              PWOTU65+PWOTU66+PWOTU120+PWOTU260+PWOTU298+
              
              PWOTU486+PWOTU747,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)

mite.env<-e3[,c(44,48)]#古菌丰度
mite.env2<-e3[,c(8,9,13,14,17,18,21,22,28,34,36,37,38,42,47,53,56,58)]#细菌丰度
mite.env3<-e3[,c(62,64,66:71)]#真菌丰度
mite.env4<-e3[,c(73,75,78,79,85,87,88,90)]#原生动物丰度
mite.env6<-e3[,c(59:61)]#后生动物

exp.list <- list(Archaea = mite.env, Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)#DCA>4 方法那里用CCA，反之用RDA

CM_Diversity_T <- barplot_hp(mod, col.fill = 'var', 
                             col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
CM_Diversity_T

###2. keystone类群对Archaea多样性的VPA分析----
spec1<-x[,c(92:99)]#Archaea多样性

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:90)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+BWOTU57+BWOTU59+
              BWOTU76+BWOTU77+BWOTU120+BWOTU123+
              BWOTU143+BWOTU156+BWOTU234+
              BWOTU284+BWOTU302+BWOTU310+BWOTU324+
              BWOTU340+BWOTU368+BWOTU416+BWOTU460+
              BWOTU728+BWOTU1635+BWOTU5198+PWOTU14+PWOTU26+
              PWOTU62+PWOTU20+PWOTU183+PWOTU191+PWOTU286+PWOTU309+PWOTU325+PWOTU721+PWOTU741+
              PWOTU65+PWOTU120+PWOTU260+PWOTU298+
              PWOTU432+PWOTU477+PWOTU486+PWOTU747,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)

mite.env<-e3[,c(44,48)]#古菌丰度
mite.env2<-e3[,c(8,9,13,14,17,18,21,22,28,34,36,37,38,42,47,53,56,58)]#细菌丰度
mite.env3<-e3[,c(62,64,66:71)]#真菌丰度
mite.env4<-e3[,c(73,75,78,79,85,87,88,90)]#原生动物丰度
mite.env6<-e3[,c(59:61)]#后生动物

exp.list <- list(Archaea = mite.env, Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)#DCA>4 方法那里用CCA，反之用RDA

CM_Diversity_Archaea <- barplot_hp(mod, col.fill = 'var', 
                                   col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
CM_Diversity_Archaea

###3. keystone类群对Bacteria多样性的VPA分析----
spec1<-x[,c(100:107)]#Bacteria多样性

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:90)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+
              BWOTU57+
              BWOTU123+BWOTU143+BWOTU156+
              BWOTU187+BWOTU229+BWOTU234+
              BWOTU284+BWOTU302+
              BWOTU310+BWOTU324+BWOTU326+BWOTU328+BWOTU333+BWOTU340+
              BWOTU385+BWOTU416+BWOTU484+
              BWOTU628+BWOTU728+BWOTU1635+
              BWOTU5198+PWOTU14+PWOTU26+PWOTU62+PWOTU20+PWOTU183+PWOTU184+
              PWOTU286+PWOTU309+PWOTU325+PWOTU741+
              PWOTU65+PWOTU66+PWOTU120+PWOTU298+
              PWOTU405+PWOTU432+
              PWOTU747,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)

mite.env<-e3[,c(44,48)]#古菌丰度
mite.env2<-e3[,c(8,9,13,14,17,18,21,22,28,34,36,37,38,42,47,53,56,58)]#细菌丰度
mite.env3<-e3[,c(62,64,66:71)]#真菌丰度
mite.env4<-e3[,c(73,75,78,79,85,87,88,90)]#原生动物丰度
mite.env6<-e3[,c(59:61)]#后生动物

exp.list <- list(Archaea = mite.env, Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)#DCA>4 方法那里用CCA，反之用RDA

CM_Diversity_Bacteria <- barplot_hp(mod, col.fill = 'var', 
                                    col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
CM_Diversity_Bacteria

###4. keystone类群对Protist多样性的VPA分析----
spec1<-x[,c(108:115)]#Protist多样性

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:90)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+BWOTU35+BWOTU39+BWOTU41+
              BWOTU57+BWOTU76+BWOTU77+
              BWOTU120+BWOTU123+BWOTU132+BWOTU143+BWOTU156+
              BWOTU213+BWOTU234+
              BWOTU251+BWOTU260+BWOTU277+BWOTU302+
              BWOTU310+BWOTU326+BWOTU340+
              BWOTU368+BWOTU416+
              BWOTU628+BWOTU1223+BWOTU1635+
              BWOTU5198+PWOTU14+PWOTU20+PWOTU183+
              PWOTU286+PWOTU325+PWOTU741+PWOTU9+
              PWOTU65+PWOTU66+PWOTU120+PWOTU260+
              PWOTU335+PWOTU413+PWOTU432+
              PWOTU747,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)

mite.env<-e3[,c(44,48)]#古菌丰度
mite.env2<-e3[,c(8,9,13,14,17,18,21,22,28,34,36,37,38,42,47,53,56,58)]#细菌丰度
mite.env3<-e3[,c(62,64,66:71)]#真菌丰度
mite.env4<-e3[,c(73,75,78,79,85,87,88,90)]#原生动物丰度
mite.env6<-e3[,c(59:61)]#后生动物

exp.list <- list(Archaea = mite.env, Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)#DCA>4 方法那里用CCA，反之用RDA

CM_Diversity_Protist <- barplot_hp(mod, col.fill = 'var', 
                                   col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
CM_Diversity_Protist

##5. keystone类群对Fungi多样性的VPA分析----
spec1<-x[,c(116:123)]#Fungi多样性

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:90)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+BWOTU41+
              BWOTU57+
              BWOTU120+BWOTU123+BWOTU127+BWOTU143+BWOTU156+
              BWOTU187+BWOTU229+
              BWOTU251+BWOTU302+
              BWOTU310+BWOTU326+BWOTU328+
              BWOTU385+BWOTU407+BWOTU416+BWOTU460+
              BWOTU628+BWOTU728+BWOTU1223+BWOTU1635+
              BWOTU5198+PWOTU14+PWOTU62+PWOTU20+PWOTU183+
              PWOTU191+PWOTU286+PWOTU309+PWOTU325+PWOTU741+
              PWOTU65+PWOTU66+PWOTU120+PWOTU238+PWOTU298+
              PWOTU300+PWOTU432+
              PWOTU747,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)

mite.env<-e3[,c(44,48)]#古菌丰度
mite.env2<-e3[,c(8,9,13,14,17,18,21,22,28,34,36,37,38,42,47,53,56,58)]#细菌丰度
mite.env3<-e3[,c(62,64,66:71)]#真菌丰度
mite.env4<-e3[,c(73,75,78,79,85,87,88,90)]#原生动物丰度
mite.env6<-e3[,c(59:61)]#后生动物

exp.list <- list(Archaea = mite.env, Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)#DCA>4 方法那里用CCA，反之用RDA

CM_Diversity_Fungi <- barplot_hp(mod, col.fill = 'var', 
                                 col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
CM_Diversity_Fungi


##6. keystone类群对Metazoa多样性的VPA分析----
spec1<-x[,c(124:131)]#Metazoa多样性

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:90)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+BWOTU39+BWOTU41+
              BWOTU57+BWOTU59+BWOTU73+
              BWOTU108+BWOTU127+BWOTU132+BWOTU143+BWOTU156+
              BWOTU213+BWOTU229+BWOTU234+
              BWOTU284+BWOTU302+
              BWOTU310+BWOTU340+BWOTU355+
              BWOTU416+BWOTU462+
              BWOTU628+BWOTU1635+
              BWOTU5198+PWOTU14+PWOTU26+PWOTU96+PWOTU183+PWOTU184+
              PWOTU191+PWOTU286+PWOTU325+PWOTU741+
              PWOTU65+PWOTU66+PWOTU120+PWOTU128+PWOTU260+
              PWOTU432+
              PWOTU747,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)

mite.env<-e3[,c(44,48)]#古菌丰度
mite.env2<-e3[,c(8,9,13,14,17,18,21,22,28,34,36,37,38,42,47,53,56,58)]#细菌丰度
mite.env3<-e3[,c(62,64,66:71)]#真菌丰度
mite.env4<-e3[,c(73,75,78,79,85,87,88,90)]#原生动物丰度
mite.env6<-e3[,c(59:61)]#后生动物

exp.list <- list(Archaea = mite.env, Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)#DCA>4 方法那里用CCA，反之用RDA

CM_Diversity_Metazoa <- barplot_hp(mod, col.fill = 'var', 
                                   col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
CM_Diversity_Metazoa



#排列图版----
library(ggpubr)
VPA_CM_keystone_diversity<-ggarrange(CM_Diversity_T,CM_Diversity_Archaea,CM_Diversity_Bacteria,CM_Diversity_Protist,
                                     CM_Diversity_Fungi,CM_Diversity_Metazoa,
                                     ncol=3,nrow=2)
VPA_CM_keystone_diversity
setwd("F:/RData/2021-Microeukaryotes/Keystone-biodiversity的驱动/VPA分析")
getwd()
ggsave("VPA_CM_keystone_diversity.pdf", VPA_CM_keystone_diversity,width =10,height = 6)


#—————二、RCI系统——————————------
x<-read.table(file="F:/RData/2021-Microeukaryotes/Keystone-biodiversity的驱动/RCI-keystone abundance.txt",
              header = T,sep = "\t",row.names = 1)
###1. keystone类群对T_Diversity的VPA分析----
spec1<-x[,c(17:56)]#所有多样性指数

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:15)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+fun_OTU19+fun_OTU98+fun_OTU333+fun_OTU986+
              fun_OTU109+fun_OTU71+fun_OTU120+fun_OTU161+bac_OTU67+
              bac_OTU119+bac_OTU203+bac_OTU291+bac_OTU37+bac_OTU211+
              bac_OTU336,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)


mite.env2<-e3[,c(9:15)]#细菌丰度
mite.env3<-e3[,c(8)]#真菌丰度
mite.env4<-e3[,c(1:3,5:7)]#原生动物丰度
mite.env6<-e3[,c(4)]#后生动物
mite.env3<-as.data.frame(mite.env3)
mite.env6<-as.data.frame(mite.env6)
exp.list <- list(Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)
#DCA>4 方法那里用CCA，反之用RDA

RCI_Diversity_T <- barplot_hp(mod, col.fill = 'var', 
                              col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
RCI_Diversity_T

###2. keystone类群对Archaea的VPA分析----
spec1<-x[,c(17:24)]#Archaea多样性指数

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:15)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+fun_OTU19+fun_OTU98+fun_OTU333+fun_OTU986+
              fun_OTU109+fun_OTU71+fun_OTU120+fun_OTU161+bac_OTU67+
              bac_OTU119+bac_OTU203+bac_OTU291+bac_OTU37+bac_OTU211+
              bac_OTU336,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)


mite.env2<-e3[,c(9:15)]#细菌丰度
mite.env3<-e3[,c(8)]#真菌丰度
mite.env4<-e3[,c(1:3,5:7)]#原生动物丰度
mite.env6<-e3[,c(4)]#后生动物
mite.env3<-as.data.frame(mite.env3)
mite.env6<-as.data.frame(mite.env6)
exp.list <- list(Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)
#DCA>4 方法那里用CCA，反之用RDA

RCI_Diversity_Archaea <- barplot_hp(mod, col.fill = 'var', 
                                    col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
RCI_Diversity_Archaea

###3. keystone类群对Bacteria的VPA分析----
spec1<-x[,c(25:32)]#Bacteria多样性指数

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:15)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+fun_OTU19+fun_OTU98+fun_OTU333+fun_OTU986+
              fun_OTU109+fun_OTU71+fun_OTU120+fun_OTU161+bac_OTU67+
              bac_OTU119+bac_OTU203+bac_OTU291+bac_OTU37+bac_OTU211+
              bac_OTU336,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)


mite.env2<-e3[,c(9:15)]#细菌丰度
mite.env3<-e3[,c(8)]#真菌丰度
mite.env4<-e3[,c(1:3,5:7)]#原生动物丰度
mite.env6<-e3[,c(4)]#后生动物
mite.env3<-as.data.frame(mite.env3)
mite.env6<-as.data.frame(mite.env6)
exp.list <- list(Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)
#DCA>4 方法那里用CCA，反之用RDA

RCI_Diversity_Bacteria <- barplot_hp(mod, col.fill = 'var', 
                                     col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
RCI_Diversity_Bacteria


###4. keystone类群对Protist的VPA分析----
spec1<-x[,c(33:40)]#Protist多样性指数

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:15)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+fun_OTU19+fun_OTU98+fun_OTU333+fun_OTU986+
              fun_OTU109+fun_OTU71+fun_OTU120+fun_OTU161+bac_OTU67+
              bac_OTU119+bac_OTU203+bac_OTU291+bac_OTU37+bac_OTU211+
              bac_OTU336,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)


mite.env2<-e3[,c(9:15)]#细菌丰度
mite.env3<-e3[,c(8)]#真菌丰度
mite.env4<-e3[,c(1:3,5:7)]#原生动物丰度
mite.env6<-e3[,c(4)]#后生动物
mite.env3<-as.data.frame(mite.env3)
mite.env6<-as.data.frame(mite.env6)
exp.list <- list(Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)
#DCA>4 方法那里用CCA，反之用RDA

RCI_Diversity_Protist <- barplot_hp(mod, col.fill = 'var', 
                                    col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
RCI_Diversity_Protist



###5. keystone类群对Fungi的VPA分析----
spec1<-x[,c(41:48)]#Fungi多样性指数

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:15)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+fun_OTU19+fun_OTU98+fun_OTU333+fun_OTU986+
              fun_OTU109+fun_OTU71+fun_OTU120+fun_OTU161+bac_OTU67+
              bac_OTU119+bac_OTU203+bac_OTU291+bac_OTU37+bac_OTU211+
              bac_OTU336,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)


mite.env2<-e3[,c(9:15)]#细菌丰度
mite.env3<-e3[,c(8)]#真菌丰度
mite.env4<-e3[,c(1:3,5:7)]#原生动物丰度
mite.env6<-e3[,c(4)]#后生动物
mite.env3<-as.data.frame(mite.env3)
mite.env6<-as.data.frame(mite.env6)
exp.list <- list(Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)
#DCA>4 方法那里用CCA，反之用RDA

RCI_Diversity_Fungi <- barplot_hp(mod, col.fill = 'var', 
                                  col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
RCI_Diversity_Fungi



###6. keystone类群对Metazoa的VPA分析----
spec1<-x[,c(49:56)]#Metazoa多样性指数

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:15)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+fun_OTU19+fun_OTU98+fun_OTU333+fun_OTU986+
              fun_OTU109+fun_OTU71+fun_OTU120+fun_OTU161+bac_OTU67+
              bac_OTU119+bac_OTU203+bac_OTU291+bac_OTU37+bac_OTU211+
              bac_OTU336,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)


mite.env2<-e3[,c(9:15)]#细菌丰度
mite.env3<-e3[,c(8)]#真菌丰度
mite.env4<-e3[,c(1:3,5:7)]#原生动物丰度
mite.env6<-e3[,c(4)]#后生动物
mite.env3<-as.data.frame(mite.env3)
mite.env6<-as.data.frame(mite.env6)
exp.list <- list(Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)
#DCA>4 方法那里用CCA，反之用RDA

RCI_Diversity_Metazoa <- barplot_hp(mod, col.fill = 'var', 
                                    col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
RCI_Diversity_Metazoa

#排列图版----
library(ggpubr)
VPA_RCI_keystone_diversity<-ggarrange(RCI_Diversity_T,RCI_Diversity_Archaea,RCI_Diversity_Bacteria,RCI_Diversity_Protist,
                                      RCI_Diversity_Fungi,RCI_Diversity_Metazoa,
                                      ncol=3,nrow=2)
VPA_RCI_keystone_diversity
setwd("F:/RData/2021-Microeukaryotes/Keystone-biodiversity的驱动/VPA分析")
getwd()
ggsave("VPA_RCI_keystone_diversity.pdf", VPA_RCI_keystone_diversity,width =10,height = 6)


#—————三、RM系统——————————------
x<-read.table(file="F:/RData/2021-Microeukaryotes/Keystone-biodiversity的驱动/RM-keystone abundance.txt",
              header = T,sep = "\t",row.names = 1)
###1. keystone类群对T_Diversity的VPA分析----
spec1<-x[,c(26:65)]#所有多样性指数

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:24)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+BWOTU18+BWOTU39+BWOTU152+BWOTU231+BWOTU347+
              BWOTU351+BWOTU395+BWOTU451+BWOTU807+PWOTU32+
              PWOTU171+PWOTU515+PWOTU29+
              PWOTU35+PWOTU106+PWOTU150+PWOTU227+PWOTU442,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)


mite.env2<-e3[,c(1:10)]#细菌丰度
mite.env3<-e3[,c(13:14)]#真菌丰度
mite.env4<-e3[,c(15:24)]#原生动物丰度
mite.env6<-e3[,c(11:12)]#后生动物
#mite.env3<-as.data.frame(mite.env3)
#mite.env6<-as.data.frame(mite.env6)
exp.list <- list(Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)
#DCA>4 方法那里用CCA，反之用RDA

RM_Diversity_T <- barplot_hp(mod, col.fill = 'var', 
                             col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
RM_Diversity_T


###2. keystone类群对Archaea的VPA分析----
spec1<-x[,c(26:33)]#Archaea多样性指数

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:24)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+BWOTU18+BWOTU39+BWOTU152+BWOTU231+BWOTU347+
              BWOTU351+BWOTU395+BWOTU451+BWOTU807+PWOTU32+
              PWOTU171+PWOTU515+PWOTU29+
              PWOTU35+PWOTU92+PWOTU106+PWOTU227+PWOTU442,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)


mite.env2<-e3[,c(1:10)]#细菌丰度
mite.env3<-e3[,c(13:14)]#真菌丰度
mite.env4<-e3[,c(15:24)]#原生动物丰度
mite.env6<-e3[,c(11:12)]#后生动物
#mite.env3<-as.data.frame(mite.env3)
#mite.env6<-as.data.frame(mite.env6)
exp.list <- list(Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)
#DCA>4 方法那里用CCA，反之用RDA

RM_Diversity_Archaea <- barplot_hp(mod, col.fill = 'var', 
                                   col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
RM_Diversity_Archaea


###3. keystone类群对Bacteria的VPA分析----
spec1<-x[,c(34:41)]#Bacteria多样性指数

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:24)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+BWOTU18+BWOTU39+BWOTU152+BWOTU231+BWOTU347+
              BWOTU351+BWOTU395+BWOTU451+BWOTU464+BWOTU807+PWOTU32+
              PWOTU171+PWOTU515+PWOTU34+
              PWOTU106+PWOTU130+PWOTU150+PWOTU227+PWOTU442,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)


mite.env2<-e3[,c(1:10)]#细菌丰度
mite.env3<-e3[,c(13:14)]#真菌丰度
mite.env4<-e3[,c(15:24)]#原生动物丰度
mite.env6<-e3[,c(11:12)]#后生动物
#mite.env3<-as.data.frame(mite.env3)
#mite.env6<-as.data.frame(mite.env6)
exp.list <- list(Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)
#DCA>4 方法那里用CCA，反之用RDA

RM_Diversity_Bacteria <- barplot_hp(mod, col.fill = 'var', 
                                    col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
RM_Diversity_Bacteria

###4. keystone类群对Protist的VPA分析----
spec1<-x[,c(42:49)]#Protist多样性指数

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:24)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+BWOTU18+BWOTU39+BWOTU152+BWOTU231+BWOTU347+
              BWOTU351+BWOTU395+BWOTU451+BWOTU464+BWOTU807+PWOTU32+
              PWOTU171+PWOTU515+PWOTU29+PWOTU34+
              PWOTU92+PWOTU106+PWOTU227+PWOTU442,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)


mite.env2<-e3[,c(1:10)]#细菌丰度
mite.env3<-e3[,c(13:14)]#真菌丰度
mite.env4<-e3[,c(15:24)]#原生动物丰度
mite.env6<-e3[,c(11:12)]#后生动物
#mite.env3<-as.data.frame(mite.env3)
#mite.env6<-as.data.frame(mite.env6)
exp.list <- list(Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)
#DCA>4 方法那里用CCA，反之用RDA

RM_Diversity_Protist <- barplot_hp(mod, col.fill = 'var', 
                                   col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
RM_Diversity_Protist


###5. keystone类群对Fungi的VPA分析----
spec1<-x[,c(50:57)]#Fungi多样性指数

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:24)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+BWOTU18+BWOTU39+BWOTU152+BWOTU231+BWOTU347+
              BWOTU351+BWOTU395+BWOTU451+BWOTU807+PWOTU32+
              PWOTU171+PWOTU515+PWOTU34+
              PWOTU106+PWOTU130+PWOTU150+PWOTU227+PWOTU442,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)


mite.env2<-e3[,c(1:10)]#细菌丰度
mite.env3<-e3[,c(13:14)]#真菌丰度
mite.env4<-e3[,c(15:24)]#原生动物丰度
mite.env6<-e3[,c(11:12)]#后生动物
#mite.env3<-as.data.frame(mite.env3)
#mite.env6<-as.data.frame(mite.env6)
exp.list <- list(Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)
#DCA>4 方法那里用CCA，反之用RDA

RM_Diversity_Fungi <- barplot_hp(mod, col.fill = 'var', 
                                 col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
RM_Diversity_Fungi

###5. keystone类群对Metazoa的VPA分析----
spec1<-x[,c(58:65)]#Metazoa多样性指数

library(missForest)

spec<-missForest(spec1)
spec<-spec$ximp

spec2<- decostand(spec,"hellinger")#物种数据进行hellinger转化

spec3=spec2[which(rowSums(spec2) > 0),]


env<-x[,c(1:24)]#keystone丰度

normalization <- function(x) { return((x - min(x)) / (max(x) - min(x))) }###0-1标准化
e1 <- apply(env,2 ,normalization) #应用apply()函数

e3<-as.data.frame(e1)



DCA <- decorana(spec3, ira = 0) 
DCA
mod1 <- cca(spec3~+BWOTU18+BWOTU39+BWOTU152+BWOTU231+BWOTU347+
              BWOTU351+BWOTU395+BWOTU451+BWOTU807+PWOTU32+
              PWOTU171+PWOTU515+PWOTU29+PWOTU34+
              PWOTU106+PWOTU150+PWOTU227+PWOTU442,e3)
#计算共线性，手动剔除VIF>10的因子，有NA不要着急挑，先挑数值大的，挑几个就重复跑这行代码，直到因子数值在10附近
vif.cca(mod1)


mite.env2<-e3[,c(1:10)]#细菌丰度
mite.env3<-e3[,c(13:14)]#真菌丰度
mite.env4<-e3[,c(15:24)]#原生动物丰度
mite.env6<-e3[,c(11:12)]#后生动物
#mite.env3<-as.data.frame(mite.env3)
#mite.env6<-as.data.frame(mite.env6)
exp.list <- list(Bacteria = mite.env2,Fungi = mite.env3,Protist = mite.env4, Metazoa = mite.env6) 

mod <- rdacca.hp(spec2,exp.list, method = 'RDA',var.part = TRUE,type = 'adjR2', scale = FALSE)
#DCA>4 方法那里用CCA，反之用RDA

RM_Diversity_Metazoa <- barplot_hp(mod, col.fill = 'var', 
                                   col.color = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3'))
RM_Diversity_Metazoa
#排列图版----
library(ggpubr)
VPA_RM_keystone_diversity<-ggarrange(RM_Diversity_T,RM_Diversity_Archaea,RM_Diversity_Bacteria,RM_Diversity_Protist,
                                     RM_Diversity_Fungi,RM_Diversity_Metazoa,
                                     ncol=3,nrow=2)
VPA_RM_keystone_diversity
setwd("F:/RData/2021-Microeukaryotes/Keystone-biodiversity的驱动/VPA分析")
getwd()
ggsave("VPA_RM_keystone_diversity.pdf", VPA_RM_keystone_diversity,width =10,height = 6)

