
library(car)
library(agricolae)
library(ggsci)
library(tidyverse)
data<-read.csv("Fig3a.csv",header = TRUE)
nom <- bartlett.test(BB~Treat,data = data)
nom
nom1<-leveneTest(BB~Treat,data = data)
nom$p.value
oneway<-aov(BB~Treat,data = data)
anova(oneway)
out <- LSD.test(oneway,"Treat",p.adj="none")
out
mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$BB,mar$groups)
sort<-newmar[order(newmar$rownamemar),]
rowname<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
marker<-sort$mar.groups
plotdata<-data.frame(rowname,mean,sd,marker)
plotdata$se<-plotdata$sd/sqrt(8)
plotdata
Treatment<-read.csv("Treatment.csv",header = TRUE,row.names = 1)
plotdata<-left_join(plotdata,Treatment)
plotdata
plotdata$Treatment <- factor(plotdata$Treatment,c("ST","RT","RB","RF","MN","MB","MF"),labels = c("ST","RT","RB","RF","MN","MB","MF"))
plotdata$Time<-factor(plotdata$Time,c("2","3","4"),labels = c("2","3","4"))
plotdata
Biomass<-ggplot(plotdata,aes(Treatment,mean,fill=Treatment))+geom_bar(position=position_dodge(0.6),width = 0.5,stat = "identity")+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),position=position_dodge(0.6),width=0.2)+
  geom_point(data = data,
             aes(x = Treatment, y = BB),
             inherit.aes = FALSE,
             position = position_jitter(width = 0.12, height = 0),
             size = 2.2, alpha = 0.55, shape = 16) +
  facet_grid(.~Time,scales = "free")+
  #geom_text(aes(x=factor(Treatment),y=mean+sd+1.2,label=marker),size=6,position= position_dodge(0.6))+
  theme_bw() +
  theme(axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 32,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 16,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size = 24),
                                                  strip.text.y = element_text(size = 10),
                                                  strip.background =
                                                    element_rect(fill = "gray"))+
  ylab("Aboveground biomass (g/pot)")+xlab("")+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+theme(legend.position = "none")+ylim(0,6.5)+scale_fill_npg()

Biomass

library(ggplot2)
library(ggsci)
feedback<-read.csv("Fig3b.csv",header = TRUE,row.names = 1)
feedback$Treatment<-factor(feedback$Treatment,levels = c("MN","MB","MF"))
#feedback<-subset(feedback,feedback$Time=="3")
oneway<-aov(Feedback~Treat,data =feedback)
anova(oneway)
out <- LSD.test(oneway,"Treat",p.adj="none")
out

p<-ggplot(feedback,aes(x=Treatment,y=Feedback,color=Treatment))+facet_wrap(~Time)+
  geom_boxplot(size=1)+
  geom_jitter(shape=16, position = position_jitter(0.2))+
  theme_bw(base_size=12)+ylab("Feedback effect\n")+
  theme(panel.grid=element_blank())+theme(legend.position="none")+
  geom_hline(yintercept=0,colour="black")+
  theme_bw() +
  theme(axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 32,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 16,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size = 24),
                                                  strip.text.y = element_text(size = 10),
                                                  strip.background =
                                                    element_rect(fill = "gray"))+
  xlab("")+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+theme(legend.position = "none")+scale_color_npg()+ylim(-0.8,0.5)
p


library(cowplot)
c<-plot_grid(Biomass,p,labels = "AUTO",ncol=1,label_size = 32)
c
ggsave("Fig3.pdf",c,height = 15,width = 15)



data<-read.csv("Fig4-5.csv",header = TRUE,row.names = 1)
library(ggpubr)
library(ggplot2)
library(ggsci)
data$Generation<- factor(data$Generation, levels = c('2','3', "4"))
a<-ggplot(data,aes(Tbbray,bbiomass,group=Generation,color=Generation))+
  geom_point(size = 4, alpha = 1)+
  stat_smooth(method="lm")+
  #facet_wrap(.~Time,scales = "free")+
  theme_bw()+
  #stat_cor(data=data,method = "pearson",size=9)+
  theme(axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 24,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 24,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size = 24),
                                                  strip.text.y = element_text(size = 10),
                                                  strip.background =
                                                    element_rect(fill = "gray"))+
  ylab("Wheat biomass feedback effect")+xlab("Contemporary bacterial dissimilarity")+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+scale_color_npg()
a


b<-ggplot(data,aes(TFbray,bbiomass,group=Generation,color=Generation))+
  geom_point(size = 4, alpha = 1)+
  stat_smooth(method="lm")+
  #facet_wrap(.~Time,scales = "free")+
  theme_bw()+
  #stat_cor(data=data,method = "pearson",size=9)+
  theme(axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 24,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 24,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size = 24),
                                                  strip.text.y = element_text(size = 10),
                                                  strip.background =
                                                    element_rect(fill = "gray"))+
  ylab("Wheat biomass feedback effect")+xlab("Contemporary fungal dissimilarity")+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+scale_fill_aaas()+scale_color_npg()
b




c<-ggplot(data,aes(PTBbray,bbiomass,group=Generation,color=Generation))+
  geom_point(size = 4, alpha = 1)+
  stat_smooth(method="lm")+
  #facet_wrap(.~Time,scales = "free")+
  theme_bw()+
  #stat_cor(data=data,method = "pearson",size=9) +
  theme(axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 24,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 24,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size = 24),
                                                  strip.text.y = element_text(size = 10),
                                                  strip.background =
                                                    element_rect(fill = "gray"))+
  ylab("Wheat biomass feedback effect")+xlab("Previous bacterial dissimilarity")+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+scale_fill_aaas()+scale_color_npg()
c


d<-ggplot(data,aes(PTFbray,bbiomass,group=Generation,color=Generation))+
  geom_point(size = 4, alpha = 1)+
  stat_smooth(method="lm")+
  #facet_wrap(.~Time,scales = "free")+
  theme_bw()+
  #stat_cor(data=data,method = "pearson",size=9)  +
  theme(axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 24,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 24,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size = 24),
                                                  strip.text.y = element_text(size = 10),
                                                  strip.background =
                                                    element_rect(fill = "gray"))+
  ylab("Wheat biomass feedback effect")+xlab("Previous fungal dissimilarity")+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+scale_fill_aaas()+scale_color_npg()
d

e<-ggplot(data,aes(sbcore,bbiomass,group=Generation,color=Generation))+
  geom_point(size = 4, alpha = 1)+
  stat_smooth(method="lm")+
  #facet_wrap(.~Generation,scales = "free")+
  #stat_cor(data=data,method = "pearson",size=9)+
  theme_bw()+
  theme(axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 24,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 24,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size = 24),
                                                  strip.text.y = element_text(size = 10),
                                                  strip.background =
                                                    element_rect(fill = "gray"))+
  ylab("Wheat biomass feedback effect")+xlab("Contemporary core bacterial dissimilarity")+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+scale_fill_aaas()+scale_color_npg()

e

f<-ggplot(data,aes(snbcore,bbiomass,group=Generation,color=Generation))+
  geom_point(size = 4, alpha = 1)+
  #stat_cor(data=data,method = "pearson",size=9)+
  stat_smooth(method="lm")+
  #facet_wrap(.~Generation,scales = "free")+
  theme_bw()+
  theme(axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 24,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 24,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size = 24),
                                                  strip.text.y = element_text(size = 10),
                                                  strip.background =
                                                    element_rect(fill = "gray"))+
  ylab("Wheat biomass feedback effect")+xlab("Contemporary divergent bacterial dissimilarity")+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+scale_fill_aaas()+scale_color_npg()
f
g<-ggplot(data,aes(pfcore,bbiomass,group=Generation,color=Generation))+
  geom_point(size = 4, alpha = 1)+
  stat_smooth(method="lm")+
  #stat_cor(data=data,method = "pearson",size=9)+
  #facet_wrap(.~Generation,scales = "free")+
  theme_bw()+
  theme(axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 24,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 24,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size = 24),
                                                  strip.text.y = element_text(size = 10),
                                                  strip.background =
                                                    element_rect(fill = "gray"))+
  ylab("Wheat biomass feedback effect")+xlab("Previous core fungal dissimilarity")+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+scale_fill_aaas()+scale_color_npg()
g


h<-ggplot(data,aes(pnfcore,bbiomass,group=Generation,color=Generation))+
  geom_point(size = 4, alpha = 1)+
  #stat_cor(data=data,method = "pearson",size=9)+
  stat_smooth(method="lm")+
  #facet_wrap(.~Generation,scales = "free")+
  theme_bw()+
  theme(axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 24,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 24,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size = 24),
                                                  strip.text.y = element_text(size = 10),
                                                  strip.background =
                                                    element_rect(fill = "gray"))+
  ylab("Wheat biomass feedback effect")+xlab("Previous divergent fungal dissimilarity")+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+scale_fill_aaas()+scale_color_npg()
h


total<-ggarrange(d,b,c,a,labels = c("(a)", "(b)","(c)","(d)","(e)","(f)","(g)","(h)"),
                 ncol =3, nrow = 2,
                 font.label = list(size = 32, face = "bold"),common.legend = TRUE)
total

ggsave("Fig4.pdf",total,height = 12,width =18)

total<-ggarrange(g,h,e,f,labels = c("(a)", "(b)","(c)","(d)","(e)","(f)","(g)","(h)"),
                 ncol = 3, nrow = 2,
                 font.label = list(size = 32, face = "bold"),common.legend = TRUE)
total
ggsave("Fig5.pdf",total,height = 12,width =18)


library(car)
library(agricolae)
library(ggsci)
library(tidyverse)F
data<-read.csv("Fig6a.csv",header = TRUE)
nom <- bartlett.test(average_genome_size~Treat,data = data)
nom
nom1<-leveneTest(average_genome_size~Treat,data = data)
nom$p.value
oneway<-aov(average_genome_size~Treat,data = data)
anova(oneway)
out <- LSD.test(oneway,"Treat",p.adj="none")
out
mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$average_genome_size,mar$groups)
sort<-newmar[order(newmar$rownamemar),]
rowname<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
marker<-sort$mar.groups
plotdata<-data.frame(rowname,mean,sd,marker)
plotdata$se<-plotdata$sd/sqrt(4)
plotdata
Treatment<-read.csv("Treatment.csv",header = TRUE,row.names = 1)
plotdata<-left_join(plotdata,Treatment)
plotdata
plotdata$Treatment <- factor(plotdata$Treatment,c("Rotation","Monoculture"),labels = c("Rotation","Monoculture"))
plotdata$Generation<-factor(plotdata$Generation,c("Phase 2","Phase 3","Phase 4"),labels = c("Phase 2","Phase 3","Phase 4"))
plotdata
data$Treatment <- factor(data$Treatment,c("Rotation","Monoculture"),labels = c("Rotation","Monoculture"))
data$Generation<-factor(data$Generation,c("Phase 2","Phase 3","Phase 4"),labels = c("Phase 2","Phase 3","Phase 4"))
a<-ggplot(plotdata,aes(Generation,mean,Group=Treatment,fill=Treatment))+
  geom_bar(position=position_dodge(0.6),width = 0.5,stat = "identity")+
  geom_errorbar(aes(Generation,ymin=mean-se,ymax=mean+se),position=position_dodge(0.6),width=0.2)+
  #geom_text(aes(GC,label =marker, y = mean + sd), vjust=-0.4,position=position_dodge(0.9),size=6)+
  theme_bw() +
  geom_point(data = data,
             aes(Generation, average_genome_size, group = Treatment),
             position = position_jitterdodge(jitter.width = 0.10,
                                             dodge.width  = 0.6),
             size = 2.2, alpha = 0.55, shape = 16) +
  theme(axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 32,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 16,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size = 24),
                                                  strip.text.y = element_text(size = 10),
                                                  strip.background =
                                                    element_rect(fill = "gray"))+
  ylab("Average Genome Size (Mbp)")+xlab("")+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+scale_fill_npg()
a




data<-read.csv("Fig6b.csv",header = TRUE,row.names = 1)
library(ggpubr)
library(ggplot2)
library(ggsci)
data$Generation<- factor(data$Generation, levels = c('2','3', "4"))

b<-ggplot(data,aes(Geom,feedback,group=Generation,color=Generation))+
  geom_point(size = 4, alpha = 1)+
  stat_smooth(method="lm")+
  theme_bw()+
  theme(axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 32,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 32,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 24),
        strip.text.y = element_text(size = 10),
        strip.background =
          element_rect(fill = "gray"))+
  ylab("Wheat biomass feedback effect")+
  xlab("Geome size ration")+
  stat_cor(data=data,method = "pearson",size=9)+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+
  scale_fill_npg()+scale_color_npg()
b


emf<-read.csv("Fig6c.csv",header=TRUE,row.names = 1)
library(ggplot2)
library(ggpubr)
library(ggsci)
emf$Treatment <- factor(emf$Treatment,c("Rotation","Monoculture"),labels = c("Rotation","Monoculture"))
emf$Time<-factor(emf$Generation,c("Phase 2","Phase 3","Phase 4"),labels = c("Phase 2","Phase 3","Phase 4"))
emf
c<-ggplot(emf,aes(average_genome_size,rl,group=Group,color=Group))+
  geom_point(aes(color=Group),size = 4)+
  stat_smooth(method="lm")+
  xlab("Average genome size (Mbp)") +theme_bw()+theme(panel.grid=element_blank())+
  ylab("Relative abundance (%)")+
  stat_cor(data=emf, method = "pearson",size=9)+theme(text = element_text(size = 24)) +
  theme(axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 32,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 32,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 24),
        strip.text.y = element_text(size = 10),
        strip.background =
          element_rect(fill = "gray"))+scale_color_npg()+
  theme(legend.position = "none")
c
library(car)
library(agricolae)
library(ggsci)
library(tidyverse)
data<-read.csv("fig6ef.csv",header = TRUE)
nom <- bartlett.test(Biomass~Treatment,data = data)
nom
nom1<-leveneTest(Biomass~Treatment,data = data)
nom$p.value
oneway<-aov(Biomass~Treatment,data = data)
anova(oneway)
out <- LSD.test(oneway,"Treat",p.adj="none")
out
mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$Biomass,mar$groups)
sort<-newmar[order(newmar$rownamemar),]
rowname<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
marker<-sort$mar.groups
plotdata<-data.frame(rowname,mean,sd,marker)
plotdata$se<-plotdata$sd/sqrt(4)
plotdata
plotdata$rowname <- factor(plotdata$rowname ,c("Control","Dv","Cb","Cl","Pf"),labels = c("Control","Dv","Cb","Cl","Pf"))
data$Treatment<-factor(data$Treatment ,c("Control","Dv","Cb","Cl","Pf"),labels = c("Control","Dv","Cb","Cl","Pf"))
plotdata
ab<-ggplot(plotdata,aes(rowname,mean,Group=rowname,fill=rowname))+
  geom_bar(position=position_dodge(0.6),width = 0.5,stat = "identity")+
  geom_errorbar(aes(rowname,ymin=mean-se,ymax=mean+se),position=position_dodge(0.6),width=0.2)+
  geom_text(aes(rowname,label =marker, y = mean + se), vjust=-0.4,position=position_dodge(0.9),size=6)+
  geom_point(data = data,
             aes(x = Treatment, y = Biomass),
             inherit.aes = FALSE,
             position = position_jitter(width = 0.12, height = 0),
             size = 2.2, alpha = 0.55, shape = 16) +
  theme_bw() +
  #facet_grid(.~Treatment,scales = "free")+
  theme(axis.text.x=element_text(colour="black",size=20,angle = 45,vjust = 0.7), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 32,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 16,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size = 24),
                                                  strip.text.y = element_text(size = 10),
                                                  strip.background =
                                                    element_rect(fill = "gray"))+
  ylab("Aboveground biomass(g/pot)")+xlab("")+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+scale_fill_npg()+
  theme(legend.position = "none")
ab


data<-read.csv("fig6ef.csv.csv",header = TRUE)
nom <- bartlett.test(Plant_height~Treatment,data = data)
nom
nom1<-leveneTest(Plant_height~Treatment,data = data)
nom$p.value
oneway<-aov(Plant_height~Treatment,data = data)
anova(oneway)
out <- LSD.test(oneway,"Treat",p.adj="none")
out
mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$Plant_height,mar$groups)
sort<-newmar[order(newmar$rownamemar),]
rowname<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
marker<-sort$mar.groups
plotdata<-data.frame(rowname,mean,sd,marker)
plotdata$se<-plotdata$sd/sqrt(4)
plotdata
plotdata$rowname <- factor(plotdata$rowname ,c("Control","Dv","Cb","Cl","Pf"),labels = c("Control","Dv","Cb","Cl","Pf"))
data$Treatment<-factor(data$Treatment ,c("Control","Dv","Cb","Cl","Pf"),labels = c("Control","Dv","Cb","Cl","Pf"))
#plotdata$Time<-factor(plotdata$Time,c("Phase 2","Phase 3","Phase 4"),labels = c("Phase 2","Phase 3","Phase 4"))
plotdata
plant_height<-ggplot(plotdata,aes(rowname,mean,Group=rowname,fill=rowname))+
  geom_bar(position=position_dodge(0.6),width = 0.5,stat = "identity")+
  geom_errorbar(aes(rowname,ymin=mean-se,ymax=mean+se),position=position_dodge(0.6),width=0.2)+
  geom_text(aes(rowname,label =marker, y = mean + se), vjust=-0.4,position=position_dodge(0.9),size=6)+
  geom_point(data = data,
             aes(x = Treatment, y = Plant_height),
             inherit.aes = FALSE,
             position = position_jitter(width = 0.12, height = 0),
             size = 2.2, alpha = 0.55, shape = 16) +
  theme_bw() +
  #facet_grid(.~Treatment,scales = "free")+
  theme(axis.text.x=element_text(colour="black",size=20,angle = 45,vjust = 0.7), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 32,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 16,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size = 24),
                                                  strip.text.y = element_text(size = 10),
                                                  strip.background =
                                                    element_rect(fill = "gray"))+
  ylab("Aboveground biomass(g/pot)")+xlab("")+
  theme(legend.title=element_text(size=28,face="plain") , 
        legend.text=element_text(size=24))+scale_fill_npg()+
  theme(legend.position = "none")
plant_height



gene<-ggarrange(a,b,c,,e,f,
                ncol =3,nrow=2,labels = "auto",
                font.label = list(size = 32, face = "bold"),common.legend = TRUE)
gene

ggsave("fig6.pdf",gene,height =11,width = 18)


library(ggplot2)
library(dplyr)
library(patchwork)


d1 <- read.csv("Fi7a.csv", header = TRUE)
d2 <- read.csv("Fi7b.csv", header = TRUE)


norm_p <- function(d){
  stopifnot("pvalue" %in% names(d))
  d$pv <- d$pvalue
  d
}
d1 <- norm_p(d1); d2 <- norm_p(d2)


cats <- sort(unique(c(as.character(d1$Category),
                      as.character(d2$Category)
)))
d1$Category <- factor(d1$Category, levels = cats)
d2$Category <- factor(d2$Category, levels = cats)



pval_lim <- range(c(d1$pv, d2$pv), na.rm = TRUE)
count_lim <- range(c(d1$Count, d2$Count), na.rm = TRUE)

shape_pool <- c(16,17,15,18,8,3,4,7,10,25,24,23,22,21)
shape_vals <- rep(shape_pool, length.out = length(cats))

# 6) 统一画图函数（颜色映射到 pvalue）
make_plot <- function(df, xlab, pval_lim, count_lim, cats, shape_vals){
  ggplot(df, aes(x = Rich_factor,
                 y = reorder(Description, Count, sum),
                 size = Count,
                 colour = pv,             # 用 pvalue
                 shape  = Category)) +
    geom_point() +
    labs(x = xlab, y = "KEGG Pathway") +
    scale_colour_gradient(name = "p-value",
                          low = "#4575B4", high = "#D73027",
                          limits = pval_lim) +
    scale_size(name = "Number",
               range = c(2, 10),
               limits = count_lim,
               breaks = pretty(count_lim, n = 3)) +
    scale_shape_manual(name = "Category",
                       values = shape_vals,
                       breaks = cats, drop = FALSE) +
    theme_bw() +
    theme(axis.text.x = element_text(colour = "black", size = 16),
          axis.text.y = element_text(size = 16),
          axis.title.y = element_text(size = 21),
          axis.title.x = element_text(size = 21),
          plot.title  = element_text(size = 24, face = "bold", hjust = 0.5),
          panel.grid.minor = element_blank())
}


p1 <- make_plot(d1, xlab = "Richfactor",  pval_lim, count_lim, cats, shape_vals)
p2 <- make_plot(d2, xlab = "Rich factor", pval_lim, count_lim, cats, shape_vals)



p2 <- p2 + guides(shape = "none")


p_final <- (p1+p2) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "top")

p_final
ggsave("Fig7.pdf", p_final, width =17, height = 5)







