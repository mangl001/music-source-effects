
#Figure1
library(ggplot2)
library(scales)
library(readxl)
Figure1 <- read_excel("figures/Figure1.xlsx")

Figure1$Rating <-factor(Figure1$Rating, 
                        levels = c("Like", "Quality","Authentic","Fit","Cost"))
Figure1$Source <-factor(Figure1$Source, 
                        levels = c("Artist","Library", "Commissioned"))
f1.prof <- ggplot(data=Figure1, aes(x=Rating, y=Value, fill=Source)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
    geom_errorbar(aes(ymin=Value-se, ymax=Value+se), width=.2,
                  position=position_dodge(.9)) +
    labs(x="Rating Scale", y = "Rating Score")+
    theme_bw()

f1.prof + scale_y_continuous(limits=c(2,5), oob= rescale_none, breaks= c(2,3,4,5)) + 
    guides(fill=guide_legend(title="Music Source", title.theme = element_text(size=14))) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14),
          legend.text=element_text(size=14),axis.title.x=element_blank()) +
    scale_fill_brewer(palette="Blues",direction = -1)

ggsave("Figure1b.pdf", width=25, height=15, units = c("cm"),
       dpi=300, device = "pdf")

#Figure 2
library(readxl)
Figure2 <- read_excel("figures/Figure2.xlsx")

Figure2$Rating <-factor(Figure2$Rating, 
                        levels = c("Like", "Quality","Authentic","Fit","Cost"))

Figure2$Source <-factor(Figure2$Source, 
                        levels = c("Artist","Library", "Com."))

f2.both <- ggplot(data=Figure2, aes(x=Source, y=Value, fill=Group)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
    geom_errorbar(aes(ymin=Value-se, ymax=Value+se), width=.2,
                  position=position_dodge(.9)) +
    scale_fill_manual(values=c("#E7B800", "#56B4E9")) +
    labs(x="Rating Scale", y = "Rating Score")+
    theme_bw()

f2.both + scale_y_continuous(limits=c(2,5), oob = rescale_none, breaks= c(2,3,4,5)) + 
    guides(fill=guide_legend(title="Group", title.theme = element_text(size=14))) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14),
          legend.text=element_text(size=14),axis.title.x=element_blank(),
          strip.text.x = element_text(size = 12)) +
    facet_grid(. ~ Rating, switch="both")

ggsave("Figure2b.pdf", width=30, height=15, units = c("cm"),
       dpi=300, device = "pdf")


#Figure 3
#boxplot
library(ggpubr)
my_comparisons <- list( c("Consumers", "Professionals"))

ggviolin(dataBOTH, x = "Group", y = "EffectLabel", fill = "Group",
         palette = c("#E69F00",  "#56B4E9"),
         add = "boxplot", add.params = list(fill = "white"))+
    rremove("legend") + ylab("Source effect awareness") +
    stat_compare_means(comparisons = my_comparisons, label = "p.signif") # Add global the p-value 

ggsave("Figure3b.jpeg", width=25, height=15, units = c("cm"),
       dpi=300, device = "jpeg")
