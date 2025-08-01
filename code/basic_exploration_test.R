setwd('D:\\Shan\\Rhythm\\code\\data')
library('biotools')
library('ez')
library('car')
library('tidyverse')
library('bruceR')
library('gridExtra')

stats_new <- read.csv(file="questionnaire_data.csv", header=TRUE)

stat_plot<-list()
stat_normal<-matrix(NA, nrow=4, ncol=3)
m=1
for (i in c('age', 'MT', 'GMS', 'BMRQ')) {
  stat_plot[[m]]<-ggplot(data=stats_new, aes_string(x=i))+
    geom_histogram(aes(y = after_stat(density), fill = country, group=country))+
    geom_density(aes(y = after_stat(density), color = country, group=country))+
    geom_density(aes(y = after_stat(density), color='black'))+
    scale_color_manual(values=c("chinese"="#F7933B","denmark"="#00BFBD"))
  stat_normal[m,1]<-round(shapiro.test(as.data.frame(stats_new)[,i])$p.value,3)
  stat_normal[m,2]<-round(shapiro.test(as.data.frame(stats_new)[(stats_new$country=='chinese'),i])$p.value,3)
  stat_normal[m,3]<-round(shapiro.test(as.data.frame(stats_new)[(stats_new$country=='denmark'),i])$p.value,3)
  m<-m+1
}
grid.arrange(stat_plot[[1]],stat_plot[[2]],stat_plot[[3]],stat_plot[[4]],nrow=2,ncol=2)

#pairwise-comparison
n1<-as.numeric(count(stats_new[(stats_new$country=='chinese'),]))
n2<-as.numeric(count(stats_new[(stats_new$country=='denmark'),]))
stat_pair<-list()
m<-1
for (i in c('age', 'MT', 'GMS', 'BMRQ')) {
  wilcox_result<-eval(parse(text = paste('wilcox.test(', i, '~country, data=stats_new)')))
  U <- wilcox_result$statistic
  N <- n1 + n2
  Z <- (U - (n1 * n2 / 2)) / sqrt((n1 * n2 * (n1 + n2 + 1)) / 12)
  # effect size r
  r <- Z / sqrt(N)
  wilcox_result$r<-r
  wilcox_result$variable<-i
  stat_pair[[m]]<-wilcox_result
  m<-m+1
}

#plot
stats_new$MT_scale<-scale(stats_new$MT)
stats_new$GMS_scale<-scale(stats_new$GMS)
stats_new$BMRQ_scale<-scale(stats_new$BMRQ)
pair_plot<-list()
m<-1
for (i in c('MT_scale', 'GMS_scale', 'BMRQ_scale')) {
  p<-ggplot(stats_new, aes_string(y=i)) +
    geom_boxplot(width=0.5, linewidth=1, aes(x = country, shape=country, fill = country)) +
    geom_jitter(width=0.08, stroke=1, aes(x = country, shape=country))+
    labs(x = NULL, y = NULL) +
    scale_shape_manual(values = c(21, 16)) +
    scale_fill_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD")) +
    coord_cartesian(ylim = c(-4, 4))+
    scale_y_continuous(breaks = seq(-4, 4, by = 2), expand = c(0,0))+
    theme_classic()+
    theme(panel.spacing = unit(0, "lines"))+
    theme(axis.text.x = element_blank())
  pair_plot[[m]]<-p
  m<-m+1
}
basic_exploration<-grid.arrange(pair_plot[[1]], pair_plot[[2]], pair_plot[[3]], ncol=3)
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\sup_fig2.pdf", plot = basic_exploration, width = 12, height = 5, device = "pdf", limitsize = FALSE)

save.image('D:\\Shan\\Rhythm\\code\\result\\basic_exploration_test.RData')