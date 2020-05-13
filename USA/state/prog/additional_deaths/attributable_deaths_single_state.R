# FOR ALL AGES AND ALL CAUSES TOGETHER IN ONE STATE

# temporary load csv
dat = read.csv("~/Desktop/elisabeth_attribution_long_2020_05_11.csv")

dat = dat[,c(1:3)]
############################
# for nationalised data
############################

p1 = ggplot(data=subset(dat), aes(x=as.factor(Year),y=Deaths,color=as.factor(Type),fill=as.factor(Type))) +
    geom_bar(width = 0.9, stat = "identity", position = 'dodge') +
    xlab('Year') + ylab('Deaths') +
    # scale_fill_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    # scale_color_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90, vjust=0.5, hjust=1), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center', legend.title = element_blank(),
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

print(p1)

ggsave(paste0('~/Desktop/elisabeth_attribution_long_2020_05_11.eps'),device='eps')


# dev.off()
