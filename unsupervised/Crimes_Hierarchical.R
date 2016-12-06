#######################################################
head(crime.df)

row.names(crime.df) <- city

crime.hc.c <- hclust(dist(crime.df),method="complete")
crime.hc.a <- hclust(dist(crime.df),method="average")
crime.hc.s <- hclust(dist(crime.df),method="single")


plot(crime.hc.c)

plot(crime.hc.a)

plot(crime.hc.s)


crime.df[row.names(crime.df)=="1353"]


#######################################################
library(ggdendro)
crime.dendr <- dendro_data(crime.hc.c)
##crime.dendr <- dendro_data(crime.hc.c, type="triangle")

#######################################################
crime.dendro.gg <- ggplot() +
  geom_segment(data=segment(crime.dendr),
               aes(x=x, y=y, xend=xend, yend=yend),
               size=.1) +
  geom_text(data=label(crime.dendr),
            aes(x=x, y=y, label=label, hjust=0),
            size=3,color="blue") +
  coord_flip() +
  scale_y_reverse(expand=c(0.2, 0))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())+
  ggtitle("Hierarchical Clustering")
crime.dendro.gg


#######################################################
## perform a cut
cutSize=4
with(crime.df,table(cluster.km,cluster.hc))

