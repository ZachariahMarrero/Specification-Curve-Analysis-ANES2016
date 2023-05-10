Ploty.mcplot.function <- function(Plotme,Effect,Plot.title="Default",cust=TRUE,ylab=ylab) {
  
  temp_plot2 <- ggplot() + #theme_set(theme_classic())+
  # guides(fill=FALSE)+
  # geom_hline(yintercept = 0) +
  scale_size_manual(values = c(2,3.25)) +
  # scale_color_manual(values = c(if(all(Specs.to.plot.R$color==2)){"#FF0000"} else{"#000000"}, "#FF0000")) +
  scale_color_manual(values = alpha(c("grey62","#FF0000","#0000FF","#ffff00","#800080","#ffa500","#ffc0cb"),.9),labels=c("N.S.","R","D","I","All","White","Non-white"),name="Subsamples") +
  # scale_color_manual(values = c(if(all(Specs.to.plot.I$color==2)){"#FF0000"} else{"#000000"}, "#FF0000")) +
  # scale_color_manual(values = c(if(all(Specs.to.plot.ALL$color==2)){"#FF0000"} else{"#000000"}, "#FF0000")) +
  # 
 
  geom_point(data = Plotme[!is.na(as.numeric(if(is.numeric(Effect)){Plotme[,Effect]} else Plotme[,which(colnames(Plotme)%in%Effect)])),], 
             aes(
               x = index, 
               y = as.numeric(if(is.numeric(Effect)){Plotme[,Effect]} else Plotme[,which(colnames(Plotme)%in%Effect)]),
               color =  factor(color,ordered = TRUE,levels = c(sort(unique(color)))),# as.factor(color)
               size = color>1),
             stroke = 3.5,
             shape = "|")  +
  theme(plot.margin = unit(c(1.5,1.5,0,2.5), "cm"),
        axis.title.x=element_blank(),
        # axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position="none") + ggtitle(Plot.title) + ylab(
        if(cust==FALSE){   paste(if(is.numeric(Effect)){colnames(Plotme)[Effect]} else {Effect})}
       else { ylab}) +
    geom_hline(aes(yintercept = median(as.numeric(if(is.numeric(Effect)){Plotme[,Effect]} else Plotme[,which(colnames(Plotme)%in%Effect)]))),color="grey62",linetype="dashed") +    
   # annotate("text", min(Plotme$index), median(as.numeric(if(is.numeric(Effect)){Plotme[,Effect]} else Plotme[,which(colnames(Plotme)%in%Effect)])), vjust = -1, label = "Median of all")+

  
    geom_hline(aes(yintercept = median(as.numeric(if(is.numeric(Effect)){Plotme[,Effect]} else Plotme[,which(colnames(Plotme)%in%Effect)])[Plotme$color>1])),color="grey62") 
   # annotate("text", min(Plotme$index), median(as.numeric(if(is.numeric(Effect)){Plotme[,Effect]} else Plotme[,which(colnames(Plotme)%in%Effect)])[Plotme$color>1]), vjust = -1, label = "Median of significant")
  
        # legend.position="none") + ylab(paste(if(is.numeric(Effect)){colnames(Plotme)[Effect]} else {Effect})) + ggtitle(Plot.title)




temp_plot <- ggplot()+
  
  scale_size_manual(values = c(1,2.5)) +
  # scale_color_manual(values = c(if(all(Specs.to.plot.R$color==2)){"#FF0000"} else{"#000000"}, "#FF0000")) +
  scale_color_manual(values = alpha(c("grey62","#FF0000","#0000FF","#ffff00","#800080","#ffa500","#ffc0cb"),.9) ,labels=c("N.S.","R","D","I","All","White","Non-white"),name="Subsamples") +
  geom_point(data = rbind(Plotme[!is.na(as.numeric(if(is.numeric(Effect)){Plotme[,Effect]} else Plotme[,which(colnames(Plotme)%in%Effect)])),],Plotme[!is.na(as.numeric(if(is.numeric(Effect)){Plotme[,Effect]} else Plotme[,which(colnames(Plotme)%in%Effect)])),]), 
             aes(
               # x = c(Plotme$index,Plotme$index)[c(order(colnames(anes2016)[Plotme$IV]),order(as.character(Plotme[,3])))],
               x = c(Plotme$index,Plotme$index),
               #factor(x$name, levels = x$name[order(x$val)])
               y = c(colnames(anes2016)[Plotme$IV],as.character(Plotme[,3])),
               # y = c(colnames(anes2016)[Plotme$IV][sort(colnames(anes2016)[Plotme$IV])],sort(as.character(Plotme[,3]))),
               color =factor(color,ordered = TRUE,levels = c(sort(unique(color)))),# as.factor(color)
               size = color>1),
             stroke = 3.5,
             shape = "|")  +
  # scale_size_manual(values = c(1, 3)) +
  geom_hline(yintercept = 0) +
  theme(legend.position="none",
        axis.title.y = element_blank() ,
        axis.text.y = element_text( angle = 15),
        aspect.ratio = 1.4,
        panel.grid.major.y = element_line(color= alpha("grey62",.25),size = .5)) +
  # scale_x_continuous(breaks = seq(1, max(Plotme$index, na.rm = TRUE), by = 10), labels = NULL) +
  xlab("Specifications") +   scale_y_discrete(limits = c(rev(unique(sort(c(as.character(Plotme[,3]))))),rev(unique(sort(c(colnames(anes2016)[Plotme$IV]))))))#)

# plot(temp_plot)

# library(gtable)
# library(grid) # low-level grid functions are required
g1 <- ggplot2::ggplotGrob(temp_plot2)
# g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend

g2 <- ggplot2::ggplotGrob(temp_plot)
g2$widths <- g1$widths

g <- rbind(g1, g2, size="first") # stack the two plots
# g$widths <- grid::unit.pmax(g1$widths, g2$widths) # use the largest widths
# center the legend vertically
# g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
return(g)}