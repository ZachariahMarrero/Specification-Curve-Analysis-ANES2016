Graphy.function <- function(data,DV1,IV=0,Effect,Color.coding.condition) {
  
  Specs <- lapply(seq_along(data),function(frames,Effect,Color.coding.condition,i){
    # return(results)}
    Toplot.data <- frames[[i]] #Each plot starts with organizing effects within subsamples to index properly later on
    if(IV==0){IV <-unique(Toplot.data$IV) }
    if(is.numeric(IV)) {Toplot.data <- Toplot.data[which(Toplot.data$IV %in% unique(IV)),]}
    if(is.character(IV)) {Toplot.data <- Toplot.data[which(colnames(anes2016)[Toplot.data$IV] %in% IV),]}
    
       
    if(is.numeric(DV1)) {Dep.var <- DV1} else {
      Dep.var <- which(colnames(anes2016)%in%DV1)}
    Specs.to.plot <- Toplot.data[Toplot.data$DV==Dep.var,] #Subset specs by chosen DV.
    
    #Dataprep: Need to remove unused factor levels and convert to numerics where needed.  
    factors <- unlist(lapply(Specs.to.plot,is.factor))
    Specs.to.plot[,factors] <- unlist(lapply(Specs.to.plot[,factors],droplevels))
    
    #Orders data in increasing order from top to bottom according to chosen effect
    if(!is.character(Effect)){int.Effect <- Effect} else{
      int.Effect = which(colnames(Specs.to.plot)%in%Effect)}
    Specs.to.plot[,int.Effect] <- as.numeric(Specs.to.plot[,int.Effect])
    Specs.to.plot <- Specs.to.plot[order(Specs.to.plot[,int.Effect],decreasing = FALSE),]
    
    #Create an index column for our plot X-axis
    Specs.to.plot$index <- 1:nrow(Specs.to.plot)
    
    #Color coding scheme.  1 = Null condition.
    Specs.to.plot$color <- rep(1,nrow(Specs.to.plot))
    if(is.numeric(Color.coding.condition[1])){x <- Specs.to.plot[,Color.coding.condition[1]]} else{
      x <- as.numeric(Specs.to.plot[,which(colnames(Specs.to.plot)%in%Color.coding.condition[1])])}
    z <- Color.coding.condition[2]
    fn <- Color.coding.condition[3]
    
    Specs.to.plot$color[do.call(fn, list(x , z))] <- 2
if(i>1){
  Specs.to.plot$color[Specs.to.plot$color==2] <- i+1 }
    return(Specs.to.plot)
  },frames=data,Effect=Effect,Color.coding.condition=Color.coding.condition)
  # final.df <- do.call(Map("rbind", split(x, 1:nrow(x))),Specs) #, split(y, 1:nrow(y))))

  final.df <- do.call(rbind,Specs)
  return(final.df)
  }
