library(beanplot)
library(ggplot2)

library(Hmisc)
library(hexbin)
library(grid)

plot_control_chart = function(df, index_col, metric_col, limits_for_y = NULL, 
                              title = NULL, outfile = NULL){
  x <- df[[index_col]]
  y <- df[[metric_col]]
  
  metric_mean = mean(y)
  three_std_below = metric_mean - (3 * sd(y))
  three_std_above = metric_mean + (3 * sd(y))
  
  vert_lines <-  numeric()
  outliers <- numeric()
  cutpoints = unique(df$cutpoint)
  
  for(i in 1:length(cutpoints)){
    chunk = subset(df, cutpoint == i)
    vert_lines[i] = head(chunk,1)$commit_seq
    outliers_below = which(chunk[,metric_col] < three_std_below)
    outliers_above = which(chunk[,metric_col] > three_std_above)
    outliers[i] = length(outliers_below) + length(outliers_above)
  }
  
  print(paste("Three std. below:",three_std_below))
  print(paste("Three std. above:",three_std_above))
  cat(outliers, sep = "\n")
  
  p <- ggplot(df, aes(x,y),environment = environment())
  p <- p + scale_y_continuous()

  p <- p + coord_cartesian(ylim = limits_for_y)

  p <- p + geom_point()

  p <- p + geom_hline(yintercept=metric_mean, linetype = "longdash", colour = "blue")
  p <- p + geom_hline(yintercept=three_std_below, linetype = "longdash", colour = "blue")
  p <- p + geom_hline(yintercept=three_std_above, linetype = "longdash", colour = "blue")
  
  p <- p + geom_vline(xintercept = vert_lines, linetype = "longdash", colour = "red")
  
  p <- p + ggtitle(title)
  p <- p + theme_bw()
  
  print_plot(p,outfile)
  
  #plot(df[,xcol], df[,ycol], type='b', main = title)
}

plot_points_2vars = function(df, xcol, ycol, breaks_for_x = waiver(), limits_for_x = NULL, breaks_for_y = waiver(), limits_for_y = NULL, title){
  x <- df[[xcol]]
  y <- df[[ycol]]
  p <- ggplot(df, aes(x,y),environment = environment())
  p <- p + scale_x_continuous(breaks=breaks_for_x)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  p <- p + geom_point()
  p <- p + geom_smooth()
  p <- p + ggtitle(title)
  p <- p + theme_bw()
  
  print_plot(p,title)
  
  #plot(df[,xcol], df[,ycol], type='b', main = title)
}


plot_line = function(df, xcol, ycol, varcol, lineColours = NULL,
                     xlab, breaks_for_x = waiver(), limits_for_x = NULL, 
                     ylab, breaks_for_y = waiver(), limits_for_y = NULL, 
                     showpoints = FALSE, smooth = FALSE, title = NULL, 
                     fontsize = 22, outfile = NULL){
  
  x <- df[[xcol]]
  y <- df[[ycol]]
  colour <- df[[varcol]]
  
  p <- ggplot(data = df, aes(x = x, y = y, colour = colour), environment = environment())
 
  p <- p + xlab(xlab) + ylab(ylab)
  #p <- p + guides(colour = FALSE)
  
  if(!is.null(lineColours)){
    p <- p + scale_color_manual(values = lineColours)  
  }

  p <- p + theme(legend.position = "bottom", legend.box = "horizontal")
  
  p <- p + scale_x_continuous(breaks=breaks_for_x)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  
  if(smooth == TRUE){
    p <- p + geom_smooth()
  }
  else{
    p <- p + geom_line()
    if(showpoints == TRUE){
      p <- p + geom_point()
    }
  }
  
  p <- p + ggtitle(title)
  p <- p + theme_bw(base_size = fontsize)
  
  print_plot(p,outfile)
  return(p)
}

plot_bubble = function(df, xcol, ycol, sizecol, 
                     xlab = xcol, breaks_for_x = waiver(), limits_for_x = NULL, 
                     ylab = ycol, breaks_for_y = waiver(), limits_for_y = NULL, 
                     sizelab = sizecol,
                     title = NULL, smooth = FALSE,
                     fontsize = 22, outfile = NULL){
  
  x <- df[[xcol]]
  y <- df[[ycol]]
  size <- df[[sizecol]]
  
  p <- ggplot(data = df, aes(x = x, y = y, size = size), environment = environment())
  
  p <- p + xlab(xlab) + ylab(ylab)
  p <- p + labs(size=sizelab)
  
  p <- p + theme(legend.position = "bottom", legend.box = "horizontal")
  
  p <- p + scale_x_continuous(breaks=breaks_for_x)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)

  if(smooth == TRUE){
    p <- p + geom_smooth()
  }
  
  p <- p + geom_point(shape = 21, alpha = 0.7, colour = "#1e6c7b", fill = "#40b8d0")
  p <- p + ggtitle(title)
  p <- p + theme_bw(base_size = fontsize)
  
  print_plot(p,outfile)
  return(p)
}


plot_hexbin = function(df, xcol, ycol, numbins = 30,
                     xlab = NULL, breaks_for_x = waiver(), limits_for_x = NULL, 
                     ylab = NULL, breaks_for_y = waiver(), limits_for_y = NULL, 
                     title = NULL, 
                     fontsize = 22, outfile = NULL){
  
  x <- df[[xcol]]
  y <- df[[ycol]]
  
  p <- ggplot(data = df, aes(x = x, y = y), environment = environment())
  
  p <- p + xlab(xlab) + ylab(ylab)
  #p <- p + guides(colour = FALSE)
  
  p <- p + theme(legend.position = "bottom", legend.box = "horizontal")
  
  p <- p + scale_x_continuous(breaks=breaks_for_x)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  
  p <- p + geom_hex(bins = numbins, colour = "grey")
  p <- p + scale_fill_gradient(low = "#ffe8e8", high = "#990000")
  #p <- p + scale_fill_gradient(low = "white", high = "black")
  
  p <- p + ggtitle(title)
  p <- p + theme_bw(base_size = fontsize)
  
  print_plot(p,outfile)
  return(p)
}

plot_point = function(df, xcol, ycol, varcol, lineColours = NULL,
                     xlab, breaks_for_x = waiver(), limits_for_x = NULL, 
                     ylab, breaks_for_y = waiver(), limits_for_y = NULL, 
                     title = NULL, fontsize = 22, outfile = NULL){
  
  x <- df[[xcol]]
  y <- df[[ycol]]
  colour <- df[[varcol]]
  
  p <- ggplot(data = df, aes(x = x, y = y, colour = colour), 
              environment = environment())
  
  p <- p + xlab(xlab) + ylab(ylab)
  p <- p + guides(colour = FALSE)
  
  if(!is.null(lineColours)){
    p <- p + scale_color_manual(values = lineColours)  
  }
  
  #p <- p + theme(legend.position = "bottom", legend.box = "horizontal")
  
  p <- p + scale_x_continuous(breaks=breaks_for_x)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  
  p <- p + geom_point()
  
  p <- p + ggtitle(title)
  p <- p + theme_bw(base_size = fontsize)
  
  print_plot(p,outfile)
  return(p)
}


plot_line_2vars = function(df, xcol, ycol, breaks_for_x = waiver(), 
                           limits_for_x = NULL, breaks_for_y = waiver(), 
                           limits_for_y = NULL, smooth = FALSE, title){
  x <- df[[xcol]]
  y <- df[[ycol]]
  
  p <- ggplot(df, aes(x,y),environment = environment())
  p <- p + scale_x_continuous(breaks=breaks_for_x)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  p <- p + geom_line()
  
  if(smooth == TRUE){
    p <- p + geom_smooth()
  }
  
  p <- p + ggtitle(title)
  p <- p + theme_bw()
  
  print_plot(p,title)
  
  #plot(df[,xcol], df[,ycol], type='b', main = title)
}

plot_line_3vars = function(df, xcol, ycol, zcol, limits_for_y = NULL, title){
  
  x <- df[[xcol]]
  y <- df[[ycol]]
  z <- df[[zcol]]
  
  p <- ggplot(df, aes(factor(x, levels = unique(x)),y,size=z),environment = environment())
  p <- p + coord_cartesian(ylim = limits_for_y)
  p <- p + geom_line()
  p <- p + ggtitle(title)
  p <- p + theme_bw()
  
  print_plot(p,title)
  
  #plot(df[,xcol], df[,ycol], type='b', main = title)
}

#Plots a frequency histogram. The function calculates the frequencies based
#on the raw observations from datacol
plot_histogram =  function(df, datacol, breaks_for_x = NULL, trim = TRUE,
                         xlab = NULL, ylab = NULL, type = "frequency",
                         space_between_bars = 0.1,
                         xticklab = NULL,
                         limits_for_x = NULL, breaks_for_y = waiver(), 
                         limits_for_y = NULL, title = NULL, outfile = NULL,
                         fontsize = 22){
  
  n = nrow(df)
  data <- df[[datacol]]
  
  #Building the frequency dataset
  df = data.table(table(data))
  setnames(df,c("data","N"),c("label","value"))
  
  if(type == "frequency"){
    if(is.null(ylab)) ylab = "Frequency"
  }
  else if(type == "cumulative_frequency"){
    df$value = cumsum(df$value)
    if(is.null(ylab)) ylab = "Cumulative Frequency"
  }
  else if(type == "density"){
    df$value = df$value / n
    if(is.null(ylab)) ylab = "Density"
  }
  else if(type == "cumulative_density"){
    df$value = df$value / n
    df$value = cumsum(df$value)
    if(is.null(ylab)) ylab = "Cumulative Density"
  }
  
  if(trim == TRUE & !is.null(limits_for_x)){
    shown_labels = seq(limits_for_x[1],limits_for_x[2],1)
    df = df[label %in% shown_labels]
  }
  
  p <- ggplot(df, aes(x=label,y=value))
  
  p <- p + geom_col(
    colour="black", fill="white", width = 1 - space_between_bars)
  
  if(!is.null(breaks_for_x)){
    p <- p + scale_x_continuous(breaks=breaks_for_x)
    p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  }
  else{
    p <- p + coord_cartesian(ylim = limits_for_y)
  }
  
  if(!is.null(xticklab)){
    p <- p + scale_x_discrete(labels=xticklab)
  }
  
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  
  p <- p + ggtitle(title)
  p <- p + xlab(xlab) + ylab(ylab)
  p <- p + theme_bw(base_size = fontsize) 
  
  print_plot(p,outfile)
}

#UNTESTED
#Plot whatever value is passed to it without performing any computation
plot_barchart = function(df, xcol, ycol, groupcol = NULL, 
                         legend_title = groupcol,
                         flip = FALSE, showvalues = TRUE,
                         breaks_for_x = NULL,
                         trim = TRUE, xlab = NULL, ylab = NULL, 
                         space_between_bars = 0.1,
                         xticklab = NULL,
                         limits_for_x = NULL, breaks_for_y = waiver(), 
                         limits_for_y = NULL, title = NULL, outfile = NULL,
                         fontsize = 22){
  
  categories <- df[[xcol]]
  values <- df[[ycol]]
  
  groups <- NULL
  if(!is.null(groupcol)){
    groups <- df[[groupcol]]
  }
  
  p <- ggplot(df, aes(x=categories,y=values,fill = groups,label=values), 
              environment = environment()) 
  
  p <- p + geom_col(
    colour="black", width = 1 - space_between_bars)
    
  if(showvalues == TRUE){
    p <- p + geom_text(size = 6, position = position_stack(vjust = 0.5))
  }
  
  if(!is.null(breaks_for_x)){
    p <- p + scale_x_continuous(breaks=breaks_for_x)
    p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  }
  else{
    p <- p + coord_cartesian(ylim = limits_for_y)
  }
  
  if(!is.null(xticklab)){
    p <- p + scale_x_discrete(labels=xticklab)
  }
  
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  
  p <- p + ggtitle(title)
  p <- p + xlab(xlab) + ylab(ylab)
  p <- p + theme_bw(base_size = fontsize) 
  
  if(flip == TRUE){
    p <- p + coord_flip()
    p <- p + theme(legend.position = "top")
  }
  
  if(!is.null(groupcol)){
    p <- p + guides(fill=guide_legend(title=legend_title))
  }
  
  print_plot(p,outfile)
}
  
  

#Plots the frequency (count) of every category in datacol 
#(e.g., datacol = c("A","A","A","B","C")). If a group col is provided, then
#a stacked bars are produced
plot_freqhistogram_categorical = function(df, categorycol, groupcol = NULL,
                                          binwidth = NULL, 
                                          breaks_for_y = waiver(), 
                                          limits_for_y = NULL, title = NULL, 
                                          outfile = NULL){
  
  data <- df[[categorycol]]
 
  if(is.null(groupcol)){
    p <- ggplot(df, aes(x=data), environment = environment()) 
  }
  else{
    groups <- df[[groupcol]]
    p <- ggplot(df, aes(x=data, fill = groups), environment = environment()) 
  }
  
  p <- p + geom_bar(binwidth=binwidth, colour="black")
  p <- p + scale_y_continuous(breaks=breaks_for_y, limits=limits_for_y)
  p <- p + ggtitle(title)
  p <- p + theme_bw() 
  
  print_plot(p,title)
  
}


plot_boxplot_1var = function(df, col, transformation = "identity", 
                             xticklab = NULL, xlab = NULL, ylab = NULL,
                             breaks_for_y = waiver(), 
                             limits_for_y = NULL, title = NULL, outfile = NULL){
  
  data <- df[[col]]
  
  p <- ggplot(df, aes(x=factor(col, levels = unique(col)),y=data), environment = environment()) 
  p <- p + geom_boxplot()
  
  p <- p + scale_y_continuous(breaks=breaks_for_y, trans = transformation)
  
  if(!is.null(xticklab)){
    p <- p + scale_x_discrete(labels=xticklab)
  }
  
  if(!is.null(xlab)){
    p <- p + xlab(xlab)  
  }
  
  if(!is.null(ylab)){
    p <- p + ylab(ylab)  
  }
  
  p <- p + coord_cartesian(ylim = limits_for_y)
  p <- p + ggtitle(title)
  p <- p + xlab(NULL)
  p <- p + theme_bw() 
  
  print_plot(p,outfile)

}

plot_boxplot_2vars = function(df, xcol, ycol, groupcol = NULL, limits_for_x = NULL, 
                              xlab = NULL, ylab = NULL, grouplab = groupcol, xticklab = NULL,
                              breaks_for_y = waiver(), limits_for_y = NULL, trans_for_y = "identity",
                              title = NULL, outfile = NULL, fontsize = 22){
  
  vars <- df[[xcol]]
  data <- df[[ycol]]
  
  if(is.null(groupcol)){
    aesthetics = aes(x=factor(vars, levels = unique(vars)),y=data)
  }
  else{
    groups <- df[[groupcol]]
    aesthetics = aes(x=factor(vars, levels = unique(vars)),y=data, fill=groups)
  }
  
  p <- ggplot(df, aesthetics, environment = environment()) 
  p <- p + geom_boxplot()
  
  p <- p + scale_y_continuous(breaks=breaks_for_y, trans = trans_for_y)
  
  if(!is.null(xticklab)){
    p <- p + scale_x_discrete(labels=xticklab)
  }
  
  if(!is.null(groupcol)){
    p <- p + scale_fill_grey(name=grouplab, start = 0.5, end = 1)
  }
  
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  p <- p + ggtitle(title)
  p <- p + xlab(xlab) + ylab(ylab)
  p <- p + theme_bw(base_size = fontsize) 
  
  print_plot(p,outfile)
}

#Most fancy function I have
#TODO: Create a single function for boxplot (and maybe others?) inspired on this one
#TODO: Remove the beanplot functions
plot_violin = function(df, xcol = NULL, ycol, groupcol = NULL, 
                       xlab = xcol, ylab = ycol, grouplab = groupcol, 
                       xticklab = NULL, trim = TRUE, split = FALSE,
                       limits_for_y = NULL, breaks_for_y = waiver(), 
                       showboxplot = FALSE, boxplot_width = 0.1, 
                       dodge_width = 0.9, scale = "area",
                       transformation = "identity", labels = waiver(),
                       outfile = NULL, title = NULL, fontsize = 22){
  
  #Sets the dodge (space between plots from the same group)
  dodge <- position_dodge(width = dodge_width)
  
  #Prepares the data
  data <- df[[ycol]]
  
  if(is.null(xcol)){
    aesthetics = aes(x = "", y=data)
  }
  else{
    vars <- df[[xcol]]
    if(!is.factor(vars)){
      vars=factor(vars, levels = unique(vars))
    }
   
    if(is.null(groupcol)){
      aesthetics = aes(x=vars,y=data)
    }
    else{
      groups <- df[[groupcol]]
      aesthetics = aes(x=vars,y=data, fill=groups)
    }
  }
  
  #Creates the empty plot
  p <- ggplot(data = df, aesthetics) 
  
  #Adds the violin plot
  if(split == FALSE){
    p <- p + geom_violin(position = dodge, scale = scale, trim = trim, colour = "black")  
    
    #Adds the boxplot if requested
    if(showboxplot == TRUE){
      p = p + geom_boxplot(width=boxplot_width, outlier.size = 1, position = dodge)  
    }
    #Otherwise, add a point to denote the median
    else{
      p <- p + stat_summary(fun.y="median", geom="point",
                            position = dodge)
    }
    
  }
  else{
    p <- p + geom_split_violin(scale = scale)
  }
  
  p <- p + coord_cartesian(ylim = limits_for_y)
  
  if(!is.null(xticklab)){
    p <- p + scale_x_discrete(labels=xticklab)
  }
  
  if(!is.null(groupcol)){
    p <- p + scale_fill_grey(name=grouplab, start = 0.65, end = 1.0)
  }
  
  p <- p + scale_y_continuous(breaks=breaks_for_y, trans = transformation, labels = labels)
  p <- p + ggtitle(title)
  
  p <- p + xlab(xlab) + ylab(ylab)
  
  p <- p + theme_bw(base_size = fontsize) 
  print_plot(p,outfile)
}



GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, draw_group = function(self, data, ..., draw_quantiles = NULL){
  data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
  grp <- data[1,'group']
  newdata <- plyr::arrange(transform(data, x = if(grp%%2==1) xminv else xmaxv), if(grp%%2==1) y else -y)
  newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
  newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
  if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
    stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 
                                              1))
    quantiles <- create_quantile_segment_frame(data, draw_quantiles)
    aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
    aesthetics$alpha <- rep(1, nrow(quantiles))
    both <- cbind(quantiles, aesthetics)
    quantile_grob <- GeomPath$draw_panel(both, ...)
    ggplot2:::ggname("geom_split_violin", grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
  }
  else {
    ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
  }
})

geom_split_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

plot_beanplot = function(df, xcol, ycol, limits_for_y = NULL, title = NULL, outfile = NULL){
  
  yvector <- df[[ycol]]
  xvector <- df[[xcol]]
  
  if(!is.null(outfile)){
    png(filename=paste("./results/imgs/",outfile,".png",sep = ""),width = 480, height = 480)
  }
  
  beanplot(yvector ~ xvector, data = df, method = "overplot",
           what = c(TRUE, TRUE, TRUE, FALSE), beanlinewd = 2,
           overallline = "median", beanlines = "median",
           col = c("lightgray","black","black","black"), border = "gray", 
           log = "",  bw="nrd0", ylim=limits_for_y, main = title)
  
  minor.tick(ny = 5)
  
  if(!is.null(outfile)){
    dev.off()
  }
  #png(filename=paste(title,".png"),width = 1920, height = 1080)
  #dev.off()
}

plot_beanplot_singlevar = function(df, col, limits_for_y = NULL, title){
  
  numvector <- df[[col]]
  
  beanplot(numvector, method = "overplot",
           what = c(TRUE, TRUE, TRUE, FALSE), beanlinewd = 2,
           overallline = "median", beanlines = "median",
           col = c("lightgray","black","black","blue"), border = "gray", 
           log = "",  bw="nrd0", ylim=limits_for_y, main = title)
  
  minor.tick(ny = 4)
  
  #png(filename=paste(title,".png"),width = 1920, height = 1080)
  #dev.off()
}

plot_beanplot_2vars = function(df, metric_col, side_col, group_col,
                               total_avg_line = TRUE, beans = TRUE,
                               bean_avg = TRUE, bean_lines = TRUE,
                               limits_for_y = NULL, cutmin = -Inf, 
                               xlab = NULL, ylab = NULL,
                               cutmax = Inf, title = NULL){
  
  metric_vector <- df[[metric_col]]
  side_vector <- df[[side_col]]
  group_vector <- df[[group_col]]
  
  colorsleft = c("lightgray","black","black","blue")
  colorsright = c("darkgray","black","black","blue")
  colors = list(colorsleft,colorsright)
  
  leftbordercolor = "lightgray"
  rightbordercolor = "darkgray"
  bordercolors = c(leftbordercolor,rightbordercolor)
  
  beanplot(metric_vector ~ side_vector * group_vector, data = df, method = "overplot",
           what = c(total_avg_line, beans, bean_avg, bean_lines), beanlinewd = 2,
           overallline = "median", beanlines = "mean",
           col = colors, log = "", cutmin = cutmin, cutmax = cutmax,
           bw="nrd0", ylim=limits_for_y, main = title, 
           side = "both", border = bordercolors, xlab = xlab, ylab = ylab)
  
  #minor.tick(ny = 4)
  
  #png(filename=paste(title,".png"),width = 1920, height = 1080)
  #dev.off()
}


plot_beanplot_2vars_withoutgroup = function(df, metric_col, side_col,
                                            xlab = "", ylab = "",
                                            leftside = NULL, rightside = NULL,
                                            limits_for_y = NULL, cutmin = -Inf, cutmax = Inf,
                                            legendposition = "topright", title = NULL,
                                            outfile = NULL){
  
  metric_vector <- df[[metric_col]]
  side_vector <- df[[side_col]]
  
  colorsleft = c("lightgray","black","black","black")
  colorsright = c("darkgray","black","black","black")
  colors = list(colorsleft,colorsright)
  
  leftbordercolor = "black"
  rightbordercolor = "black"
  bordercolors = c(leftbordercolor,rightbordercolor)
  
  if(!is.null(outfile)){
    png(filename=outfile,width = 1600, height = 900)
  }
  
  beanplot(metric_vector ~ side_vector, data = df, method = "overplot",
           what = c(TRUE, TRUE, TRUE, FALSE), beanlinewd = 2,
           overallline = "median", beanlines = "median",
           col = colors, log = "", cutmin = cutmin, cutmax = cutmax,
           bw="nrd0", ylim=limits_for_y, main = title, 
           side = "both", border = bordercolors,
           xlab = xlab, ylab = ylab, show.names = FALSE, 
           cex.axis=1.25, cex.main=1.5, cex.lab=1.5)
  
  if(!is.null(leftside) || !is.null(rightside)){
    
    legend(legendposition, bty="n", c(leftside, rightside),
           fill = c("lightgray", "darkgray"), cex = 1.5)  
  }
  
  if(!is.null(outfile)){
    dev.off()
  }
  
}

plot_lowess = function(df, xcol, ycol, limits_for_y = NULL, 
                       breaks_for_y = waiver(), 
                       show_hexbin = TRUE,
                       spanvalue = 2/3, iter = 3, 
                       title = NULL, outfile = NULL){
   
  xvector <- df[[xcol]]
  yvector <- df[[ycol]]
  
  #### Calculates LOWESS curve ######
  
  #Function as.numeric is applied to x and y components to make sure lowess
  #can be calculated.
  curve <- lowess(as.numeric(xvector), y = as.numeric(yvector), f = spanvalue, iter = iter)
  
  #first point and last point of curve
  
  x0 = round(head(curve$x, n=1), digits = 2)
  y0 = round(head(curve$y, n=1), digits = 2)
  xn = round(tail(curve$x, n=1), digits = 2)
  yn = round(tail(curve$y, n=1), digits = 2)
  
  #Converts list of 2 elements to dataframe
  curve <- data.frame(x = curve$x, y = curve$y)
  
  #GGPlot with hexbin and the lowess curve
  p <- ggplot(df, aes(x = df[,xcol], y = df[,ycol]), environment = environment()) 
  
  #Hexbin
  if(show_hexbin == TRUE){
    p <- p + geom_hex(colour = 'black',bins = 20) 
    p <- p + scale_fill_gradientn(colours=c('grey96','black')) 
  }
  else{
    p <- p + geom_point(colour = 'black')  
  }
  
  p <- p + geom_line(data=curve, colour = 'red', aes(x = curve$x, y = curve$y)) 
  #p <- p + geom_smooth(se = FALSE)
  
  #p <- p + ylim(1,50) --> This removes data, so I use the other option below
  #check coord_cartesian from ggplot
  p <- p + coord_cartesian(ylim = limits_for_y)
  #p <- p + scale_y_continuous(breaks=breaks_for_y)
  
  p <- p + ggtitle(title)
  p <- p + xlab(xcol)
  p <- p + ylab(ycol)
  p <- p + theme_bw()
  
  text = paste("LOWESS Curve: (",x0,",",y0,"),(",xn,",",yn,")", sep = "")
  my_grob = grobTree(textGrob(text, x=0.1,  y=0.95, hjust=0, gp=gpar(col="red", fontsize=12, fontface="italic")))
  p <- p + annotation_custom(my_grob)
  
  print_plot(p,outfile)
}

plot_loess = function(df, xcol, ycol, limits_for_y = NULL, breaks_for_y = waiver(), spanvalue, 
                      show_hexbin = TRUE, title = NULL, outfile = NULL){
  
  #xvector <- df[[xcol]]
  #yvector <- df[[ycol]]
  
  #### Calculates LOESS curve ######
  #lo <- loess(y ~ x, data = data.frame(x = xvector, y = yvector), control = loess.control(iterations = 10))
  #y.predict <- predict(lo, data.frame(x = xvector))
  #curve <- data.frame(x = xvector, y = y.predict)
  
  #first point and last point of curve
  
  #x0 = round(head(curve$x, n=1), digits = 2)
  #y0 = round(head(curve$y, n=1), digits = 2)
  #xn = round(tail(curve$x, n=1), digits = 2)
  #yn = round(tail(curve$y, n=1), digits = 2)
  
  #GGPlot with hexbin and the loess curve
  p <- ggplot(df, aes(x = df[[xcol]], y = df[[ycol]]), environment = environment()) 
  
  #Hexbin
  if(show_hexbin == TRUE){
    p <- p + geom_hex(colour = 'black',bins = 20) 
    p <- p + scale_fill_gradientn(colours=c('grey96','black')) 
  }
  else{
    p <- p + geom_point(colour = 'black')  
  }
  
  p <- p + geom_smooth(se = TRUE, method = "loess")
  #p <- p + geom_line(data=curve, colour = 'red', aes(x = curve$x, y = curve$y)) 
  
  #p <- p + ylim(1,50) --> This removes data, so I use the other option below
  #check coord_cartesian from ggplot
  p <- p + coord_cartesian(ylim = limits_for_y)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  
  p <- p + ggtitle(title)
  p <- p + xlab(xcol)
  p <- p + ylab(ycol)
  p <- p + theme_bw()
  
  #text = paste("LOESS Curve: (",x0,",",y0,"),(",xn,",",yn,")", sep = "")
  #my_grob = grobTree(textGrob(text, x=0.1,  y=0.95, hjust=0, gp=gpar(col="red", fontsize=12, fontface="italic")))
  #p <- p + annotation_custom(my_grob)
  
  print_plot(p,outfile)
}

plot_loess_two_vars = function(df, xcol, ycol, limits_for_y = NULL, breaks_for_y = waiver(), spanvalue, 
                      show_hexbin = TRUE, title = NULL, outfile = NULL){
  
  #GGPlot with hexbin and the loess curve
  p <- ggplot(df, aes(x = df[[xcol]], y = df[[ycol]], ), environment = environment()) 
  
  #Hexbin
  if(show_hexbin == TRUE){
    p <- p + geom_hex(colour = 'black',bins = 20) 
    p <- p + scale_fill_gradientn(colours=c('grey96','black')) 
  }
  else{
    p <- p + geom_point(colour = 'black')  
  }
  
  p <- p + geom_smooth(se = TRUE, method = "loess")
  #p <- p + geom_line(data=curve, colour = 'red', aes(x = curve$x, y = curve$y)) 
  
  #p <- p + ylim(1,50) --> This removes data, so I use the other option below
  #check coord_cartesian from ggplot
  p <- p + coord_cartesian(ylim = limits_for_y)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  
  p <- p + ggtitle(title)
  p <- p + xlab(xcol)
  p <- p + ylab(ycol)
  p <- p + theme_bw()
  
  #text = paste("LOESS Curve: (",x0,",",y0,"),(",xn,",",yn,")", sep = "")
  #my_grob = grobTree(textGrob(text, x=0.1,  y=0.95, hjust=0, gp=gpar(col="red", fontsize=12, fontface="italic")))
  #p <- p + annotation_custom(my_grob)
  
  print_plot(p,outfile)
}

plot_lowess_loess = function(df, xcol, ycol, limits_for_y = NULL, breaks_for_y = waiver(), spanvalue, title){
  
  xvector <- df[[xcol]]
  yvector <- df[[ycol]]
  
  #### Calculates LOWESS curve ######
  
  #Function as.numeric is applied to x and y components to make sure lowess
  #can be calculated.
  curve <- lowess(as.numeric(xvector), y = as.numeric(yvector), f = spanvalue, iter = 10)
  
  #first point and last point of curve
  
  x0 = round(head(curve$x, n=1), digits = 2)
  y0 = round(head(curve$y, n=1), digits = 2)
  xn = round(tail(curve$x, n=1), digits = 2)
  yn = round(tail(curve$y, n=1), digits = 2)
  
  #Converts list of 2 elements to dataframe
  curve <- data.frame(x = curve$x, y = curve$y)
  
  #GGPlot with hexbin and the lowess curve
  p <- ggplot(df, aes(x = df[,xcol], y = df[,ycol]), environment = environment()) 
  p <- p + geom_hex(colour = 'black',bins = 20) 
  p <- p + scale_fill_gradientn(colours=c('grey96','black')) 
  
  p <- p + geom_line(data=curve, colour = 'red', aes(x = curve$x, y = curve$y)) 
  #LOESS curve
  p <- p + geom_smooth(se = FALSE)
  
  #p <- p + ylim(1,50) --> This removes data, so I use the other option below
  #check coord_cartesian from ggplot
  p <- p + coord_cartesian(ylim = limits_for_y)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  
  p <- p + ggtitle(title)
  p <- p + xlab(xcol)
  p <- p + ylab(ycol)
  p <- p + theme_bw()
  
  text = paste("LOWESS Curve: (",x0,",",y0,"),(",xn,",",yn,")", sep = "")
  my_grob = grobTree(textGrob(text, x=0.1,  y=0.95, hjust=0, gp=gpar(col="red", fontsize=12, fontface="italic")))
  p <- p + annotation_custom(my_grob)
  
  print_plot(p,title)
}


old_plot_lowess = function(df, xcol, ycol, spanvalue = 2/3, iter = 3, title){
  
  title <- paste(title,"(",spanvalue,")")
  
  xvector <- df[[xcol]]
  yvector <- df[[ycol]]
  
  hbin <- hexbin(xvector,yvector, xbins = 10)
  hplt <- plot(hbin, main = title)
  
  pushHexport(hplt$plot.vp)
  
  #Calculates LOWESS curve
  #Function as.numeric is applied to x and y components to make sure lowess
  #can be calculated.
  curve <- lowess(as.numeric(xvector), y = as.numeric(yvector), f = spanvalue, iter = iter)
  
  #Adds the LOWESS curve to the graph
  #The super intuitive parameter 'gp = gpar(col = 2)' is just the line color
  grid.lines(curve$x, curve$y, gp = gpar(col = 2), default.units = "native")
  
  #Saves
  #png(filename=paste(title,".png"),width = 1920, height = 1080)
  #dev.off()
}

plot_scatterplot = function(df, xcol, ycol, limits_for_x = NULL, limits_for_y = NULL, title){
  
  xvector <- df[[xcol]]
  yvector <- df[[ycol]]
  data<-data.frame(xvector,yvector)
  
  medianForX = median(xvector)
  medianForY = median(yvector)
  
  print(paste("Median for X:",medianForX))
  print(paste("Median for Y:",medianForY))
  
  # Building the Scatterplot
  p = ggplot(data,aes(x=xvector,y=yvector)) 
  p = p + geom_point()
  #plot = plot + scale_x_continuous(breaks=seq(0,12,0.5))
  p = p + geom_hline(linetype = "dashed", yintercept=medianForY, colour = "red")
  p = p + geom_vline(linetype = "dashed", xintercept=medianForX, colour = "red")
  p = p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  p = p + xlab(xcol)
  p = p + ylab(ycol)
  p = p + theme_bw()  
  p = p + ggtitle(title)
  
  print_plot(p,title)
  
  #scale_x_continuous(breaks=1:30, limits=c(1,30)
  
  #inquadrant = fn$sqldf("select count(*) as count from data where 
  #                      support_count > '$medianSupportCount' and 
  #                      median_hops > '$medianHops'")
  
  #cat("Number of elements in 1st Quadrant:", inquadrant$count, "\n")
  #cat("Percentage:", percent(inquadrant$count / nrow(project)))
  
  #return(plot)
}

#Beta function
getBreaksForNumTicks = function(data, numticks, round_to = NULL){
  max_value = max(data)
  if(is.null(round_to)){
    each_tick = max_value/numticks 
  }
  else{
    each_tick = round_any(max_value/numticks, round_to)  
  }
  max_tick = ceiling(max_value/each_tick) * each_tick
  breaks = seq(0,max_tick,each_tick)
  return(breaks)
}

#Prints plot to a file. 
#Creates the sub-dirs in the path (in case they don't exist)
print_plot = function(p, outfile = NULL, width=16, height=9, dpi=100){
  
  if (!is.null(outfile)){
    dir = str_extract(outfile,".*/")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    ggsave(outfile, plot = p, width=width, height=height, dpi=dpi)
  }
  p
}

#Trim images inside R using imager (relies on GraphicsMagick)
#
#library(imager)
#trim_images = function(){
#  
#  original_path = "./results/imgs/"
#  trimmed_path = "./results/imgs/trimmed/"
#  
#  image_file_list = list.files(path = original_path, pattern = "*.png")
#  for(image_file in image_file_list){
#    i = load.image(paste(original_path,image_file,sep=""))
#    i = autocrop(i,color = c(255,255,255))
#    save.image(i,paste(trimmed_path,image_file,sep=""))
#  }
#}
