library(ggrepel)
library(ggplot2)
library(Hmisc)
library(hexbin)
library(grid)
library(RColorBrewer)

plot_point = function(
  df, xcol = NULL, ycol, varcol = NULL, xlab = NULL, ylab = NULL, use_colors = TRUE, use_shapes = FALSE, 
  breaks_for_x = waiver(), limits_for_x = NULL, minor_breaks_for_x = waiver(), trans_for_x = "identity",
  breaks_for_y = waiver(), limits_for_y = NULL, minor_breaks_for_y = waiver(), trans_for_y = "identity",
  include_labels = FALSE, labelcol = varcol, labelsize = 4.0, 
  point_color = "black", point_size = 3.0, flip = FALSE, 
  smooth = FALSE, smooth_span = 0.75, smooth_alpha = 0.4,
  title = NULL, fontsize = 22, outfile = NULL, pre_func = NULL){
  
  #Prepares aesthetics
  if(is.null(xcol)){
    aesthetics <- aes_string(x = as.factor(""), y = ycol)
  }
  else{
    if(is.null(varcol)){
      aesthetics <- aes_string(x = xcol, y = ycol)
    }
    else{
      
      if(use_colors == TRUE & use_shapes == FALSE){
        aesthetics <- aes_string(x = xcol, y = ycol, color = varcol)  
      }
      if(use_colors == FALSE & use_shapes == TRUE){
        aesthetics <- aes_string(x = xcol, y = ycol, shapes = varcol)
      }
      if(use_colors == TRUE & use_shapes == TRUE){
        aesthetics <- aes_string(x = xcol, y = ycol, color = varcol, shapes = varcol)
      }
      
    }
  }
  
  p <- ggplot(df, aesthetics, environment = environment()) 
  
  p <- p + xlab(xlab) + ylab(ylab)
  p <- p + guides(colour = FALSE)
  
  if(is.factor(df[[xcol]])){
    p <- p + scale_x_discrete(limits = levels(df[[xcol]]))
    
    num_levels = length(levels(df[[xcol]]))
    reps = ceiling(num_levels/8)
    p <- p + scale_color_manual(values = rep(brewer.pal(n = 8, "Dark2"),reps))
  }
  else{
    p <- p + scale_x_continuous(breaks=breaks_for_x, minor_breaks = minor_breaks_for_x, trans = trans_for_x)  
  }
  
  if(!is.factor(df[[ycol]])){
    p <- p + scale_y_continuous(breaks=breaks_for_y, minor_breaks = minor_breaks_for_y, trans = trans_for_y)
  }
  else{
    p <- p + scale_y_discrete(limits = levels(df[[ycol]]))
  }
  
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  
  #Should flip?
  if(flip == TRUE){
    p <- p + coord_flip(ylim = limits_for_y)
  }
  
  if(!is.null(pre_func)){
    p <- p + pre_func
  }
  
  if(include_labels == TRUE & !is.null(labelcol)){
    p <- p + geom_text_repel(aes_string(label = labelcol), size = labelsize, max.iter = 10000) 
  }
  
  if(is.null(varcol)){
    p <- p + geom_point(size = point_size, color = point_color)  
  }
  else{
    p <- p + geom_point(size = point_size)
  }
  
  if(smooth == TRUE){
    p <- p + geom_smooth(span = smooth_span, alpha = smooth_alpha)
  }
  
  p <- p + ggtitle(title)
  p <- p + theme_bw(base_size = fontsize)
  p <- p + theme(plot.title = element_text(hjust = 0.5))
  
  print_plot(p,outfile)
  return(p)
}

plot_line = function(
  df, xcol = NULL, ycol, varcol = NULL, xlab, ylab, use_colors = TRUE, use_shapes = FALSE, 
  smooth = FALSE, smooth_alpha = 0.4, showpoints = FALSE,
  breaks_for_x = waiver(), limits_for_x = NULL, minor_breaks_for_x = waiver(),  
  breaks_for_y = waiver(), limits_for_y = NULL, minor_breaks_for_y = waiver(),
  include_labels = FALSE, labelcol = varcol, labelsize = 4.0, 
  flip = FALSE, title = NULL, fontsize = 22, outfile = NULL, pre_func = NULL){
  
  #Prepares aesthetics
  if(is.null(xcol)){
    aesthetics <- aes_string(x = as.factor(""), y = ycol)
  }
  else{
    if(is.null(varcol)){
      aesthetics <- aes_string(x = xcol, y = ycol)
    }
    else{
      
      if(use_colors == TRUE & use_shapes == FALSE){
        aesthetics <- aes_string(x = xcol, y = ycol, color = varcol)  
      }
      if(use_colors == FALSE & use_shapes == TRUE){
        aesthetics <- aes_string(x = xcol, y = ycol, shapes = varcol)
      }
      if(use_colors == TRUE & use_shapes == TRUE){
        aesthetics <- aes_string(x = xcol, y = ycol, color = varcol, shapes = varcol)
      }
      
    }
  }
  
  p <- ggplot(df, aesthetics, environment = environment()) 
 
  p <- p + xlab(xlab) + ylab(ylab)
  p <- p + guides(colour = FALSE)
  
  #p <- p + theme(legend.position = "bottom", legend.box = "horizontal")
  
  if(is.factor(df[[xcol]])){
    p <- p + scale_x_discrete(limits = levels(df[[xcol]]))
    
    num_levels = length(levels(df[[xcol]]))
    reps = ceiling(num_levels/8)
    p <- p + scale_color_manual(values = rep(brewer.pal(n = 8, "Dark2"),reps))
  }
  else{
    p <- p + scale_x_continuous(breaks=breaks_for_x, minor_breaks = minor_breaks_for_x)  
  }
  
  if(!is.factor(df[[ycol]])){
    p <- p + scale_y_continuous(breaks=breaks_for_y, minor_breaks = minor_breaks_for_y)
  }
  else{
    p <- p + scale_y_discrete(limits = levels(df[[ycol]]))
  }
  
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  
  #Should flip?
  if(flip == TRUE){
    p <- p + coord_flip(ylim = limits_for_y)
  }
  
  if(!is.null(pre_func)){
    p <- p + pre_func
  }
  
  if(include_labels == TRUE & !is.null(labelcol)){
    p <- p + geom_text_repel(aes_string(label = labelcol), size = labelsize, max.iter = 10000) 
  }
  
  if(smooth == TRUE){
    p <- p + geom_smooth(alpha = smooth_alpha)
  }
  else{
    p <- p + geom_line()
    if(showpoints == TRUE){
      p <- p + geom_point()
    }
  }
  
  p <- p + ggtitle(title)
  p <- p + theme_bw(base_size = fontsize)
  p <- p + theme(plot.title = element_text(hjust = 0.5))
  
  print_plot(p,outfile)
  return(p)
}


plot_bubble = function(
  df, xcol, ycol, sizecol, xlab = xcol, breaks_for_x = waiver(), limits_for_x = NULL, ylab = ycol, 
  breaks_for_y = waiver(), limits_for_y = NULL, sizelab = sizecol, flip = FALSE, pre_func = NULL,
  include_labels = FALSE, labelsize = 4.0, title = NULL, smooth = FALSE, fontsize = 22, 
  legend_position = "right", outfile = NULL){
  
 
  aesthetics <- aes_string(x = xcol, y = ycol, size = sizecol)
  p <- ggplot(df, aesthetics, environment = environment()) 
  
  p <- p + xlab(xlab) + ylab(ylab)
  p <- p + labs(size=sizelab)
  
  if(is.factor(df[[xcol]])){
    p <- p + scale_x_discrete(limits = levels(df[[xcol]]))
  }
  else{
    p <- p + scale_x_continuous(breaks=breaks_for_x, minor_breaks = minor_breaks_for_x, trans = trans_for_x)  
  }
  
  if(!is.factor(df[[ycol]])){
    p <- p + scale_y_continuous(breaks=breaks_for_y, minor_breaks = minor_breaks_for_y, trans = trans_for_y)
  }
  else{
    p <- p + scale_y_discrete(limits = levels(df[[ycol]]))
  }
  
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  
  #Should flip?
  if(flip == TRUE){
    p <- p + coord_flip(ylim = limits_for_y)
  }
  
  if(!is.null(pre_func)){
    p <- p + pre_func
  }
  
  if(include_labels == TRUE & !is.null(sizecol)){
    p <- p + geom_text_repel(aes_string(label = sizecol), size = labelsize, max.iter = 10000) 
  }
  
  p <- p + geom_point(shape = 21, alpha = 0.7, colour = "#1e6c7b", fill = "#40b8d0")
  
  if(smooth == TRUE){
    p <- p + geom_smooth(alpha = smooth_alpha)
  }
  
  p <- p + ggtitle(title)
  p <- p + theme_bw(base_size = fontsize)
  p <- p + theme(plot.title = element_text(hjust = 0.5),
                 legend.position = legend_position, 
                 legend.box = "horizontal")
  
  print_plot(p,outfile)
  return(p)
}

plot_hexbin = function(
  df, xcol = NULL, ycol, numbins = 30, xlab = NULL, ylab = NULL,
  breaks_for_x = waiver(), limits_for_x = NULL, minor_breaks_for_x = waiver(), trans_for_x = "identity",
  breaks_for_y = waiver(), limits_for_y = NULL, minor_breaks_for_y = waiver(), trans_for_y = "identity",
  flip = FALSE, 
  smooth = FALSE, smooth_span = 0.75, smooth_alpha = 0.4,
  title = NULL, fontsize = 22, outfile = NULL, pre_func = NULL){
  
  #Prepares aesthetics
  if(is.null(xcol)){
    aesthetics <- aes_string(x = as.factor(""), y = ycol)
  }
  else{
    aesthetics <- aes_string(x = xcol, y = ycol)
  }
  
  p <- ggplot(df, aesthetics, environment = environment()) 
  
  p <- p + xlab(xlab) + ylab(ylab)
  p <- p + guides(colour = FALSE)
  
  #p <- p + theme(legend.position = "bottom", legend.box = "horizontal")
  
  if(is.factor(df[[xcol]])){
    p <- p + scale_x_discrete(limits = levels(df[[xcol]]))
  }
  else{
    p <- p + scale_x_continuous(breaks=breaks_for_x, minor_breaks = minor_breaks_for_x, trans = trans_for_x)  
  }
  
  if(is.factor(df[[ycol]])){
    p <- p + scale_y_discrete(limits = levels(df[[ycol]]))
  }
  else{
    p <- p + scale_y_continuous(breaks=breaks_for_y, minor_breaks = minor_breaks_for_y, trans = trans_for_y)
  }
  
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  
  #Should flip?
  if(flip == TRUE){
    p <- p + coord_flip(ylim = limits_for_y)
  }
  
  if(!is.null(pre_func)){
    p <- p + pre_func
  }
  
  p <- p + geom_hex(bins = numbins, colour = "grey")
  p <- p + scale_fill_gradient(low = "#fff1f1", high = "#990000")
  
  if(smooth == TRUE){
    p <- p + geom_smooth(alpha = smooth_alpha, span = smooth_span)
  }
  
  p <- p + ggtitle(title)
  p <- p + theme_bw(base_size = fontsize)
  p <- p + theme(plot.title = element_text(hjust = 0.5))
  
  print_plot(p,outfile)
  return(p)
}

#Plots a frequency histogram. The function calculates the frequencies based
#on the raw observations from datacol
plot_histogram =  function(
  df, datacol, breaks_for_x = NULL, trim = TRUE, xlab = NULL, ylab = NULL, type = "frequency", space_between_bars = 0.1,
  xticklab = NULL, limits_for_x = NULL, breaks_for_y = waiver(), limits_for_y = NULL, title = NULL, outfile = NULL,
  fontsize = 22){
  
  n <- nrow(df)
  data <- df[[datacol]]
  
  #Building the frequency dataset
  df <- data.table(table(data))
  setnames(df,c("data","N"),c("label","value"))
  
  if(type == "frequency"){
    if(is.null(ylab)){
      ylab <- "Frequency"
    } 
  }
  else if(type == "cumulative_frequency"){
    df$value = cumsum(df$value)
    if(is.null(ylab)) {
      ylab <- "Cumulative Frequency"
    }
  }
  else if(type == "density"){
    df$value <- df$value / n
    if(is.null(ylab)){
      ylab <- "Density"
    } 
  }
  else if(type == "cumulative_density"){
    df$value <- df$value / n
    df$value <- cumsum(df$value)
    if(is.null(ylab)){
      ylab <- "Cumulative Density"
    } 
  }
  
  if(trim == TRUE & !is.null(limits_for_x)){
    shown_labels <- seq(limits_for_x[1],limits_for_x[2],1)
    df <- df[label %in% shown_labels]
  }
  
  p <- ggplot(df, aes(x=label,y=value))
  
  p <- p + geom_col(colour="black", fill="white", width = 1 - space_between_bars)
  
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

#Plot whatever value is passed to it without performing any computation
plot_barchart = function(
  df, xcol, ycol, groupcol = NULL, flip = FALSE, showvalues = TRUE, values_size = 6, position = "stack", breaks_for_x = NULL, 
  trim = TRUE, xlab = NULL, ylab = NULL, space_between_bars = 0.1, xticklab = NULL, limits_for_x = NULL, 
  breaks_for_y = waiver(), limits_for_y = NULL, title = NULL, show_legend = TRUE, legend_title = groupcol, 
  legend_labels = waiver(), legend_position = "right", outfile = NULL, fontsize = 22){
  
  categories <- df[[xcol]]
  values <- df[[ycol]]
  
  groups <- NULL
  if(!is.null(groupcol)){
    groups <- as.factor(df[[groupcol]])
    groups <- factor(groups, levels=rev(levels(groups)))
    
    p <- ggplot(df, aes(x=categories, y=values, fill = groups, label=values), environment = environment()) 
  }
  else{
    p <- ggplot(df, aes(x=categories, y=values, label=values), environment = environment()) 
  }
  
  p <- p + geom_col(width = 1 - space_between_bars, position = position)
    
  if(showvalues == TRUE){
    if(position == "stack"){
      p <- p + geom_text(size = values_size, position = position_stack(vjust = 0.5))  
    }
    else if(position == "dodge"){
      p <- p + geom_text(size = values_size, position = position_dodge(1 - space_between_bars), vjust = -0.3)  
    }
  }
  
  if(!is.null(breaks_for_x)){
    p <- p + scale_x_continuous(breaks=breaks_for_x)
    p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  }
  else{
    p <- p + coord_cartesian(ylim = limits_for_y)
  }
  
  if(!is.null(xticklab)){
    if(!is.null(groupcol)){
      xticklab <- rev(xticklab)  
    }
    p <- p + scale_x_discrete(labels=xticklab)
  }
  
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  
  p <- p + ggtitle(title)
  p <- p + xlab(xlab) + ylab(ylab)
  p <- p + theme_bw(base_size = fontsize) 
  
  if(flip == TRUE){
    p <- p + coord_flip()
  }
  
  if(!is.null(groupcol)){
    
    if(show_legend == TRUE){
      p <- p + scale_fill_discrete(labels = legend_labels, l = 85, c = 40)
      p <- p + theme(legend.position = legend_position, 
                     legend.margin=margin(t = 0, unit='cm'))
      p <- p + guides(fill = guide_legend(title = legend_title, reverse = FALSE))  
    }
    else{
      p <- p + guides(fill = FALSE)
    }
  }
   
  print_plot(p,outfile)
}

#Plots the frequency (count) of every category in datacol 
#(e.g., datacol = c("A","A","A","B","C")). If a group col is provided, then
#a stacked bars are produced
plot_freqhistogram_categorical = function(
  df, categorycol, groupcol = NULL, binwidth = NULL, breaks_for_y = waiver(), limits_for_y = NULL, title = NULL, 
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

plot_boxplot = function(
  df, xcol = NULL, ycol, groupcol = NULL, facetcol = NULL, xlab = NULL, ylab = NULL, facetlabs = NULL, 
  colored_groups = FALSE, xticklab = NULL, breaks_for_y = waiver(), limits_for_y = NULL, 
  trans_for_y = "identity", flip = FALSE, flip_facet = FALSE, facet_spacing = 1, show_legend = TRUE, 
  legend_title = groupcol, legend_labels = waiver(), legend_position = "right", title = NULL, outfile = NULL, 
  fontsize = 22){
  
  #Prepares aesthetics
  if(is.null(xcol)){
    aesthetics <- aes_string(x = as.factor(""), y = ycol)
  }
  else{
    if(is.null(groupcol)){
      aesthetics <- aes_string(x = xcol, y = ycol)
    }
    else{
      aesthetics <- aes_string(x = xcol, y = ycol, fill = groupcol)  
    }
  }
  
  #If xcol is given, then it should map to a factor column
  if(!is.null(xcol)){
    if(!is.factor(df[[xcol]])){
      #Labels are given in the order they appear in the data (because of 'unique')
      df[[xcol]] <- factor(df[[xcol]], levels = unique(df[[xcol]]))
    }
  }
  
  #Same thing for groupcol
  if(!is.null(groupcol)){
    if(!is.factor(df[[groupcol]])){
      df[[groupcol]] <- factor(df[[groupcol]], levels = unique(df[[groupcol]]))
    }
  }
  
  #Same thing for facetcol
  if(!is.null(facetcol)){
    if(!is.factor(df[[facetcol]])){
      df[[facetcol]] <- factor(df[[facetcol]], levels = unique(df[[facetcol]]))
    }
    if(!is.null(facetlabs)){
      levels(df[[facetcol]]) = facetlabs
    }
  }
  
  p <- ggplot(df, aesthetics, environment = environment()) 
  p <- p + geom_boxplot()
  
  #Deals with groups
  if(!is.null(groupcol)){
    
    #Fill choice
    if(colored_groups == TRUE){
      p <- p + scale_fill_hue(name=legend_title, labels = legend_labels, l = 85, c = 40)
    }
    else{
      p <- p + scale_fill_grey(name=legend_title, labels = legend_labels, start = 0.65, end = 1.0)  
    }
    
    #Show legend
    if(show_legend == FALSE){
      p <- p + guides(fill=FALSE)
    }
  }
  
  #Deals with facets
  if(!is.null(facetcol)){
    if(flip_facet == TRUE){
      p = p + facet_grid(reformulate(".",facetcol))  
    }
    else{
      p = p + facet_grid(reformulate(facetcol,"."))
    }
    
  }
  
  #Limits for the axis
  p <- p + coord_cartesian(ylim = limits_for_y)
  
  #Should flip?
  if(flip == TRUE){
    p <- p + coord_flip(ylim = limits_for_y)
  }
  
  #X scale
  if(!is.null(xticklab)){
    p <- p + scale_x_discrete(labels=xticklab)
  }
  
  #Y-scale is assumed to be continuous
  p <- p + scale_y_continuous(breaks=breaks_for_y, trans = trans_for_y)
  
  #Axis labels
  p <- p + xlab(xlab) + ylab(ylab)
  
  #Title
  p <- p + ggtitle(title)
  
  #Theme Black/White
  p <- p + theme_bw(base_size = fontsize) 
  p <- p + theme(legend.position = legend_position, 
                 legend.box = "horizontal",
                 panel.spacing = unit(facet_spacing, "lines"))
  
  
  print_plot(p,outfile)
  return(p)
}

plot_violin = function(
  df, xcol = NULL, ycol, groupcol = NULL, facetcol = NULL, xlab = xcol, ylab = ycol, facetlabs = NULL, 
  colored_groups = FALSE, xticklab = NULL, trim = TRUE,  split = FALSE, limits_for_y = NULL, breaks_for_y = waiver(), 
  showboxplot = FALSE, boxplot_width = 0.1, dodge_width = 0.9, scale = "area", transformation = "identity", 
  flip = FALSE, flip_facet = FALSE, show_legend = TRUE, legend_title = groupcol, legend_labels = waiver(), 
  legend_position = "right", outfile = NULL, title = NULL, fontsize = 22){
  
  #Sets the dodge (space between plots from the same group)
  dodge <- position_dodge(width = dodge_width)
  
  #Prepares aesthetics
  if(is.null(xcol)){
    aesthetics <- aes_string(x = as.factor(""), y = ycol)
  }
  else{
    if(is.null(groupcol)){
      aesthetics <- aes_string(x = xcol, y = ycol)
    }
    else{
      aesthetics <- aes_string(x = xcol, y = ycol, fill = groupcol)  
    }
  }
  
  #If xcol is given, then it should map to a factor column
  if(!is.null(xcol)){
    if(!is.factor(df[[xcol]])){
      #Labels are given in the order they appear in the data (because of 'unique')
      df[[xcol]] <- factor(df[[xcol]], levels = unique(df[[xcol]]))
    }
  }
  
  #Same thing for groupcol
  if(!is.null(groupcol)){
    if(!is.factor(df[[groupcol]])){
      df[[groupcol]] <- factor(df[[groupcol]], levels = unique(df[[groupcol]]))
    }
  }
  
  #Same thing for facetcol
  if(!is.null(facetcol)){
    if(!is.factor(df[[facetcol]])){
      df[[facetcol]] <- factor(df[[facetcol]], levels = unique(df[[facetcol]]))
    }
    if(!is.null(facetlabs)){
      levels(df[[facetcol]]) = facetlabs
    }
  }
  
  p <- ggplot(df, aesthetics, environment = environment())
  
  #Adds the violin plot
  if(split == FALSE){
    p <- p + geom_violin(position = dodge, scale = scale, trim = trim, colour = "black")  
    
    #Adds the boxplot if requested
    if(showboxplot == TRUE){
      p = p + geom_boxplot(width=boxplot_width, position = dodge, outlier.size = 1, outlier.alpha = 0.5, color = "gray30")  
    }
    #Otherwise, add a point to denote the median
    else{
      p <- p + stat_summary(fun.y=median, geom="point", shape=23, size=3, position = dodge)
    }
  }
  else{
    #Split violin requires aesthetics to be in this order: "x,y,fill", so we make
    #sure aesthetics are given in this order to it
    geom_split_aes = aesthetics["x","y","fill"]
    p <- p + geom_split_violin(groupcol, mapping = geom_split_aes, scale = scale, trim = trim)
    
    if(is.null(groupcol)){
      p <- p + stat_summary(fun.y=median, geom="point", shape=23, size=3, fill = "black", position = dodge)
    }
    else{
      p <- p + stat_summary(
        fun.data="plot.median", geom="errorbar", colour="grey20", width=0.90, size=0.8, position = dodge)  
    }
    
  }
  
  #Deals with groups
  if(!is.null(groupcol)){
    
    #Fill choice
    if(colored_groups == TRUE){
      p <- p + scale_fill_hue(name=legend_title, labels = legend_labels, l = 85, c = 40)
    }
    else{
      p <- p + scale_fill_grey(name=legend_title, labels = legend_labels, start = 0.65, end = 1.0)  
    }
    
    #Show legend
    if(show_legend == FALSE){
      p <- p + guides(fill=FALSE)
    }
    
  }
  
  #Facets choice
  if(!is.null(facetcol)){
    if(flip_facet == TRUE){
      p = p + facet_grid(reformulate(".",facetcol))  
    }
    else{
      p = p + facet_grid(reformulate(facetcol,"."))
    }
    
  }
  
  p <- p + coord_cartesian(ylim = limits_for_y)
  
  #Should flip?
  if(flip == TRUE){
    p <- p + coord_flip(ylim = limits_for_y)
  }
  
  if(!is.null(xticklab)){
    p <- p + scale_x_discrete(labels=xticklab)
  }
  
  # y-scale is assumed to be continuous
  p <- p + scale_y_continuous(breaks=breaks_for_y, trans = transformation)
  
  # labels for the axes
  p <- p + xlab(xlab) + ylab(ylab)
  
  # title for the plot
  p <- p + ggtitle(title)
  
  # black and white theme
  p <- p + theme_bw(base_size = fontsize) 
  p <- p + theme(legend.position = legend_position, legend.box = "horizontal")
  
  print_plot(p,outfile)
  return(p)
}

## custom median function to be used by split violins
plot.median <- function(x) {
  m <- median(x)
  c(y = m, ymin = m, ymax = m)
}

geom_split_violin <- function (
  groupcol, mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., draw_quantiles = NULL, trim = TRUE, 
  scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  
  GeomSplitViolin <- ggproto(
    "GeomSplitViolin",
    GeomViolin, 
    
    groupcol = NULL,
    
    set_groupcol = function(self, newgroupcol){
      self$groupcol <- newgroupcol
    },
    
    draw_group = function(self, data, ..., draw_quantiles = NULL){
      
      data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
      grp <- data[1,'group']  
      
      #Debug
      #View(data)
      #print(paste("group",grp))
      #print(paste("x",data[1,'x']))
      #print(paste("fill",data[1,'fill']))
 
      #This 'if' overrides grp to enable 'one half' violin plots when groupcol does not exist
      if(is.null(self$groupcol)){
        grp <- 1
      }
      
      newdata <- plyr::arrange(transform(data, x = if(grp%%2==1) xminv else xmaxv), if(grp%%2==1) y else -y)
      newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
      newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
      if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
        stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
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
    }
  ) 
  
  GeomSplitViolin$set_groupcol(groupcol)
  
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, 
        show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

#Not working
getBreaksForNumTicks = function(data, numticks, round_to = NULL){
  min_value <- min(data)
  max_value <- max(data)
  if(is.null(round_to)){
    each_tick <- max_value/numticks 
  }
  else{
    each_tick <- round_any(max_value/numticks, round_to)  
  }
  max_tick <- ceiling(max_value/each_tick) * each_tick
  breaks <- seq(min_value,max_tick,each_tick)
  return(breaks)
}

#Prints plot to a file. 
#Creates the sub-dirs in the path (in case they don't exist)
print_plot = function(p, outfile = NULL, width=16, height=9, dpi=100){
  if (!is.null(outfile)){
    dir <- str_extract(outfile,".*/")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    ggsave(outfile, plot = p, width=width, height=height, dpi=dpi)
  }
  return(p)
}