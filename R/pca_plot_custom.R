#' PCA plot
#'
#' This function is a pca plot with .
#' @param pc
#' @param comps
#' @param nc1
#' @param nc2
#' @param colorby
#' @param shapeby
#' @param highlight
#' @param textby
#' @param axisLimits
#' @keywords pca_plot
#' @export
#' @examples
#' pca_plot_custom()

pca_plot_custom = function(pc,comps, nc1, nc2,colorby,shapeby=NULL,
                              highlight=NULL,textby=NULL,axisLimits=NULL, size = 3) {
  require(ggplot2)
  require(ggrepel)
  c1str = paste0("PC", nc1)
  c2str = paste0("PC", nc2)

  if (!is.null(shapeby)){
    scatter<-ggplot(comps, aes_string(c1str, c2str)) +
    geom_point(aes_string(c1str, c2str,color=colorby,shape=shapeby),size= size) + theme_bw() +
    xlab(paste0(c1str, ": ", round(pc$percentVar[nc1] * 100), "% variance")) +
    ylab(paste0(c2str, ": ", round(pc$percentVar[nc2] * 100), "% variance"))+
    theme(legend.text=element_text(size=12))+theme(legend.title=element_blank())
  }else{
    scatter<-ggplot(comps, aes_string(c1str, c2str)) +
    geom_point(aes_string(c1str, c2str,color=colorby),size= size) + theme_bw() +
    xlab(paste0(c1str, ": ", round(pc$percentVar[nc1] * 100), "% variance")) +
    ylab(paste0(c2str, ": ", round(pc$percentVar[nc2] * 100), "% variance")) +
    theme(legend.text=element_text(size=12))+theme(legend.title=element_blank())
  }
  if (!is.null(highlight)){
    scatter<-scatter+geom_label_repel(data=highlight, aes_string(c1str, c2str,label=textby), size=size, show.legend = FALSE,
                                      fontface = 'bold',
                                      box.padding = unit(0.25,"lines"),
                                      point.padding = unit(0.5,"lines"))
  }

  if(!is.null(axisLimits)){
    scatter<-scatter+xlim(axisLimits[[1]])+ylim(axisLimits[[2]])
  }
  scatter
}
