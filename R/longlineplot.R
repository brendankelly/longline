#' @title Plot longitudinal exposure data.
#' @description Generates a plot with horizontal bar for enrollment period and horizontal lines for exposure duration.
#' @import ggplot2
#' @import grid
#' @import plyr
#' @param long_line_df a data frame formated according to long_line_example
#' @param rect_fill color of fill for bar denoting enrollment period
#' @param rect_color color of line for bar denoting enrollment period
#' @param text_size point size of text for axis and subject labels
#' @param x_label label for x-axis
#' @param y_label label for y-axis
#' @return a ggplot plot object
#' @seealso \code{\link{ggplot}}, \code{\link{geom_segment}}
#' @export
#' @examples
#' longlineplot(long_line_example,"grey40",NA,10,"","Subjects")
longlineplot <- function(long_line_df=longline_example,rect_fill="grey80",rect_color=NA,text_size=12,x_label="Time (Days)",y_label="") {
  long_line_df$exposure_number <- as.numeric(as.factor(long_line_df$exposure))
  long_line_df$subject_label <- long_line_df$subject
  long_line_df <- ddply(long_line_df,.(subject),transform, subject_label=replace(subject_label,seq(length(subject_label)-1),NA))
  long_line_df$subject_label_y <- mean(unique(long_line_df$exposure_number))
  time_range <- diff(range(c(long_line_df$enroll,long_line_df$exit)))
  long_line_df$subject_label_x <- long_line_df$enroll - 0.2*time_range
  p <- ggplot(long_line_df)
  p <- p + geom_rect(aes_string(xmin="enroll",xmax="exit",ymin=0,ymax=max("exposure_number")),fill=rect_fill,colour=rect_color)
  p <- p + geom_segment(aes_string(x="exposure_start",xend="exposure_end",y="exposure_number",yend="exposure_number",colour="exposure"),width=0.5)
  p <- p + geom_text(aes_string(x="subject_label_x",y="subject_label_y",label="subject_label"),hjust=0,size=text_size/3,na.rm=TRUE)  
  p <- p + facet_wrap(~subject,ncol=1,shrink=FALSE)
  p <- p + theme_classic()
  p <- p + theme(axis.ticks.y=element_blank(),axis.line.y=element_blank(),axis.text.y=element_blank())
  p <- p + theme(strip.background=element_blank())
  p <- p + theme(strip.text=element_blank())
  p <- p + theme(panel.background=element_blank())
  p <- p + theme(panel.margin.y=unit(0,"lines"))
  p <- p + theme(text=element_text(size=text_size))
  p <- p + scale_x_continuous(limits=c(min(long_line_df$subject_label_x),max(long_line_df$exit)),breaks=seq(min(long_line_df$enroll),max(long_line_df$exit),length.out=3))
  p <- p + labs(x=x_label,y=y_label)  
  return(p)
}

