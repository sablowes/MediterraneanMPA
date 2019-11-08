stack_plot <- function (mob_out, trt_group, prop = FALSE, stretch = TRUE, common_scale = FALSE, 
    label_indiv = TRUE, cols = c("#1b9e77", "#d95f02", "#7570b3"), legend = FALSE, plot=TRUE, uncertainty=FALSE, 
    xlab = '', xlab_top = '', ylab = '', main_title = '') 
{
    tests = mob_out$tests
    SAD = data.frame(type = "SAD", mob_out$SAD[mob_out$SAD$group == 
        trt_group, c("effort_ind", "deltaS_emp", "deltaS_null_low", "deltaS_null_high")])
    N = data.frame(type = "N", mob_out$N[mob_out$N$group == trt_group, 
        c("effort_sample", "ddeltaS_emp", "ddeltaS_null_low", "ddeltaS_null_high")])
    agg = data.frame(type = "agg", mob_out$agg[mob_out$agg$group == 
        trt_group, c("effort_sample", "ddeltaS_emp", "ddeltaS_null_low", "ddeltaS_null_high")])
    names(SAD) = names(N) = names(agg) = c("type", "effort", "effect", 'lower', 'upper')

    virt_effort = seq(min(SAD$effort), max(SAD$effort), length.out = length(agg$effort))
    effort = agg$effort

    SAD_interp = pracma::pchip(SAD$effort, SAD$effect, virt_effort)
    SAD_lower = pracma::pchip(SAD$effort, SAD$lower, virt_effort)
    SAD_upper = pracma::pchip(SAD$effort, SAD$upper, virt_effort)
    N_interp = pracma::pchip(N$effort, N$effect, virt_effort)
    N_lower = pracma::pchip(N$effort, N$lower, virt_effort)
    N_upper = pracma::pchip(N$effort, N$upper, virt_effort)
    SAD = data.frame(type = "SAD", effort = effort, effect = SAD_interp, lower = SAD_lower, upper = SAD_upper)
    N = data.frame(type = "N", effort = effort, effect = N_interp, lower = N_lower, upper = N_upper)
    dat = rbind(SAD, N, agg)

    if(plot==FALSE){return(dat)}
    
    if(plot==TRUE & uncertainty==FALSE){
		plot(unique(dat$effort), dat$effect[dat$type == "SAD"], xlab = xlab, ylab = ylab, 
        col = "#1b9e77", 
		     frame.plot = F, 
		     ylim = c(min(dat$lower), max(dat$effect)), 
		     type='l', lwd=3,
		     type='l', lwd=3,
		     cex.main = 2)
      title(main = main_title, adj = 0, line = 5, cex.main = 2)
    	lines(unique(dat$effort), dat$effect[dat$type == "N"], col = "#d95f02", xaxt=NULL, yaxt=NULL, lwd=3) 
	    lines(unique(dat$effort), dat$effect[dat$type == "agg"], col = "#7570b3", xaxt=NULL, yaxt=NULL, lwd=3)    
    	abline(h=0, lty=2)
	    ticks = axTicks(side = 3)
    	n_indices = round(seq(1, length(virt_effort), length.out = length(ticks)))
	    axis(side = 3, at = effort[n_indices], labels = round(virt_effort[n_indices]))
    	mtext(side = 3, xlab_top, padj = -4, cex = 2)
	    if(legend)
    	legend(0.3*max(dat$effort), 0.9*max(dat$effect), 
    	       legend=c('SAD effect', 'N effect', 'Aggregation\neffect'), 
    	       lty=1, col=c("#1b9e77", "#d95f02", "#7570b3"), bty='n', 
    	       y.intersp = 1.5,
    	       cex=1.5, lwd=3)
    }
    if(plot==TRUE & uncertainty==TRUE){
      plot(unique(dat$effort), dat$effect[dat$type == "SAD"], xlab = xlab, ylab = ylab, 
           col = "#1b9e77", 
           frame.plot = F, 
           ylim = c(min(dat$lower), max(dat$effect)),
           type='l', lwd=3,
           cex.lab = 2,
           cex.axis = 1.5)
      title(main = main_title, adj = 0, line = 5, cex.main = 2)
      polygon(c(unique(dat$effort), rev(unique(dat$effort))), 
              c(dat$lower[dat$type == "SAD"], rev(dat$upper[dat$type == "SAD"])), 
              col='#1b9e77', border = '#1b9e77', density=20)
      polygon(c(unique(dat$effort), rev(unique(dat$effort))), 
              c(dat$lower[dat$type == "N"], rev(dat$upper[dat$type == "N"])), 
              col='#d95f02', border = '#d95f02', density=40)
      polygon(c(unique(dat$effort), rev(unique(dat$effort))), 
              c(dat$lower[dat$type == "agg"], rev(dat$upper[dat$type == "agg"])), 
              col='#7570b3', border = '#7570b3', density=20, angle=-45)
      lines(unique(dat$effort), dat$effect[dat$type == "N"], col = "#d95f02", xaxt=NULL, yaxt=NULL, lwd=3) 
      lines(unique(dat$effort), dat$effect[dat$type == "agg"], col = "#7570b3", xaxt=NULL, yaxt=NULL, lwd=3)    
      abline(h=0, lty=2)
      ticks = axTicks(side = 3)
      n_indices = round(seq(1, length(virt_effort), length.out = length(ticks)))
      axis(side = 3, at = effort[n_indices], labels = round(virt_effort[n_indices]),
           cex.axis = 1.5)
      mtext(side = 3, xlab_top, padj = -2.25, cex = 1.25)
      if(legend)
        legend(0.15*max(dat$effort), 0.95*max(dat$effect), 
               legend=c('SAD effect', 'N effect', 'Aggregation\neffect'), 
               lty=1, col=c("#1b9e77", "#d95f02", "#7570b3"), bty='n', 
               y.intersp = 1.5,
               lwd=3, cex = 1.5)
    }
}


