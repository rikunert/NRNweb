#This script visualises information about the references used by reviews in the journal nature reviews neuroscience (NRN)

#(c) Richard Kunert 
#For questions please e-mail RiKunert at gmail dot com

###################################################################################################
# load libraries

if(!require(Hmisc)){install.packages('Hmisc')}# correlations
library(Hmisc)

if(!require(MASS)){install.packages('MASS')}# robust correlations
library(MASS)

if(!require(psychometric)){install.packages('psychometric')}# robust correlations
library(psychometric)

###################################################################################################
# load libraries

#to be done


###################################################################################################
# visualise data

########################################################
#prepare general look (very clean)

theme_set(theme_bw(18)+#remove gray background, set font-size
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  legend.key = element_blank(),
                  legend.title = element_blank(),#remove all sorts of lines for a cleaner look
                  legend.position = 'top',#specify the legend to be on top
                  legend.direction = 'vertical'))#specify the legend to be arranged vertically

########################################################
#FIGURE 1: development of number of references over time

#prepare data
y = diff(NRNdat_startyear[NRNdat_startyear>0]) - 1#the number of references in each issue, minus one because first number is year of review itself
x = 1:length(y)
dat_corr = data.frame("x" = 1:length(y),#x-axis: number of review
                      "y" = y)#y-axis

fig1 = ggplot(dat_corr, aes(x = x, y = y))+
  geom_point(size = 2) +#add points
  stat_smooth(method = "lm", size = 2, se = FALSE,
              aes(colour = "least squares regression")) +
  stat_smooth(method = "rlm", size = 2, se = FALSE,
              aes(colour = "robust regression")) +
  labs(x = "Number of review", y = "Reference count") +#axis labels
  scale_color_grey()+#colour scale for lines
  stat_smooth(method = "lm", size = 2, se = FALSE,
              aes(colour = "least squares regression"),
              lty = 2) +
  stat_smooth(method = "lm", size = 2, se = FALSE,
              aes(colour = "least squares regression"),
              lty = 2)

#add annotation
Pea_r = rcorr(dat_corr$x, dat_corr$y, type = "pearson")
Spe_r = rcorr(dat_corr$x, dat_corr$y, type = "spearman")
text_plotting = data.frame(x = Inf, y = -0.1,
                           t = sprintf("Pearson r = %s\nSpearman rho = %s",
                                       gsub("0\\.","\\.", sprintf('%1.2f', Pea_r$r[1,2])),#r-value without trailing zero
                                       gsub("0\\.","\\.", sprintf('%1.2f', Spe_r$r[1,2]))),#rho-value without trailing zero
                           hjust = 1, vjust = 0.5)
fig1 = fig1 + 
  geom_text(data = text_plotting,
            aes(x=x,y=y,hjust=hjust, vjust = vjust, label=t),
            size = 5)#annotation

fig1

########################################################                    
#FIGURE 2: distribution of the age of referred to scholarly articles

dat_hist = data.frame("x" = NRNdata_refyears_adj[
  NRNdata_refyears_adj < 50 &
    NRNdata_refyears_adj > -1])#x-axis: age of referred to scholarly articles (within sensible range)

fig2 = ggplot(dat_hist, aes(x=x)) +
  geom_histogram(color = "black", fill = "gray50") +#add histogram
  labs(x = "Age of reference", y = "Count")+ #add axis titles
  scale_x_continuous(limits = c(0, 50)) +#restrict x-axis range
  ggtitle(" ")#add title

fig2

########################################################                    
#FIGURE 3: distribution of the age of referred to scholarly articles in different parts of each review

layout(matrix(1:4, 1, 4))#organise the histograms in a 1x4 array
coefficients = numeric()#model fit coefficients

for(p in 1:4){#for every plot, i.e every quarter in a review
  
  NRN_dat_ref_count = (NRNdat_endyear - NRNdat_startyear) / 4#number of references in this quarter
  
  NRNdat_startendyear = numeric()
  
  for(r in 1 : length(NRN_dat_ref_count)){#for each review
    
    NRNdat_startendyear_r = (NRNdat_startyear[r] + NRN_dat_ref_count[r] * (p-1)):
      (NRNdat_startyear[r] + NRN_dat_ref_count[r] * p)#this review's indexes for this quarter
    
    NRNdat_startendyear = c(NRNdat_startendyear, NRNdat_startendyear_r)#append to other reviews
  }
  
  refyears_adj_ext = NRNdat_refyears_adj[NRNdat_startendyear]#subset of reference ages which are in this quarter
  
  dat_hist = data.frame("x" = refyears_adj_ext[
    NRNdata_refyears_adj_ext < 50 &
      NRNdata_refyears_adj_ext > -1])#x-axis: age of referred to scholarly articles (within sensible range)
  
  fig3x = ggplot(dat_hist, aes(x = x)) +
    geom_histogram(color = "black", fill = "gray50", binwidth = 1) +#add histogram
    labs(x = "Age of reference", y = "Count")+ #add axis titles
    scale_x_continuous(limits = c(0, 50)) +#restrict x-axis range
    scale_y_continuous(limits = c(0, 2000)) +#restrict x-axis range
    ggtitle(sprintf("Quarter within review: %d", p))#add title
  
  #add model fit
  age = 1 : 49
  h = hist(refyears_adj_ext[refyears_adj_ext < max(50) & refyears_adj_ext > -1], breaks = 50)#histogram function provides bin counts
  m1 = lm(h$counts ~ log(age))#model bin counts as a function of reference logged age (log-model is best fitting model after trying out a few)  
  pred = predict(m1, interval="conf", newdata=data.frame(age))  
  
  fig3x = fig3x + 
    geom_line(x = age, y = pred[1], linetype = 3, colour = "gray50")
  
  fig3x
  
  #save for next step
  coefficients = c(coefficients, coefficients(summary(m1))[2])
}


########################################################                    
#FIGURE 4: development of model fit coefficient ('decline rate') over parts of reviews
dat_vis = data.frame(x = 1:4, y = coefficients)
fig4 = ggplot(dat_vis, aes(x = x, y = y)) +
  stat_smooth(method = "loess", size = 2, se = FALSE) +#add loess line (a smooth fit)
  geom_point(size = 2) +#add points
  labs(x = "Quarter within review", y = "Decline rate") +#axis labels
  scale_color_grey()#colour scale for lines