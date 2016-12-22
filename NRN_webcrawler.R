#This program looks at the properties of references in the journal nature reviews neuroscience (NRN)

#(c) Richard Kunert 
#For questions please e-mail RiKunert at gmail dot com

###################################################################################################
# load libraries

if(!require(RCurl)){install.packages('RCurl')} #html-tree parsing
library(RCurl)
options(RCurlOptions = list(useragent = "zzzz"))

if(!require(XML)){install.packages('XML')} #html-tree parsing
library(XML)

if(!require(Hmisc)){install.packages('Hmisc')}# correlations
library(Hmisc)

if(!require(MASS)){install.packages('MASS')}# robust correlations
library(MASS)

if(!require(psychometric)){install.packages('psychometric')}# robust correlations
library(psychometric)

###################################################################################################
# custom functions

#web_pre takes a webpage of address web_add and pre-processes it
#input: website address
#output: parsed html tree

web_pre = function(web_add){
  
  web_nod <- getURL(web_add)#load the webpage
  
  #sometimes error called 'transfer closed with outstanding read data remaining'
  #probably due to suboptimal internet connection.
  
  web_nod <- readLines(tc <- textConnection(web_nod))# Process escape characters
  close(tc)
  
  tree <- htmlTreeParse(web_nod, error=function(...){}, useInternalNodes = TRUE)# Parse the html tree, ignoring errors on the page
  
  return(pagetree)#the parsed html tree is returned
}

###################################################################################################
# extract all relevant data from NRN (Nature Reviews Neuroscience)

#find NRN-issues' web addresses
NRN_ind = web_pre('http://www.nature.com/nrn/archive/index.html')#extract NRN index
iss_nod = getNodeSet(NRN_ind, '//p[@class="issue"]/*')#issue nodes (those whose class attribute is 'issue')
iss_add = sapply(iss_nod, xmlGetAttr, "href")#issues' web addresses (value of the href-attributes for the nodes in iss_nod)

#loop through every issue and extract relevant data

##preallocate for variables for speed
NRNdat_name = list();
NRNdat_pubyear = numeric(1000);#
NRNdat_startyear = numeric(1000);#
NRNdat_endyear = numeric(1000);#
NRNdat_refyears_raw = rep(-1, 1e+7);#initially filled with impossible number: -1, this way even zeros can be meaningful
NRNdat_refyears_adj = rep(-1, 1e+7);#initially filled with impossible number: -1, this way even zeros can be meaningful
counter_rev = 0;
counter_ref = 0;

for(i in 1:length(iss_add)){#for every issue of NRN
  
  #find individual reviews' web addresses in this issue of NRN
  iss = web_pre(paste(c("http://www.nature.com", iss_add[i], "index.html#rv"), sep="", collapse=""))#preprocess review web address (notice '#rv')  
  rev_nod = getNodeSet(iss, '//li[@class="full-text"]/*')#nodes indicative of a full-text link  
  rev_add = sapply(reviews_nod, xmlGetAttr, "href")#reviews' web addresses (value of the href-attributes for the nodes in rev_nod)
  
  for(r in 1 : length(reviews_addresses)){#for every review of this issue    
    
    #Construction of this issue's web-address is complicated
    #by the fact that sometimes the link on the issues page
    #points to a finder function.
    if(regexpr('uidfinder',rev_add[r]) > 0){#finder function case
      
      add = paste(c("http://www.nature.com",iss_add[i],"full/",
                                substr(rev_add[r], regexpr('nrn', rev_add[r]), nchar(rev_add[r])), ".html"),
                                sep="", collapse="")
      
    }else{#not the finder function case
      
      add = paste(c("http://www.nature.com", rev_add[r], ""), sep="", collapse="")
      
    }
    
    rev = web_pre(add)#parse html tree of this review        
    ref = getNodeSet(review, '//p[@class="details"]')#relevant info about references (nodes in the reference section with class attribute details)
    
    #Fill variables initialised earlier. However, this only makes sense if there is relevant information.
    if(length(ref) > 0) {#if there are actually any references in this review
            
      years_raw = xpathApply(ref[[1]], '//span[@class="cite-month-year"]', xmlValue)#for each node in ref: look for span with attribute class ='cite-month-year', extract the values
      #Note: the first node with year is the publication date of the review. Conveniently, it returns a NA because it includes a month.
      refyears  = as.numeric(years_raw)#turn years_raw into numbers
      pubyear   = as.numeric(substr(years_raw[[1]], nchar(years_raw[[1]])-5, nchar(years_raw[[1]]) - 1))#publication year of this review
      
      #save data for visualisation and analysis later
      NRNdat_name                                                                = c(NRNdat_name, add)#add webaddress of this review
      NRNdat_pubyear[counter_rev]                                                = pubyear#publication year of this review
      NRNdat_refyears_raw[(counter_ref + 1) : (counter_ref + length(refyears))]  = refyears#publication years of the references in this review
      NRNdat_refyears_adj[(counter_ref + 1) : (counter_ref + length(refyears))]  = (pubyear - refyears)#age of references at review publication
      NRNdat_startyear[counter_rev]                                              = counter_ref + 1#index of this review's first reference
      NRNdat_endyear[counter_rev]                                                = counter_ref + length(ref)#index of this review's last reference
      
      #update counters
      counter_rev = counter_rev + 1    
      counter_ref = counter_ref + length(refyears)
      
    }else {print('No references found in:')}
        
    #display publication years just so that the user knows where we are in the programme
    print(add)
    print(pubyear)
    
  }
}

###################################################################################################
# visualise data
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