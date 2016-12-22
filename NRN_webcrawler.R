#This program looks at the properties of references in the journal nature reviews neuroscience (NRN)
#The aim is to establish that NRN suffers from a recency bias, i.e. that the older a paper is, the less it is cited in NRN

#(c) Richard Kunert 
#For questions please e-mail RiKunert at gmail dot com

#set working directory to where publications_1975-2012.csv is
setwd("C:/Users/Richard/Desktop/Psychology_UvA/R")

#############################################
# load libraries needed for html-tree parsing 
require(RCurl)
options(RCurlOptions = list(useragent = "zzzz"));
require(XML)

###################################################################################################
#All the webpages to be loaded need to be pre-processed in the same way, create a function for that
webpage_preprocessing = function(webpage_address)
{
  #load the webpage
  webpage_nodes <- try(getURL(webpage_address))#sometimes this function throws an error called 'transfer closed with outstanding read data remaining', I believe it is to do with a suboptimal internet connection
  # Process escape characters
  webpage_nodes <- readLines(tc <- textConnection(webpage_nodes)); close(tc)
  # Parse the html tree, ignoring errors on the page
  pagetree <- htmlTreeParse(webpage_nodes, error=function(...){}, useInternalNodes = TRUE)
  
  return(pagetree)#the parsed html tree is returned
}

#################################
#get all the NRN issues' webpages
NRN_index = webpage_preprocessing('http://www.nature.com/nrn/archive/index.html')
#extract nodes of interest (those whose class attribute is 'issue')
issues_nodes = getNodeSet(NRN_index,'//p[@class="issue"]/*')
#extract the value of the href-attributes for the nodes in issues_nodes
issues_addresses = sapply(issues_nodes,xmlGetAttr,"href")

##############################################################################################################
#Loop through all the issues and within each issue through all the reviews and extract relevant reference data

#Initialise some variables
NRNdata_name = list();
NRNdata_pubyear = numeric(1000);#preallocate for speed
NRNdata_startyear = numeric(1000);#preallocate for speed
NRNdata_endyear = numeric(1000);#preallocate for speed
NRNdata_refyears_raw = rep(-1,1e+7);#preallocate for speed (I fill the vector with an impossible number: -1, this way even zeros can be meaningful)
NRNdata_refyears_adjusted = rep(-1,1e+7);#preallocate for speed (I fill the vector with an impossible number: -1, this way even zeros can be meaningful)
counter_pubyear = 0;
counter_refyears = 0;
for(i in 1:length(issues_addresses)){#for every issue of NRN
  
  issue = webpage_preprocessing(paste(c("http://www.nature.com",issues_addresses[i],"index.html#rv")
                                      , sep="", collapse=""))#note that this address focusses on reviews through '#rv'
  # Find all the nodes indicative of a full-text link
  reviews_nodes = getNodeSet(issue,'//li[@class="full-text"]/*')
  #extract the value of the attribute href in each revew node, this value is the webpage of the review article
  reviews_addresses = sapply(reviews_nodes,xmlGetAttr,"href")
  
  for(r in 1:length(reviews_addresses))#for every review of this issue
  {    
    
    if(regexpr('uidfinder',reviews_addresses[r])>0){#if the link on the issues page is to a finder function, webpage_preprocessing doesn't work. In this case construct correct webpage as follows:
      current_address = paste(c("http://www.nature.com",issues_addresses[i],"full/",
                                substr(reviews_addresses[r],regexpr('nrn',reviews_addresses[r]),nchar(reviews_addresses[r]))
                                , ".html")
                              , sep="", collapse="")
    }else{#if link on issues page is not to a uidfinder site
      current_address = paste(c("http://www.nature.com",reviews_addresses[r],"")
                              , sep="", collapse="")
    }
    
    review = webpage_preprocessing(current_address)
    # Find all the nodes in the reference section which include the crucial reference info
    references = getNodeSet(review,'//p[@class="details"]')
    
    if(length(references)>0) {#if there are actually any references in this review, some perspective articles are without references and thus throw an error
      # Find all the nodes in the reference section which include a publication year 
      #careful, the first one is the publication date of the article, conveniently, it returns a NA because it includes a month
      #Look for nodes in references of the form span with attribute class ='cite-month-year', extract the values; do this with each node you find
      years_raw = xpathApply(references[[1]], '//span[@class="cite-month-year"]', xmlValue)
      refyears = as.numeric(years_raw)#turn years_raw into numbers  
      pubyear = as.numeric(substr(years_raw[[1]],nchar(years_raw[[1]])-5,nchar(years_raw[[1]])-1))#publication year of this review    
      
      #save data for visualisation and analysis later
      NRNdata_name = c(NRNdata_name, current_address)
      NRNdata_pubyear[counter_pubyear] = pubyear
      NRNdata_refyears_raw[(counter_refyears+1):(counter_refyears+length(refyears))]=refyears
      NRNdata_refyears_adjusted[(counter_refyears+1):(counter_refyears+length(refyears))] = (pubyear - refyears)
      NRNdata_startyear[counter_pubyear] = counter_refyears+1
      NRNdata_endyear[counter_pubyear] = counter_refyears+length(refyears)
      counter_pubyear = counter_pubyear+1    
      counter_refyears = counter_refyears+length(refyears)
      
    }else {print('No references found in:')}
    
    
    #display publication years just so that the user knows where we are in the programme
    print(current_address)
    print(pubyear)
  }
}

###############################
#Visualise and Analyse the data

#is there pressure to include less and less references?
layout(1)#one plot in the middle of the screen
y = diff(NRNdata_startyear[NRNdata_startyear>0]) - 1#the number of references in each issue, minus one because first number is year of review itself
x = 1:length(y)#number of review
plot(x,y, xlab='Review #', ylab = 'References')#scatter plot
text(770,280, sprintf('r = %.2f (p= %.3f)', cor(x,y), cor.test(x,y,method = 'pearson')$p.value))#info box with strength and significance of association, p-value is rounded
abline(lm(y~x))#bet fit line

#Raw histogram of publication years in all references of NRN
layout(1)
xlimits = c(2012,1800)#note that most recent year is on left, this is done to ensure comparability with the following plots
hist(NRNdata_refyears_raw[NRNdata_refyears_raw>min(xlimits)],
     xlab='Publication year', main = "Raw Publication Year Histogram",
     ylab='Frequency',col = 'green',xlim=xlimits, breaks = abs(xlimits[1]-xlimits[2]))

#Histogram of age of references in NRN
layout(1)
xlimits = c(0, 50)
h = hist(NRNdata_refyears_adjusted[NRNdata_refyears_adjusted<max(xlimits) & NRNdata_refyears_adjusted>-1],
         xlab='Publication Age', main = "Publication Year Histogram adjusted for Review Publication Time",
         ylab='Frequency',col = 'green',xlim=xlimits, breaks = abs(xlimits[1]-xlimits[2]))

#Histogram of publication rate in Neuroscience
layout(1)
adj = read.csv('publications_1975-2012.csv')#found through WoS search
bp1 = barplot(as.double(adj[37:1,6])*1000, main = 'Neuroscience Publication rate', ylab = 'Count', xlab = 'Year',
              ylim = c(0,50000), axes = T)# all publications
bp2 = barplot(as.double(adj[37:1,11])*1000, main = 'Neuroscience Publication rate', ylab = 'Count', xlab = 'Year',
              col = 'white', add = T, ylim = c(0,10000))# reviews
axis(1, at = bp1[seq(1, 37, 5)], labels=adj[seq(37,1,-5),1])#add x-axis ticks


#Development of age of references in NRN - within each publication
layout(matrix(1:10,2,5))#organise the histograms in a 2x6 array
percentile = seq(0,100,10)#Matlab code = 1:10:100
age = 1:49;
xlimits = c(0, 50)
coefficients = numeric()
for(p in 1:10){#for every plot, i.e every decile
  
  NRN_data_seyear_percentile_length = (NRNdata_endyear-NRNdata_startyear)/length(percentile)  
  
  data_region_extracted = numeric()
  for(x in 1:length(NRN_data_seyear_percentile_length)){#for each review
    
    data_region_extracted_x = (NRNdata_startyear[x] + NRN_data_seyear_percentile_length[x]*(p-1)):
      (NRNdata_startyear[x] + NRN_data_seyear_percentile_length[x]*p)#this review's indexes for this decile
    
    data_region_extracted = c(data_region_extracted, data_region_extracted_x)
  }
  
  refyears_adjusted_extracted = NRNdata_refyears_adjusted[data_region_extracted]#subset of NRNdata_refyears_adjusted indicated by data_region_extracted
  data_region = which(refyears_adjusted_extracted<max(xlimits) & refyears_adjusted_extracted>-1)#indexing of interest
  
  h = hist(refyears_adjusted_extracted[data_region],
           xlab='Publication Age', main = as.character(percentile[p]),
           ylab='Frequency',col = 'green',xlim=xlimits, ylim =c(0,2000), breaks = abs(xlimits[1]-xlimits[2]))
  
  #add a little statistical analysis and display the result in the plot
  #I tried out some models, and without fail the following was the best one
  m1 = lm(h$counts~log(age))#P(publication) as a function of age
  
  pred=predict(m1,interval="conf",newdata=data.frame(age))  
  lines(age,pred[,1],lty=2,col = 'red')
  
  #save for next step
  coefficients = c(coefficients, coefficients(summary(m1))[2])
}

#Development of age of references in NRN - within each publication (model-based summary)
layout(1)#organise the histograms in a 2x6 array
plot(coefficients, xlab = 'decile', ylab = 'decline rate')

#Development of age of references in NRN - over the years adjusted for publication rate in each year
adj = read.csv('publications_1975-2012.csv')
layout(matrix(1:12,2,6))#organise the histograms in a 2x6 array
year = 2000:2011#only full years considered
age = 1:26

for(p in 1:12){#for every plot
  
  xlimits = c(0, 25)
  
  data_region_range = min(NRNdata_startyear[NRNdata_pubyear == year[p]]):max(NRNdata_endyear[NRNdata_pubyear == year[p]])#range of indexes in NRNdata_refyears of interest for this histogram
  refyears_adjusted_range = NRNdata_refyears_adjusted[data_region_range]
  refyears_raw_range = NRNdata_refyears_raw[data_region_range]
  data_region = which(refyears_adjusted_range<max(xlimits) & refyears_adjusted_range>-1)  
  
  adjusted_count_total=numeric()
  count_total = numeric()
  for(y in year[p]:(year[p]-25)) {
    adjusted_count = sum(refyears_raw_range[data_region]==y,na.rm = T)/(adj[adj[,1]==y,6]*1000)
    adjusted_count_total = c(adjusted_count_total, adjusted_count)
    count = sum(refyears_raw_range[data_region]==y,na.rm = T)
    count_total = c(count_total, count)
  }
  
  bp = barplot(adjusted_count_total,
               xlab='Publication Age', main = as.character(year[p]),
               ylab='P(Cited)',col = 'blue',xlim=xlimits, ylim=c(0,0.08))
  axis(1,at = bp[seq(1, 25, 5)], labels=seq(0,24,5))#add x-axis ticks
  
  #add a little statistical analysis and display the result in the plot
  #Here the best model is differnet between different plots, so one of the two best contenders is chosen
  m1 = lm(adjusted_count_total~age)#P(publication) as a function of age
  m2 = lm(adjusted_count_total~age+I(age^2))#P(publication) as a function of age and age squared
  
  if(anova(m1,m2)$'Pr(>F)'[2]<.05 & summary(m1)$r.squared < summary(m2)$r.squared){#if polynomial model is significantly better than linear model
    text(10,0.060, labels = sprintf('Best fit: \npolynomial model. \nRsq = %.2f\n f(P)=%.2f+\n(%.4f*age)+\n(%.4f*(age^2) \np=%.2f, p=%.2f',
                                    summary(m2)$r.squared, coefficients(m2)[1],coefficients(m2)[2], coefficients(m2)[3],coef(summary(m2))[11], coef(summary(m2))[12])         
         , col = 'black')
    pred=predict(m2,interval="conf",newdata=data.frame(age))
    
  }else{#if both models equally good or linear model even better
    text(10,0.060, labels = sprintf('Best fit: \nlinear model. \nRsq = %.2f\n f(P)=%.2f+\n(%.4f*age) \np=%.2f',
                                    summary(m1)$r.squared, coefficients(m1)[1],coefficients(m1)[2],coef(summary(m1))[8])
         , col = 'black')
    pred=predict(m1,interval="conf",newdata=data.frame(age))    
  }  
  lines(age,pred[,1],lty=2,col = 'green')
}