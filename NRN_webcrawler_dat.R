#This script saves information about the references used by reviews in the journal nature reviews neuroscience (NRN)

#(c) Richard Kunert 
#For questions please e-mail RiKunert at gmail dot com
###################################################################################################
# load libraries

if(!require(RCurl)){install.packages('RCurl')} #html-tree parsing
library(RCurl)
options(RCurlOptions = list(useragent = "zzzz"))

if(!require(XML)){install.packages('XML')} #html-tree parsing
library(XML)

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
  
  return(tree)#the parsed html tree is returned
}

###################################################################################################
# extract all relevant data from NRN (Nature Reviews Neuroscience)

#find NRN-issues' web addresses
NRN_ind = web_pre('http://www.nature.com/nrn/archive/index.html')#extract NRN index
iss_nod = getNodeSet(NRN_ind, '//p[@class="issue"]/*')#issue nodes (those whose class attribute is 'issue')
iss_add = sapply(iss_nod, xmlGetAttr, "href")#issues' web addresses (value of the href-attributes for the nodes in iss_nod)
iss_add = rev(iss_add)#reverse order in order to ensure chronological order

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
  rev_add = sapply(rev_nod, xmlGetAttr, "href")#reviews' web addresses (value of the href-attributes for the nodes in rev_nod)
  
  for(r in 1 : length(rev_add)){#for every review of this issue    
    
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
    
    review = web_pre(add)#parse html tree of this review        
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
# save data

save(NRNdat_name, NRNdat_pubyear, NRNdat_startyear,
     NRNdat_endyear, NRNdat_refyears_raw, NRNdat_refyears_adj,       
     file=sprintf("NRNdat_%s.RData", Sys.Date()))
