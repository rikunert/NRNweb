# NRNweb
A simple R-based web-crawler of Nature Reviews Neuroscience and some analysis of its references

# Structure
The R-code is divided in three parts: data acquisition, data visualisation, write-up

# Data acquisition
All data were acquired using the script NRN_webcrawler_dat.R . Careful, you need a subscription to Nature Reviews Neuroscience for this code to run properly. The data I acquired is stored in NRNdat_[date].RData

# Data visualisation
Data were visualised using the R-script NRN_webcrawler_vis.R . Resulting figures are called NRN_FigX.png

# Write-up
The blog post was written up using a R-markdown script called NRN_post_brainsidea.Rmd . The final version was published on Brain's Idea [here](https://brainsidea.wordpress.com/2017/01/05/how-to-write-a-nature-style-review/).
