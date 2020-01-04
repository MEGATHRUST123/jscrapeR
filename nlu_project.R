# Create a NLU project based on company review on jobstreet
# create my own  scraper any company
# website - job street

library(httr)
library(xml2)
library(rvest)
library(purrr)

##################################### scraping function #####################################
# task - create a R package by uploading it to github
# gdscrapeR
# install.packages('devtools')
# devtools::install_github('mguideng/gdscrapeR')
# library(gdscrapeR)

# clean_string
clean_string <- function(string){
  # lowercase string 
  s <- tolower(string)
  s <- gsub('\\$','dollar', s)
  s <- gsub('[[:digit:]]+', ' ', s)
  s <- gsub('[[:punct:]]+', ' ', s)
  s <- trimws(s)
  s <- gsub("\\s+", " ",s)
  return(s)
}

# scrape data from website
scrape_jobstreet <- function(company_name) {

  base_url <- 'https://www.jobstreet.com.sg/en/companies/'
  
  # look for the companies
  url_company_name <-gsub(' ','%20',company_name)
  
  # search results
  # function that scrapes company name and company number
  # browse reviews - https://www.jobstreet.com.sg/en/companies/browse-reviews?q=ocbc
  # browse reviews - https://www.jobstreet.com.sg/en/companies/browse-reviews?q=ocbc%20bank
  jobSearchUrl <- paste0(base_url,'browse-reviews?q=',url_company_name)
  
  # get the company name and company number of all the search listings
  jobsearchResults <- read_html(jobSearchUrl) %>%
                      html_nodes('a') %>%
                      html_attr('href') %>%
                      trimws()
  # filter urls that begins with /en/companies/
  # does not end with numbers
  # https://www.jobstreet.com.sg/en/companies/475290-dbs-bank-limited/reviews
  jobsearchResults <- unique(jobsearchResults[!grepl('/en/companies/(.*)[0-9]$',jobsearchResults) & 
                                        grepl('/en/companies/',jobsearchResults) & 
                                       !grepl('browse|jobstreet',jobsearchResults)])
  
  # gather all 
  review_base_url = 'https://www.jobstreet.com.sg'
  jobstreet_link <- paste0(review_base_url,jobsearchResults[1],'/','reviews')
  
  totalReviews <- read_html(jobstreet_link) %>%
                  html_nodes('h2') %>%
                  html_nodes('strong') %>%
                  html_text() %>%
                  trimws() %>%
                  as.numeric() 
  
  totalReviews <- totalReviews[!is.na(totalReviews)]
  
  print(paste0(company_name,' has ',totalReviews,' reviews'))
                  
  # jobstreet reviews:
  # Number of pages to look through
  numPages <- as.integer(ceiling(totalReviews/10))
  
  # Create empty list 
  listofDF <- list()

# Create data frames for: Date, Summary, Rating, Title, Pros, Cons, Helpful
# <p class="_1_dikBfyioZMrCoMA4C9hZ"><span>Jan 2020</span></p>
# <p class="_1xPYIUfA9IwtVyBsOt_SaR">KYC Analyst</p>
# <div class="_3oDy3INXM4D5NEhRbMgqjy" style="height: auto;"><div>It is great to work for you because you are a big company and I will add to you from my experiences It is wonderful to work with you because you are a big company and I will add to you from my experiences and I will add a great addition to your company because I have the qualifications to work in your company.</div></div>
# <div class="_3oDy3INXM4D5NEhRbMgqjy" style="height: auto;"><div>Difficulties in work must be overcome and I am sure that work with you will not have any difficult challenges and if any we will overcome them.</div></div>

# Extract all data from all pages 
  for(i in 1:numPages){
  
    print(paste0('page: ',i))
  # update url to extract from the right page
  # ?page=2
    if(i == 1){
      pageUrl <- jobstreet_link
    }else{
      pageUrl <- paste(jobstreet_link,'?page=',i,sep='')
    }
    
    # date 
    dateofReview <- read_html(pageUrl) %>%
                    html_nodes('p._1_dikBfyioZMrCoMA4C9hZ') %>%
                    html_text() %>%
                    trimws()
    
    # rating
    # <span class="_2s16Kji2BFASw8VCHpxUzG">5 out of 5</span>
    reviewRating <- read_html(pageUrl) %>%
                    html_nodes('div') %>%
                    html_nodes('span._2s16Kji2BFASw8VCHpxUzG') %>%
                    html_text()
    reviewRating <- gsub(' out of 5','',reviewRating)
    # remove rating with decimals and is 0
    reviewRating <- reviewRating[!grepl('[0-9].',reviewRating) & !grepl('0',reviewRating)]
    
    # position
    position <- read_html(pageUrl) %>%
      html_nodes('p._1xPYIUfA9IwtVyBsOt_SaR') %>%
      html_text() %>%
      trimws()
    
    # the good things
    goodThings <- read_html(pageUrl) %>%
      html_nodes('div') %>%
      html_nodes('#good-review') %>%
      html_nodes('div') %>%
      html_nodes('div') %>%
      html_text() %>%
      trimws()
    
    # the challenges
    # .challange-review._3oDy3INXM4D5NEhRbMgqjy
    challenges <- read_html(pageUrl) %>%
      html_nodes('div') %>%
      html_nodes('#challange-review') %>%
      html_nodes('div') %>%
      html_nodes('div') %>%
      html_text() %>%
      trimws()
    
    # create a dataframe for one page
    df = data.frame('date_of_review' = dateofReview, 'rating' = reviewRating, 'position' = position, 'good_review' = goodThings, 'challenges' = challenges)
    
    listofDF[[i]] = df
  }

  # combine the dataframes together 
  output <- do.call("rbind", listofDF)
  output['company'] <- company_name
  output['position'] <- apply(output['position'], 1, clean_string)
  output['good_review'] <- apply(output['good_review'], 1,  clean_string)
  output['challenges'] <- apply(output['challenges'], 1, clean_string)
  return(output)
}

######################################## NLU function ########################################


##############################################################################################