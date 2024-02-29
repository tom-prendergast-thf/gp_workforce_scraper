#### PROJECT SETUP

packages <- c('tidyverse', 'rvest', 'here')

installed_packages <- packages %in% row.names(installed.packages())

if (any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)


# Create parent folders if not present

ifelse(!dir.exists(file.path(here('Raw_data/'))), dir.create(file.path(here('Raw_data/'))), print('Raw data directory already exists'))  

ifelse(!dir.exists(file.path(here('Outputs/'))), dir.create(file.path(here('Outputs/'))), print('Outputs directory already exists'))


# Create sub folders if not present

data_subfolders <- c('CSVs')


lapply(data_subfolders, function(i){
  ifelse(!dir.exists(file.path(here('Raw_data/', i))), dir.create(file.path(here('Raw_data/', i))), print('Directory already exists'))
})

################################################
######## SCRAPE AND DOWNLOAD LATEST DATA #######
################################################


## Identify which CSV files are currently in our directory
current_files <- list.files(here('Raw_data/CSVs/'), pattern='csv')


## Scrape the NHS webpage to identify which months are currently available

workforce_mainpage_link <- 'https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services'  # Link to NHS webpage


monthly_names <- read_html(workforce_mainpage_link) %>%                # Identify which months of data are listed on the web page
  html_nodes(xpath="//a[contains(@class, 'cta__button')]") %>%
  html_text() %>%
  tolower() %>%
  as.data.frame() %>%
  rename(labels = '.')

monthly_links <- read_html(workforce_mainpage_link) %>%                      # Extract links to all the dataset subpages available on the webpage
  html_nodes(xpath="//a[contains(@class, 'cta__button')]/@href") %>%
  html_text() %>%
  paste0("https://digital.nhs.uk",.) %>%
  as.data.frame() %>%
  rename(links = '.')

months_links_df <- cbind(monthly_names, monthly_links) %>%              # Create dataframe joining monthly labels with their links, amd isolate months in an amenable format from
  filter(grepl('general and personal medical services', labels) == FALSE) %>%                           # the end of each link. Use these to create date format versions of each of these labels (useful for ultimate ordering of dataframes) 
  mutate(names_only = sub('https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services/', 
                          '', links)) %>%                               
  mutate(dates_as_dates = lubridate::dmy(names_only)) %>%
  mutate(dates_as_characters = paste0(substr(dates_as_dates, 6, 7), substr(dates_as_dates, 1, 4))) %>%
  filter(names_only != '31-december-2021')



## Compare the list of available data to the data we alread,y have in our directory

checklist_files <- lapply(1:nrow(months_links_df), function(i){                      # Check for monthly files in our raw data directory
  file.exists(paste0('Raw_data/CSVs/', months_links_df$names_only[[i]], '.csv'))
})

month_present_in_files <- sapply(1:length(checklist_files), function(i){      # Create vector describing whether a month is in our data directory
  sum(checklist_files[[i]])
})

months_links_df <- cbind(months_links_df, month_present_in_files) %>%        # Compare existing files to those available online, and filter out any which are already downloaded
  filter(month_present_in_files == 0)

# The function below takes any available monthly datasets which we have not already downloaded, and uses the links we scraped from the main webpage to navigate to that data's 
# publication page. It will then find the individual level zip, unpack it, and deposit the CSV in the CSVs_only folder. 
# Any cases where the csv/dataset page links we're after can't be found or the zips contain more than one CSV are flagged when this function is run.

scrape_download_function <- function(i){

  month <- months_links_df$names_only[[i]]  # Isolate month of interest
  
  date <- months_links_df$dates_as_characters[[i]] # Isolate month name as format featured in CSV names/links
  
  subpage_link <- months_links_df$links[[i]] # Isolate relevant subpage link for our month 
  
  
  # Find all nhsd-a-box-link nodes on our subpage and isolate those linking to the individual-level zip files
  subpage_scrape <- read_html(subpage_link) %>%
    html_nodes(xpath="//a[contains(@class, 'nhsd-a-box-link')]/@href") %>%
    html_text() %>%
    as.data.frame() %>%
    rename(links = '.') %>%
    filter(grepl('.zip', links) == TRUE & grepl('ndividual', links) == TRUE)    # Left off the I just in case they don't capitalise in some months
  
  datasets_link <- subpage_scrape$links[grepl(paste0(date), subpage_scrape$links) == TRUE]  # Double checks that the link matches the month we're labelling it as
  
  if (length(datasets_link) != 1){
    
    print(paste0('Warning: check ', month))   # Tells us if there's more than one link picked up by the function so far - there should only be one 
    
  }else{
    
    temp <- tempfile()   # Creates temporary file where the zip is downloaded saved. From here, we unzip it into a temporary directory, find the csv, and move it to the CSV folder 
    
    download.file(datasets_link, temp)
    
    inside_zip <- unzip(temp, list = TRUE) 
    
    zip_csvs <- inside_zip %>%
      filter(grepl('.csv', Name) == TRUE)
    
    temp_dir <- tempdir()
    
    if (nrow(zip_csvs)!= 1){ print(paste0('Warning: check zip for ', month)) # Stops the function and warns us if there's more than one csv in the zip - in a normal month there's only one 
    }else{ unzip(temp, exdir = temp_dir)
      
      file.rename(paste0(temp_dir, '/', zip_csvs$Name[[1]]), paste0('Raw_data/CSVs/', month, '.csv'))
    }
    
  }
  
}


if(nrow(months_links_df >= 1)){
  lapply(1:nrow(months_links_df), scrape_download_function)     # Feed all undownloaded files available on the NHSD page into the scraping and download function
}

