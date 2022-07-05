# make_links_file3.R

# Load required packages
library(tidyverse)
library(lubridate)
library(urltools)

# Set the year and output filename
# 2009, 2014, 2018 or 2022
the_year <- 2009
output_filename <- "gephi_export_dedup.csv"
  
#  Construct the path to the data file from components
#  in a platform-independent way.
path <- "/Volumes/ARTS_videnskab-dk/Links"
filename <- "extract.csv"
file_to_import <- file.path(path, the_year, filename)
file_to_export <- file.path(path, the_year, output_filename)

# Set the data format
# Define data format of the CSV metadata file
#column_names <- spec(df1)
#  id;arc_harvest;arc_job;crawl_date;wayback_date;hash;url;title;content_type_norm;content_length;links_domains
select_columns <- cols_only(
  id = col_character(),
  arc_harvest = col_integer(),
  arc_job = col_integer(),
  crawl_date = col_character(),
  wayback_date = col_character(),
  hash = col_character(),
  url = col_character(),
  links_domains = col_character()
)

# Read the CSV file
# We import only select columns
print(paste("Reading file:", file_to_import))
df1 <- read_csv(file_to_import, col_types = select_columns, n_max = Inf)

df1_dedup <- df1 %>%   
  # Remove duplicates
  distinct(url, links_domains, .keep_all = TRUE)

df2 <- df1_dedup %>% 
  # We are not interested in documents without an URL
  filter(!is.na(url),
         !is.na(links_domains),
         links_domains != "") %>%
  mutate(domain = domain(url),
         domain_normalised = paste(suffix_extract(domain)$domain, suffix_extract(domain)$suffix, sep = "."),
         dt = ymd_hms(wayback_date), 
         year = year(dt), 
         month = month(dt)
         ) 

df3 <- df2 %>% 
  mutate(link = stringr::str_split(links_domains, "\t")) %>% 
  # we don't want links_domains repeated for all rows, so we remove it
  select(-links_domains) %>%
  # If you have a list-column, unnest() makes each element of the list its own row
  unnest(cols = c(link)) %>% 
  select(Source = domain_normalised, Target = link) %>%
  # remove internal links
  filter(Source != Target) 

# make Gephi compatible data e.g. 
# example:
# website1.dk,website2.dk,1
# website1.dk,website3.dk,3
df_links_count <- df3 %>% 
  group_by(Source, Target) %>% 
  mutate(Count = n()) %>% 
  select(Source, Target, Count) %>%
  arrange(desc(Count), Source)  %>% 
  distinct()

# Writing the result file
print(paste("Writing file:", file_to_export))
write_excel_csv2(df_links_count, file = file_to_export)

