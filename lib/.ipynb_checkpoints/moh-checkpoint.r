DATASET_FOLDER <- 'input'
BASE_MOH_URL <- 'https://www.health.govt.nz/our-work/mental-health-and-addiction/mental-health-and-addiction-monitoring-reporting-and-data'

get_filename <- function(filename){ # gets a filename with correct path
    return(paste(DATASET_FOLDER, filename, sep='/')) # concatenates a base path + file name
}


get_moh_file_data <- function(){ # gets MOH data files from 2011 to 2020 by scraping the MOH mental health dataset list.
     mental_health_data_urls_all_new_years <- BASE_MOH_URL %>%  
                                              read_html() %>% # reads MOH base url 
                                              html_element('#node-12245 > div > div > div > ul:nth-child(19)') %>% # finds list to scrape
                                              html_elements("a") %>%  # gets all links
                                              html_attrs() %>%  # gets the url path
                                              str_c('https://www.health.govt.nz/', .) # adds base_url to uris gathered in links
    
    file_metadata_df <- data.frame(year=double(), # creates empty dataset to add information about files
                                   file_name=character(),
                                   url=character(),
                                   separator=character())

    start_year = 2020 # we will start in 2020 and go down to 2014
    counter = 0
    for (url_yearly_health in mental_health_data_urls_all_new_years){ # reading all urls from the dataset list
        file_name = str_c('mental_health', start_year) %>% 
                        str_c('.xlsx') # creating dataset name by year
        url_to_download = url_yearly_health 
        if (counter == 4){ # exception for error in HTML list, the 2016 value is the same as 2017 so we have to add it manually.
            url_to_download = 'https://www.health.govt.nz/publication/mental-health-and-addiction-service-use-2015-16'
        }
        download_xlsx_file(url_to_download, file_name) # downloads the file
        separator <- get_sheet_separator(get_filename(file_name)) # parses the sheets to figure out what is the separator for sheet name
        file_metadata_df <- file_metadata_df %>% add_row(year=start_year, # adds the data to the dartaset
                                                         file_name=file_name,
                                                         url=url_to_download,
                                                         separator=separator)
        start_year = start_year - 1
        counter= counter + 1

    }
    #we add some custom years to the list, this years are not part of any list as they have been revisited in 2020, so we will add them manually to our dataframe.
    file_metadata_df <- file_metadata_df %>% add_row(year=2011:2013, 
                                                     file_name=c('mental_health2011.xlsx',
                                                                 'mental_health2012.xlsx',
                                                                 'mental_health2013.xlsx'),
                                                     url=c('https://www.health.govt.nz/system/files/documents/publications/mh_201011_revised_mar2020.xlsx',
                                                          'https://www.health.govt.nz/system/files/documents/publications/mh_201112_revised_mar2020.xlsx',
                                                          'https://www.health.govt.nz/system/files/documents/publications/mh2012-13_revisedmarch2020.xlsx'),
                                                     separator='&')
    get_unknown_years(file_metadata_df %>% filter(year<=2013)) # downloads years 2011 to 2013 excel files .

    return(file_metadata_df) # returns metadata for posterior usage.
}

get_legacy_files <- function(){ # Scrapes MOH page to get links for datasets froom 2001 to 2007
    url <- 'https://www.health.govt.nz/publication/mental-health-service-use-new-zealand-2001-02-2006-07'
    mental_health_data_urls_all_new_years <- url %>% 
                                              read_html() %>% # scrape MOH 2001 to 2007 dataset list
                                              html_element('.view-file-attachments-revisioning') %>% # find attachments
                                              html_elements("a") %>% # get links
                                              html_attrs() %>% str_c('', .) # do not concatenate anything, leave link as it is
    start_year = 2002 # we start from 2002 and will go all the way to 2008
    for (donwload_url in mental_health_data_urls_all_new_years){
        download.file(url=donwload_url, destfile=get_filename(paste('mental_health', start_year, '.xls', sep=''))) # download file
        start_year<- start_year +1 # increment year
    }
}

get_files_outside_norm <- function(){ # this function downloads 2010 and 2008 files that are not listed in any MOH list and have custom URLS.
    download.file(url='https://www.health.govt.nz/system/files/documents/publications/mental-health-addiction-service-use-2009-10.xls',
                  destfile=get_filename('mental_health2010.xls'))
    download.file(url='https://www.health.govt.nz/system/files/documents/publications/mh2007-08-app-final.xls',
                  destfile=get_filename('mental_health2008.xls'))
}

get_unknown_years <- function(unknown_years){ # download files that are not part of metadata_df , 2001 to 2008
    for (i in 1:nrow(unknown_years)) {  # iterate over the unkwown years dataframe
        row <- unknown_years[i,]
            download.file(url=row$url, destfile=get_filename(row$file_name)) # download file.
    }
}
download_xlsx_file <- function(heath_url, file_name){ # receives a url and a filename, downloads the file from the url and saves it with the given filena,e.
    download_url <- heath_url %>% 
                    read_html() %>% # read HTML
                    html_element("#block-system-main > div > div:nth-child(2) > div.panel-panel.right.col-xs-12.col-sm-12.col-md-3.col-lg-3 > div.panel-pane.pane-views.pane-file-attachments-revisioning.pane-downloads-right.standard-block.even.hidden-xs.hidden-sm > div > div > div > div > ul > li > span > a") %>%  # finds links to download file
                    html_attr("href")   # gets the link
    download.file(url=download_url, destfile=get_filename(file_name)) # downloads file.
}


#build sheet name for getting data from xlsx
build_sheet_name <- function(str1, str2, separator){  # given 2 strings and a separator returns the sheet name
    data <- paste(str1, str2, sep=separator)
    return(data)
}

#decide which separator to use for different file
get_sheet_separator <- function(file_name){  # goes trough sheet names and returns the type of separator used for this excel file, Options are : , and &
    sheets <- file_name %>% excel_sheets() # reads excel file and gets sheet names.
    if ('table1,2' %in% sheets){
        separator <- ','
    }else{
        separator <- '&'
    }
}

get_table_positions <- function(file_metadata_df){ # Returns cell position for information about gender, age and ethnic group 
    table_1_2_positions <- tibble(ethnic_group=c('Maori', 'Pacific', 'Asian', 'Other'), # creates 
                                  range=c('B9:U10', 'B12:U13', 'B15:U16', 'B18:U19'))
    
    return(table_1_2_positions)
}

get_age_column_names_table1 <- function(file_metadata_df){ # get column names for gender and age groups
    first_file = file_metadata_df[1,] # read the first file of the metadata_df
    suppressMessages( # we are using supprss message to filter col name changes of the readxl package.
    age_group_col_names <- paste(DATASET_FOLDER, first_file$file_name, sep='/') %>% read_excel(sheet=build_sheet_name('table1',  # reads excel file with current separator to get column names
                                                                                                                      '2',
                                                                                                                      first_file$separator),
                                                                                               range=cell_rows(4:4)) %>% 
                                                                                    colnames() # gets column names
    )
    add_sex_totl_age_group_col_names <- c('gender', "Total", age_group_col_names) # add this columns to gender and Total for full dataset.

    return (add_sex_totl_age_group_col_names)
}

#get data from 2010 file for spcific sheet range etc.
get_2010_dfs <- function(file, sheet, range, target_gender, target_ethtnic_group, age_group){  # given file, sheet range target gender, target ethnic group and age group, get the values for the custom 2010 file.
    suppressMessages(
        csv_file_read <- file %>% read_excel(sheet = sheet, range=range) # read file
    )
    colnames(csv_file_read) <- c('amount') # add amount column to amount
    csv_file_read <- csv_file_read %>% 
                     mutate(gender=target_gender, ethnic_group= target_ethtnic_group) %>%  # add the data to the target ethnic group given by the parameters.
                     cbind(age_group) # add column
    return(csv_file_read)
}


get_clients_by_gender_ethnic_group <- function(file_metadata, table_positions, columns){ # receives a file, table positions and columns and returns a dataset with year, gender, ethnic group, age_group and amount of users.

    clients_by_age_ethnic_group <- data.frame(year=double(), # create an empty dataset to add rows to.
                                         gender=character(), 
                                         ethnic_group=character(),
                                         age_group=character(),
                                         amount=integer()) 
    
    for (i in 1:nrow(table_positions)) { # iterate over table positions
        row <- table_positions[i,]  # get the ith row.
        range = row$range # get the range of cells for this row
        if (file_metadata$year == '2011'){ # exception for year 2011 that uses a Camel case naming convention
            sheet <- 'Table1'  
        }else if(file_metadata$year == '2012'){ # year 2012 has an an exception for table name as it is lowercase and only uses 1
            sheet <- 'table1'
        } else if (file_metadata$year == '2016'){ # year 2016 changes the range where the data is stored.
            if (range == 'B9:U10'){
                range = 'B8:U9'
            }
            else if (range=='B12:U13'){ # changing custom ranges to original ones
                range = 'B11:U12'
            }
            else if (range=='B15:U16'){ # same as last row, but for another type of row
                range='B14:U15'
            }else{
                range = 'B17:U18' # this is the standard range .
            }
           
            sheet <- build_sheet_name("table1", "2", file_metadata$separator) # get the sheetname for this row
        }else{
            sheet <- build_sheet_name("table1", "2", file_metadata$separator)# get the sheetname for this row
           
        }
        suppressMessages( # suppresisng messages for styling in jupyter notebook

            ethnic_group_df <- paste(DATASET_FOLDER, file_metadata$file_name, sep='/') %>% read_excel(sheet=sheet,
                                                       range = range,
                                                       col_names = columns)  #reads excel with already parsed parameters

        )        
        ethnic_group_df <- ethnic_group_df %>% add_column(ethnic_group=row$ethnic_group, year=file_metadata$year) # adds ethnic group and year columns from metadata.
        
        ethnic_group_df <- ethnic_group_df %>% select(-Total) # removes Total column, this is because even though some files have the correct fields being pulled they sometimes return Total for custom files.
        
        long_col_name <- ethnic_group_df %>% select(ends_with("-"), ends_with('+')) %>% names() # get only age_group columns
        
        clients_by_age_ethnic_group <- clients_by_age_ethnic_group %>% 
                                       add_row(ethnic_group_df %>% 
                                       gather(age_group, amount, long_col_name, factor_key=TRUE)) # turn this dataset into a long dataset by age group and ethnic group.
    }  
    return(clients_by_age_ethnic_group)
}

get_all_year_clients_by_gender_ethnic_group <- function(clients_by_age_ethnic_group_df, file_metadata_df){ # gets datasets for years 2011 to 2020 for ethnic, age group and gender.
    table_positions <- get_table_positions(file_metadata_df) # gets the table positions for this file.
    age_group_col_names = get_age_column_names_table1(file_metadata_df) # gets the age column groups
    for (i in 1:nrow(file_metadata_df)){ # iterate through the metadata file
        row <- file_metadata_df[i,] # gets ith rowh
        clients_by_age_ethnic_group_df <- clients_by_age_ethnic_group_df %>% 
                                          add_row(get_clients_by_gender_ethnic_group(row,  #gets the spreadsheet for the current row.
                                                                                     table_positions,
                                                                                     age_group_col_names))
    }
    return(clients_by_age_ethnic_group_df) 
    
}

# 2010 File is different to every other file, so we need a different parser for it.

get_2010_client_ages <- function(){ # returns dataset for 2010 by gender, ethnicity and age_Group
        file_2010 <- paste(DATASET_FOLDER, "mental_health2010.xls", sep='/') # gets filepath
        suppressMessages(
            client_ages_group_2010 <- file_2010 %>% read_excel(sheet = "T7 Nos Rates 0910 EthSexAge",  range='A4:A22')  #reads excel file
        )
        colnames(client_ages_group_2010) <- c('age_group') # adds age group column

        data_2010_clients <- get_2010_dfs(file_2010, "T7 Nos Rates 0910 EthSexAge", 'B4:B22', 'Male', 'Maori', client_ages_group_2010 ) %>%  # gets maori male data
                                rbind(get_2010_dfs(file_2010, "T7 Nos Rates 0910 EthSexAge", 'D4:D22', 'Female', 'Maori', client_ages_group_2010 )) %>%  # get maori female data
                                rbind(get_2010_dfs(file_2010, "T7 Nos Rates 0910 EthSexAge", 'H4:H22', 'Male', 'Unknown', client_ages_group_2010 )) %>%  # gets unknown male data
                                rbind(get_2010_dfs(file_2010, "T7 Nos Rates 0910 EthSexAge", 'H4:H22', 'Female', 'Unknown', client_ages_group_2010 )) %>%  # gets unknown female data
                                mutate('year'=2010) # adds year column with 2010 in it.
}


#make all df 2002-2007 data tidy
get_df_tidy <- function(df, year_num){
    #get num of row
    n_row_age_sheeta1 <- nrow(df)
    #set all na values with activity type 
    for (row_num in 1:n_row_age_sheeta1){
        if(df$ethnic_group[[row_num]] %>% is.na()){
            df$ethnic_group[[row_num]] = df$ethnic_group[[row_num - 1]]
        }
    }
    result <- df %>% 
              filter(gender!='NA') %>% 
              gather(age_group, amount, -c(ethnic_group,gender,amount)) %>% 
              mutate('year'=year_num) 
    result <- result %>% filter(gender!='Total') %>% filter(ethnic_group!='Total')
    result <- result[,c(5,2,1,3,4)]
    return(result)
}

#get 2002/2008 data
file_metadata_2001_2007 <- tibble(year=2002:2008,
                                  sheet=c('A1', 'A1', 'A1', 'A1', '1A', '1B', 'A1'),
                                  range=c('A5:U25', 'A4:U24', 'A5:U26', 'A5:U25', 'A5:U26', 'A5:U26', 'A5:U25'),
                                  titles=c('D5:U5', 'D4:U4', 'D5:U5', 'D5:U5', 'D5:U5', 'D5:U5', 'D5:U25')
                                 )

get_2001_2007_file_df <- function(file_name, year_num){
    file_metadata <- file_metadata_2001_2007 %>% filter(year==year_num)
    #get client age group names
    suppressMessages(

        client_ages_sheeta1 <- get_filename(file_name) %>% read_excel(sheet=file_metadata$sheet,
                                                                      range=file_metadata$titles) %>% colnames()
    )
    col_names_seen_by_age_sheeta1  <- c('ethnic_group', 'gender', 'amount', client_ages_sheeta1)
    
    #get data from the file rename with names before
    suppressMessages(
        client_seen_by_age_data_a1 <-  get_filename(file_name) %>% read_excel(sheet=file_metadata$sheet,
                                                            range=file_metadata$range)
    )
    colnames(client_seen_by_age_data_a1) <- col_names_seen_by_age_sheeta1 

    return(get_df_tidy(client_seen_by_age_data_a1, year_num))
}

clean_ages <- function(df){ # cleans up age_groups . removes unnecesary data.
    df <- df %>% mutate(age_group=gsub("0–4","0–", age_group)) %>%   
                 mutate(age_group=gsub("0–5", "5–", age_group)) %>%  
                 mutate(age_group=gsub("5–9", "10–", age_group)) %>%  
                 mutate(age_group=gsub("10–14", "15–", age_group)) %>%  
                 mutate(age_group=gsub("15–19", "20–", age_group)) %>%  
                 mutate(age_group=gsub("20–24", "25–", age_group)) %>%  
                 mutate(age_group=gsub("25–29", "30–", age_group)) %>%  
                 mutate(age_group=gsub("30–34", "35–", age_group)) %>%  
                 mutate(age_group=gsub("35–39", "40–", age_group)) %>%  
                 mutate(age_group=gsub("40–44", "45–", age_group)) %>%  
                 mutate(age_group=gsub("45–49", "50–", age_group)) %>%  
                 mutate(age_group=gsub("50–54", "55–", age_group)) %>%  
                 mutate(age_group=gsub("55–59", "60–",  age_group)) %>%  
                 mutate(age_group=gsub("60–64", "65–", age_group)) %>%  
                 mutate(age_group=gsub("65–69", "70–",  age_group)) %>%  
                 mutate(age_group=gsub("70–74", "75–", age_group)) %>%  
                 mutate(age_group=gsub("75–79", "80–",  age_group)) %>%  
                 mutate(age_group=gsub("80–84", "85+", age_group)) %>%
                 mutate(age_group=gsub("55–4", "60–", age_group)) %>% 
                 mutate(age_group=gsub("-", "–", age_group)) # filters hyphen error.
return(df)
}

#get all data from 2002 to 2007
get_all_a1sheet_2002_2007_df <- function(){ # gets all 
    
    result <- get_2001_2007_file_df('mental_health2002.xls',2002) %>% 
             rbind(get_2001_2007_file_df('mental_health2003.xls',2003)) %>% 
             rbind(get_2001_2007_file_df('mental_health2004.xls',2004)) %>% 
             rbind(get_2001_2007_file_df('mental_health2005.xls',2005)) %>% 
             rbind(get_2001_2007_file_df('mental_health2006.xls',2006)) %>% 
             rbind(get_2001_2007_file_df('mental_health2007.xls',2007)) %>% 
             rbind(get_2001_2007_file_df('mental_health2008.xls',2008)) 


    return(result)
}


normalize_2002_2008_column_names <- function(columns){ # unifies names of cities for DHB information. from 2002 to 2008
    columns <- str_replace_all(columns, "Capital & Coast", "Capital and Coast")
    columns <- str_replace_all(columns, "MidCentral", "Mid Central")
    columns <- str_replace_all(columns, "Midcentral", "Mid Central")
    columns <- str_replace_all(columns, "Hutt Valley", "Hutt")
    columns <- str_replace_all(columns, "Southern", "Southland")
    columns <- stri_trans_general(columns, "Latin-ASCII")
    columns <- str_replace_all(columns, "unknown domicile", "Unknown")
    columns <- str_replace_all(columns, "Unknown domicile", "Unknown")
    return(columns)
}

normalize_2011_ownwards_column_names <- function(columns) { # unifies names of cities for DHB information, from 2011 onwards.
    columns <- gsub("NA", "", columns)
    columns <- gsub("\r\n", " ", columns)
    columns <- str_replace_all(columns, "[[:punct:]]", "")
    columns <- str_replace_all(columns, "[[:number:]]", "")
    columns <- str_replace_all(columns, "MidCentral", "Mid Central")
    columns <- str_replace_all(columns, "Hutt Valley", "Hutt")
    columns <- str_replace_all(columns, "Southern", "Southland")
    columns <- str_replace_all(columns, "Capital  Coast", "Capital and Coast")
    columns <- stri_trans_general(columns, "Latin-ASCII")
    columns <- trimws(columns)    
    return(columns)
    
}

get_2002_to_2008_dhb_data <- function(file, sheet, range, year){ # gets data for DHB usage.
    suppressMessages(
        dhb_df <- get_filename(file) %>% read_excel(sheet=sheet, range=range)
    )
    colnames(dhb_df)[1] <- 'dhb'
    dhb_df <- dhb_df %>% mutate(dhb=gsub(" District Health Board", "", dhb)) # unifies destination column
    colnames(dhb_df) <- colnames(dhb_df) %>% normalize_2002_2008_column_names() # adds column name normalised
    dhb_df <- dhb_df %>% mutate(year=year) # adds year column
    return(dhb_df)
}

get_2010_dhb_data <- function(filename, sheet, range){ # gets custom 2010 DHB data.
    suppressMessages(
        dhb_df <- get_filename(filename) %>% read_excel(sheet=sheet, range=range) %>% select(-Rate) 
    )
    colnames(dhb_df)[1] <- 'dhb'    
    colnames(dhb_df)[2] <- 'amount'
    dhb_df <- dhb_df %>% mutate(origin=dhb) %>%           # this table is different to the other files, it only contains the destination and the amount of users
                         pivot_wider(names_from=origin,   # so we mutate the column to add the same column twice and then create an identity matrix type of table
                                     values_from=amount,  # that matches the rest of the files format.
                                     values_fill=0) %>%
                         mutate(year=2010)  # adds year column for 2010
    colnames(dhb_df) <- normalize_2002_2008_column_names(colnames(dhb_df))

    return(dhb_df)
}


get_2011_onwards_dhb_data <- function(file, sheet, header, range, year){ # gets information about dhb service usage from 2011 onwards
    suppressMessages(
        header <- get_filename(file) %>% read_excel(sheet=sheet, range=header) # gets the header information from the sheet
    )
    suppressMessages(
        dhb_df <- get_filename(file) %>% read_excel(sheet=sheet,range=range) # gets the data from the sheet
    )
    if (any(is.na(dhb_df[1,]))){ # if the column is a two row title, we have to fill the data with the first row of the dataset into the title
        colnames(dhb_df) <- paste(colnames(header), as.character(dhb_df[1,])) # her ewe add that value
        dhb_df<- dhb_df[-1,]  # here we remove the first row from the dataset in order to keep the data tidy
    } else if('...1' %in% colnames(dhb_df[1,])){  # in case the columns are empty we need to paste the data of the multi line rows to get values like Bay of Plenty.
        colnames(dhb_df) <- paste(colnames(header), colnames(dhb_df[1,])) 
    }else{
        colnames(dhb_df) <- colnames(header)
    } 
    colnames(dhb_df)[1] <- 'dhb'
    colnames(dhb_df) <- colnames(dhb_df) %>% normalize_2011_ownwards_column_names()
    dhb_df <- dhb_df %>% mutate(year=year)
    if (!('Otago' %in% colnames(dhb_df))){
        dhb_df <- dhb_df %>% mutate(Otago=0)  # some reports don't include otago, so we add it as an empty column for those years.
    }
    return(dhb_df)
}


get_service_data <- function(filename, sheet, range, year, cols=NULL){ # gets information about service usage, this uses a custom cols parameters that allows us to work with different types of files.
    suppressMessages(
        data <- get_filename(filename) %>% read_excel(sheet=sheet, range=range) 
   )
   colnames(data)[1] <- 'activity'
   colnames(data)[2] <- 'gender'
   colnames(data)[3] <- 'Total'

   if (year == 2003){
       colnames(data)[4] <- '0-4' # year 2003 has a different year format that we need to take care individually.
   }
   colnames(data) <- gsub('-', '–', colnames(data)) # that took a long time.
   data <- data %>% mutate(activity = as.character(na_if(activity, '.'))) %>% 
             fill(activity, .direction = 'down') %>%  # here we use the fill method to get the name of the activity from the row above to fill missing information for activity.
             na.omit(gender) %>%  # remove the gender in case its not defined, this has tod o with the format of the file not having some values.
             filter(gender!='Total') %>% # remove total fields
             select(-Total) %>% # remove the total column
             mutate(year=year) # add the current year
    
    if (!(missing(cols))){ # if we have assigned custom columns, use them now.
        colnames(data) <- cols
    }
    return(data)
}