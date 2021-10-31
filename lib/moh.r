#function is to download xlsx from health url 
DATASET_FOLDER <- 'input'

get_filename <- function(filename){
    return(paste(DATASET_FOLDER, filename, sep='/'))
}


get_moh_file_data <- function(){
     mental_health_data_urls_all_new_years <- 'https://www.health.govt.nz/our-work/mental-health-and-addiction/mental-health-and-addiction-monitoring-reporting-and-data' %>% 
                                        read_html() %>%
                                        html_element('#node-12245 > div > div > div > ul:nth-child(19)') %>%
                                        html_elements("a") %>%
                                        html_attrs() %>% 
                                        str_c('https://www.health.govt.nz/', .)
    
    file_metadata_df <- data.frame(year=double(),
                                   file_name=character(),
                                   url=character(),
                                   separator=character())

    start_year = 2020
    counter = 0
    for (url_yearly_health in mental_health_data_urls_all_new_years){
        file_name = str_c('mental_health', start_year) %>% 
                        str_c('.xlsx')
        url_to_download = url_yearly_health
        if (counter == 4){
            url_to_download = 'https://www.health.govt.nz/publication/mental-health-and-addiction-service-use-2015-16'
        }
        download_xlsx_file(url_to_download, file_name)
        separator <- get_sheet_separator(get_filename(file_name))
        file_metadata_df <- file_metadata_df %>% add_row(year=start_year,
                                                         file_name=file_name,
                                                         url=url_to_download,
                                                         separator=separator)
        start_year = start_year - 1
        counter= counter + 1

    }
    #we add some custom years to the list
    file_metadata_df <- file_metadata_df %>% add_row(year=2011:2013, 
                                                     file_name=c('mental_health2011.xlsx',
                                                                 'mental_health2012.xlsx',
                                                                 'mental_health2013.xlsx'),
                                                     url=c('https://www.health.govt.nz/system/files/documents/publications/mh_201011_revised_mar2020.xlsx',
                                                          'https://www.health.govt.nz/system/files/documents/publications/mh_201112_revised_mar2020.xlsx',
                                                          'https://www.health.govt.nz/system/files/documents/publications/mh2012-13_revisedmarch2020.xlsx'),
                                                     separator='&')
    get_unknown_years(file_metadata_df %>% filter(year<=2013))

    return(file_metadata_df)
}

get_legacy_files <- function(){
    url <- 'https://www.health.govt.nz/publication/mental-health-service-use-new-zealand-2001-02-2006-07'
    mental_health_data_urls_all_new_years <- url %>% 
                                              read_html() %>%
                                              html_element('.view-file-attachments-revisioning') %>%
                                              html_elements("a") %>%
                                              html_attrs() %>% str_c('', .)
    start_year = 2002
    for (donwload_url in mental_health_data_urls_all_new_years){
        download.file(url=donwload_url, destfile=get_filename(paste('mental_health', start_year, '.xls', sep='')))
        start_year<- start_year +1
    }
}

get_files_outside_norm <- function(){
    download.file(url='https://www.health.govt.nz/system/files/documents/publications/mental-health-addiction-service-use-2009-10.xls',
                  destfile=get_filename('mental_health2010.xls'))
    download.file(url='https://www.health.govt.nz/system/files/documents/publications/mh2007-08-app-final.xls',
                  destfile=get_filename('mental_health2008.xls'))
}

get_unknown_years <- function(unknown_years){
    for (i in 1:nrow(unknown_years)) {
        row <- unknown_years[i,]
            download.file(url=row$url, destfile=get_filename(row$file_name))
    }
}
download_xlsx_file <- function(heath_url, file_name){
    download_url <- heath_url %>% 
                    read_html() %>%
                    html_element("#block-system-main > div > div:nth-child(2) > div.panel-panel.right.col-xs-12.col-sm-12.col-md-3.col-lg-3 > div.panel-pane.pane-views.pane-file-attachments-revisioning.pane-downloads-right.standard-block.even.hidden-xs.hidden-sm > div > div > div > div > ul > li > span > a") %>% 
                    html_attr("href") 
    download.file(url=download_url, destfile=get_filename(file_name))
}


#build sheet name for getting data from xlsx
build_sheet_name <- function(str1, str2, separator){
    data <- paste(str1, str2, sep=separator)
    return(data)
}

#decide which separator to use for different file
get_sheet_separator <- function(file_name){
    sheets <- file_name %>% excel_sheets()
    
    if ('table1,2' %in% sheets){
        separator <- ','
    }else{
        separator <- '&'
    }
}

get_table_positions <- function(file_metadata_df){
     #get all col names for table use after
    #get metadata from one of the files.
    first_file = file_metadata_df[1,]
    suppressMessages(
    age_group_col_names <- paste(DATASET_FOLDER, first_file$file_name, sep='/') %>% read_excel(sheet=build_sheet_name('table1', 
                                                                                                                      '2',
                                                                                                                      first_file$separator),
                                                                                               range=cell_rows(4:4)) %>% 
                                                                                    colnames()
    )

    table_1_2_positions <- tibble(ethnic_group=c('Maori', 'Pacific', 'Asian', 'Other'),
                                  range=c('B9:U10', 'B12:U13', 'B15:U16', 'B18:U19'))
    
    return(table_1_2_positions)
}

get_age_column_names_table1 <- function(file_metadata_df){
        first_file = file_metadata_df[1,]
        suppressMessages(

        age_group_col_names <-   paste(DATASET_FOLDER, first_file$file_name, sep='/') %>% read_excel(sheet=build_sheet_name('table1', 
                                                                                                                        '2',
                                                                                                                        first_file$separator),
                                                                                                 range=cell_rows(4:4) )%>% 
                                                                                  colnames()
        )
        add_sex_totl_age_group_col_names <- c('gender', "Total", age_group_col_names)

        return (add_sex_totl_age_group_col_names)
}

#get data from 2010 file for spcific sheet range etc.
get_2010_dfs <- function(file, sheet, range, target_gender, target_ethtnic_group, age_group){
    suppressMessages(
        csv_file_read <- file %>% read_excel(sheet = sheet, range=range)
    )
    colnames(csv_file_read) <- c('amount')
    csv_file_read <- csv_file_read %>% 
                     mutate(gender=target_gender, ethnic_group= target_ethtnic_group) %>% 
                     cbind(age_group)
    return(csv_file_read)
}


#get cilents gender ethnic data from file with columns 
get_clients_by_gender_ethnic_group <- function(file_metadata, table_positions, columns){

    clients_by_age_ethnic_group <- data.frame(year=double(),
                                         gender=character(), 
                                         ethnic_group=character(),
                                         age_group=character(),
                                         amount=integer()) 
    
    for (i in 1:nrow(table_positions)) {
        row <- table_positions[i,]
        range = row$range
        if (file_metadata$year == '2011'){
            sheet <- 'Table1'
        }else if(file_metadata$year == '2012'){
            sheet <- 'table1'
        } else if (file_metadata$year == '2016'){
            if (range == 'B9:U10'){
                range = 'B8:U9'
            }
            else if (range=='B12:U13'){
                range = 'B11:U12'
            }
            else if (range=='B15:U16'){
                range='B14:U15'
            }else{
                range = 'B17:U18'
            }
           
            sheet <- build_sheet_name("table1", "2", file_metadata$separator)
        }else{
            sheet <- build_sheet_name("table1", "2", file_metadata$separator)
           
        }
        suppressMessages(

            ethnic_group_df <- paste(DATASET_FOLDER, file_metadata$file_name, sep='/') %>% read_excel(sheet=sheet,
                                                       range = range,
                                                       col_names = columns)  

        )        
        ethnic_group_df <- ethnic_group_df %>% add_column(ethnic_group=row$ethnic_group, year=file_metadata$year)
        
        ethnic_group_df <- ethnic_group_df %>% select(-Total)
        
        long_col_name <- ethnic_group_df %>% select(ends_with("-"), ends_with('+')) %>% names()
        
        clients_by_age_ethnic_group <- clients_by_age_ethnic_group %>% 
                                       add_row(ethnic_group_df %>% 
                                       gather(age_group, amount, long_col_name, factor_key=TRUE))
    }  
    return(clients_by_age_ethnic_group)
}

get_all_year_clients_by_gender_ethnic_group <- function(clients_by_age_ethnic_group_df, file_metadata_df){
    table_positions <- get_table_positions(file_metadata_df)
    age_group_col_names = get_age_column_names_table1(file_metadata_df)
    for (i in 1:nrow(file_metadata_df)){
        row <- file_metadata_df[i,]
        clients_by_age_ethnic_group_df <- clients_by_age_ethnic_group_df %>% 
                                          add_row(get_clients_by_gender_ethnic_group(row,
                                                                                     table_positions,
                                                                                     age_group_col_names))
    }
    return(clients_by_age_ethnic_group_df) 
    
}

# 2010 File is different to every other file, so we need a different parser for it.

get_2010_client_ages <- function(){
        file_2010 <- paste(DATASET_FOLDER, "mental_health2010.xls", sep='/')
        suppressMessages(
            client_ages_group_2010 <- file_2010 %>% read_excel(sheet = "T7 Nos Rates 0910 EthSexAge",  range='A4:A22') 
        )
        colnames(client_ages_group_2010) <- c('age_group')

        data_2010_clients <- get_2010_dfs(file_2010, "T7 Nos Rates 0910 EthSexAge", 'B4:B22', 'Male', 'Maori', client_ages_group_2010 ) %>% 
                                rbind(get_2010_dfs(file_2010, "T7 Nos Rates 0910 EthSexAge", 'D4:D22', 'Female', 'Maori', client_ages_group_2010 )) %>% 
                                rbind(get_2010_dfs(file_2010, "T7 Nos Rates 0910 EthSexAge", 'H4:H22', 'Male', 'Unknown', client_ages_group_2010 )) %>% 
                                rbind(get_2010_dfs(file_2010, "T7 Nos Rates 0910 EthSexAge", 'H4:H22', 'Female', 'Unknown', client_ages_group_2010 )) %>% 
                                mutate('year'=2010)
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

clean_ages <- function(df){
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
                                     mutate(age_group=gsub("-", "–", age_group))
    return(df)
}

#get all data from 2002 to 2007
get_all_a1sheet_2002_2007_df <- function(){
    
    result <- get_2001_2007_file_df('mental_health2002.xls',2002) %>% 
             rbind(get_2001_2007_file_df('mental_health2003.xls',2003)) %>% 
             rbind(get_2001_2007_file_df('mental_health2004.xls',2004)) %>% 
             rbind(get_2001_2007_file_df('mental_health2005.xls',2005)) %>% 
             rbind(get_2001_2007_file_df('mental_health2006.xls',2006)) %>% 
             rbind(get_2001_2007_file_df('mental_health2007.xls',2007)) %>% 
             rbind(get_2001_2007_file_df('mental_health2008.xls',2008)) 


    return(result)
}




normalize_2002_2008_column_names <- function(columns){
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

normalize_2011_ownwards_column_names <- function(columns) {
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

get_2002_to_2008_dhb_data <- function(file, sheet, range, year){
    suppressMessages(
        dhb_df <- get_filename(file) %>% read_excel(sheet=sheet, range=range)
    )
    colnames(dhb_df)[1] <- 'dhb'
    dhb_df <- dhb_df %>% mutate(dhb=gsub(" District Health Board", "", dhb))
    colnames(dhb_df) <- colnames(dhb_df) %>% normalize_2002_2008_column_names()
    dhb_df <- dhb_df %>% mutate(year=year)
    return(dhb_df)
}

get_2010_dhb_data <- function(filename, sheet, range){
    suppressMessages(
        dhb_df <- get_filename(filename) %>% read_excel(sheet=sheet, range=range) %>% select(-Rate) 
    )
    colnames(dhb_df)[1] <- 'dhb'    
    colnames(dhb_df)[2] <- 'amount'
    dhb_df <- dhb_df %>% mutate(origin=dhb) %>% 
                         pivot_wider(names_from=origin,
                                     values_from=amount,
                                     values_fill=0) %>%
                         mutate(year=2010)
    colnames(dhb_df) <- normalize_2002_2008_column_names(colnames(dhb_df))

    return(dhb_df)
}


get_2011_onwards_dhb_data <- function(file, sheet, header, range, year){
    suppressMessages(
        header <- get_filename(file) %>% read_excel(sheet=sheet, range=header)
    )
    suppressMessages(
        dhb_df <- get_filename(file) %>% read_excel(sheet=sheet,range=range)
    )
    if (any(is.na(dhb_df[1,]))){
        colnames(dhb_df) <- paste(colnames(header), as.character(dhb_df[1,]))
        dhb_df<- dhb_df[-1,]
    } else if('...1' %in% colnames(dhb_df[1,])){
        colnames(dhb_df) <- paste(colnames(header), colnames(dhb_df[1,]))
    }else{
        colnames(dhb_df) <- colnames(header)
    }
    colnames(dhb_df)[1] <- 'dhb'
    colnames(dhb_df) <- colnames(dhb_df) %>% normalize_2011_ownwards_column_names()
    dhb_df <- dhb_df %>% mutate(year=year)
    if (!('Otago' %in% colnames(dhb_df))){
        dhb_df <- dhb_df %>% mutate(Otago=0)
    }
    return(dhb_df)
}





get_service_data <- function(filename, sheet, range, year, cols=NULL){
    suppressMessages(
        data <- get_filename(filename) %>% read_excel(sheet=sheet, range=range) 
   )
   colnames(data)[1] <- 'activity'
   colnames(data)[2] <- 'gender'
   colnames(data)[3] <- 'Total'

   if (year == 2003){
       colnames(data)[4] <- '0-4'
   }
   colnames(data) <- gsub('-', '–', colnames(data)) # that took a long time.
   data <- data %>% mutate(activity = as.character(na_if(activity, '.'))) %>% 
             fill(activity, .direction = 'down') %>% 
             na.omit(gender) %>% 
             filter(gender!='Total') %>% 
             select(-Total) %>%
             mutate(year=year)
    
    if (!(missing(cols))){
        colnames(data) <- cols
    }
    return(data)
}