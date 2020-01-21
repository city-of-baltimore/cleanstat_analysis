

mutate_hmt_group <- function(df, type.field){

  type.field <- enquo(type.field)
  
  df <- df %>% mutate(., 
    hmt.group = case_when(
      !! type.field %in% c("A", "B", "C") ~ "healthy",
      !! type.field %in% c("D", "E") ~ "upper middle",
      !! type.field %in% c("F", "G", "H") ~ "lower middle",
      !! type.field %in% c("I", "J") ~ "distressed",
      TRUE ~ "other")
  )
}

get_egis_table <- function(db, table){
  
  library(RODBC)

  table.full <- paste0(db, ".", table)
  
  message(paste0(Sys.time(), ": ", "Retrieving table ", table.full)) 
  
  db <- paste0('MSSQL:server=', VARS$EGIS_SERVER, ';',
               'uid=', VARS$EGIS_SERVER_USER,';',
               'pwd=', VARS$EGIS_SERVER_PWD, ';',
               'database=', db, ';',
               'trusted_connection=No')
  
  raw <- readOGR(db, table.full, verbose = F)
  
  message(paste0(Sys.time(), "Table ", table.full, " retrieved")) 
  return(raw)
}

get_hmt_data <- function(){
  
  hmt.raw <- get_egis_table("housing", "HMT2017")
  
  
  
}


clean_sales_spreadsheets <- function(){
  
  library(here)
  library(readxl)
  library(lubridate)
  
  filename.2017 <- paste0(VARS$RAW_DATA, "/sales/City Sales Data Jan 1 2017 through September 2018.xlsx")
  filename.2010 <- paste0(VARS$RAW_DATA, "/sales/Sales Data 2010 - Sept 2017.xlsx")
  
  sales.2017 <- read_excel(filename.2017, skip = 2, col_names = T)
  sales.2010 <- read_excel(filename.2010, skip = 2, col_names = T)
  
  
  sales <- bind_rows(sales.2010, sales.2017)
  
  sales <- sales[!duplicated(sales %>% select(Block, Lot, `Date Sale Entered in Land Records`, Liber, Folio)), ]
  
  sales <- sales %>% mutate(
    deed.date = ymd(`Deed Date`),
    sale.enter.date = ymd(`Date Sale Entered in Land Records`),
    zip.five = substr(Zipcode, 1, 5),
    new.owner = substring(`New Owner`, first = 2),
    blocklot = paste0(Block, Lot))
  
  return(sales)
}

load_sales_data <- function(load.cache = T){
  
  
  library(here)
  if(load.cache == T){
    
    dir <- paste0(VARS$PROCESSED_DATA, "/sales")
    filename <- paste0(dir, "/sales.rds")
    
    # load existing cache or create it if it doesn't exist
    sales <- tryCatch({
      message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Attempting to load cached data."))
      message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "File at: ", filename)) 
      cached.data <- readRDS(filename)
    },
    
    error = function(cond){
      
      message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Cache does not exist. Creating cache."))
      
      sales <- clean_sales_spreadsheets()
      
      if(file.exists(dir) == F){
        message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Directory does not exist. Creating directory.")) 
        dir.create(dir)
      }
      
      saveRDS(sales, file = filename)
      message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Cache saved to ", filename)) 
      return(sales)
    }
    )
  } else {
    # load data directly from server 
    message(paste0(Sys.time(), ": ", "Loading directly from spreadsheets (no cache created or loaded)."))
    sales <- clean_sales_spreadsheets()
  }
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Sales data succesfully loaded."))
  return(sales)
}

load_block_group_data <- function(load.cache = T){
  
  library(here)
  if(load.cache == T){
    
    dir <- paste0(here(), "/data/processed/hmt")
    filename <- paste0(dir, "/hmt_join_acs.rds")
    
    # load existing cache or create it if it doesn't exist
    hmt <- tryCatch({
        message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Attempting to load cached data."))
        message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "File at: ", filename)) 
        cached.data <- readRDS(filename)
      },
      
      error = function(cond){
        
        message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Cache does not exist. Creating cache."))
        server.data <- get_block_group_data_from_server()
        
        if(file.exists(dir) == F){
          message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Directory does not exist. Creating directory.")) 
          dir.create(dir)
        }
    
        saveRDS(server.data, file = filename)
        message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Cache saved to ", filename)) 
        return(server.data)
      }
    )
  } else {
    # load data directly from server 
    message(paste0(Sys.time(), ": ", "Loading directly from server."))
    server.data <- get_block_group_data_from_server()
  }
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "HMT data succesfully loaded."))
  return(hmt)
}


get_block_group_data_from_server <- function(){
  
  suppressPackageStartupMessages(library(rgdal))
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(sp))
  
  # Census block groups: Housing Market Typology plus Census block group data
  
  housing.db <-   paste0('MSSQL:server=', VARS$EGIS_SERVER, ';',
                         'uid=', VARS$EGIS_SERVER_USER,';',
                         'pwd=', VARS$EGIS_SERVER_PWD, ';',
                         'database=housing;',
                         'trusted_connection=No')
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": Grabbing HMT geospatial table from EGIS server."))
  hmt <- readOGR(housing.db, "housing.HMT2017", verbose = F)
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": Formatting HMT geospatial table."))
  hmt <- SpatialPolygonsDataFrame(hmt, hmt@data)
  hmt@proj4string <- CRS("+init=epsg:2248")
  hmt <- spTransform(hmt, CRS("+init=epsg:4326"))
  
  
  hmt@data <- hmt@data %>% mutate(
    
    # HMT again, but with middle broken in two
    hmt.tier = case_when(
      MVA17HrdCd %in% c("A", "B", "C") ~ "healthy",
      MVA17HrdCd %in% c("D", "E") ~ "upper middle",
      MVA17HrdCd %in% c("F", "G", "H") ~ "lower middle",
      MVA17HrdCd %in% c("I", "J") ~ "distressed",
      TRUE ~ "other")
    
  )
  
  # census block group data (spreadsheets provided by M. Galdi)

  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": Loading ACS block group data from Excel sheets."))
  acs.2016 <- read_excel(paste0(VARS$RAW_DATA, "/acs/Shortened Blockgroup, More Info.xlsx"), 
                         sheet = 2, range = "A1:CP654")
  acs.2011 <- read_excel(paste0(VARS$RAW_DATA, "/acs/Shortened Blockgroup, More Info.xlsx"), 
                         sheet = 3, range = "A1:CP654")
  
  planning.db <- paste0('MSSQL:server=', VARS$EGIS_SERVER, ';',
                        'uid=', VARS$EGIS_SERVER_USER, ';',
                        'pwd=', VARS$EGIS_SERVER_PWD, ';',
                        'database=planning;',
                        'trusted_connection=No')
  
  # @Justin replace spreadsheets with table on EGIS server (planning.ACS2016_BLOCKGROUP)
  
  #message(paste0(format(Sys.time(), format="%H:%M:%S"), ": Loading ACS block group data from EGIS server."))
  #acs <- readOGR(planning.db, "planning.ACS2016_BLOCKGROUP")
  
  # join census bg data to hmt layer
  colnames(acs.2016) <- paste0(colnames(acs.2016), ".2016")
  colnames(acs.2011) <- paste0(colnames(acs.2011), ".2011")
  
  acs.2016$AreaYear.2016 <- as.factor(acs.2016$AreaYear.2016)
  acs.2011$AreaYear.2011 <- as.factor(acs.2011$AreaYear.2011)
  hmt$bg <- as.factor(hmt$bg)
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": Joining ACS data to HMT geospatial table."))
  hmt@data <- hmt@data %>% 
    left_join(acs.2016, by = c("bg"="AreaYear.2016")) %>%
    left_join(acs.2011, by = c("bg"="AreaYear.2011"))

  return(hmt)
}


get_neighborhood_boundaries <- function(){
  suppressPackageStartupMessages(library(geojsonsf))
  suppressPackageStartupMessages(library(sf))
  suppressPackageStartupMessages(library(rgdal))
  
  
  # load Baltimore neighborhood boundaries
  # neighborhood boundaries
  hoods.url <- "https://data.baltimorecity.gov/resource/h3fx-54q3.geojson"
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", 
                 "Getting neighborhood boundaries from Open Baltimore @ \n", hoods.url)) 
  
  
  # surely there is a better way, and i tried, but couldn't get anything else to work.
  # from geojson to sf to spatial df. geojson_sp didn't work for me.
  hoods <- geojson_sf(hoods.url)
  hoods <- as_Spatial(hoods)
  hoods <- spTransform(hoods, CRS("+init=epsg:4326"))
  
  return(hoods)
}

get_911cfs_type <- function(cfs.type){
  
  
  
}

load_2018_service_requests <- function(load.cache = T){
  
  library(here)
  if(load.cache == T){
    
    dir <- paste0(here(), "/data/processed/311")
    filename <- paste0(dir, "/311_sr_2018.rds")
    
    # load existing cache or create it if it doesn't exist
    sr <- tryCatch({
      message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Attempting to load cached data."))
      message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "File at: ", filename)) 
      cached.data <- readRDS(filename)
    },
    
    error = function(cond){
      
      message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Cache does not exist. Creating cache."))
      server.data <- get_2018_service_requests_from_ob()
      
      if(file.exists(dir) == F){
        message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Directory does not exist. Creating directory.")) 
        dir.create(dir)
      }
      
      saveRDS(server.data, file = filename)
      message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Cache saved to ", filename)) 
      return(server.data)
    }
    )
  } else {
    # load data directly from server 
    message(paste0(Sys.time(), ": ", "Loading directly from Open Baltimore."))
    server.data <- get_2018_service_requests_from_ob()
  }
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "311 service request data succesfully loaded."))
  return(sr)
  
}

get_2018_service_requests_from_ob <- function(){
  
  library(RSocrata)
  library(sp)
  
  sr.endpoint <- "https://data.baltimorecity.gov/resource/9agw-sxsr.json"
  sr.query <- paste0(sr.endpoint, "?$where=date_trunc_y(createddate)='2018'")
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Grabbing 311 data from Open Baltimore.")) 
  
  sr <- read.socrata(url = sr.query, app_token = VARS$SOCRATA_TOKEN) %>%
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude),
           createddate = as.Date(createddate)) 
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "311 data retrieved; processing.")) 
  
  sr <- sr %>% filter(!is.na(longitude), !is.na(latitude))
  
  sr <- SpatialPointsDataFrame(
    coords = sr %>% 
      select(longitude, latitude) %>% 
      as.matrix(),
    data = sr,
    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  proj4string(sr) <- CRS("+proj=longlat")
  sr <- spTransform(sr, CRS("+init=epsg:4326"))
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "311 data processing complete.")) 
  
  return(sr)
}



unlist_lat_long <- function(df, colname, long.position){
  
  lat.position = ifelse(long.position == 1, 2, 1)
  
  df$long <- sapply(
    df[, colname], 
    FUN = function(x){toString(x[[long.position]][[1]])}) %>%
    unlist() %>% 
    as.numeric()
  
  df$lat <- sapply(
    df[, colname], 
    FUN = function(x){toString(x[[lat.position]][[1]])}) %>%
    unlist() %>% 
    as.numeric()
  
  return(df)
}

#define a function that will process googles server responses for us.
get_geo_details <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}
