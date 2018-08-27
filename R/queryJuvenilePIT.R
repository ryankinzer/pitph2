#' @title Columbia River PIT Tag Observation Data by Interogation Site for Juvenile Fish
#'
#' @description Query DART's PIT Tag Observations by Observation Site Detail
#'
#' @author Ryan N. Kinzer and Kevin See
#'
#' @param site The observation site code to query for PIT tag data. 
#' @param year Year to query. Available years depend on site.
#' @param start_day Start date (\code{month / day}) of query. 
#' @param end_day End date (\code{month / day}) of query.
#'
#' @source \url{http://www.cbr.washington.edu/dart}
#'
#' @import lubridate readr httr dplyr tidyr
#' @export
#' @return NULL
#' @examples 
#' 
#' queryPITtagAdult(spawn_yr = 2015)

queryJuvenilePIT = function(species = c('Chinook', 'Steelhead'),
                            run = c('Spring', 'Summer', 'Fall', 'Winter', 'Unknown', 'All'),
                            rear_type = c('Wild', 'Hatchery', 'Unknown', 'All'),
                            life_stage = c('Juvenile', 'Adult', 'Unknown', 'All'),
                            site = c('LWG','LGS','LMN','IHA','MCN','JDA','TDA','BON'), 
                            year = NULL,
                            start_day = NULL,
                            end_day = NULL) {
  
  # webpage url: http://www.cbr.washington.edu/dart/query/pit_obs_detail
  
  species = match.arg(species)
  
  species = switch(species,
                   Chinook = '1',
                   Steelhead = '3')
  
  run = match.arg(run)
  
  run = switch(run,
               Spring = '1',
               Summer = '2',
               Fall = '3', 
               Winter = '4',
               Unknown = '5',
               All = 'Null')
  
  rear_type = match.arg(rear_type)
  
  rear_type = switch(rear_type,
                     Wild = 'W',
                     Hatchery = 'H',
                     Unknown = 'U',
                     All = 'Null')
  
  life_stage = match.arg(life_stage)
  
  life_stage = switch(life_stage,
                      Juvenile = 'J',
                      Adult = 'A',
                      Unknown = 'U',
                      All = 'Null')
  
  # pull out default observation site
  site = match.arg(site)
  
  site = switch(site,
                LWG = 'LWG%3ALower+Granite+Dam+Juvenile+%28GRJ%29+rkm+522.173',
                LGS = 'LGS%3ALittle+Goose+Dam+Juvenile+%28GO2+GOJ%29+rkm+522.113',
                LMN = 'LMN%3ALower+Monumental+Dam+Juvenile+%28LM2+LMJ%29+rkm+522.067',
                IHA = 'IHA%3AIce+Harbor+Dam+Combined+%28ICH+IHA%29+rkm+522.016',
                MCN = 'MCN%3AMcNary+Dam+Juvenile+%28MCJ%29+rkm+470',
                JDA = 'JDA%3AJohn+Day+Dam+Juvenile+%28JDJ%29+rkm+347',
                TDA = 'TDA%3AThe+Dalles+Dam+Adult+Fishways+%28TD1+TD2%29+rkm+308',
                BON = 'BON%3ABonneville+Dam+Juvenile+%28B1J+B2J+BCC+BVJ+BVX%29+rkm+234')
  
  # need a year
  stopifnot(!is.null(year))
  
  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/ryankinzer/cuyem')
  
  # compose url with query
  #url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pitall_obs_de.php'
  
  # build query for DART
  #?sc=1&queryName=pit_obs_de&outputFormat=default&year=2018&proj=BON%3ABonneville+Dam+Juvenile+%28B1J+B2J+BCC+BVJ+BVX%29+rkm+234&species=1&run=1&rear_type=W&stage=Null&span=no&startdate=1%2F1&enddate=12%2F31&syear=2018&eyear=2018&summary=no
  # queryList = list(sc = 1,
  #                  queryName = 'pit_obs_de',
  #                  outputFormat = 'csv',
  #                  year = year,
  #                  proj = as.character(site),
  #                  species = species,
  #                  run = run,
  #                  rear_type = rear_type,
  #                  stage = life_stage,
  #                  span = 'no',
  #                  startdate = start_day,
  #                  enddate = end_day,
  #                  syear = year,
  #                  eyear = year,
  #                  summary = 'no')
  # 
  # # Check URL
  # tmp_url <- modify_url(url_req, query = queryList)
  # tmp_url <- gsub("%25", "%", tmp_url)
  # tmp_url <- gsub("%2B", "+", tmp_url)
  # 
  # # send query to DART
  # web_req = httr::GET(url_req, ua,
  #                     query = queryList)
  
  url <- paste0('http://www.cbr.washington.edu/dart/cs/php/rpt/pitall_obs_de.php?sc=1&queryName=pit_obs_de&outputFormat=csv&year=',year,
                    '&proj=',site,'&species=', species, '&run=', run, '&rear_type=', rear_type, '&stage=',life_stage,
                    '&span=no&startdate=',gsub("/", "%2F", start_day),'&enddate=',gsub("/","%2F",end_day),"&syear=",year,
                    "&eyear=", year, "&summary=no")
  
  #tmp_web_req <- httr::GET(tmp_url)

  # test --- WORKS!!!!
  #url <- paste0('http://www.cbr.washington.edu/dart/cs/php/rpt/pitall_obs_de.php?sc=1&queryName=pit_obs_de&outputFormat=csv&year=2017&proj=LWG%3ALower+Granite+Dam+Juvenile+%28GRJ%29+rkm+522.173&species=1&run=1&rear_type=W&stage=J&span=no&startdate=1%2F1&enddate=12%2F31&syear=2017&eyear=2017&summary=no')
  
  web_req <- httr::GET(url)
  
    # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from DART')
  
  # what encoding to use?
  # stringi::stri_enc_detect(content(web_req, "raw"))
  
  # parse the response
  parsed = httr::content(web_req,
                         'text') %>% #text
    read_delim(delim = ',',
               col_names = T)
  
  if(is.null(parsed)) {
    message(paste('DART returned no data for', species, 'in', year, '\n'))
    stop
  }
  
  if(class(parsed)[1] == 'xml_document') {
    message(paste('For', species, 'in', year, 'XML document returned by DART instead of data\n'))
    stop
  }
  
  if (httr::status_code(web_req) != 200) {
    message(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        status_code(web_req),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
    stop
  }
  
  names(parsed) <- gsub(" ", "_", tolower(names(parsed)))

  return(parsed)
}
