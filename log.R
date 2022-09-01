library(xml2)
library(purrr)
library(dplyr)
library(magrittr)

grab_and_log = function(){
  
  api_key = pins::pin_read(pins::board_local(), "api_key")
  
  url = list()
  url["x6"] = stringr::str_c("https://data.bus-data.dft.gov.uk/api/v1/datafeed/?operatorRef=FBRA&lineRef=X6&api_key=", api_key)
  url["72"] = stringr::str_c("https://data.bus-data.dft.gov.uk/api/v1/datafeed/?operatorRef=FBRA&lineRef=72&api_key=", api_key)
  
  data = map(url, read_xml)
  
  
  parse_one_vehicle = function(vehicle){
    vehicle =   pluck(vehicle, 1, "MonitoredVehicleJourney") 
    
    tibble(
      line_ref  = pluck(vehicle, "LineRef", 1),
      direction = pluck(vehicle, "DirectionRef", 1),
      origin = pluck(vehicle, "OriginName", 1),
      destination = pluck(vehicle, "DestinationName", 1),
      target_depart_time = pluck(vehicle, "OriginAimedDepartureTime", 1),
      target_arrive_time = pluck(vehicle, "DestinationAimedArrivalTime", 1),
      lon = pluck(vehicle, "VehicleLocation","Longitude", 1) %>% 
        as.numeric(),
      lat = pluck(vehicle, "VehicleLocation","Latitude", 1) %>% 
        as.numeric(),
      bearing = pluck(vehicle, "Bearing", 1) %>% 
        as.numeric(),
      vehicle_ref = pluck(vehicle, "VehicleRef", 1)
    )
  }
  
  parse = function(data){
    data = xml_ns_strip(data)
    
    vehicles = xml_find_all(data, "//VehicleActivity") %>% 
      as_list()
    
    map(seq_along(vehicles), function(x) parse_one_vehicle(vehicles[x])) %>% 
      bind_rows()
  }
  
  file = file.path("/home/james/bin/R/bus/data/", stringr::str_c(lubridate::today(), ".csv")) #hardcoded path so it works with cron

  out = map_dfr(data, parse) %>% 
    mutate(timestamp = lubridate::now()) 
  
  readr::write_csv(out, file, append = file.exists(file))
  
}
grab_and_log()
