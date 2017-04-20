library(foreign)
library(rgdal)
library(dplyr)
library(rgeos)
library("stringr")
library(data.table)

# for initially reading in the data
nyc = fread("/home/vis/cr173/Sta523/data/nyc/nyc_311.csv")

nyc_loc = nyc %>%
 dplyr::select(Unique.Key, Location.Type, Incident.Zip, Incident.Address,
        Intersection.Street.1, Intersection.Street.2, Address.Type, City, Borough, Complaint.Type) %>%
   filter(Borough != "Unspecified") %>%
   as.data.frame()

# save(nyc_loc, file = "nyc_loc.RData")

# load("nyc_loc.RData")

sample_data = sample_n(nyc_loc, size = 500000, replace=FALSE)

#final_word = word(sample_data$Incident.Address, start = -1 )
#sort(unique(final_word))

sample_data$Incident.Address = sample_data$Incident.Address %>%
  str_replace_all("AVE$", "AVENUE") %>%
  str_replace_all("ave$", "AVENUE") %>%
  str_replace_all("avenue$", "AVENUE") %>%
  str_replace_all("BLVD$", "BOULEVARD") %>%
  str_replace_all("BVLD$", "BOULEVARD") %>%
  str_replace_all("EXPY$", "EXPRESSWAY") %>%
  str_replace_all("XPWY$", "EXPRESSWAY") %>%
  str_replace_all("PL$", "PLACE") %>%
  str_replace_all("PLZA$", "PLAZA") %>%
  str_replace_all("[:space:]+RD$", "ROAD") %>%
  str_replace_all("ROAD$", " ROAD") %>%
  str_replace_all("[:space:]{2,}ROAD$", " ROAD") %>%
  str_replace_all("PKWY$", "PARKWAY") %>%
  str_replace_all("DR$", "DRIVE") %>%
  str_replace_all("[:space:]+W$", "WEST") %>%
  str_replace_all("WEST$", " WEST") %>%
  str_replace_all("[:space:]{2,}WEST$", " WEST") %>%
  str_replace_all("[:space:]+E$", "EAST") %>%
  str_replace_all("EAST$", " EAST") %>%
  str_replace_all("[:space:]{2,}EAST$", " EAST") %>%
  str_replace_all("[:space:]+N$", "NORTH") %>%
  str_replace_all("NORTH$", " NORTH") %>%
  str_replace_all("[:space:]{2,}NORTH$", " NORTH") %>%
  str_replace_all("CT$", "COURT") %>%
  str_replace_all("TER$", "TERRACE") %>%
  str_replace_all("TERR$", "TERRACE") %>%
  str_replace_all("ALY$", "ALLEY") %>%
  str_replace_all("CIR$", "CIRCLE") %>%
  str_replace_all("CRCL$", "CIRCLE") %>%
  str_replace_all("CV$", "COVE") %>%
  str_replace_all("HWY$", "HIGHWAY") %>%
  str_replace_all("LN$", "LANE") %>%
  str_replace_all("SQ$", "SQUARE") %>%
  str_replace_all("[:space:]+ST$", "STREET") %>%
  str_replace_all("STREET$", " STREET") %>%
  str_replace_all("[:space:]{2,}STREET$", " STREET") %>%
  str_replace_all("KINGSHIGWY$", "KINGSHIGHWAY") %>%
  str_replace_all("KINSGHIGHWAY$", "KINGSHIGHWAY") %>%
  str_replace_all("TPKE$", "TURNPIKE") %>%
  str_replace_all("TNPK$", "TURNPIKE") %>%
  #str_replace_all("[:space:]+ST\\.$", "STREET") %>%
  str_replace_all("[:space:]+st$", "STREET") %>%
  str_replace_all("STREET$", " STREET") %>%
  str_replace_all("[:space:]{2,}STREET$", " STREET") %>%
  str_replace_all("PK$", "PARK") %>%
  str_replace_all("loop$", "LOOP") %>%
  str_replace_all("broadway$", "BROADWAY") %>%
  str_replace_all("TRL$", "TRAIL") 
  

sample_data$Intersection.Street.1 = sample_data$Intersection.Street.1 %>%
  str_replace_all("AVE$", "AVENUE") %>%
  str_replace_all("ave$", "AVENUE") %>%
  str_replace_all("avenue$", "AVENUE") %>%
  str_replace_all("BLVD$", "BOULEVARD") %>%
  str_replace_all("BVLD$", "BOULEVARD") %>%
  str_replace_all("EXPY$", "EXPRESSWAY") %>%
  str_replace_all("XPWY$", "EXPRESSWAY") %>%
  str_replace_all("PL$", "PLACE") %>%
  str_replace_all("PLZA$", "PLAZA") %>%
  str_replace_all("[:space:]+RD$", "ROAD") %>%
  str_replace_all("ROAD$", " ROAD") %>%
  str_replace_all("[:space:]{2,}ROAD$", " ROAD") %>%
  str_replace_all("PKWY$", "PARKWAY") %>%
  str_replace_all("DR$", "DRIVE") %>%
  str_replace_all("[:space:]+W$", "WEST") %>%
  str_replace_all("WEST$", " WEST") %>%
  str_replace_all("[:space:]{2,}WEST$", " WEST") %>%
  str_replace_all("[:space:]+E$", "EAST") %>%
  str_replace_all("EAST$", " EAST") %>%
  str_replace_all("[:space:]{2,}EAST$", " EAST") %>%
  str_replace_all("[:space:]+N$", "NORTH") %>%
  str_replace_all("NORTH$", " NORTH") %>%
  str_replace_all("[:space:]{2,}NORTH$", " NORTH") %>%
  str_replace_all("CT$", "COURT") %>%
  str_replace_all("TER$", "TERRACE") %>%
  str_replace_all("TERR$", "TERRACE") %>%
  str_replace_all("ALY$", "ALLEY") %>%
  str_replace_all("CIR$", "CIRCLE") %>%
  str_replace_all("CRCL$", "CIRCLE") %>%
  str_replace_all("CV$", "COVE") %>%
  str_replace_all("HWY$", "HIGHWAY") %>%
  str_replace_all("LN$", "LANE") %>%
  str_replace_all("SQ$", "SQUARE") %>%
  str_replace_all("[:space:]+ST$", "STREET") %>%
  str_replace_all("STREET$", " STREET") %>%
  str_replace_all("[:space:]{2,}STREET$", " STREET") %>%
  str_replace_all("KINGSHIGWY$", "KINGSHIGHWAY") %>%
  str_replace_all("KINSGHIGHWAY$", "KINGSHIGHWAY") %>%
  str_replace_all("TPKE$", "TURNPIKE") %>%
  str_replace_all("TNPK$", "TURNPIKE") %>%
  #str_replace_all("[:space:]+ST\\.$", "STREET") %>%
  str_replace_all("[:space:]+st$", "STREET") %>%
  str_replace_all("STREET$", " STREET") %>%
  str_replace_all("[:space:]{2,}STREET$", " STREET") %>%
  str_replace_all("PK$", "PARK") %>%
  str_replace_all("loop$", "LOOP") %>%
  str_replace_all("broadway$", "BROADWAY") %>%
  str_replace_all("TRL$", "TRAIL") 
  
sample_data$Intersection.Street.2 = sample_data$Intersection.Street.2 %>%
  str_replace_all("AVE$", "AVENUE") %>%
  str_replace_all("ave$", "AVENUE") %>%
  str_replace_all("avenue$", "AVENUE") %>%
  str_replace_all("BLVD$", "BOULEVARD") %>%
  str_replace_all("BVLD$", "BOULEVARD") %>%
  str_replace_all("EXPY$", "EXPRESSWAY") %>%
  str_replace_all("XPWY$", "EXPRESSWAY") %>%
  str_replace_all("PL$", "PLACE") %>%
  str_replace_all("PLZA$", "PLAZA") %>%
  str_replace_all("[:space:]+RD$", "ROAD") %>%
  str_replace_all("ROAD$", " ROAD") %>%
  str_replace_all("[:space:]{2,}ROAD$", " ROAD") %>%
  str_replace_all("PKWY$", "PARKWAY") %>%
  str_replace_all("DR$", "DRIVE") %>%
  str_replace_all("[:space:]+W$", "WEST") %>%
  str_replace_all("WEST$", " WEST") %>%
  str_replace_all("[:space:]{2,}WEST$", " WEST") %>%
  str_replace_all("[:space:]+E$", "EAST") %>%
  str_replace_all("EAST$", " EAST") %>%
  str_replace_all("[:space:]{2,}EAST$", " EAST") %>%
  str_replace_all("[:space:]+N$", "NORTH") %>%
  str_replace_all("NORTH$", " NORTH") %>%
  str_replace_all("[:space:]{2,}NORTH$", " NORTH") %>%
  str_replace_all("CT$", "COURT") %>%
  str_replace_all("TER$", "TERRACE") %>%
  str_replace_all("TERR$", "TERRACE") %>%
  str_replace_all("ALY$", "ALLEY") %>%
  str_replace_all("CIR$", "CIRCLE") %>%
  str_replace_all("CRCL$", "CIRCLE") %>%
  str_replace_all("CV$", "COVE") %>%
  str_replace_all("HWY$", "HIGHWAY") %>%
  str_replace_all("LN$", "LANE") %>%
  str_replace_all("SQ$", "SQUARE") %>%
  str_replace_all("[:space:]+ST$", "STREET") %>%
  str_replace_all("STREET$", " STREET") %>%
  str_replace_all("[:space:]{2,}STREET$", " STREET") %>%
  str_replace_all("KINGSHIGWY$", "KINGSHIGHWAY") %>%
  str_replace_all("KINSGHIGHWAY$", "KINGSHIGHWAY") %>%
  str_replace_all("TPKE$", "TURNPIKE") %>%
  str_replace_all("TNPK$", "TURNPIKE") %>%
  #str_replace_all("[:space:]+ST\\.$", "STREET") %>%
  str_replace_all("[:space:]+st$", "STREET") %>%
  str_replace_all("STREET$", " STREET") %>%
  str_replace_all("[:space:]{2,}STREET$", " STREET") %>%
  str_replace_all("PK$", "PARK") %>%
  str_replace_all("loop$", "LOOP") %>%
  str_replace_all("broadway$", "BROADWAY") %>%
  str_replace_all("TRL$", "TRAIL") 

#path.expand("~cr173")
#inter = readOGR(path.expand("~cr173/Sta523/data/nyc/intersections/"),"intersections")
#load("/home/vis/cr173/Sta523/data/nyc/intersections/intersections.Rdata")
#load("intersections.Rdata")
#inter <- data
#names(inter)[4:5] =c("street1","street2")

#final_word = word(inter$street2, start = -1 )
#unique(final_word)
#inter$street1 = str_replace_all(inter$street1, "AVE", "AVENUE")
#inter$street1 = str_replace_all(inter$street1, "BLVD", "BOULEVARD")
#inter$street1 = str_replace_all(inter$street1, "EXPY", "EXPRESSWAY")
#inter$street1 = str_replace_all(inter$street1, "PL", "PLAZA")
#inter$street1 = str_replace_all(inter$street1, "RD", "ROAD")
#inter$street1 = str_replace_all(inter$street1, "PKWY", "PARKWAY")
#inter$street1 = str_replace_all(inter$street1, "DR", "DRIVE")
#inter$street1 = str_replace_all(inter$street1, "W", "WEST")
#inter$street1 = str_replace_all(inter$street1, "E", "EAST")
#inter$street1 = str_replace_all(inter$street1, "N", "NORTH")
#inter$street1 = str_replace_all(inter$street1, "CT", "COURT")
#inter$street1 = str_replace_all(inter$street1, "TER", "TERRACE")
#inter$street1 = str_replace_all(inter$street1, "ALY", "ALLEY")
#inter$street1 = str_replace_all(inter$street1, "CIR", "CIRCLE")
#inter$street1 = str_replace_all(inter$street1, "CV", "COVE")
#inter$street1 = str_replace_all(inter$street1, "HWY", "HIGHWAY")
#inter$street1 = str_replace_all(inter$street1, "LN", "LANE")
#inter$street1 = str_replace_all(inter$street1, "SQ", "SQUARE")
#inter$street1 = str_replace_all(inter$street1, "ST", "STREET")
#inter$street1 = str_replace_all(inter$street1, "TRL", "TRAIL")

#inter$street2 = str_replace_all(inter$street2, "AVE", "AVENUE")
#inter$street2 = str_replace_all(inter$street2, "BLVD", "BOULEVARD")
#inter$street2 = str_replace_all(inter$street2, "EXPY", "EXPRESSWAY")
#inter$street2 = str_replace_all(inter$street2, "PL", "PLAZA")
#inter$street2 = str_replace_all(inter$street2, "RD", "ROAD")
#inter$street2 = str_replace_all(inter$street2, "PKWY", "PARKWAY")
#inter$street2 = str_replace_all(inter$street2, "DR", "DRIVE")
#inter$street2 = str_replace_all(inter$street2, "W", "WEST")
#inter$street2 = str_replace_all(inter$street2, "E", "EAST")
#inter$street2 = str_replace_all(inter$street2, "N", "NORTH")
#inter$street2 = str_replace_all(inter$street2, "CT", "COURT")
#inter$street2 = str_replace_all(inter$street2, "TER", "TERRACE")
#inter$street2 = str_replace_all(inter$street2, "ALY", "ALLEY")
#inter$street2 = str_replace_all(inter$street2, "CIR", "CIRCLE")
#inter$street2 = str_replace_all(inter$street2, "CV", "COVE")
#inter$street2 = str_replace_all(inter$street2, "HWY", "HIGHWAY")
#inter$street2 = str_replace_all(inter$street2, "LN", "LANE")
#inter$street2 = str_replace_all(inter$street2, "SQ", "SQUARE")
#inter$street2 = str_replace_all(inter$street2, "ST", "STREET")
#inter$street2 = str_replace_all(inter$street2, "TRL", "TRAIL")

load("/home/vis/cr173/Sta523/data/nyc/pluto/pluto.Rdata")

merge_sample = inner_join(sample_data, pluto, by = c("Incident.Address" = "Address"))
#merge_sample = left_join(merge_sample, inter, 
 #                        by = c("Intersection.Street.1" = "street1", 
  #                              "Intersection.Street.2" = "street2"))
merge_sample = merge_sample[!(is.na(merge_sample$x)) | !(is.na(merge_sample$y)), ]
merge_sample = merge_sample %>% 
  mutate(Borough = Borough.y) %>%
  dplyr::select(Unique.Key, Location.Type, Incident.Zip, Incident.Address, Intersection.Street.1,
         Intersection.Street.2, Address.Type, City, Complaint.Type, x, y, Borough) %>%
  distinct(Unique.Key)
names(merge_sample) = c("Unique.Key", "Location.Type", "Incident.Zip", "Incident.Address", 
                        "Intersection.Street.1", "Intersection.Street.2", 
                        "Address.Type",  "City", "Complaint.Type", 
                        "lon", "lat", "Borough")

save(merge_sample, file = "nyc_merged_clean.Rdata")
# plot(merge_sample$x, merge_sample$y)
