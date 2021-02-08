asmnts = readxl::read_xlsx("C:\\Users\\ehinman\\Documents\\GitHub\\asmnt_map1820\\Draft_IR_tables.xlsx", sheet = "SECOND DRAFT")
asmnts1 = unique(asmnts[,names(asmnts)%in%c("Assessment Unit ID","Assessment Unit Category")])

dups= names(table(asmnts1$`Assessment Unit ID`)[table(asmnts1$`Assessment Unit ID`)>1])
dups1 = subset(asmnts1, asmnts1$`Assessment Unit ID`%in%dups)
dups1$`Assessment Unit Category`=5
dups1$`Assessment Unit Category`[dups1$`Assessment Unit ID`=="UT16020101-014_00"] = "4A"
dups1 = unique(dups1)


tof_nodups = subset(asmnts1, !asmnts1$`Assessment Unit ID`%in%dups)

tof = plyr::rbind.fill(dups1, tof_nodups)
names(tof) = c("ASSESS_ID","AU_Cat")

tof = within(tof,{
  AU_DWQCat1 = NA
  AU_DWQCat1[AU_Cat=="5"] = "Not Supporting"
  AU_DWQCat1[AU_Cat=="4A"] = "Approved TMDL"
  AU_DWQCat1[AU_Cat=="4"] = "Approved TMDL"
  AU_DWQCat1[AU_Cat=="3"] = "Insufficient Data"
  AU_DWQCat1[AU_Cat=="2"] = "No Evidence of Impairment"
  AU_DWQCat1[AU_Cat=="1"] = "Fully Supporting"
})

tot = asmnts[,c("Assessment Unit ID","Impaired Parameter","Category Description")]
totw = subset(tot, tot$`Category Description`=="Not Supporting")
totw = unique(totw) # some have new impaired uses that were included in a separate line
totw = totw%>%pivot_wider(id_cols = `Assessment Unit ID`, names_from = `Impaired Parameter`, values_from = `Impaired Parameter`)
totw = totw%>%unite("cat5_params", 2:28, sep = ", ",na.rm = TRUE, remove = TRUE)
names(totw) = c("ASSESS_ID","cat5_params")

tot4 = subset(tot, tot$`Category Description`=="Approved TMDL")
tot4 = tot4%>%pivot_wider(id_cols = `Assessment Unit ID`, names_from = `Impaired Parameter`, values_from = `Impaired Parameter`)
tot4 = tot4%>%unite("cat4A_params", 2:17, sep = ", ",na.rm = TRUE, remove = TRUE)
names(tot4) = c("ASSESS_ID", "cat4A_params")

tota = merge(totw, tot4, all = TRUE)

tmdlz = read.csv("C:\\Users\\ehinman\\Desktop\\ATTAINS_export_01192021\\associated-actions.csv")
tmdlz = unique(tmdlz[,c("ASSESSMENT_UNIT_ID","ACTION_ID")])
table(tmdlz$ASSESSMENT_UNIT_ID)[table(tmdlz$ASSESSMENT_UNIT_ID)>1]
tmdlz = tmdlz%>%pivot_wider(id_cols = ASSESSMENT_UNIT_ID, names_from = ACTION_ID, values_from = ACTION_ID)
tmdlz = tmdlz%>%unite("TMDL ID", 2:86, sep = ", ",na.rm = TRUE, remove = TRUE)
names(tmdlz) = c("ASSESS_ID","TMDL ID")
tmdlz = unique(tmdlz[,c("ASSESS_ID","TMDL ID")])

tof = merge(tof, tota, all.x = TRUE)
tof = merge(tof, tmdlz, all.x = TRUE)

aup = wqTools::au_poly
aup = merge(aup, tof, all.x = TRUE)
aup$AU_DWQCat1[is.na(aup$AU_DWQCat1)] = "Not Assessed"
aup$AU_Cat[is.na(aup$AU_Cat)]="Not Assessed"
# aup$AU_Cat[aup$AU_Cat=="4"] = "4A"
aup$AU_DWQCat1 = factor(aup$AU_DWQCat1, levels = c("Fully Supporting","No Evidence of Impairment","Insufficient Data","Not Supporting","Approved TMDL","Not Assessed"))
aup = within(aup,{
  lab=paste0(
    '<p>',
    "AU name: ", AU_NAME,
    '<br />', "AU ID: ", ASSESS_ID,
    '<br />', "Assessment: ", AU_DWQCat1,
    '<br />', "Not Supporting Parameters: ", cat5_params,
    '<br />', "Approved TMDL Parameters: ", cat4A_params,
    '<br />', "TMDL ID: ", `TMDL ID`
    )
})

aup$centroids = sf::st_centroid(aup$geometry)
coords = sf::st_coordinates(aup$centroids)
aup = cbind(aup, coords)
names(aup)[names(aup)=="X"] = "Long"
names(aup)[names(aup)=="Y"] = "Lat"

save(aup, file = "C:\\Users\\ehinman\\Documents\\GitHub\\asmnt_map1820\\au_asmnt_dat.RData")
save(aup, file = "C:\\Users\\ehinman\\Desktop\\ATTAINS_export_01192021\\au_asmnt_dat.RData")

factpal <- colorFactor(c("#4daf4a","#377eb8","#ff7f00","#e41a1c","#984ea3","#6a6666"), unique(aup$AU_DWQCat1))

map=leaflet()%>%
  addWMSTiles("https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer", group = "USGS topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE), layers = "0") %>%
  addWMSTiles("https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", group = "Hydrography", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE), layers = "0") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
  addProviderTiles("Esri.WorldTopoMap", group = "World topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
  addMapPane("underlay_polygons", zIndex = 410) %>%
  addMapPane("au_poly", zIndex = 415)  %>%
  addMapPane("markers", zIndex = 420)  %>%
  addPolygons(data=wqTools::bu_poly,group="Beneficial uses",fillOpacity = 0.1,weight=3,color="green", options = pathOptions(pane = "underlay_polygons"),
              popup=paste0(
                "Description: ", wqTools::bu_poly$R317Descrp,
                "<br> Uses: ", wqTools::bu_poly$bu_class)
  ) %>% 
  addPolygons(data=aup[aup$AU_Cat=="1",],group="Fully Supporting AUs",fillOpacity = 0.2,weight=2,color=~factpal(AU_DWQCat1), options = pathOptions(pane = "au_poly"),
              popup=~lab
  ) %>% 
  addPolygons(data=aup[aup$AU_Cat=="2",],group="No Evidence of Impairment AUs",fillOpacity = 0.2,weight=2,color=~factpal(AU_DWQCat1), options = pathOptions(pane = "au_poly"),
              popup=~lab
  ) %>% 
  addPolygons(data=aup[aup$AU_Cat=="3",],group="Insufficient Data AUs",fillOpacity = 0.2,weight=2,color=~factpal(AU_DWQCat1), options = pathOptions(pane = "au_poly"),
              popup=~lab
  ) %>% 
  addPolygons(data=aup[aup$AU_Cat=="4A",],group="Approved TMDL AUs",fillOpacity = 0.2,weight=2,color=~factpal(AU_DWQCat1), options = pathOptions(pane = "au_poly"),
              popup=~lab
  ) %>% 
  addPolygons(data=aup[aup$AU_Cat=="5",],group="Not Supporting AUs",fillOpacity = 0.2,weight=2,color=~factpal(AU_DWQCat1), options = pathOptions(pane = "au_poly"),
              popup=~lab
  ) %>% 
  addPolygons(data=aup[aup$AU_Cat=="Not Assessed",],group="Not Assessed AUs",fillOpacity = 0.2,weight=2,color=~factpal(AU_DWQCat1), options = pathOptions(pane = "au_poly"),
              popup=~lab
  ) %>% 
  addPolygons(data=wqTools::ss_poly,group="Site-specific standards",fillOpacity = 0.1,weight=3,color="blue", options = pathOptions(pane = "underlay_polygons"),
              popup=paste0("SS std: ", wqTools::ss_poly$SiteSpecif)
  ) %>%
  addPolygons(data=wqTools::wmu_poly,group="Watershed management units",fillOpacity = 0.1,weight=3,color="red", options = pathOptions(pane = "underlay_polygons"),
              popup=wqTools::wmu_poly$Mgmt_Unit
  ) %>%
  addPolygons(data=wqTools::ut_poly,group="UT boundary",smoothFactor=3,fillOpacity = 0.2,weight=2,color="purple", options = pathOptions(pane = "underlay_polygons")) %>%
  addCircles(data = aup, lng=~Long, lat=~Lat, group = "AUID",stroke=F, fill=F, label=~ASSESS_ID,
             popup = aup$ASSESS_ID) %>%
  addCircles(data = aup, lng =~Long, lat =~Lat, group = "AUName",stroke=F, fill=F, label=~AU_NAME,
             popup = aup$AU_NAME)%>%
  hideGroup("Site-specific standards") %>%
  hideGroup("Beneficial uses") %>%
  hideGroup("UT boundary") %>%
  hideGroup("Watershed management units")%>%
  addLayersControl(map,position ="topleft",baseGroups = c("World topo","USGS topo", "Hydrography", "Satellite"),overlayGroups = c("Fully Supporting AUs","No Evidence of Impairment AUs","Insufficient Data AUs","Approved TMDL AUs","Not Supporting AUs","Not Assessed AUs","Beneficial uses", "Site-specific standards", "Watershed management units", "UT boundary"),
                                options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))%>%
  addLegend("topright", pal = factpal, values = unique(aup$AU_DWQCat1),title = "Assessment Category",opacity = 0.8)%>%wqTools::addMapResetButton()%>%
  leaflet.extras::removeSearchFeatures() %>% 
  leaflet.extras::addSearchFeatures(
    targetGroups = c('AUID','AUName'),
    options = leaflet.extras::searchFeaturesOptions(
      zoom=12, openPopup = FALSE, firstTipSubmit = TRUE,
      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))
map

library(htmlwidgets)
saveWidget(map, "assessment_map_v1.html")
