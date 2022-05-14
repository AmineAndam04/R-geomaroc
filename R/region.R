#' Plot regions
#'
#' plot the shape of each region
#'
#' @param name The name of the region to plot. The notation should be respected.To get the notation execute: regions()
#' @param id Each region has an id. To get the id of each region please execute : regions()
#' @return return a sf object
#'
#' @examples
#' \dontrun{
#'
#' region=getRegion("Tanger-Tetouan-Al-Hoceima") #Use region name
#' #plot(region$coordinates)
#' region=getRegion(id=1) # use id
#' plot(region$coordinates)
#' }
#'
#' @export
#'
getRegion=function(name=NULL,id=NULL){
  path=system.file("extdata", "Maroc.json", package = "geomarocdata")
  data=jsonlite::read_json(path)
  if (is.null(name) & !is.null(id)){
    if (is.numeric(id)){
      path_p=system.file("extdata", "region_id.json", package = "geomarocdata")
      ids=jsonlite::read_json(path_p)
      name=names(which(ids==id))}
    else {
      print("id must be an integer")
      return(0)
    }
  }
  x=as.numeric(unlist(data[name][[1]]["Coordinates"][[1]][[1]]))
  y=as.numeric(unlist(data[name][[1]]["Coordinates"][[1]][[2]]))
  coordinates=sf::st_polygon(list(cbind(x,y)))
  coordinates=sf::st_sfc(coordinates)
  libelle_fr=c(data[name][[1]]$libelle_fr)
  libelle_ar=c(data[name][[1]]$libelle_ar)
  code_reg=c(data[name][[1]]$code_reg)
  data=data.frame(libelle_fr,libelle_ar,code_reg)
  geo=sf::st_sf(data,coordinates)
  return(geo)
}

#' Plot multiple regions
#'
#' Helps to plot the shape of multiple regions.
#'
#' @param name vector of the name of the regions to plot. The notation should be respected.To get the notation execute: regions()
#' @param id vector of regions id.Each region has an id. To get the id of each region please execute : regions()
#' @return return a sf object
#'
#' @examples
#' \dontrun{
#' #Plot two regions :Casablanca-Settat and Laayoune-Sakia-El-Hamra
#' regions=getMultiRegion(c("Eddakhla-Oued-Eddahab","Laayoune-Sakia-El-Hamra"))
#' plot(regions$coordinates)
#' #Plot the map of Morocco
#' mar=getMultiRegion(id=1:12)
#' plot(mar$coordinates)
#' }
#' @export
#'
getMultiRegion=function(name=NULL,id=NULL){
  df=sf::st_sfc()
  if (!is.null(name)){
  for (reg in name){
    geo=getRegion(reg)
    df=rbind(df,geo)
  }}
  else{
    for (ids in id){
      geo=getRegion(id=ids)
      df=rbind(df,geo)
  }}
  return(df)
}

