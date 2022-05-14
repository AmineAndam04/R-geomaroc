#' Plot provinces within a region
#'
#' Helps to plot the shape of provinces within a region.
#'
#' @param n_region The name of the region to plot. The notation should be respected.To get the notation : regions()
#' @param id Each region has an id. To get the id of each region : regions()
#' @return return a sf object
#'
#' @examples
#' \dontrun{
#' #Use region name
#' region=getProvince("Tanger-Tetouan-Al-Hoceima")
#' plot(region$coordinates)
#' #Use id
#' region=getProvince(id=1) #Don't forget getRegion(id=1) not getRegion(1)
#' plot(region$coordinates)
#' }
#' @export
#'
getProvince=function(n_region=NULL,id=NULL){
  path=getPathRegion(n_region,id)
  data=jsonlite::read_json(path)
  nbr_prov=length(data["Data"][[1]])
  libelle_ar=c()
  libelle_fr=c()
  region_fr=c()
  region_ar=c()
  code_reg=c()
  code_prov=c()
  coordinates=sf::st_sfc()
  for (i in 1:nbr_prov)
  {
    libelle_fr=append(libelle_fr,data["Data"][[1]][[i]]$libelle_fr)
    libelle_ar=append(libelle_ar,data["Data"][[1]][[i]]$libelle_ar)
    region_fr=append(region_fr,data["Data"][[1]][[i]]$region_fr)
    region_ar=append(region_ar,data["Data"][[1]][[i]]$region_ar)
    code_reg=append(code_reg,data["Data"][[1]][[i]]$code_reg)
    code_prov=append(code_prov,data["Data"][[1]][[i]]$code_prov)
    x=as.numeric(unlist(data["Data"][[1]][[i]]$coordinates[[1]]))
    y=as.numeric(unlist(data["Data"][[1]][[i]]$coordinates[[2]]))
    coordinate=sf::st_polygon(list(cbind(x,y)))
    coordinate=sf::st_sfc(coordinate)
    coordinates=rbind(coordinates,coordinate)
  }
  data=data.frame(libelle_fr,libelle_ar,region_fr,region_ar,code_reg,code_prov)
  geo=sf::st_sf(data,coordinates)
  return(geo)
}

#' Plot multiple regions
#'
#' Helps to plot the shape of multiple regions.
#'
#' @param n_region vector of the name of the regions to plot. The notation should be respected.To get the notation execute: regions()
#' @param id vector of regions id.Each region has an id. To get the id of each region please execute : regions()
#' @return return a sf object
#'
#' @examples
#' \dontrun{
#' # Plot two regions :Casablanca-Settat and Rabat-Sale-Kenitra
#' regions=getMultiProvince(c("Casablanca-Settat","Rabat-Sale-Kenitra"))
#' plot(regions$coordinates)
#' #Plot provinces of Oriental and Tanger-Tetouan-AL-Hoceima
#' regions=getMultiProvince(id=c(1,2))
#' plot(regions$coordinates)
#' }
#'
#' @export
#'
getMultiProvince=function(n_region=NULL,id=NULL){
  if (!is.null(n_region)){
    df=getProvince(n_region[1])
    for (i in 2:length(n_region)){
      reg=n_region[i]
      geo=getProvince(reg)
      df=rbind(df,geo)
    }
    return(df)}
  else {
    df=getProvince(id=id[1])
    for (i in 2:length(id)){
      geo=getProvince(id=id[i])
      df=rbind(df,geo)
    }
    return(df)}
}

#' Internal function
#' @param n_region name
#' @param id  id of region
#'
getPathRegion=function(n_region=NULL,id=NULL){
  if (is.null(n_region) & !is.null(id)){
    if (is.numeric(id)){
      path_p=system.file("extdata", "region_id.json", package = "geomarocdata")
      ids=jsonlite::read_json(path_p)
      n_region=names(which(ids==id))}
    else {
      print("id must be an integer")
    }
  }
  short_path=paste("region/",n_region,".json",sep = "")
  path=system.file("extdata", short_path, package = "geomarocdata")
  return(path)
}

