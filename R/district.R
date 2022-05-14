#' Plot districts within a province
#'
#' Helps to plot the shape of districts within a province.
#'
#' @param n_province The name of the province to plot. The notation should be respected.To get the notation: provinces()
#' @param id the id of the province.To get the id of each province provinces()
#' @return return a sf object
#'
#' @examples
#' \dontrun{
#' #Use DISTRICT name
#' prov=getDistrict("Casablanca")
#' plot(prov$coordinates)
#' #Use id
#' prov=getDistrict(id=141)
#' plot(prov$coordinates)
#' }
#' @export
getDistrict=function(n_province=NULL,id=NULL){
  path=getPathProvince(n_province,id)
  data=jsonlite::read_json(path)
  nbr_dist=length(data["Data"][[1]])
  prov_fr=c()
  prov_ar=c()
  code_prov=c()
  com_fr=c()
  cod_com=c()
  cercle_fr=c()
  cod_cercle=c()
  libelle_ar=c()
  libelle_fr=c()
  region_fr=c()
  region_ar=c()
  code_reg=c()
  coordinates=sf::st_sfc()
  for (i in 1:nbr_dist)
  {
    libelle_fr=append(libelle_fr,data["Data"][[1]][[i]]$libelle_fr)
    libelle_ar=append(libelle_ar,data["Data"][[1]][[i]]$libelle_ar)
    region_fr=append(region_fr,data["Data"][[1]][[i]]$region_fr)
    region_ar=append(region_ar,data["Data"][[1]][[i]]$region_ar)
    code_reg=append(code_reg,data["Data"][[1]][[i]]$code_reg)
    code_prov=append(code_prov,data["Data"][[1]][[i]]$code_prov)
    prov_fr=append(prov_fr,data["Data"][[1]][[i]]$prov_fr)
    prov_ar=append(prov_ar,data["Data"][[1]][[i]]$prov_ar)
    if (length(data["Data"][[1]][[i]]$com_fr)==0){com_fr=append(com_fr,"NA")
    }else{com_fr=append(com_fr,data["Data"][[1]][[i]]$com_fr)}

    if (length(data["Data"][[1]][[i]]$cod_com)==0){cod_com=append(cod_com,"NA")
    }else{cod_com=append(cod_com,data["Data"][[1]][[i]]$cod_com)}

    if (length(data["Data"][[1]][[i]]$cercle_fr)==0){cercle_fr=append(cercle_fr,"NA")
    }else{cercle_fr=append(cercle_fr,data["Data"][[1]][[i]]$cercle_fr)}

    if (length(data["Data"][[1]][[i]]$cod_cercle)==0){cod_cercle=append(cod_cercle,"NA")
    }else{cod_cercle=append(cod_cercle,data["Data"][[1]][[i]]$cod_cercle)}
    x=as.numeric(unlist(data["Data"][[1]][[i]]$Data[[1]]))
    y=as.numeric(unlist(data["Data"][[1]][[i]]$Data[[2]]))
    coordinate=sf::st_polygon(list(cbind(x,y)))
    coordinate=sf::st_sfc(coordinate)
    coordinates=rbind(coordinates,coordinate)
  }

  data=data.frame(libelle_fr,libelle_ar,region_fr,region_ar,code_reg,prov_fr,prov_ar,code_prov,com_fr,cercle_fr)
  geo=sf::st_sf(data,coordinates)
  return(geo)
}

#' Plot districts of multiple provinces
#'
#' Helps to plot the shape of districts of multiple provinces.
#'
#' @param n_province vector of The name of the province to plot. The notation should be respected.To get the notation: provinces()
#' @param id vector of provinces id.Each province has an id. To get the id of each province  : provinces()
#' @return return a sf object
#'
#' @examples
#' \dontrun{
#' prov=getMultiDistrict(c("Tanger-Assilah","Fahs-Anjra"))
#' plot(prov$coordinates)
#' prov=getMultiDistrict(id=c(227,511))
#' plot(prov$coordinates)
#' }
#' @export
getMultiDistrict=function(n_province=NULL,id=NULL){
  if(!is.null(n_province)){
    if(!is.character(n_province)){print('the first argument must be a vector of chacharcter')}
    df=getDistrict(n_province[1])
    for (i in 2:length(n_province)){
      prov=n_province[i]
      geo=getDistrict(prov)
      df=rbind(df,geo)
  }
  return(df)}
  else{
    df=getDistrict(id=id[1])
    for (i in 2:length(id)){
      geo=getDistrict(id=id[i])
      df=rbind(df,geo)
    }
    return(df)
  }
}

#' Internal function
#' @param n_province name
#' @param id  id of region
#' @noRd
getPathProvince=function(n_province=NULL,id=NULL){
  if (is.null(n_province) & !is.null(id)){
    if (is.numeric(id)){
      path_p=system.file("extdata", "prov_id.json", package = "geomarocdata")
      ids=jsonlite::read_json(path_p)
      n_province=names(which(ids==id))}
    else {
      print("id must be an integer")
    }
  }
  path=system.file("extdata", "prov_region.json", package = "geomarocdata")
  n_region=jsonlite::read_json(path)[n_province][[1]]
  short_path=paste(n_region,"/",n_province,".json",sep = "")
  path=system.file("extdata", short_path, package = "geomarocdata")
  return(path)
}


