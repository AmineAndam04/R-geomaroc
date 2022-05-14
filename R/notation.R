#' Notation : regions
#'
#' Hepls to respect the notation and to get the id and the name of each region
#' @return return a dataframe
#' @examples
#' \dontrun{
#' region=regions()
#' region}
#' @export
#'
regions=function(){
  path=system.file("extdata", "region_id.json", package = "geomarocdata")
  id=jsonlite::read_json(path)
  id=data.frame(unlist(id))
  return(id)
}

#' Notation : provinces
#'
#' Hepls to respect the notation and to get the id and the name of each province
#' @return return a dataframe
#' @examples
#' \dontrun{
#' province=provinces()
#' province
#' }
#' @export
#'
provinces=function(){
  path_p=system.file("extdata", "region_prov.json", package = "geomarocdata")
  id=jsonlite::read_json(path_p)
  regs=names(id)
  Region=c()
  Province=c()
  code=c()
  for (reg in regs){
    provs=names(id[reg][[1]])
    for (prov in provs){
      Region=append(Region,reg)
      Province=append(Province,prov)
      code=append(code,id[reg][[1]][prov][[1]])
    }
  }
  return(data.frame(Region,Province,code))
}
