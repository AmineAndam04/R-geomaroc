#' Notation : regions
#'
#' Hepls to respect the notation and to get the id and the name of each region
#' @return return a dataframe
#' @examples
#' #region=regions()
#' #region
#' @export
#'
regions=function(){
  path=system.file("extdata", "extdata.zip", package = "geomaroc")
  id=jsonlite::read_json(utils::unzip(path,"region_id.json"))
  id=data.frame(unlist(id))
  return(id)
}

#' Notation : regions
#'
#' Hepls to respect the notation and to get the id and the name of each province
#' @return return a dataframe
#' @examples
#' #province=provinces()
#' #province
#' @export
#'
provinces=function(){
  path=system.file("extdata", "extdata.zip", package = "geomaroc")
  id=jsonlite::read_json(utils::unzip(path,"region_prov.json"))
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
