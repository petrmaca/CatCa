#' Assign the global paths
#'
#'
#' Assign \code{.datadir} (owncloud with the data) and \code{.workdir} to be used as data and working directory.
#'
#' @return
#' @export give_paths
#'
#' @examples
give_paths <- function(){
where <- if(.Platform[["OS.type"]] == 'unix') (Sys.info()['nodename']) else (Sys.getenv('COMPUTERNAME'))

switch(where,
         'match' = {
           .datadir = "/home/owc/BILAN_UPOV/used_data/"
           .workdir = "/home/owc/RUserGroup/data/"
         },
         'desrt' = {
           .datadir <- "/media/mha/Windows/OwnCloud/RUserGroup/data/"
           .workdir <- '/home/hanel/KVHEM/Rusergroup/code/'
         },
         'DESKTOP-444RM63' = {
           .datadir <- "C://Users//PetrP//ownCloud//data//used_data//"
           .workdir <- "C://Users//PetrP//Documents//BILAN_OWNCL//"
         }, 
       "LEST" = {
         .datadir = "C://testR//data//"
         .workdir = "C://testR//"
       }

)

  if (is.null(.datadir)) {
    .datadir = NA
    warning('Pocitac ', where, 'nenalezen v environment.R - data nedostupna!\n Uprav environment.R a push na github.com/KVHEM/CatCa')
    }

  if (is.null(.workdir)) {
    .workdir = getwd()
    warning('Pocitac ', where, 'nenalezen v environment.R - pracovni adresar nastaven automaticky!')
  }

  assign('.datadir', .datadir, envir = .GlobalEnv)
  assign('.workdir', .workdir, envir = .GlobalEnv)
}


.onLoad <- function(libname, pkgname) {
  give_paths()
  invisible()
}
