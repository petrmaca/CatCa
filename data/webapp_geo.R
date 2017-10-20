REKY = rgdal::readOGR(file.path(.datadir,"geo/reky.shp"), "reky")
JEZERA = rgdal::readOGR(file.path(.datadir,"geo/jezera.shp"), "jezera")
POVODI = rgdal::readOGR(file.path(.datadir,"geo/povodi.shp"), "povodi")
STANICE = rgdal::readOGR(file.path(.datadir,"chmu/stanice.shp"), "stanice")