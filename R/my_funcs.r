
#' lsos
#'
#' \code{lsos} summarizes objects in environments by their size
#'
#' @param n Number of objects to list.
#' @param pos Environment position \code{1} is the current (global) environment
#' @param order.by Attribute to order by, either \code{size} or \code{class},
#' @param decreasing logical.  decreasing or increasing order
#'
#' @return a \code{data.frame}
#'
#' @examples
#'  lsos()
#'
#' @export
#'
lsos<- function(n=10, pos=1, order.by = "Size", decreasing=TRUE) {
# List objects in environment ordered by size
obj.class<- sapply(ls(pos=pos), function(x)as.character(class(get(x,pos=pos))[1]))
obj.mode <- sapply(ls(pos=pos), function(x)mode(get(x,pos=pos)))
obj.size <- sapply(ls(pos=pos), function(x)object.size(get(x,pos=pos)))/10^6

obj.type<- ifelse(is.na(obj.class), obj.mode, obj.class)
out <- data.frame(obj.type, obj.size)
  names(out) <- c("Type", "Size")
  out <- out[order(out[[order.by]], decreasing=decreasing), ]
  out[1:n,]

}

#' rescale_shp
#'
#' \code{rescale_shp} rescales a \code{sf} object with different distance units
#'  i.e. meters to km, feet to meters etc.
#'
#' @param shp object of class \code{sf}.
#' @param units distance units to rescale to (i.e. "m","km").
#'
#' @return a \code{sf} object
#'
#' @export
#'
rescale_shp<- function(shp, units="m") {
# rescale shapefile with new distance units
  p4str<- st_crs(shp)$proj4string
  bits<- strsplit(p4str,"+", fixed=T)[[1]]
  ind<- pmatch("units", bits)
  bits[ind]<- paste0("units=",units," ")
  newstr<- paste(bits, collapse="+")
  shp<- st_transform(shp, crs=newstr)
  shp
}
