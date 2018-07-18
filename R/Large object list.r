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

