library(RCurl)
library(RJSONIO)

tns <- getURL('http://tnslistene.no/?metric=uv&list_id=1&year=2015&week=10')
tns <- unlist(strsplit(tns, split='var all = ', fixed=TRUE))[2]
tns <- unlist(strsplit(tns, split='\nfunction check_fix_value(a_val)', fixed=TRUE))[1]

json_file <- fromJSON(tns)

json_file <- lapply(json_file, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
})

tns <- do.call("rbind", json_file)