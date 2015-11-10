library(RCurl)
library(RJSONIO)

#Sett uke
#uke <- as.integer(readline("Uke : "))

#Skrap data
urlnett <- paste0('http://tnslistene.no/?metric=uv&list_id=1&year=2015&week=',uke)
urlmob <- paste0('http://tnslistene.no/?metric=uv&list_id=6&year=2015&week=',uke)
tnsnett <- getURL(urlnett)
tnsmob <- getURL(urlmob)

#Se etter disse linjene i dataene
btnettref <- '<a href="http://www.bt.no">Bergens Tidende</a>'
banettref <- '<a href="http://www.ba.no">Bergensavisen</a>'
btmobref <- '<a href="http://mobil.bt.no/bt.mob">Bt Mobil</a>'
bamobref <- '<a href="mobil.ba.no">Bergensavisen Mobil</a>'

#Funksjon for å renske data
rensk.data <- function(x) {
        tns <- unlist(strsplit(x, split='var all = ', fixed=TRUE))[2]
        tns <- unlist(strsplit(tns, split='\nfunction check_fix_value(a_val)', fixed=TRUE))[1]
        json <- fromJSON(tns)
        tns <- do.call("rbind", json$data)
        tns <- as.data.frame(tns)
        tns$Graf <- NULL
        return(tns)
        }

tnsnett <- rensk.data(tnsnett)
tnsmob <- rensk.data(tnsmob)

btnett <- subset(tnsnett, tnsnett[,2] %in% btnettref)
btnett <- as.integer(unlist(btnett[3]))
banett <- subset(tnsnett, tnsnett[,2] %in% banettref)
banett <- as.integer(unlist(banett[3]))

btmob <- subset(tnsmob, tnsmob[,2] %in% btmobref)
btmob <- as.integer(unlist(btmob[3]))
bamob <- subset(tnsmob, tnsmob[,2] %in% bamobref)
bamob <- as.integer(unlist(bamob[3]))

