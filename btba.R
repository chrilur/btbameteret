btba <- read.table('data/btba.csv', sep=",", header=TRUE, stringsAsFactors=FALSE)

#Fikse på dataene
names(btba) <- c('uke', 'btmob', 'bamob', 'prosmob', 'btmob1år', 'bamob1år', 'btnett', 'banett', 'prosnett', 'btnett1år', 'banett1år', 'bttotal', 'batotal')

##Fiks data frame for å få tall
btba[1:71,6] <- NA
btba[1:19,3] <- NA
btba[1:19,4] <- NA
btba[1:19,13] <- NA
btba[92,2:13] <- NA

btba[,3] <- as.numeric(btba[,3])
btba[,4] <- as.numeric(btba[,4])
btba[,6] <- as.numeric(btba[,6])
btba[,13] <- as.numeric(btba[,13])
btba[,2] <- as.numeric(btba[,2])
btba[,5] <- as.numeric(btba[,5])
btba[,6] <- as.numeric(btba[,6])
btba[,7] <- as.numeric(btba[,7])
btba[,8] <- as.numeric(btba[,8])
btba[,9] <- as.numeric(btba[,9])
btba[,10] <- as.numeric(btba[,10])
btba[,11] <- as.numeric(btba[,11])
btba[,12] <- as.numeric(btba[,12])

print(btba)

# Hent inn nye data
new.data <- as.integer(readline("Uke: "))
new.data2 <- as.integer(readline("Trafikk, BT mobil: "))
new.data3 <- as.integer(readline("Trafikk, BA mobil: "))
new.data4 <- as.integer(readline("Trafikk, BT desktop: "))
new.data5 <- as.integer(readline("Trafikk, BA desktop: "))

#Regn ut prosent markedsandel for BA og BT
new.data6 <- round((100*new.data3/new.data2), digits=2)
new.data7 <- round((100*new.data5/new.data4), digits=2)

#Regn ut BT total og BA total
new.data8 <- new.data2 + new.data4
new.data9 <- new.data3 + new.data5

#Legg nye data til matrisen
new.data <- c(new.data, new.data2, new.data3, new.data6, NA, NA, new.data4, new.data5, new.data7, NA, NA, new.data8, new.data9)
btba <- rbind(btba, new.data)



#Beregn endring fra ett år tilbake
last.row <- nrow(btba)
year <- last.row - 52
btba[last.row, 5] <- round((btba[last.row,2]-btba[year,2]) * 100 / btba[year, 2], digits=2)
btba[last.row, 6] <- round((btba[last.row,3]-btba[year,3]) * 100 / btba[year, 3], digits=2)
btba[last.row, 10] <- round((btba[last.row,7]-btba[year,7]) * 100 / btba[year, 7], digits=2)
btba[last.row, 11] <- round((btba[last.row,8]-btba[year,8]) * 100 / btba[year, 8], digits=2)

#Transformer dataene slik at vi kan liste dem ut i en streng 

btba[1:71,6] <- 'null'
btba[1:19,3] <- 'null'
btba[1:19,4] <- 'null'
btba[1:19,13] <- 'null'
btba[92,2:13] <- 'null'

uker <- t(btba$uke)
uker2 <- t(uker[53:length(uker)])
btmobil <- t(btba$btmob)
bamobil <- t(btba$bamob)
prosmob <- t(btba$prosmob)
btmob1Y <- t(btba$btmob1år[53:length(btba$btmob1år)])
bamob1Y <- t(btba$bamob1år[53:length(btba$bamob1år)])
btnett <- t(btba$btnett)
banett <- t(btba$banett)
prosnett <- t(btba$prosnett)
btnett1Y <- t(btba$btnett1år[53:length(btba$btnett1år)])
banett1Y <- t(btba$banett1år[53:length(btba$banett1år)])
bttot <- t(btba$bttotal)
batot <- t(btba$batotal)

#Funksjon for å lage tekststrenger av data
    get.txt <- function(x){
    txt <- character()
    for (i in 1:length(x)) {
            txt <- paste0(txt, x[i], ", ")
            }
            return(txt)
            }
    
#Klistre sammen javascript-fil som rommer alle data
    jsfil <- paste(
    'var btmobil = { name: ',"'",'BT Mobil', "',",' data: [',get.txt(btmobil),']};',
    'var bamobil = { name: ',"'",'BA Mobil', "',",' data: [',get.txt(bamobil),']};',
    'var btnett = { name: ',"'",'Bergens Tidende', "',",' data: [',get.txt(btnett),']};',
    'var banett = { name: ',"'",'Bergensavisen', "',",' data: [',get.txt(banett),']};',
    'var prosmob = { name: ',"'",'Mobil: BA i % av BT', "',",' data: [',get.txt(prosmob),']};',
    'var prosnett = { name: ',"'",'Nett: BA i % av BT', "',",' data: [',get.txt(prosnett),']};',
    'var btmobil1Y = { name: ',"'",'BT Mobil', "',",' data: [',get.txt(btmob1Y),']};',
    'var badesktop1Y = { name: ',"'",'BA desktop', "',",' data: [',get.txt(banett1Y),']};',
    'var btdesktop1Y = { name: ',"'",'BT desktop', "',",' data: [',get.txt(btnett1Y),']};',
    'var bamobil1Y = { name: ',"'",'BA Mobil', "',",' data: [',get.txt(bamob1Y),']};',
    'var uker = [',get.txt(uker),'];',
    'var uker2 = [',get.txt(uker2),'];',
    'var bttot = { name: ',"'",'BT total', "',",' data: [',get.txt(bttot),']};',
    'var batot = { name: ',"'",'BA total', "',",' data: [',get.txt(batot),']};'
    , sep="")

#Skriv ut dataene
write.table(btba, 'data/btba.csv', sep=",", col.names=TRUE)
write.table(jsfil, 'data/data.js', row.names=FALSE, col.names=FALSE, quote=FALSE)
print(btba)

## Sett inn ekstra rad
##btba2 <- rbind(btba[1:91,],newrow,btba[-(1:91),])

## Fikse utregning
##    fiks.tall <- function(col,datacol){
##            for (n in 93:110) {
##                rad1 <- n-52
##                btba[n,col] <- round((btba[n,datacol]-##btba[rad1,datacol])*100/btba[rad1,datacol], digits=2)
##                }
##                return(btba)
##            }

#Noen funksjoner for å beregne utvikling basert på mediantall

hent.tall <- function(rad1,rad2,kol) {as.numeric(btba[rad1:rad2, kol])}

snitt <- function(rad1,rad2,kolbt,kolba){
    bt <- median(as.numeric(btba[rad1:rad2, kolbt]))
    ba <- median(as.numeric(btba[rad1:rad2, kolba]))
    utregning <- ba*100/bt
    return(utregning)
    }