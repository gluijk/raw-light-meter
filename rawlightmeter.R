# El RAW como fotómetro de precisión
# www.datosimagensonido.com


# FUNCIONES RAW

LoadRAW = function(filename, verbose=T, scale=T, integer=F) {
    # Lee archivo RAW cargándolo en una matriz
    # scale=T: sustrae offset de negro y escala saturación al fondo de escala
    #          en otro caso no escala valores numéricos RAW
    # integer=F: datos RAW en rango coma flotante 0..1
    #            en otro caso rango entero 0..65535 (16 bits)
    library(tiff)  # Para leer el TIFF generado por DCRAW
    
    cmd=paste0("dcraw ",  # Construimos comando de línea DCRAW
               iif(verbose, "-v ", ""),
               iif(scale, "-d -r 1 1 1 1", "-D"),  # RAW escalado (-d) o puro (-D)
               " -S 16382 -t 0 -4 -T ", filename)  # Sin rotación (-t), lineal (-4), TIFF (-T)
    if (verbose) cat(paste0(cmd, "\n"))  # Mostrar comando en consola
    system(cmd)
    
    raw=readTIFF(paste0(substr(filename, 1, nchar(filename)-4), ".tiff"))
    if (integer) raw=round(raw*65535)
    if (verbose) cat(paste0("Options: scale=", scale,
                            ", integer=", integer, "\n"))
    return (raw)
}

DebayerRAW = function(raw, pattern="RG/GB", averageG=T) {
    # Deshace patrón de Bayer (requiere RAW)
    NROW=nrow(raw)
    if (NROW%%2 | ncol(raw)%%2 |
        !(pattern %in% c("RG/GB", "BG/GR", "GR/BG", "GB/RG"))) return (-1)
    
    # Índices Bayer: se calculan y almacenan solo una vez
    i=which(row(raw)%%2 & col(raw)%%2)  # Fotosito R de un patrón RG/GB
    if (averageG) {  # Devuelve {R, (G1+G2)/2, B}
        if        (pattern=="RG/GB") {
            img=c(raw[i], (raw[i+NROW]+raw[i+1])/2, raw[i+NROW+1])
        } else if (pattern=="BG/GR") {
            img=c(raw[i+NROW+1], (raw[i+NROW]+raw[i+1])/2, raw[i])
        } else if (pattern=="GR/BG") {
            img=c(raw[i+NROW], (raw[i]+raw[i+NROW+1])/2, raw[i+1])
        } else {
            img=c(raw[i+1], (raw[i]+raw[i+NROW+1])/2, raw[i+NROW])
        }
        dim(img)=c(dim(raw)/2, 3)
    } else {  # Devuelve {R, G1, G2, B}
        if        (pattern=="RG/GB") {
            img=c(raw[i], raw[i+NROW], raw[i+1], raw[i+NROW+1])
        } else if (pattern=="BG/GR") {
            img=c(raw[i+NROW+1], raw[i+NROW], raw[i+1], raw[i])
        } else if (pattern=="GR/BG") {
            img=c(raw[i+NROW], raw[i], raw[i+NROW+1], raw[i+1])
        } else {
            img=c(raw[i+1], raw[i], raw[i+NROW+1], raw[i+NROW])
        }
        dim(img)=c(dim(raw)/2, 4)    
    }
    
    return(img)
}

ShowRAW = function(img, trunc=T, gamma=2.2, interpolate=F) {
    # Muestra RAW en pantalla (admite RAW y de-bayer)
    # Solo si trunc=F y la imagen excede de 1 se reescala a 1
    if (length(dim(img))>2 & dim(img)[3]>3) {
        img=img[,,c(1:2,4)]
        warning("Using G1 sub-channel, G2 sub-channel ignored")
    }
    img[img<0]=0
    if (trunc) img[img>1]=1
    plot(as.raster((img / max(max(img),1))^(1/gamma), max=1),
         interpolate=interpolate)
}

SaveRAW = function(img, filename, trunc=T, gamma=2.2) {
    # Guarda RAW en formato PNG (admite RAW y de-bayer)
    # Solo si trunc=F y la imagen excede de 1 se reescala a 1
    library(tiff)
    img[img<0]=0
    if (trunc) img[img>1]=1
    if (tolower(substr(filename, nchar(filename)-3,
                       nchar(filename))) != ".tif") filename=paste0(filename, ".tif")
    writeTIFF((img / max(max(img),1))^(1/gamma), filename,
              bits.per.sample=16, compression="LZW")
}

iif = function(condicion, val1, val2) {
    if (condicion) return(val1)
    return(val2)
}


# ANÁLISIS DEL RAW
library(tiff)
Gamma=2.2

# Lee RAW
# dcraw -v -d -r 1 1 1 1 -S 16382 -t 0 -4 -T blackcat.dng
raw=LoadRAW("blackcat.dng")

# De-bayering
img=DebayerRAW(raw, averageG=F)
img=img[,,c(1,2,4)]  # Descartamos G2 apilando R, G1, B
SaveRAW(img, filename="debayer.tif", gamma=Gamma)

# Reescalados
for (N in 4:12) {
    DIMY=as.integer(dim(img)[1]/N)
    DIMX=as.integer(dim(img)[2]/N)
    print(paste0("N=",N," -> ", DIMX, "x", DIMY,
        " pixels (", dim(img)[1]-DIMY*N,
        " rows and ", dim(img)[2]-DIMX*N, " cols dropped)"))
    imgresize=array(0, c(DIMY,DIMX,3))
    
    for (i in 1:DIMY) {
        for (j in 1:DIMX) {
            for (k in 1:3) {
                imgresize[i,j,k]=mean(img[((i-1)*N+1):(i*N),
                                          ((j-1)*N+1):(j*N),
                                          k])
            }
        }
    }
    
    writeTIFF(imgresize^(1/Gamma), paste0("imgresize_",N,".tif"),
              bits.per.sample=16, compression="LZW")
}
ShowRAW(imgresize)

