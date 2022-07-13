n <- 100
X <- cbind(rnorm(n),rnorm(n))
X.NA <- X
X.NA[,1] <- ifelse(stats::runif(n)<=0.2, NA, X[,1])
imputations <- list()
imputations[[1]] <- lapply(1, function(i) {
  X.loc <- X.NA
  X.loc[is.na(X.NA[,1]),1] <- mean(X.NA[,1],na.rm=TRUE)
  return(X.loc)
})
imputations[[2]] <- lapply(1, function(i) {
  X.loc <- X.NA
  X.loc[is.na(X.NA[,1]),1] <- sample(X.NA[!is.na(X.NA[,1]),1],
                                     size = sum(is.na(X.NA[,1])), replace = TRUE)
  return(X.loc)
})
methods <- c("mean","sample")
Iscores(imputations,
        methods,
        X.NA)
library(Iscores)



imputations[1]
doevaluation(imputations)
precioshoja = read_excel("precios_drogas_2.xlsx",sheet = 1)%>%clean_names()
tshoja = precioshoja[1]

a = data.frame(imputacion1[[1]][["central"]])
hoja = list(imputacion2[[1]][["central"]],imputacion3[[1]][["central"]])


Iscores(hoja,
        methods = c("seas","kalman"),
        tshoja)
