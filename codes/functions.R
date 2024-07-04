
# for processing ----------------------------------------------------------
read_cbop <- function(file, qdata) {
  cbop_file <- readLines(file)
  cbop_file <- fparse(json = cbop_file[1])
  
  prac_list <- list()
  for (i in 1:length(cbop_file)) {
    prac <- cbop_file[[i]]$danePracodawcy
    names(prac) <- cbop_file[[i]]$hash
    prac_df <-  rbindlist(prac, idcol = "hash")
    setnames(prac_df, names(prac_df)[-1], paste0("prac_", names(prac_df)[-1]))
    
    pozostaleDane <- rbindlist(cbop_file[[i]]$pozostaleDane)
    setnames(pozostaleDane, names(pozostaleDane), paste0("poz_", names(pozostaleDane)))
    
    warunkiPracyIPlacy <- lapply(cbop_file[[i]]$warunkiPracyIPlacy, as.data.table)
    warunkiPracyIPlacy <- lapply(warunkiPracyIPlacy, \(x) x[1,])
    warunkiPracyIPlacy <- rbindlist(warunkiPracyIPlacy, fill = T)
    setnames(warunkiPracyIPlacy, names(warunkiPracyIPlacy), paste0("war_", names(warunkiPracyIPlacy)))
    
    prac_df <- cbind(prac_df, pozostaleDane, warunkiPracyIPlacy)
    
    prac_df[, ":="(typOferty = cbop_file[[i]]$typOferty,
                   typOfertyNaglowek = cbop_file[[i]]$typOfertyNaglowek,
                   zagranicznaEures = cbop_file[[i]]$zagranicznaEures,
                   kodJezyka =  cbop_file[[i]]$kodJezyka,
                   czyWazna = cbop_file[[i]]$czyWazna,
                   statusOferty = cbop_file[[i]]$statusOferty,
                   zaintUA = if (is.null(cbop_file[[i]]$pracodZainteresZatrUA)) NA else cbop_file[[i]]$pracodZainteresZatrUA,
                   tlumUA = if (is.null(cbop_file[[i]]$zgodNaTlumaczenieUA)) NA else cbop_file[[i]]$zgodNaTlumaczenieUA)]
    prac_list[[i]] <- prac_df
  }
  
  prac_list_df <- rbindlist(prac_list, fill = T)
  prac_list_df[, .SD, .SDcols = names(prac_list_df) %like% "data|oferta"]
  prac_list_df[, ":="(poz_dataPrzyjZglosz=dmy(poz_dataPrzyjZglosz),
                      poz_ofertaWaznaDo=dmy(poz_ofertaWaznaDo))]
  
  final_df <- prac_list_df[, ":="(prac_nip = str_remove_all(prac_nip, "-"),
                                  poz_dni = qdata-poz_dataPrzyjZglosz,
                                  kod_pocztowy = str_extract(war_miejscePracy, "\\d{2}\\-\\d{3}"))]
  
  final_df[prac_pracodawca == "kontakt przez PUP", prac_pracodawca:=NA]
  final_df[prac_pracodawca == "kontakt przez OHP", prac_pracodawca:=NA]
  
  final_df[, ":="(war_gmina = tolower(war_gmina), war_ulica=tolower(war_ulica), war_miejscowosc=tolower(war_miejscowosc))]
  final_df[, ":="(war_gmina = str_remove(war_gmina, "m.st. "))]
  final_df[, prac_nip := str_remove_all(prac_nip, "-")]
  final_df[, war_ulica:=str_replace(war_ulica,  "pl\\.", "plac ")]
  final_df[, war_ulica:=str_replace(war_ulica,  "al\\.", "aleja ")]
  final_df[, war_ulica:=str_replace(war_ulica,  "  ", " ")]
  final_df[, war_ulica:=str_remove(war_ulica,  "^\\.|-$")]
  final_df[, kod_pocztowy:=str_remove(kod_pocztowy, "00-000")]
  final_df[kod_pocztowy == "", kod_pocztowy := NA]
  final_df[, poz_kodZawodu := str_remove(poz_kodZawodu, "RPd057\\|")]
  final_df[, qdata:=qdata]
  
  return(final_df[!is.na(prac_regon) | !is.na(prac_nip)])
}

# for estimation ---------------------------------------------------------

log_reg_misclass <- function(par, y, x, probs, m) {
  
  x_par <- par[1:ncol(x)]
  m_par <- par[(ncol(x) + 1):length(par)]
  
  x_beta <- x %*% x_par
  x_beta <- matrix(x_beta, nrow = nrow(x), ncol = NROW(m_par)+1, byrow = F)
  m_beta <- t(m %*% m_par)
  
  eta <- x_beta + matrix(m_beta, nrow = nrow(x), ncol = ncol(m_beta), byrow = T)
  ll <- rowSums(dbinom(y, 1, plogis(eta))*probs)
  return(-sum(log(ll)))
}



## check regons

regon_check <- function(x, last, digits = 9) {
  
if (digits == 9) {
  regon_weights <- c(8,9,2,3,4,5,6,7) ## mod 11
  splitted <- as.numeric(str_split(x, "", simplify = T)[1:8])
  mod <- sum(splitted*regon_weights) %% 11
  if (mod == 10) mod <- 0
  test <- mod %% 11 == last
} else {
  regon_weights <- c(2,4,8,5,0,9,7,3,6,1,2,4,8) ## mod 11
  splitted <- as.numeric(str_split(x, "", simplify = T)[1:13])
  mod <- sum(splitted*regon_weights) %% 11
  if (mod == 10) mod <- 0
  test <- mod %% 11 == last
}
  return(test)
}

regon_check_vec <- Vectorize(regon_check, vectorize.args = c("x", "last"))

nip_check <- function(x, last) {
    nip_weights <- c(6, 5, 7, 2, 3, 4, 5, 6, 7) ## mod 11
    splitted <- as.numeric(str_split(x, "", simplify = T)[1:9])
    mod <- sum(splitted*nip_weights) %% 11
    test <- mod %% 11 == last
  return(test)
}

nip_check_vec <- Vectorize(nip_check, vectorize.args = c("x", "last"))

