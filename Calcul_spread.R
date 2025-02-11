library(readxl)

# Chargement des données
Obligation <- read_excel("C:/Users/MSI/OneDrive - GENES/Bureau/Stat app/BD/Obligation.xlsx")
COURBE_EIOPA <- read_excel("C:/Users/MSI/OneDrive - GENES/Bureau/Stat app/BD/COURBE_EIOPA.xlsx")

# Fonction d'interpolation linéaire
linear_interpolation <- function(min, max, int, rmin, rmax) {
  rint <- rmin + (int - min) * (rmax - rmin) / (max - min)
  return(rint)
}


# Fonction d'interpolation cubique
cubic_interpolation <- function(x, y, new_x) {
  spline_result <- spline(x, y, xout = new_x, method = "natural")
  return(spline_result$y)
}

#Algo de Newton-Raphson
newton_raphson_s0 <- function(F, r, t, VM0, s0, tol, max_iter) {
  for (i in 1:max_iter) {
    # Calcul de la fonction à minimiser
    survival_prob <- 1 / (1 + s0)^t
    discount_factor <- 1 / (1 + r)^t
    f_s0 <- sum(F * discount_factor * survival_prob) - VM0
    
    # Calcul de la dérivée
    df_s0 <- sum((-F * discount_factor * t * survival_prob) / (1 + s0))
    
    # Vérification si df_s0 est nul ou NA
    if (is.na(df_s0) || df_s0 == 0) {
      warning("Dérivée nulle ou indéfinie, arrêt de Newton-Raphson.")
      return(NA)
    }
    
    # Mise à jour de s0
    s0_new <- s0 - (f_s0 / df_s0)
    
    # Vérification si s0_new est NA avant de continuer
    if (is.na(s0_new)) {
      warning("Newton-Raphson a produit un NA, arrêt.")
      return(NA)
    }
    
    # Vérification de la convergence
    if (abs(s0_new - s0) < tol) {
      return(s0_new)
    }
    
    s0 <- s0_new
  }
  
  warning("Newton-Raphson n'a pas convergé après", max_iter, "itérations.")
  return(NA)
}


# Initialisation de la colonne Spread
Obligation$Spread <- 1

# Calcul du spread pour chaque obligation
for (j in 1:nrow(Obligation)) {
  n <- Obligation$Maturité[j]
  r <- numeric(trunc(n) + 1)
  t <- c(1:trunc(n), n)
  
  for (i in 1:trunc(n)) {
    r[i] <- COURBE_EIOPA$Rate[i]
  }
  
  #r[trunc(n) + 1] <- linear_interpolation(trunc(n), trunc(n) + 1, n, COURBE_EIOPA$Rate[trunc(n)], COURBE_EIOPA$Rate[trunc(n) + 1])
  r[trunc(n)+1]<-cubic_interpolation(c(trunc(n),trunc(n)+1),c(COURBE_EIOPA$Rate[trunc(n)],COURBE_EIOPA$Rate[trunc(n)+1]),n)
  # Ajout du nominal au dernier paiement
  F <- rep(Obligation$`Taux nominal`[j] * Obligation$Nominal[j], length(t) - 1)
  F <- c(F, F[1] + Obligation$Nominal[j])  # Dernier paiement = coupon + nominal
  
  VM0 <- Obligation$VM[j]
  
  # Calcul du spread
  Obligation$Spread[j] <- newton_raphson_s0(F, r, t, VM0, 1e-2, 0.000001, 1000000)
}



