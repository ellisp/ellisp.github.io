# ###################################################################
# (c) Chancellery of the Prime Minister 2012-2015                   #
# Licence terms for gEcon can be found in the file:                 #
# http://gecon.r-forge.r-project.org/files/gEcon_licence.txt        #
#                                                                   #
# gEcon authors: Grzegorz Klima, Karol Podemski,                    #
#          Kaja Retkiewicz-Wijtiwiak, Anna Sowińska                 #
# ###################################################################


library(gEcon)
library(dplyr)
library(tidyr)
library(showtext)

font.add.google("Poppins", "myfont")
showtext.auto()

# download model definition and import into R:
download.file("http://gecon.r-forge.r-project.org/models/SW_03/SW_03.gcn",
              destfile = "SW_03.gcn")
sw_gecon1 <- make_model('SW_03.gcn')

# set some initial variable values:
initv <- list(z = 1, z_f = 1, Q = 1, Q_f = 1, pi = 1, pi_obj = 1,
              epsilon_b = 1, epsilon_L = 1, epsilon_I = 1, epsilon_a = 1, epsilon_G = 1,
              r_k = 0.01, r_k_f = 0.01)

sw_gecon1 <- initval_var(sw_gecon1, init_var = initv)

# set some initial parameter values:
initf <- list(
   beta = 0.99,            # Discount factor
   tau = 0.025,            # Capital depreciation rate
   varphi = 6.771,         # Parameter of investment adjustment cost function
   psi = 0.169,            # Capacity utilisation cost parameter
   sigma_c = 1.353,        # Coefficient of relative risk aversion
   h = 0.573,              # Habit formation intensity
   sigma_l = 2.4,          # Reciprocal of labour elasticity w.r.t. wage
   omega = 1               # Labour disutility parameter
)
sw_gecon1 <- set_free_par(sw_gecon1, initf)
 
# find the steady state for that set of starting values:
sw_gecon2 <- steady_state(sw_gecon1)
get_ss_values(sw_gecon2)

# solve the model in linearised form for 1st order perturbations/randomness:
sw_gecon2 <- solve_pert(sw_gecon2, loglin = TRUE)

summary(sw_gecon2)

one_path <- random_path(sw_gecon2, var_list = list("Y", "K", "I", "C", "W"))

svg("../img/0031-onepath.svg", 7.5, 4.5)
par(family = "myfont")
plot_simulation(one_path) # shows deviation from the steady_state:
dev.off()



#==============shocks=================

# set covariance matrix of the parameters to be used in shock simulation:
a <- c(eta_b = 0.336 ^ 2, eta_L = 3.52 ^ 2, eta_I = 0.085 ^ 2, eta_a = 0.598 ^ 2,
       eta_w = 0.6853261 ^ 2, eta_p = 0.7896512 ^ 2,
       eta_G = 0.325 ^ 2, eta_R = 0.081 ^ 2, eta_pi = 0.017 ^ 2)
sw_gecon3  <- set_shock_cov_mat(sw_gecon2, shock_matrix = diag(a), shock_order = names(a))

# compute the moments with that covariance matrix:
sw_gecon3 <- compute_moments(sw_gecon3)

sw_gecon_irf <- compute_irf(sw_gecon3, var_list = c('C', 'Y', 'K', 'I', 'L'), chol = T,
                            shock_list = list('eta_pi'), path_length = 40)
svg("../img/0031-irf.svg", 7.5, 4.5)
par(family = "myfont")
plot_simulation(sw_gecon_irf, to_tex = FALSE)
dev.off()

#---------------shinyapp prep----------------

shocks <- data.frame(param = names(a), 
                     value = sqrt(a), 
                     longer_name = c("Preference", "Labour supply", "Investment",
                                     "Productivity", "Wage markup", "Price markup",
                                     "Government spending", "Interest rates",
                                     "Inflation objective"),
                     stringsAsFactors = FALSE)
save(shocks, file = "_output/0031-shiny/shocks.rda")

library(shinyapps)
deployApp("_output/0031-shiny", account = "ellisp")
