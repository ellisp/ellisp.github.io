library(shiny)
library(gEcon)
load("shocks.rda")
sw_gecon_orig <- make_model('SW_03.gcn')

shinyServer(function(input, output) {

   
   
   sw_gecon <- reactive({
      # set our starting values for various parameters
      isolate({
         initv <- list(z = 1, z_f = 1, Q = 1, Q_f = 1, pi = 1, pi_obj = 1,
                       epsilon_b = 1, epsilon_L = 1, epsilon_I = 1, epsilon_a = 1, epsilon_G = 1,
                       r_k = 0.01, r_k_f = 0.01)
      
         tmp <- initval_var(sw_gecon_orig, init_var = initv)
         
         initf <- list(
            beta = input$beta,            # Discount factor
            tau = input$tau,            # Capital depreciation rate
            varphi = input$varphi,         # Parameter of investment adjustment cost function
            psi = input$psi,            # Capacity utilisation cost parameter
            sigma_c = input$sigma_c,        # Coefficient of relative risk aversion
            h = input$h,              # Habit formation intensity
            sigma_l = 1 / input$sigma_l_inv,         # Reciprocal of labour elasticity w.r.t. wage
            omega = input$omega,               # Labour disutility parameter,
            alpha = input$alpha,
            gamma_w = input$gamma_w,
            lambda_w = input$lambda_w,
            xi_w = input$xi_w,
            gamma_p = input$gamma_p,
            xi_p = input$xi_p,
            r_pi = input$r_pi
         )
         tmp <- set_free_par(tmp, initf)
         
         
   
         # find the steady state for that set of starting values
         tmp <- steady_state(tmp)
      
         # solve the model in linearised form for 1st order perturbations/randomness
         tmp <- solve_pert(tmp, loglin = TRUE)
      })
      input$goButton
      return(tmp)
      
   })
   
   
   
   sw_gecon_shocked <- reactive({
      # set covariance matrix of the parameters to be used in shock simulation
      a <- c(eta_b = input$eta_b ^ 2, eta_L = input$eta_L ^ 2, eta_I = input$eta_I ^ 2, 
             eta_a = input$eta_a ^ 2,
             eta_w = input$eta_w ^ 2, eta_p = input$eta_p ^ 2,
             eta_G = input$eta_G ^ 2, eta_R = input$eta_R ^ 2, eta_pi = input$eta_pi ^ 2)
      tmp  <- set_shock_cov_mat(sw_gecon(), shock_matrix = diag(a), shock_order = names(a))
      
      # compute the moments with that covariance matrix
      tmp <- compute_moments(tmp)
      return(tmp)
   })
   
   sw_gecon_irf <- reactive({
      shock_row <- which(shocks$longer_name == input$shock_var)
      compute_irf(sw_gecon_shocked(), var_list = c('C', 'Y', 'K', 'I', 'L'), chol = T,
                               shock_list = list(shocks[shock_row, "param"]), path_length = 40)
   })
      
   output$irf_plot <- renderPlot(
      plot_simulation(sw_gecon_irf(), to_tex = FALSE)
   )
   
   
})
