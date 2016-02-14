library(shiny)
library(gEcon)

sw_gecon_orig <- make_model('SW_03.gcn')

shinyServer(function(input, output) {

   
   
   sw_gecon <- reactive({
      # set our starting values for various parameters
      isolate({
         initv <- list(z = input$z, z_f = input$z_f, Q = input$Q, Q_f = input$Q_f, 
                    pi = input$pi, pi_obj = input$pi_obj,
                    epsilon_b = input$epsilon_b, epsilon_L = input$epsilon_L, 
                    epsilon_I = input$epsilon_I, epsilon_a = input$epsilon_a, 
                    epsilon_G = input$epsilon_G,
                    r_k = input$r_k, r_k_f = input$r_k_f)
      
      tmp <- initval_var(sw_gecon_orig, init_var = initv)
   
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
      compute_irf(sw_gecon_shocked(), var_list = c('C', 'Y', 'K', 'I', 'L'), chol = T,
                               shock_list = list(input$shock_var), path_length = 40)
   })
      
   output$irf_plot <- renderPlot(
      plot_simulation(sw_gecon_irf(), to_tex = FALSE)
   )
   
   
})
