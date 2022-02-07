
server <- function(input, output){
  
  dataIn1 <- reactive({
    arm0 <- input$file1
    if (is.null(arm0)) {
      return(NULL)
    }
    
    arm0 <- readxl::read_excel(arm0$datapath)
  })
  
  dataIn2 <- reactive({
    arm1 <- input$file2
    
    if (is.null(arm1)) {
      return(NULL)
    }
    
    
    arm1 <- readxl::read_excel(arm1$datapath)
  })
  
  
  
  data1 <- reactive({
    
    res1 <- dataIn1()
    preprocess_arm0 = preprocess(res1,
                                 trisk = seq(0,
                                             as.numeric(input$maxx),
                                             as.numeric(input$intervalx)),
                                 nrisk = as.numeric(unlist(strsplit(input$nrisk_arm0,","))),
                                 maxy = as.numeric(input$maxy))
    
    getIPD_arm0 <-  getIPD(preprocess_arm0,
                           armID = 1)[["IPD"]]
    
    
    
  })
  
  data2 <- reactive({
    
    res2 <- dataIn2()
    preprocess_arm1 = preprocess(res2,
                                 trisk = seq(0,
                                             as.numeric(input$maxx),
                                             as.numeric(input$intervalx)),
                                 nrisk = as.numeric(unlist(strsplit(input$nrisk_arm1,","))),
                                 maxy = as.numeric(input$maxy))
    
    getIPD_arm1 <-  getIPD(preprocess_arm1,
                           armID = 2)[["IPD"]]
    
    
  })
  
  ipd <- reactive({
    
    a <- data.frame(data1(), arm=0, group = input$label_arm0)
    b <- data.frame(data2(), arm=1,group = input$label_arm1)
    
    ipd1 <- rbind(a,b)
    ipd1
    
  })
  
  
  
  
  output$ipd <- renderTable({
    head(ipd())
  })
  
  output$km_table <- renderTable({
    
    
    fit_km <- survfit(Surv(time, status) ~ arm, data = ipd())
    fit_model <- unclass(summary(fit_km))
    km_table <- round(fit_model$table,2)
    km_table <- data.frame("Group" = c(input$label_arm0,input$label_arm1),km_table )
    km_table
    
  })
  
  output$km_plot <- renderPlot({
    
    dane <- as.data.frame(ipd())
    fit_km <- survfit(Surv(time, status) ~ group, data = ipd())
    # plot(fit_km)
    km_plot <- survminer::ggsurvplot(fit_km,
                                     data=ipd(), # GGSURVPLOT W SHINY POTRZEBUJE POWTORZENIA ARGUMENTU Z DANYMI
                                     pval = TRUE,
                                     pval.method = T,
                                     conf.int = TRUE,
                                     conf.int.style = "step",
                                     ggtheme = theme_pubclean(),
                                     surv.median.line = "hv",
                                     palette = c("#E7B800", "#2E9FDF"))
    
    plot(km_plot$plot)
  })
  
  
  
  output$rm_table <- renderTable({
    
    rm_table <- rmst2(time = ipd()$time, status = ipd()$status, ipd()$arm) #utwórz model rmean
    rm_table <-  data.frame(rm_table$RMST.arm0$result)
    rm_table
    
    
  })
  
  
  
  output$rmean_arm0 <- renderTable({
    
    rmean_arm0 <- rmst2(time = ipd()$time, status = ipd()$status, ipd()$arm) #utwórz model rmean
    rmean_arm0 <-  data.frame(rmean_arm0$RMST.arm0$result)
    rmean_arm0
    
  })
  
  output$rmean_arm1 <- renderTable({
    
    rmean_arm1 <- rmst2(time = ipd()$time, status = ipd()$status, ipd()$arm) #utwórz model rmean
    rmean_arm1 <-  data.frame(rmean_arm1$RMST.arm1$result)
    rmean_arm1
    
    
  })
  
  output$rmean_diff <- renderTable({
    
    rmean_diff <- rmst2(time = ipd()$time, status = ipd()$status, ipd()$arm) #utwórz model rmean
    rmean_diff <-  data.frame(rmean_diff$unadjusted.result)
    rmean_diff
    
    
  })
  
  output$rm_plot <- renderPlot({
    
    rmst2(time = ipd()$time, status = ipd()$status, ipd()$arm) %>%
      plot(
        xlab = "Czas",
        ylab = "Częstość przeżycia",
        col = "black",
        col.RMST = "#2E9FDF",
        col.RMTL = "#E7B800",
        density = 80,
        angle = 85)
    
    
  })
  
  
  dataIn3 <- reactive({
    rwe <- input$file3
    if (is.null(rwe)) {
      return(NULL)
    }
    
    rwe <- readxl::read_excel(rwe$datapath)
  })
  
  output$rwe <- renderTable({
    
    head(dataIn3(),2)
  })
  
  
  data3 <- reactive({
    
    res1 <- dataIn1()
    preprocess_arm0 = preprocess(res1,
                                 trisk = seq(0,
                                             as.numeric(input$maxx),
                                             as.numeric(input$intervalx)),
                                 nrisk = as.numeric(unlist(strsplit(input$nrisk_arm0,","))),
                                 maxy = as.numeric(input$maxy))
    
    getIPD_arm0 <-  getIPD(preprocess_arm0,
                           armID = 1)[["IPD"]]
    
    
    
  })
  
  
  ipd_rwe <- reactive({
    
    ipd1 <- dataIn3()
    ipd1
    
  })
  
  
  output$ipd <- renderTable({
    head(data3())
  })
  
  output$rwe_km_table <- renderTable({
    
    
    fit_km <- survfit(Surv(time, status) ~ arm, data = ipd_rwe())
    fit_model <- unclass(summary(fit_km))
    km_table <- round(fit_model$table,2)
    km_table <- t(data.frame(km_table))
    
  })
  
  output$rwe_km_plot <- renderPlot({
    
    dane <- as.data.frame(ipd_rwe())
    fit_km <- survfit(Surv(time, status) ~ group, data = ipd_rwe())
    # plot(fit_km)
    km_plot <- survminer::ggsurvplot(fit_km,
                                     data=ipd_rwe(), # GGSURVPLOT W SHINY POTRZEBUJE POWTORZENIA ARGUMENTU Z DANYMI
                                     pval = TRUE,
                                     pval.method = T,
                                     conf.int = TRUE,
                                     conf.int.style = "step",
                                     ggtheme = theme_pubclean(),
                                     surv.median.line = "hv")
    
    plot(km_plot$plot)
  })
  
  
  output$rwe_rm_plot <- renderPlot({
    
    rmst2(time = ipd_rwe()$time, status = ipd_rwe()$status, ipd_rwe()$arm) %>%
      plot(
        xlab = "Czas",
        ylab = "Częstość przeżycia",
        col = "black",
        col.RMST = "#2E9FDF",
        col.RMTL = "#E7B800",
        density = 80,
        angle = 85)
    
    
  })
  
  
}