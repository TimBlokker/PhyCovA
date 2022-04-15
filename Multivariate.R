
log_choices<-function(transition_distances){
  log_validated<-names(which(apply(transition_distances[,1:dim(transition_distances)[2]-1], 2, min)>0))
  return(log_validated)
}
logs_multi <- reactive({
  req(transition_distances, vals)#require to compute the transitions and the distance matrices first before executing this code
  transition_distances <- transition_distances[ vals$keepZerosMulti, , drop = FALSE]
  transition_distances<-mutate(transition_distances, across(all_of(input$standardize), scale))
  transition_distances
  log_validated = log_choices(transition_distances)
  return(c(log_validated[log_validated %in% c(input$variable,  "Transitions")]))
})



output$log <- renderUI({
  req(transition_distances)
  output <- checkboxGroupInput(
    inputId = "Log",
    label = "Log",
    choices = logs_multi(),
    selected = NULL
  )
})

output$standardize <- renderUI({
  req(transition_distances)
  output <- checkboxGroupInput(
    inputId = "standardize",
    label = "Standardize:",
    choices = column_names()[which(column_names() %in% input$variable)],
    selected = NULL
  )
})

output$response_multi_out <- renderUI({
  req(transition_distances)
  responses<-"Transitions"
  if(input$Reconstruction_Method=="TT"){
    responses<-c("Transitions", "Transition_Rates")
  }
  output<-selectizeInput(
    inputId= "response_multi",
    label="Response variable: ",
    choices = responses ,
    selected =  "Transitions"
  )
})

observeEvent(input$includeZerosMulti,{
  req(transition_distances, vals, logs)
  if(input$includeZerosMulti=="FALSE"){
    vals$keepZerosMulti[which(transition_distances$Transitions==0)]<- FALSE
  }else if(input$includeZerosMulti=="TRUE"){
    vals$keepZerosMulti <- rep(TRUE, nrow(transition_distances))
  }
  vals<<-vals
})

observeEvent(input$multi_input_control, {
  shinyjs::toggle(selector = "div.multi_input_control", animType = "fade", anim=T, condition = input$multi_input_control==TRUE)
})

observeEvent(input$multi_regression_output, {
  shinyjs::toggle(selector = "div.multi_regression_output", animType = "fade", anim=T, condition = input$multi_regression_output==TRUE)
})

observeEvent(input$multi_plot_output, {
  shinyjs::toggle(selector = "div.multi_plot_output", animType = "fade", anim=T, condition = input$multi_plot_output==TRUE)
})

  
  
data_transform<-reactive({
  req(input$height, input$width)
  transition_distances <- transition_distances[ vals$keepZerosMulti, , drop = FALSE]
  transition_distances <-mutate(transition_distances, across(all_of(input$standardize), function(predictor){
    as.numeric(scale(x=predictor, scale=TRUE, center=TRUE))
  }))  #scale does return a matrix with attributes which gather drops and gives a warning, that could be ignored, since I do not want a warning, I drop the attributes manually before 
  transition_distances<-transition_distances%>%
    select(input$response_multi, input$variable, Key)%>%
    mutate(across(.cols=input$Log, .fns= log))
  
  colnames(transition_distances)<-sapply(colnames(transition_distances), function(colname){
    if(colname %in% input$Log) {
      paste0(colname, "_log")
    }else{
      colname
    }
  })
  transition_distances
})


plotting_muĺti<-reactive({
  req(data_transform())
  transition_distances<-data_transform()
  selected_col_response<-grep(input$response_multi, colnames(transition_distances))
  #transform the data into high format and then group by distance matrix
  transition_distances_high<- tidyr::gather(data=transition_distances, key="Predictor", value="Distance", -c(colnames(transition_distances)[selected_col_response], Key))
  theme_set(theme_classic())

  p1 <-ggplot(transition_distances_high, aes_string(y = colnames(transition_distances_high)[1], x ="Distance", group = "Predictor", key="Key")) + #color = Predictors
    facet_wrap(. ~ Predictor, scale="free", ncol=3 )+
    geom_smooth(method = "lm")+
    geom_point(shape=21, colour="#4D4D4D", fill= "#0e74af80")
  p2<- p1 %>% plotly::ggplotly(tooltip = c("Predictor","Distance",  colnames(transition_distances_high)[1], "Key"), source="multi_plot",  width = cdata$output_multi_plot_width*0.95, height =  ceiling(length(unique(transition_distances_high$Predictor))/2)*300)
  p2
})

plotting_corr<-reactive({
  req(data_transform())
  transition_distances<-data_transform()
  data<-transition_distances[colnames(transition_distances )!="Key"]
  data<-data[,order(colnames(data))]
  cor_matrix<-corrplot::corrplot(cor(data), type="upper",
           col=brewer.pal(n=8, name="RdYlBu"),order="original", tl.cex = 1.5, addCoef.col = "black", diag = FALSE)
  cor_matrix
})

observeEvent(input$downloadCorr, {
    transition_distances<-data_transform()
    #png(filename = paste("output/correlation",  input$tree_file$name,"zeros_",input$includeZerosMulti, "_reconstruction_",  input$Reconstruction_Method,"_Annotation_",input$annotations), unit="cm", width=20, height=20, res=300)
    data<-transition_distances[colnames(transition_distances )!="Key"]
    data<-data[,order(colnames(data))]
    cor_matrix<-corrplot::corrplot(cor(data), type="upper", 
                                   col=brewer.pal(n=8, name="RdYlBu") )
    corrplot::corrplot(cor_matrix, type="upper",order="original", diag = FALSE, )
    dev.off()
  }
)

plotting_pairs<-reactive({
  req(transition_distances)
  transition_distances<-data_transform()
  plot(transition_distances[colnames(transition_distances )!="Key"])
})

plotting_ggpairs<-reactive({
  req(transition_distances)
  transition_distances<-data_transform()

  GGally::ggpairs(transition_distances[colnames(transition_distances )!="Key"])
})

lm_multi<-reactive({
  req(transition_distances, vals)
  transition_distances <- transition_distances[vals$keepZerosMulti, , drop = FALSE]
  transition_distances <-mutate(transition_distances, across(all_of(input$standardize), scale))
  
  variable=as.vector(sapply(input$variable, function(variable) {
    if (variable %in% input$Log){
      variable=paste0("log(", variable, ")")
    }
    variable
  }))
  
  
  if(input$response_multi %in% variable){
    variable<-variable[variable != input$response_multi]
    shiny::showNotification(
      ui=paste0("The response appeared on the right-hand side and was dropped as predictor. This is equivalent to R standard behaviour."),
      type = "warning",
      duration=10)
  }
  
  #check whether both transitions and transtions rate are in the model
  if(length(grep("Transition", c(input$response_multi,variable)))>1){
    shiny::showNotification(
      ui=paste0("Your regression model contains both Transition counts and Transition rates, this might be redundant information 
                but if you know what you are doing, continue to do so."),
      type = "message",
      duration=10)
  }
    
  if(input$response_multi %!in% input$Log){
    f<-paste0(input$response_multi,"~", paste(variable, collapse="+"))
  }
  if(input$response_multi %in% input$Log){
    f<-paste0("log(", input$response_multi, ") ~",paste(variable, collapse="+"))
  }
  
  lm_multi=lm(as.formula(f), data=transition_distances)
  lm_multi[["call"]][["formula"]]<-eval(lm_multi[["call"]][["formula"]]) #this seemed to be the easiest way to have the evaluated variables printed to the summary output under "call" 
  list("lm_multi"=lm_multi,"f"=f, "data"=transition_distances)
})

#### k ####
output$k<-renderUI({
  output<-selectInput(
    inputId="k_crit",
    label="Input criterion",
    choices=c("AIC (k=2)"=2, "BIC (k=log(n))"=log(sum(vals$keepZerosMulti)))
  )
})

observe({
  req(transition_distances)
  lm_multi()
})
# Multivariate ####
## Plot ####
observe({
  output$multi_plot = renderPlotly({
    req(transition_distances, logs_multi(), input$variable)
    plotting_muĺti()
  }) # output$plot = renderPlot({
  
  ## Glance #######
  output$lm_multi=renderTable({
    req(lm_multi())
    glance(lm_multi()$lm_multi)
  }) # output$plot = renderTable({
  
  output$lm.summary_multi=renderPrint({
    req(lm_multi())
    summary(lm_multi()$lm_multi)
  }) # output$plot = renderTable({
  
  output$stepAIC<-renderPrint({
    req(input$k_crit, lm_multi())
    data<-lm_multi()$data
    f<-lm_multi()$f
    model<-lm(as.formula(f), data=data)
    MASS::stepAIC(
      object = model,
      direction = "both",
      trace=3,
      k=as.numeric(input$k_crit))
  })
  
  lm_regsubsets<-reactive({
    req(lm_multi(), input$step_method)
    data<-lm_multi()$data
    lm_regsubsets <- leaps::regsubsets(
      x=as.formula(lm_multi()$f),
      data=lm_multi()$data,
      nvmax = length(colnames(lm_multi()$data)),
      method=input$step_method)
    lm_regsubsets[["call"]][[2]]<-as.formula(lm_multi()$f)
    #lm_regsubsets[["call"]][[3]]<-eval(data()) 
    lm_regsubsets
  })
  
  observeEvent(input$downloadRegsubsets, {
    #png(filename = paste("output/regsubsets_",  input$tree_file$name,"zeros_",input$includeZerosMulti, "no_coef"), unit="cm", width=20, height=20, res=300)
    plot(lm_regsubsets())
    dev.off()
  }
  )
  
  output$regsubsets<-renderPlot({
    req(input$crit.plot, lm_regsubsets())
    plot(lm_regsubsets(),scale=input$crit.plot)
  })
  
  
  output$typeMultiPlot <- renderText({ 
    switch(
      input$typeMultiPlot,
      "scatter" = paste0("<b>Scatter plot for all predictors and ", input$response_multi, " \b"),
      "corr_balls" = paste0("<b> Correlation between all variables \b"),
      "pairs"="<b>Base plot of pair-wise correlations \b",
      "ggpairs"="<b>GGally version of pair-wise correlations \b"
    )
  })
  
  output$corr_balls = renderPlot(
    expr={
   # req(transition_distances, logs_multi(), input$variable)
    plotting_corr()
    }, 
    width = input$width, 
    height = input$height
    ) # output$plot = renderPlot({
  
  
  output$pairs = renderPlot({
   # req(transition_distances, logs_multi(),data_transform())
    plotting_pairs()
  }, 
  width = input$width, 
  height = input$height
  ) # output$plot = renderPlot({
  
  
  
  output$ggpairs = renderPlot(
    expr={
    #req(transition_distances, logs_multi(), data_transform())
    plotting_ggpairs()
    }, 
    width = input$width, 
    height = input$height
    )
  
  
})