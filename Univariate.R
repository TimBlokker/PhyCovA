# Univariate ####
observe({
##scatter plot output####
output$plot = renderPlotly({
  req(transition_distances, vals, logs)
  plotting_fun()
}) # output$plot = renderPlot({

##bar plot output####
output$bar = renderPlotly({
  req(transition_distances, vals, logs)
  histo_fun()
}) # output$plot = renderPlot({

##residual plot output####
output$plot_res = renderPlotly({
  req(transition_distances, vals, logs)
  linear_regression()$x%>%
    plotting_residuals(.)
}) # output$plot = renderPlot({

##glance output ####
output$lm=renderTable({
  req(transition_distances, vals, logs)
  glance(linear_regression()$lm)
}) # output$plot = renderTable({

## lm summary output ####
output$lm.summary=renderPrint({
  req(transition_distances, vals, logs)
  summary(linear_regression()$lm)
}) # output$plot = renderTable({
})
observeEvent(input$log_transitions, {
  req(transition_distances, vals, logs)
  logs$logtransform[1]=!logs$logtransform[1]
})

observeEvent(input$log_distances, {
  req(transition_distances, vals, logs)
  logs$logtransform[2]=!logs$logtransform[2]
})

# Toggle points that are brushed, when button is clicked only 
observeEvent(input$exclude_toggle, {
  req(transition_distances, vals, logs)
  selected_data<-event_data("plotly_selected", source = "plot")
  res<- data.frame(selected_=rep(FALSE, nrow(transition_distances)))
  res$selected_[which(transition_distances$Key %in% selected_data$key)]<-TRUE
  vals$keeprows <- xor(vals$keeprows, res$selected_)
})

# Toggle points that have 0 transitions when button is clicked
observeEvent(input$includeZerosUni,{
  req(transition_distances, vals, logs)
  if(input$includeZerosUni=="FALSE"){
    vals$keepZerosUni[which(transition_distances$Transitions==0)]<- FALSE
  }else if(input$includeZerosUni=="TRUE"){
    vals$keepZerosUni <- rep(TRUE, nrow(transition_distances))
  }
  vals<<-vals
})

# Reset all points
observeEvent(input$exclude_reset, {
  req(transition_distances, vals, logs)
  vals$keeprows <- rep(TRUE, nrow(transition_distances))
})

plotting_fun<-reactive({
  req(vals)
  selected_col<-grep(input$Predictor_uni, colnames(transition_distances))
  selected_col_response<-grep(input$response_uni, colnames(transition_distances))
  
  #data modification part
  keep    <- transition_distances[intersect(which(vals$keepZerosUni), which(vals$keeprows)) , , drop = FALSE]
  if(input$standardize_uni){
    keep <- dplyr::mutate(keep, across(input$Predictor_uni, scale)) #scale again to account for possibly removed 0 transitions
  }
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE] 
  if(dim(exclude)[1]>0 && input$standardize_uni){#scale the excluded point the same way as the included points, still the scaling is different then when including
    #the points of course. but depending on how many points are excluded, not scaling the excluded points might push them of the plot but not sure
    #what is the best to do here. anyway the excluded points are not regarded for regression and scaling of the included points.
    exclude[input$Predictor_uni]<-get(input$Predictor_uni, exclude)-attributes(get(input$Predictor_uni, keep))$`scaled:center`
    exclude[input$Predictor_uni]<-get(input$Predictor_uni, exclude)/attributes(get(input$Predictor_uni, keep))$`scaled:scale`
  }
  keep<-log_uni_data(keep)
  exclude<-log_uni_data(exclude)
  
  #plotting part
  ggplot2::theme_set(theme_classic())
  p <- ggplot(keep, mapping= aes_string(x=colnames(keep)[selected_col],y=colnames(keep)[selected_col_response], key=colnames(keep)[dim(keep)[2]]))
  p<-log_uni(p, keep, exclude) #to have the axis properly scaled only pass in the values that are actually shown, but keep in
  #brushed points that can be individually excluded to show them in different colour to the user still
  p<-p + geom_point(data = exclude, shape = 21, fill = NA, color = "red", alpha = input$alpha, stroke = input$stroke, size=input$size)
  p<-colour_by_states_uni(p, keep)

   if(!input$regression_line==FALSE){ 
    p <- p + geom_smooth(mapping=aes(key=NULL), method = "lm", se=as.logical(input$se), level=input$level) 
   }
  p <- p %>% plotly::ggplotly(tooltip =c("x", "y", "key"), source="plot")
  
  return(p)
})


log_uni_data<-function(transition_distances){
  #since for the predictor the column number is not fixed and the column name is not fixed we need to select via pattern search
  selected_col<-grep(input$Predictor_uni, colnames(transition_distances))
  selected_col_response<-grep(input$response_uni, colnames(transition_distances))
  if(logs$logtransform[1]==TRUE)   {
      if(dim(transition_distances)[1]>0 && min(get(input$response_uni, transition_distances))<=0){
        shiny::showNotification(
          ui=paste0("There are values smaller or equal to 0 in ", input$response_uni, " the log transformation is not possible.
                  The transform is rolled back and displayed as before." ),
          type = 'warning',
          duration=30)
        logs$logtransform[1]=FALSE
        return(transition_distances)
      }
    transition_distances <- transition_distances %>%
      dplyr::mutate(across(input$response_uni, log))
      colnames(transition_distances)[selected_col_response]<- paste(input$response_uni, "log", sep ="_")
  }
  ##Give a warning if the predictive variable is log transformed but contains values equal or below 0.
  
  if(logs$logtransform[2]==TRUE)   {
    if(dim(transition_distances)[1]>0 && min(get(input$Predictor_uni, transition_distances))<=0){
      shiny::showNotification(
        ui=paste0("There are values smaller or equal to 0 in ", input$Predictor_uni, " the log transformation is not possible.
                The transform is rolled back and displayed as before." ),
        type = 'warning',
        duration=30)
      logs$logtransform[2]=FALSE
      return(transition_distances)
    }
    
    transition_distances <- transition_distances%>%
      dplyr::mutate(across(input$Predictor_uni, log))
      colnames(transition_distances)[selected_col]<- paste(input$Predictor_uni, "log", sep ="_")
  }
  transition_distances
}


histo_fun<-function(){
  keep_exclude<-summarize_to_from()
  keep<-keep_exclude$keep
  exclude<-keep_exclude$exclude
  p<-ggplot(data=keep, aes(x=get(input$to_from, keep), y=sum ))+ geom_bar(stat="identity")
  p[["labels"]][["x"]]<-paste0(input$response_uni, input$to_from)
  plotly::ggplotly(p)%>%layout(xaxis=list(tickangle=45))
}

summarize_to_from<-function(){
  keep    <- transition_distances[vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]
    keep<-keep%>% 
    separate(Key, c("From", "To"), "->")%>% 
    group_by(across(input$to_from))%>%
    summarise("sum"=sum(!!sym(input$response_uni)))
  exclude<-exclude%>% 
    separate(Key, c("From", "To"), "->")%>% 
    group_by(across(input$to_from))%>%
    summarise("sum"=sum(!!sym(input$response_uni)))
  
  list("keep"=keep, "exclude"=exclude)
}

log_uni<-function(p, keep, exclude){
  transition_distances<-rbind(keep, exclude)
  numeric_transitions_distances<-transition_distances[,colnames(transition_distances)!="Key"]
  
  selected_col<-grep(input$Predictor_uni, colnames(numeric_transitions_distances))
  selected_col_response<-grep(input$response_uni, colnames(numeric_transitions_distances))
  p<-p+scale_x_continuous(name =colnames(numeric_transitions_distances)[selected_col])+
        scale_y_continuous(name = colnames(numeric_transitions_distances)[selected_col_response])+
        coord_cartesian(xlim =c(min(numeric_transitions_distances[,selected_col]), max(numeric_transitions_distances[,selected_col])), ylim=c(min(numeric_transitions_distances[,selected_col_response]),max(numeric_transitions_distances[,selected_col_response])))
  return(p)
}

colour_by_states_uni<-function(p, keep){
  if(input$colour_by_states_uni=="To"){
    toStates<-unlist(lapply(keep$Key, function(key) first.word(my.string = key,sep =  "->", n= 2)))
    nbColoursUni_to<-length(unique(toStates))#the states are added as a list in order to unlist them I need to take only the first word otherwise we get too many states
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))#these 9 colours will be interpolated to obtain  the most divergent result
    p <- p + geom_point(aes(fill= toStates),alpha =  input$alpha , data=keep, shape=21, colour="#4D4D4D", stroke = input$stroke, size=input$size)+
      scale_fill_manual(values=getPalette(nbColoursUni_to))
  }else if(input$colour_by_states_uni=="From"){
    fromStates<-unlist(lapply(keep$Key, function(key) first.word(my.string = key,sep =  "->", n= 1)))
    nbColoursUni_from<-length(unique(fromStates))#  
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))#these 9 colours will be interpolated to obtain  the most divergent result
    p <- p + geom_point(aes( fill= fromStates),alpha =  input$alpha , data=keep, shape=21, colour="#4D4D4D",  stroke = input$stroke, size=input$size)+
      scale_fill_manual(values=getPalette(nbColoursUni_from))
  }else if (input$colour_by_states_uni=="All blue"){
    p<-p +geom_point(data=keep, shape=21, colour="#4D4D4D" ,fill=alpha("#0e74af", input$alpha),  stroke = input$stroke, size=input$size) 
  }
  return(p)
}



plotting_residuals<-function(x){
  #keep    <- transition_distances[ vals$keeprows, , drop = FALSE]#not needed because only called after linear regression
  #exclude <- transition_distances[!vals$keeprows, , drop = FALSE]# see above
  theme_set(theme_classic())
  p_res<-ggplot(x, aes(fitted, residuals))+
    geom_point(shape=21, colour="#4D4D4D", fill=alpha("#0e74af", input$alpha), stroke = input$stroke, size=input$size) + 
    coord_cartesian(xlim = c(min(x$fitted), max(x$fitted)), ylim = c(min(x$residuals),max(x$residuals)))
  p_res1 <- p_res %>% plotly::ggplotly(tooltip =c("fitted", "residuals"), source="plot_res")
  return(p_res)
}

linear_regression<-function(cut_off_residual=NULL, percentile=95){
  keep    <- transition_distances[intersect(which(vals$keepZerosUni), which(vals$keeprows)) , , drop = FALSE]
  if(input$standardize_uni){
    keep <- dplyr::mutate(keep, across(input$Predictor_uni, scale))
  }
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]
  variable=input$Predictor_uni
  if(logs$logtransform[2]==TRUE)   {
    variable=paste0("log(", input$Predictor_uni, ")")
  }
  
  if(logs$logtransform[1]==FALSE)   {
    f <- paste(input$response_uni,variable, sep="~")
  }
  if(logs$logtransform[1]==TRUE)   {
    f <- paste(paste0("log(", input$response_uni, ")"), variable, sep = "~")
  }
  
  lm=lm(as.formula(f), data=keep)
  lm[["call"]][["formula"]]<-lm$terms #this seemed to be the easiest way to have the evaluated variables printed to the summary output under "call" 
  x<-data.frame(lm$residuals,lm$fitted.values)
  colnames(x)<-c("residuals", "fitted")
  list(lm=lm, x=x)
}   