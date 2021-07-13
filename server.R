options(shiny.maxRequestSize=10*1024^2) #10mb max file size

#' Shiny server is doing the back-end work for the treedist application. It consists of 5 files in total. This server.R file itsels, 3 files that are sourced upon startup of the app
#' (Functions.R, Multivariate.R, AncestralReconstruction.R) and the Tree.R file which is encapsulated within the "RUN" observer. 
#' @param input
#' @param output
#' @param session
shinyServer(function(input, output, session) {
  source("Functions.R", local=T)
  
  reactlog_enable() #logging a reactivity tree
  cdata <- session$clientData #https://community.rstudio.com/t/controlling-the-height-of-fluidrow-in-shiny/4968/3
  #the cdata is used to set the size of the facet wrap plot for multivariate Plotly
  
  #Session variables
  transition_distances<-NULL 
  vals<-NULL
  tree<-NULL
  #this variable assignment here, is then found by R when the actual value is assigned using the
  #"<<-" operator which goes up through the environments to assign this value, we need to declare it here
  #to keep the variable at session level. otherwise the variable will be global and available across sessions, for all users using
  #the app at the same time. 
  
  #reactive observe combination to make the recognition of state eager
  state<-reactive({
    req(input$annotations)
    if(input$annotations){
      state= input$Annotation_State
    }else{
      state="states"
    }
  })
  
  #the names of the distance matrices
  column_names<-reactive({
    req(input$distances_file$name)
    column_names<-unlist(lapply(input$distances_file$name, first.word))
    if(input$annotations==FALSE & input$Reconstruction_Method!="MP"){
      column_names<-c(column_names, "Transition_Rates")
    }
    if(!is.null(pop_sizes$data)){
      column_names<-c(column_names, "Ori_Pop_Size", "Dest_Pop_Size")
    }
    column_names
  })
  
  distances_raw<-reactiveValues(data=NULL)
  observe({
    req(input$distances_file)
    if(input$annotations==FALSE) {
      req(input$sampling_locations)
    }
    distances_raw$data <- tryCatch(
      {
        lapply(input$distances_file$datapath , function(distances_raw_file) importingDist(distances_raw_file, input$delimiter))
      },
      error=function(cond) {
        shiny::showNotification(
          ui="This does not seem to be a distance matrix, please provide a matrix with unique row and column names.",
          type="error",
          duration=10
        )
        distances_raw$data<-NULL
        shinyjs::reset("distances_file")
        Sys.sleep(5)
        return()
      }
    )  
  })
  
  pop_sizes<-reactiveValues(data=NULL)
  observe({
    req(input$population_sizes_file)
    pop_sizes$data <- tryCatch(
      {
        importingPopSizes(input$population_sizes_file$datapath)
      },
      error=function(cond) {
        shiny::showNotification(
          ui="This does not seem to be a 2 column file containing the population sizes of the states at the ancestral nodes.",
          type="error",
          duration=10
        )
        pop_sizes$data<-NULL
        shinyjs::reset("population_sizes_file")
        return()
      }
    )  
  })
  
  tip_states<-reactiveValues(data=NULL)
  observe({
    req(input$distances_file$name, input$sampling_locations$datapath)
    sampling_locations = input$sampling_locations$datapath
    tip_states$data<-tryCatch(
      {
        as.factor(importingSamplingLocations(sampling_locations))
      },
      error=function(cond){
        tip_states$data<-NULL
        shinyjs::reset("sampling_locations")
        return()
      }
    )
  })
  
  observe({
    #Depending on choice of annotations included or not in the tree different file types can be selected.
    if(input$annotations==FALSE){
      shinyjs::show("tag_sampling_locations")
      updateSelectInput(
        session,
        inputId= "file_type",
        choices = c("Nexus" = "nexus",
                    "Newick" = "newick"),
        selected = "nexus"
      )
    }else{
      shinyjs::hide("tag_sampling_locations")
      updateSelectInput(
        session,
        inputId= "file_type",
        choices = c("Beast" = "beast"),
        selected = "beast"
      )
    }
  })
  
  #distances_raw is a list of the actual distance matrices, the length of the list is the number of seected matrices
  observe({
    req(distances_raw$data, input$tree_file)
    if(input$annotations==TRUE){
      if(is.null(tree)){
        shinyjs::disable(selector = "div.run")#disable the run button while tree is loaded and annotation guessed
        tree<<-tryCatch(
          {
            importingTree(input$tree_file$datapath, "beast")
          },
          error=function(cond){
            shiny::showNotification(
              ui=paste0("This file: ", input$tree_file$name, " does not appear to be a beast formatted phylogeny. 
                        Please provide an appropriate file!"),
              type = "error",
              duration=10)
            tree<<-NULL
            shinyjs::reset("tree_file")
            return()
          }
        )
        if(is.null(tree)){return()}#in that case we already informed the user
        candidate_annotation_columns<-colnames(tree[,unique(which(tree==colnames(distances_raw$data[[1]])[1], arr.ind=TRUE)[,2])])
        if(length(candidate_annotation_columns)>0){
          shiny::showNotification(
            ui=paste0("Done! \n Check the suggestions in the dropdown list for 'Annotation label in tree'."),
            type = 'message',
            duration=10)
          shinyjs::disable(selector="div.tree_file")#disable the run button while tree is loaded and annotation guessed
          shinyjs::disable(selector="div.distance_matrix_input")#disable the run button while tree is loaded and annotation guessed
          shinyjs::enable(selector = "div.run")#enable the "Run" button again
        } else{
          shiny::showNotification(
            ui="There seems to be no annotation in the tree that matches the column names of the distance matrices.",
            type="error",
            duration=10
          )
          tree<<-NULL
          shinyjs::reset("tree_file")
          return()
        }
      }else{
        candidate_annotation_columns<-colnames(tree[,unique(which(tree==colnames(distances_raw$data[[1]])[1], arr.ind=TRUE)[,2])])
      }
      updateSelectInput(
        session,
        inputId= "Annotation_State",
        choices = candidate_annotation_columns,
        selected = candidate_annotation_columns[1]
      )
    }
  })
  
  #This observer creates the list:
  ##file types depending on whether the tree is already annotated.
  ##extracts the names of the covariates (the distance matrices) that are uploaded by the user. And then the selectInput widget for univariate predictor and the checkbox list for the
  ##multivariate predictor are updated.
  observe({
    req(input$distances_file$name, column_names())
    #For the univariate dropdown list of predictors.
    updateSelectInput(
      session,
      inputId= "Predictor_uni",
      choices = column_names()[column_names() %!in% c("Transition_Rates")],
      selected = column_names()[1]    
    )
    responses<-"Transitions"
    if(input$Reconstruction_Method=="TT"){
      responses<-c("Transitions", "Transition_Rates")
    }
    updateSelectInput(
      session,
      inputId= "response_uni",
      choices = responses,
      selected = "Transitions"
    )
    
    #Multivariate checkbox group with the same names of the covariates as for the univariat analysis.
    updateCheckboxGroupInput(session, 
                             inputId= "variable", 
                             label = "Variables:", 
                             choices = column_names()[column_names() %!in% c("Transition_Rates")],
                             selected = column_names()[column_names() %!in% c("Transition_Rates")]
    )
  })         
  
  logs<-reactiveValues(logtransform= rep(FALSE, 2))  #whenever this variable "vals" changes then all dependencies, expressions that
  #contain vals is called
  #reactiveValues are eager, and this would not work using reactive, unless I add an observer
  #around reactive which makes it eager as well
  
  #Objects in this file are defined in each session
  #These documents are outside the actionbutton "RUN", everything outside of "RUN" can be updated without doing all calculations again. 
  #When we need to add new distance matrices, another tree or another sampling_location file, then "RUN" is needed.
  source("Multivariate.R", local=T)
  source("AncestralReconstruction.R", local=T)
  source("Univariate.R", local=T)
  source("Tree.R", local=T)
  
  observeEvent(input$uni_regression_output, {
    shinyjs::toggle(selector = "div.uni_regression_output", animType = "fade", anim=T)
  })
  
  #' observeEvent (RUN) that triggers the import of the files, the calculation of transition matrix and plotting of all plots.
  #' @param input$start Actionbutton that is pressed by the user triggers the execution of the code within this observer
  observeEvent(input$start, {
    if(is.null(input$tree_file)|| is.null(input$distances_file)){
      shiny::showNotification(
        ui=paste0("Please upload a tree file and at least one distance matrix."),
        type = 'error',
        duration=10)
    }
    req(input$tree_file, input$distances_file)
    if(input$annotations==FALSE) {
      if(is.null(input$sampling_locations)){
        shiny::showNotification(
          ui=paste0("Please upload a sampling locations file, consisting of all tip states."),
          type = 'error',
          duration=10)
      }
      req(input$sampling_locations)
    }
    shinyjs::show(selector = "div.regression_control", animType = "fade", anim=T)
    shinyjs::disable(selector="div.sidebar")
    shinyjs::enable(id="reset")
    if (input$annotations==FALSE){
      tree<-tryCatch(
        {
          importingTree(input$tree_file$datapath, input$file_type)
        },
        error=function(cond){
          shiny::showNotification(
            ui=paste0("This file: ", input$tree_file$name, " does not appear to be a rooted, binary phylogeny. 
                        Please provide an appropriate file!"),
            type = "error",
            duration=10)
          tree<<-NULL
          shinyjs::reset("tree_file")
          Sys.sleep(5)
          return()
        }
      )
      if(is.null(tree)){
        return()
      }
      
      input_reconstruction<-tryCatch(
        {
          validate_sampling_locations(session, tip_states$data, distances_raw$data, tree)
        },
        error=function(cond){
          shiny::showNotification(
            ui=paste0("This sampling locations number of states do not match the number of tip nodes.  
                        Please provide an appropriate sampling locations and tree file!"),
            type = "error",
            duration=10)
          tip_states<-NULL
          shinyjs::reset("sampling_locations")
          tree<<-NULL
          shinyjs::reset("tree_file")
          shinyjs::enable("sidebar")
          return()
        }
      )
      if(is.null(input_reconstruction)){
        return()
      }
      
      tree <<- tree <- tryCatch(
        {
          chooseReconstructionMethod(input_reconstruction$sampling_locations,  input_reconstruction$tree)
        },
        error=function(cond){
          shiny::showNotification(
            ui=paste0("The ancestral reconstruction failed, make sure you provide a rooted and fully dichotomous tree."),
            type = "error",
            duration=10)
          tree<<-NULL
          shinyjs::reset("tree_file")
          shinyjs::enable("sidebar")
          return()
        }
      )
    }
    #show all elements of type "div" that have the html class "regression.control"
    #Within the UI a range of these divs are hidden in the univariate tab to make the page look cleaner.
    transitions<-GenerateRawTransitionMatrix(distances_raw$data[[1]], tree=tree) 
    #take any distance matrix to get the col names and dimensions for the transitions matrix
    
    
    transition_distances <<- transition_distances <- GenerateFinal_Transitions_Distances(transitions_raw=transitions, distances_raw=distances_raw$data)
    
    #double assignment, to keep variable also locally...
    vals <-reactiveValues(keeprows = rep(TRUE, nrow(transition_distances)), keepZerosUni=rep(TRUE, nrow(transition_distances)), keepZerosMulti=rep(TRUE, nrow(transition_distances)), transitions=transition_distances$Transitions )    
    #do not display 0 transitions by default
    vals$transitions<-transition_distances$Transitions
    vals$keepZerosUni[which(transition_distances$Transitions==0)]<- FALSE
    vals$keepZerosMulti[which(transition_distances$Transitions==0)]<- FALSE
    vals<<-vals
  }) # observeEvent(input$start, {
  
  observeEvent(input$reset, {
     tree<-NULL
     distances_raw$data<-NULL
     tip_states$data<-NULL
     pop_sizes$data<-NULL
     shinyjs::reset("sidebar")
    session$reload()
  })
}) # shinyServer(function(input, output) {