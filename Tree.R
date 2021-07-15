#This tree_df function
#Rendering the ui elements to select the node to subset,

tree_data<-reactive({
  req(tree, transition_distances)
  tree_data<-as.treedata(tree)
  # The if clause below, checks if the location where ggtree is checking for the edge.length (tree@phylo$edge.length) is null
  #if it is null then the conversion from data.frame/tibble to treedata object did not work correctly and the edge.length has been written to
  #tree@data$edge.length, and in our case the root node has an associated edge.length of "NA". So I drop that value and then assign the edge.length data
  #to the location where ggtree expects it and the tree is plotted correctly.
  
  #I confirmed this by assigning the edge.length from na.omit(tree@data$edge.length) to a global variable when running the app with the annotated tree
  #in a next step the app can be run using the browser() function below with the non-annotated version and then the edge.length can be compared via the
  #following line of code, which can be entered in the console:
  #all.equal(x_global, tree$phylo$edge.length, check.attributes=F) 
  #check attributes false becasue x_global has attributes such as "na.action"=1368, which is the root node which was found to be NA.
  #browser() #uncomment for debugging mode
  if(is.null(tree_data@phylo$edge.length)){
    x=na.omit(tree_data@data$edge.length)
    tree_data@phylo$edge.length=x
    #x_global <<- x
  }
  tree_data
})

output$select_node_render <- renderUI({
  req(tree_data())
  output <- tagList(
    fluidRow(
      column(
        12,
        numericInput(
          inputId = "node",
          label= "Annotation tree - Internal Node number:",
          value= length(getTipLabels(tree_data()))+1
        ))))
})



# creating the tree
output$plotly_tree <- renderPlotly({
  req(tree_data())
  tree_data<-tree_data()
  
  # creating the plot
  #aes_string(y = colnames(keep_high)[1], x ="value", group = "Predictors", key="Key")
  tree_data <- tree_data %>%
    fortify() %>% 
    as_tibble()
  
  Ancestral_States<-get(state(), tree_data)
  tree_data[which(tree_data$isTip==FALSE), "label"]<- "Internal Node"
  p <- tree_data %>%
    ggtree()+
    geom_point(aes("node"=node, "parent"=parent, label = Ancestral_States, label2=label), size=0.01)
  #geom_point seems to be needed to have the mapping done correctly for the tooltip
  if(input$colour_by_states) {
    nbColours<-length(unique(unlist(lapply(Ancestral_States, first.word))))#the states are added as a list in order to unlist them I need to take only the first word otherwise we get too many states
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))#these 9 colours will be interpolated to obtain  the most divergent result
    p <- p + geom_point(aes("node"=node, "parent"=parent,label = Ancestral_States, label2=label,color=unlist(lapply(Ancestral_States, first.word))))+
      scale_colour_manual(values=getPalette(nbColours))
  }
  p<- ggplotly(p, tooltip =c("parent", "node", "label", "label2"), source="plotly_tree",dynamicTicks = T ) %>%
    layout(legend = list(orientation = "h" , y=-0.01, title=get(state(), tree_data), font = list(size=input$annotation_plot_legend_size )))
  if(input$boost){
    p <-p %>% toWebGL()
  }#TODO either supress warnings because "hoveron" not known by toWebGL() or figure out to transfer info to 
  #label that is known to toWebGL() or perhaps rewrite plot to a non ggplot tree and then pipe to plotly from there
  p
  })

output$plotly_ui <- renderUI({
  plotlyOutput("plotly_tree", height = input$tree_plot_height)
})

output$plot_tree <- renderPlot({
  req(tree_data())
  tree_data<-tree_data()

  # creating the plot
  subtree <- tree_data %>%
    fortify() %>% 
    offspring(.node=input$node, include_self=F, tiponly = FALSE) #include_self is not working properly
  
  subtree2 <- tree_data %>%
    fortify() %>%
    as_tibble() %>%
    filter(node %in% c(subtree$node, input$node))
  
  p<- subtree2 %>% ggtree(layout=input$select_layout)
  if(input$node_shapes) p <- p + geom_nodepoint(aes(label = get(state(), subtree2), label2=label))
  if(input$tip_shapes) p <- p + geom_tippoint(aes(label = get(state(), subtree2) , label2=label))
  if(input$tip_labels)  p <- p + geom_tiplab(size = input$tree_text_size)
  if(input$ancestral_states) p <- p + geom_label(aes(label= get(state(), subtree2)), size= input$ancestral_states_size )
  if(input$node_number) p <- p +    geom_label(mapping = aes(label = node), size = input$node_number_size)
  if(input$colour_by_states) {
    nbColours<-length(unique(unlist(lapply(get(state(), subtree2), first.word))))#the states are added as a list in order to unlist them I need to take only the first word otherwise we get too many states
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))#these 9 colours will be interpolated to obtain  the most divergent result
    p <- p + geom_nodepoint(aes(color=unlist(lapply(get(state(), subtree2), first.word))))+
      scale_colour_manual(values=getPalette(nbColours))+
      theme(legend.position="bottom", legend.text=element_text(size= input$annotation_plot_legend_size), legend.title = element_blank())
  }
  p$data$x<-p$data$x-min(p$data$x)
  p + xlim(c(0, max(p$data$x)*input$xlim_scaling)) 
})

output$plot_ui <- renderUI({
  req(tree_data())
  plotOutput("plot_tree", height = input$tree_plot_height)
})