chooseReconstructionMethod<-function(tip_states, tree_not_annotated){
  if(input$Reconstruction_Method=="MP"){
    max_ancestral_positions<-prepare_states_MP(tip_states)%>%
      MaximumParsimonyReconstruction(tip_states_numerical = ., tree_not_annotated)%>%
      max_ancestral_positions_MP(tip_states, tree_not_annotated=tree_not_annotated, ancestral_positions= .)
      tree_annotated<-writeAnnotatedTree(tree_not_annotated=tree_not_annotated, max_ancestral_positions = max_ancestral_positions, tip_states = tip_states )
      return(tree_annotated)
  }
  if(input$Reconstruction_Method=="ML"){
    ERreconstruction<-ML_Reconstruction(tip_states, tree_not_annotated)
    Q<-matrix(ERreconstruction$rate, length(levels(tip_states)), length(levels(tip_states)))
    max_ancestral_positions<-max_ancestral_positions_ML(tree_not_annotated=tree_not_annotated, ancestral_positions= ERreconstruction)
    tree_annotated<-writeAnnotatedTree(tree_not_annotated=tree_not_annotated, max_ancestral_positions = max_ancestral_positions, tip_states = tip_states )
  }
  
  if(input$Reconstruction_Method=="TT"){
    annotated_tree_Q<-treeTime_fun(tip_states, tree_not_annotated)
    annotated_tree<-annotated_tree_Q$annotated_tree 
    Q<-annotated_tree_Q$Q
    tree_annotated<-as_tibble(annotated_tree)
    old<-data.frame("label"=tree_not_annotated$tip.label, tip_states) #the tips are in a different order in the treeTime generated tree
    new<-data.frame("label"=tree_annotated$label[1:length(annotated_tree$tip.label)]) #new order
    tip_states<-left_join(new, old)[,2] #join the two dataframes on the tiplabel and keep only the new order, now with assigned states
    tree_annotated[which(colnames(tree_annotated)=="label")+1]<-tree_annotated[which(colnames(tree_annotated)=="label")] #make a column called "states to be 
    #consistent with the other methods ans to be able to pass through the matrix generation functions
    colnames(tree_annotated)[which(colnames(tree_annotated)=="label")+1]<-"states"
    tree_annotated[1:length(annotated_tree$tip.label),which(colnames(tree_annotated)=="states")]<-tip_states
  }
  #applicable for both ML methods
  colnames(Q)<-levels(tip_states)
  rownames(Q)<-levels(tip_states)
  system("mkdir -p treeTime")
  write.csv(file = "treeTime/transition_rates.csv", x = Q)
  return(tree_annotated)
}

ML_Reconstruction<-function(tip_states, tree_not_annotated){
  shiny::showNotification(
    ui=paste0("Creating Maximum Likelihood reconstruction"),
    type = "message",
    duration=10)
  ERreconstruction<-   ape::ace(x=tip_states,
                                phy=tree_not_annotated,
                                type = "discrete", method = "ML",
                                marginal = FALSE,
                                model="ER" )
  return(ERreconstruction)
}

treeTime_fun<-function(tip_states, tree_not_annotated){
  shiny::showNotification(
    ui=paste0("Creating Maximum Likelihood reconstruction via TreeTime - sit tight, we are right back!"),
    type = "message",
    duration=30)
  
  states<-data.frame(tree_not_annotated$tip.label, tip_states)
  colnames(states)<-c("tip_labels", "tip_states")
  system("mkdir -p treeTime")
  write.csv(file = "treeTime/states.csv", x = states)
  ape::write.tree(file="treeTime/tree_not_annotated.nwk", phy = tree_not_annotated)
  system("treetime mugration --tree treeTime/tree_not_annotated.nwk --states treeTime/states.csv --name-column tip_labels --attribute tip_states --outdir treeTime > tree_time.log")
  tree_time_log<-read_file(file = "tree_time.log")
  
  shiny::showNotification(
    ui=paste0(tree_time_log),
    type = "message",
    duration=30)
  
  system("grep -A5000 -m1 -e 'Actual rates from j->i (Q_ij):' treeTime/GTR.txt | tail -n+2 > Q.txt")
  Q<-read.table("Q.txt")
  treetext<-read_file("treeTime/annotated_tree.nexus")
  #find all ancestral node annotations, internal nodes get a label NODE_xxxxxxx, followed by the ancestral state, this can be extracted
  #the NODExx value and the belonging state are found this way and can be sorted to replace the label that read.nexus finds in the annotated tree (which is
  #NODE_xxx) with the actual state in the correct order. this can then be returned to the calling function (chooseReconstructionMethod)
  matched_string<-stringr::str_match_all(treetext, pattern="NODE_(\\d+):\\d+.\\d+\\[&tip_states=\"(\\w+)\"")
  matched_string<-data.frame(matched_string)[,2:3] #allowing for different data types
  colnames(matched_string)<-c("node_number", "state")
  matched_string<-rbind(c(0, "missing_root_state"), matched_string)# treeTime does not annotate the root ancestral state
  matched_string<-matched_string[order(matched_string$node_number),] #sort by NODE_xxx
  annotated_tree<-read.nexus("treeTime/annotated_tree.nexus") #read the file as tree
  annotated_tree$node.label<-matched_string$state #replace node numbers with belonging states
  return(list("annotated_tree"=annotated_tree, "Q"=Q))
}

####Maximum Parsimony
prepare_states_MP<-function(tip_states){
  as.numeric(as.factor(tip_states))
  #encoding<-cbind(levels(tip_states),as.numeric(1:length(levels(tip_states))))
  #tip_states_numerical<-as.numeric(lapply(tip_states, function(tip_state) encoding[which(encoding[,1]==tip_state),2]))
  #tip_states_numerical
}

MaximumParsimonyReconstruction<-function(tip_states_numerical, tree_not_annotated){
  shiny::showNotification(
    ui=paste0("Creating Maximum Parsimony reconstruction"),
    type = "message",
    duration=10)
  MP_ER <-   asr_max_parsimony(
    tree = tree_not_annotated,
    tip_states = tip_states_numerical,
    Nstates = NULL,
    transition_costs = "all_equal",
    edge_exponent = 0,
    weight_by_scenarios = TRUE,
    check_input = TRUE
  )
  return(MP_ER)
}

#taking the most likely state at each node
max_ancestral_positions_ML<-function(tree_not_annotated, ancestral_positions){
  max_ancestral_positions<-sapply(1:tree_not_annotated$Nnode, function(i) names(which(ancestral_positions$lik.anc[i,]==max(ancestral_positions$lik.anc[i,]))))
  max_ancestral_positions
}

#taking the first item at each node
max_ancestral_positions_MP<-function(tip_states, tree_not_annotated, ancestral_positions){
  colnames(ancestral_positions$ancestral_likelihoods)<-levels(tip_states)
  max_ancestral_positions<-sapply(1:tree_not_annotated$Nnode, function(i) names(which(ancestral_positions$ancestral_likelihoods[i,]==max(ancestral_positions$ancestral_likelihoods[i,]))))
  max_ancestral_positions
}


writeAnnotatedTree<-function(tree_not_annotated, max_ancestral_positions, tip_states){
  write.nexus(tree_not_annotated, file = "input/not_annotated.tree")
  N <- Nnode2(tree_not_annotated)
  ancestral_states_all<-c(as.vector(tip_states),max_ancestral_positions)
  annotations <- tibble(node = 1:N, states = ancestral_states_all)
  annotated_tree <- full_join(tree_not_annotated, annotations, by = "node")
  write.beast(annotated_tree, file = "input/annotated.tree")
  #system("cp output/test.tree input/")
  as_tibble(annotated_tree)
}

# original_states<-function(tree_annotated){
#   original.states=c(tree_annotated$root.annotation$states,
#                     sapply(which(tree_annotatedree2$edge[,2]>(tree_annotated$Nnode)),function(index){
#                       tree_annotated$annotations[[index]]$states
#                     })
#   )
# }
# 
# comparison<-function(tree_annotated, tree_not_annotated, ancestral_positions){#get ancestral positions and original states and output ratio of correctly reconstructed states
#   
#   max_ancestral_positions<-max_ancestral_positions(tree_not_annotated, ancestral_positions)
#   original.states<-original_states(tree_annotated)
#   
#   print(paste0(round(sum(max_ancestral_positions==original.states)/tree2$Nnode,2),"% of the states were correctly reconstructed."))
# }