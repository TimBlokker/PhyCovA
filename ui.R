#Univariate ####
##Sidebar####
library(shinyBS)
shinyUI(fluidPage(
  shinyjs::useShinyjs(), #activate Shinyjs
  tabsetPanel(tabPanel(
    "Univariate analysis",
    titlePanel(
      list(
        column(12,tags$img(
          src="Logo_Lemey_Lab.jpeg", height="100px", width="100px"),
          "PhyCovA")
        ),
      windowTitle="PhyCovA"
    ),
    fluidRow(
      column(3,tags$a(href="https://github.com/TimBlokker/PhyCovA/tree/master/input", h5("Example files can be downloaded here"), align ="center"))
    ),
    sidebarLayout(
      tags$div(class="sidebar", id="sidebar",
               sidebarPanel(
                 width = 3,
                 tags$div(class="tree_file",
                          tags$h4("Univariate analysis"),
                          tags$h4("Input controls:"),
                          fluidRow(column(12,
                                          fileInput(
                                            "tree_file", 
                                            label = h5(HTML(paste("<b>", "Tree file", "</b>")),
                                               tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                               bsButton("q1", label = "", icon = icon("question"), style = "info", size = "extra-small"
                                                        )
                                               )
                                          ),
                                          bsTooltip("q1", "Add a tree file, e.g. a beast annotated tree file or an unannotated nexus file by clicking BROWSE and selecting the tree file in the appearing pop-up. Note: File names can not start with a hyphen (-) or a digit.In case the tree is already annotated, the annotations in the tree must exactly match the column names in the pairwise distance matrices. For both annotated and not annotated trees the phylogeny needs to be rooted",
                                                        "right", options = list(container = "body"))
                                          
                                          ))),
                 fluidRow(column(
                   12,
                   selectInput(
                     inputId = "file_type",
                     label = h5(HTML(paste("<b>", "Select tree file type:", "</b>")),
                                tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                bsButton("q2", label = "", icon = icon("question"), style = "info", size = "extra-small"
                                )
                     ),
                                  choices = c("Beast" = "beast",
                                 #"MrBayes" = "mrbayes",
                                 #"phylip" = "phylip",
                                 #"Nexus" = "nexus",
                                 #"Newick" = "newick"),
                                 selected = "beast"
                     )),
                   bsTooltip("q2", "Select the format of the tree file, e.g. a beast annotated tree file or an unannotated nexus file. The options depend on whether annotation is required or not, see below at the selection field: Is the tree annotated?",
                             "right", options = list(container = "body"))
                   ),
                 ),
                 tags$div(class="distance_matrix_input",
                          fluidRow(column(12,
                                          fileInput(
                                            "distances_file", label = h5(HTML(paste("<b>", "Matrix-based predictors", "</b>")),
                                                                         tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                                         bsButton("q3", label = "", icon = icon("question"), style = "info", size = "extra-small"
                                                                         )
                                            ),
                                          multiple=T
                                          ),
                                          bsTooltip("q3", "Select at least 1 distance matrix, this fie input field accepts more than 1 file. These files can be selected as in a regular file explorer by holding CTRL pressed while selecting the files intended for upload. Note: File names can not start with a hyphen (-) or a digit. The column names of the file that is uploaded here are a central element in the analysis. The annotation states of the tree (if it is already annotated) need to match these column names. Also the population sizes file needs to have the exact names of the states in the first column. ",
                                                    "right", options = list(container = "body"))
                          )
                          ),
                          fluidRow(column(
                            12,
                            textInput(inputId = "delimiter",
                                      label = h5(HTML(paste("<b>", "Delimiter distance matrices (optional)", "</b>")),
                                                 tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                 bsButton("q4", label = "", icon = icon("question"), style = "info", size = "extra-small"
                                                 )
                                      )
                          ),
                          bsTooltip("q4", "Here you can specify the delimiter for the distance matrix files. PhyCovA is able to identify the most common delimiters (comma , tab, semicolon, space , dash, underscore) but in case your favourite delimiter happens to be an @, then you can specify this here.",
                                    "right", options = list(container = "body"))
                          ))),
                 fluidRow(column(12,
                                 fileInput(
                                   "population_sizes_file", label = h5(HTML(paste("<b>", "List of population sizes (optional)", "</b>")),
                                                                       tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                                       bsButton("q5", label = "", icon = icon("question"), style = "info", size = "extra-small"
                                                                       )
                                   )
                                 ),
                                 bsTooltip("q5", "The population size file needs to be a 2-column file with the first column being the states and the second column being the population sizes. The states must have the exact same name as in the column names of the distance matrix.",
                                           "right", options = list(container = "body"))
                                 )),
                 fluidRow(column(
                   12,
                   radioButtons(
                     inputId = "annotations",
                     choices = c(
                       "No, please annotate my tree" = FALSE,
                       "Yes, I took care of this!" = TRUE),
                     label = h5(HTML(paste("<b>", "Is the tree annotated?", "</b>")),
                                tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                bsButton("q6", label = "", icon = icon("question"), style = "info", size = "extra-small"
                                )
                     )
                   ),
                   bsTooltip("q6", "Indicate wether the internal nodes of the tree are already annotated. In case you select YES, then the application will look for the annotation in the tree that matches the column names in the distance matrix. Iy you specify NO then you can choose how you would like to calculate the annotation of the nodes.",
                             "right", options = list(container = "body"))
                   )
                 ),
                 wellPanel("Annotations",
                           conditionalPanel(condition = "input.annotations=='TRUE'",
                                            fluidRow(column(
                                              12,
                                              selectInput(
                                                inputId = "Annotation_State",
                                                "Annotation label in tree",
                                                #c("host", "state", "states", "city", "location.states")
                                                NULL
                                              )
                                            ))),
                           conditionalPanel(condition = "input.annotations=='FALSE'",
                                            fluidRow(column(
                                              12,
                                              radioButtons(
                                                inputId = "Reconstruction_Method",
                                                "AR Method",
                                                c(
                                                  "Maximum parsimony" = "MP",
                                                  "Maximum likelihood" = "ML",
                                                  "TreeTime ML - GTR method" = "TT")))))),
                 tags$div(id="tag_sampling_locations",
                          fluidRow(column(
                            12,
                            fileInput("sampling_locations", label = h5(HTML(paste("<b>", "Sampling locations", "</b>")),
                                                                       tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                                       bsButton("q7", label = "", icon = icon("question"), style = "info", size = "extra-small"
                                                                       )
                            )
                            ),
                            bsTooltip("q7", "Ordered list of tip states (only for non-annotated trees). This file needs to contains a 1 column text file. The length of the states needs to match the number of the tip states in the tree and the states need to be the same as column names in the distance matrix.",
                                      "right", options = list(container = "body"))
                          ))
                 ),
                 # fluidRow(column(
                 #   12,
                 #   radioButtons(inputId = "Symmetrie", "Make matrix symmetric?",  c("No" =
                 #                                                                      FALSE, "Yes" = TRUE))
                 # )),
                 tags$div(class="run",
                          fluidRow(
                            column(6,actionButton("start", label = h4("RUN"), col.label ="red")),
                            column(6,actionButton("reset", label = h4("RESET"), col.label ="red")),
                            bsTooltip("start", "RUN calculaions, Inputs will be frozen until RESET is pressed",
                                      "right", options = list(container = "body"))
                            
                          ))
               )),
      ##MainPanel ####
      mainPanel(
        fluidRow(
          shinyjs::hidden(
            tags$div(class="regression_control",
          tags$h5("Note: PhyCovA is not designed to produce statistically rigorous results that are 
                                      publication-ready, but is intended as a data exploration tool allowing users to quickly
                                      test hypotheses that help in directing a more formal analysis.")
        ))),
        fluidRow(
          shinyjs::hidden(
            tags$div(class="regression_control",
                     conditionalPanel(condition = "input.Scatter_residual=='scatter'",
                                      tags$h4("Plotly - scatterplot"),
                                      plotlyOutput(outputId = "plot")
                     ),
                     conditionalPanel(condition= "input.Scatter_residual=='residuals'",
                                      tags$h4("Plot - residuals plot"),
                                      plotlyOutput(outputId = "plot_res")
                     ),
                     conditionalPanel(condition = "input.Scatter_residual=='bar'",
                                      tags$h4("Plotly - Barplot"),
                                      plotlyOutput(outputId = "bar")
                     )))),
        shinyjs::hidden(
          tags$div(class="regression_control",
                   wellPanel(
                     fluidRow(
                       column(
                         4,
                         selectInput(
                           inputId = "Scatter_residual",
                           "Scatter, residual plot selection",
                           c("Scatter plot (plotly)"="scatter", "Residuals plot"="residuals", "Barplot of Sum"="bar"))
                       ),
                       conditionalPanel(condition = "input.Scatter_residual!='bar'",
                                        column(4,selectInput(inputId = "Predictor_uni", label="Univariate predictor", choices=c(NULL))),
                                        column(4, checkboxInput(inputId = "standardize_uni",label= "Standardize"))
                       )),
                     fluidRow(
                       column(
                         4,
                         selectInput(
                           inputId = "response_uni", label="Univariate response", choices=c(NULL))),
                                        column(4, 
                                               checkboxInput(inputId = "includeZerosUni", label="Include observations with 0 transitions:",  value = FALSE
                                               ))),
                     conditionalPanel(condition = "input.Scatter_residual=='bar'",
                                      column(6,radioButtons(inputId = "to_from",
                                                            label= "Transitions to/from", 
                                                            choices = c("To"="To", "From"="From"), 
                                                            selected = "To")
                                      )),
                     fluidRow(
                       conditionalPanel(condition = "input.Scatter_residual!='bar'",
                                        tags$h4("Plot layout controls:"),
                                        column(4,numericInput(inputId = "stroke",label= "Stroke thickness", value=0.2,min = 0, step = 0.1)),
                                        column(4,numericInput(inputId = "size",label= "Point size", value=3, min = 0, step = 0.25 )),
                                        column(4,numericInput(inputId = "alpha",label= "Shading", value=0.5, min = 0, max = 1, step = 0.05))
                       )),
                     fluidRow(
                       conditionalPanel(condition = "input.Scatter_residual=='scatter'",
                                        column(3,actionButton("exclude_toggle", "Toggle points")),
                                        column(3,actionButton("exclude_reset", "Reset")),
                                        column(3,actionButton("log_transitions", "Toggle Log-Transitions")),
                                        column(3,actionButton("log_distances", "Toggle Log-Distance Metric"))
                       )),
                     fluidRow(
                       conditionalPanel(condition = "input.Scatter_residual=='scatter'",
                                        tags$h4("Regression line controls:"),
                                        column(4,checkboxInput(inputId = "regression_line",label= "Regression line")),
                                        column(4, conditionalPanel(condition = "input.regression_line",
                                                                   radioButtons(inputId = "se",
                                                                                label= "Show Confidence Interval", 
                                                                                choices = c(TRUE, FALSE), 
                                                                                selected = FALSE))),
                                        column(4, conditionalPanel(condition = "input.regression_line && input.se=='TRUE'",
                                                                   numericInput(inputId = "level",
                                                                                label= "Confidence level - shaded area",
                                                                                value = 0.95, min = 0, max = 1, step = 0.05)))
                       )),
                     fluidRow(
                       conditionalPanel(condition = "input.Scatter_residual=='scatter'",
                                        tags$h4("Colouring by Transition to/from states:"),
                                        column(
                                          12,
                                          selectInput(
                                            inputId = "colour_by_states_uni",
                                            label= "Colour by states",
                                            choices = c("To", "From", "All blue"),
                                            selected = "All blue"
                                          ))))
                   ))),
        #tags$h4("Hovering output:"),
        #fluidRow(verbatimTextOutput("hover")),
        shinyjs::hidden(
          tags$div(class="regression_control",
                   wellPanel(
                     fluidRow(
                       column(width=4, tags$h4("Univariate regression analysis:")),
                       column(offset = 6, width = 2, checkboxInput(inputId = "uni_regression_output",label= "Show"))
                     ),
                     fluidRow(
                       tags$div(class="uni_regression_output",
                                tableOutput(outputId = "lm"))),
                     fluidRow(
                       tags$div(class="uni_regression_output",
                                verbatimTextOutput(outputId = "lm.summary")))
                     
                   )))))),
    #tags$h4("Possible outliers:"),
    #fluidRow(tableOutput(outputId = "output"))
    
    #Multiple Univariate Regression ####
    tabPanel(
      "Multivariate analysis",
      headerPanel(
        list(HTML('<img src="Logo_Lemey_Lab.jpeg" height=100 width=100/>'), "PhyCovA"),
        windowTitle="PhyCovA multivariate analysis"
      ),
      ##MainPanel ####
      mainPanel(
        width = 12,
        wellPanel(
          fluidRow(
            column(width=4, tags$h4("Multivariate input control:")),
                             column(3, 
                                    tags$div(class="multi_input_control",checkboxInput(inputId = "includeZerosMulti", label="Include observations with 0 transitions:",  value = FALSE
                                    ))),
            column(offset = 3, width = 2, checkboxInput(inputId = "multi_input_control",label= "Show",value=T))
            
          ),
          fluidRow(
            tags$div(class="multi_input_control",
                       column(3, uiOutput( outputId ="response_multi_out", label="Response variable" )),
                       column(3, checkboxGroupInput(inputId = "variable", label = "Predictors:")),
                       column(3, uiOutput(outputId ="log", label="Log-transoform:")),
                       column(3, uiOutput(outputId="standardize", label="Standardize"))))),
        wellPanel(
          fluidRow(
            column(width=4, tags$h4("Multivariate regression analysis:")),
            column(offset = 6, width = 2, checkboxInput(inputId = "multi_regression_output",label= "Show", value =F))
          ),
          tags$div(class="multi_regression_output",
                   tags$h4("Basic statistical overview:"),
                   fluidRow(tableOutput(outputId = "lm_multi")),
                   tags$h4("Multivariate regression model:"),
                   fluidRow(column(3, 
                                   selectInput(
                                     inputId="regressionOutput",
                                     choices = c("lm.summary", "stepAIC", "regsubset.plot"),
                                     label="Regression Output")
                   )),
                   conditionalPanel(condition = "input.regressionOutput=='lm.summary'",
                                    fluidRow(verbatimTextOutput(outputId = "lm.summary_multi")),
                   ),
                   conditionalPanel(condition = "input.regressionOutput=='stepAIC'",
                                    fluidRow(column(3,uiOutput("k"))),
                                    fluidRow(verbatimTextOutput(outputId="stepAIC"))
                   ),
                   conditionalPanel(condition = "input.regressionOutput=='regsubset.plot'",
                                    fluidRow(actionButton(label="Download",inputId = "downloadRegsubsets" )),
                                    fluidRow(column(6,selectInput("step_method",
                                                                  label = "Stepwise selection method:",
                                                                  choices =  c("exhaustive", "backward", "forward"))),
                                             column(6,selectInput(
                                               inputId="crit.plot",
                                               label="Selection criterium:",
                                               choices=c("bic", "Cp", "r2", "adjr2"),
                                               selected="bic"
                                             ))),
                                    fluidRow(plotOutput(outputId = "regsubsets"))
                   )
          )),
        wellPanel(
          fluidRow(
            column(width=4, htmlOutput("typeMultiPlot")),
            column(offset = 6, width = 2, checkboxInput(inputId = "multi_plot_output",label= "Show"))
          ),
          tags$div(class="multi_plot_output",
                   fluidRow(column(4,
                                   selectInput(
                                     inputId="typeMultiPlot",
                                     label="Type of plot",
                                     choices=c("Scatter plot"="scatter", 
                                               "Correlation Matrix"="corr_balls", 
                                               "Base pair plot"="pairs", 
                                               "GGally pairs plot"="ggpairs")
                                   ))),
                   conditionalPanel(condition = "input.typeMultiPlot!='scatter'",
                   fluidRow(
                     column(6, sliderInput(
                       inputId = "width",
                       label = "Width",
                       min = 400, 
                       max = 2000,
                       step = 50,
                       value = 800
                     )),
                     column(6, sliderInput(
                       inputId = "height",
                       label = "Height",
                       min = 400, 
                       max =2000,
                       step =50,
                       value = 800
                     )))),
                   conditionalPanel(condition="input.typeMultiPlot=='scatter'",
                                    fluidRow(plotlyOutput(outputId = "multi_plot"))),
                   conditionalPanel(condition="input.typeMultiPlot=='corr_balls'",
                                    fluidRow(actionButton(label="Download",inputId = "downloadCorr" )),
                                    fluidRow(plotOutput(outputId = "corr_balls"))),
                   conditionalPanel(condition="input.typeMultiPlot=='pairs'",
                                    fluidRow(plotOutput(outputId = "pairs"))),
                   conditionalPanel(condition="input.typeMultiPlot=='ggpairs'",
                                    fluidRow(plotOutput(outputId = "ggpairs"))),
          ))
      )
    ),
    #Explore Tree ####
    tabPanel(title = "Explore tree",
             headerPanel(
               list(HTML('<img src="Logo_Lemey_Lab.jpeg" height=100 width=100/>'), "PhyCovA"),
               windowTitle="PhyCovA tree exploration"
             ),
             sidebarLayout(
               ##Sidebar ####
               sidebarPanel(
                 tags$h4("Tree"),
                 width=3,
                 fluidRow(
                   column(
                     12,
                     selectInput(
                       inputId = "Z_A_Tree",
                       "Zoom or annotation tree",
                       c("Zoom Tree (plotly)"="Z_Tree", "Annotation Tree (ggtree)"="A_Tree"))
                   )),
                 fluidRow(
                   column(
                     12,
                     checkboxInput(
                       inputId = "colour_by_states",
                       label= "Colour by states"
                     ))),
                 wellPanel("Coloured by states",
                           conditionalPanel(condition = "input.colour_by_states",
                                            fluidRow(
                                              column(
                                                12,
                                                numericInput(
                                                  inputId = "annotation_plot_legend_size",
                                                  label= "Annotation tree -  Legend text size",
                                                  value =15
                                                ))))),
                 fluidRow(
                   column(
                     12,
                     numericInput(
                       inputId = "tree_plot_height",
                       label = "Select plot height",
                       value = 1000
                     ))),
                 conditionalPanel(condition = "input.Z_A_Tree=='Z_Tree'",
                                  fluidRow(
                                    column(
                                      12,
                                      checkboxInput(
                                        inputId = "boost",
                                        label= "Reduce datapoints for large trees (scattergl) in trade for a considerable performance boost",
                                        value = TRUE
                                      )))),
                 ###Annotation Tree only ####
                 wellPanel("Annotation Tree",
                           conditionalPanel(condition = "input.Z_A_Tree=='A_Tree'",
                                            fluidRow(
                                              column(
                                                12,
                                                checkboxInput(
                                                  inputId = "ancestral_states",
                                                  label= "Ancestral states"
                                                ))),
                                            wellPanel("Ancestral states options",
                                                      conditionalPanel(condition = "input.ancestral_states",
                                                                       fluidRow(
                                                                         column(
                                                                           12,
                                                                           numericInput(
                                                                             inputId = "ancestral_states_size",
                                                                             label= "Ancestral states size",
                                                                             value =3
                                                                           ))))),
                                            ### Annotation tree - Selection of internal node #####
                                            ### This ui is generated in the tree.R file, so server side
                                            ### It is used to select the root node of the subtree that
                                            ### will be displayed when selected.
                                            uiOutput("select_node_render"),        
                                            fluidRow(
                                              column(
                                                12,
                                                checkboxInput(
                                                  inputId = "tip_labels",
                                                  label= "Tip label"
                                                ))),
                                            wellPanel("Tip label options",
                                                      conditionalPanel(condition = "input.tip_labels",
                                                                       fluidRow(
                                                                         column(
                                                                           12,
                                                                           numericInput(
                                                                             inputId = "tree_text_size",
                                                                             label = "Tip label size:",
                                                                             min = 0,
                                                                             value = 3
                                                                           ))))),
                                            fluidRow(
                                              column(
                                                12,
                                                numericInput(
                                                  inputId = "xlim_scaling",
                                                  label = "Scale width",
                                                  min = 1,
                                                  value = 1,
                                                  max=5,
                                                  step = 0.1
                                                ))),
                                            fluidRow(
                                              column(
                                                12,
                                                selectizeInput(
                                                  inputId = "select_layout",
                                                  label = "Select layout:",
                                                  choices = c("rectangular", 
                                                              "slanted",
                                                              "fan", 
                                                              "circular", 
                                                              "radial", 
                                                              "unrooted", 
                                                              "equal_angle", 
                                                              "daylight"
                                                  ),
                                                  width = "100%"
                                                ))),
                                            fluidRow(
                                              column(
                                                12,
                                                checkboxInput(
                                                  inputId = "node_number",
                                                  label= "Node number"
                                                ))),
                                            wellPanel("Node number options",
                                                      conditionalPanel(condition = "input.node_number",
                                                                       fluidRow(
                                                                         column(
                                                                           12,
                                                                           numericInput(
                                                                             inputId = "node_number_size",
                                                                             label= "Node number size",
                                                                             value = 3
                                                                           ))))),
                                            fluidRow(
                                              column(
                                                12,
                                                checkboxInput(
                                                  inputId = "tip_shapes",
                                                  label= "Annotation tree - tip shapes"
                                                ))),
                                            fluidRow(
                                              column(
                                                12,
                                                checkboxInput(
                                                  inputId = "node_shapes",
                                                  label= "Node shapes"
                                                )))
                           )#)wellPanel
                 )#)conditionalPanel
               ),#)sidebarPanel
               ## MainPanel ####
               mainPanel(fluidRow(
                 wellPanel("Tree",
                           conditionalPanel(condition = "input.Z_A_Tree=='Z_Tree'",
                                            uiOutput(outputId = "plotly_ui")
                           ),
                           conditionalPanel(condition= "input.Z_A_Tree=='A_Tree'",
                                            uiOutput(outputId = "plot_ui")
                           )
                 )#)wellpanel
               )#)fluidrow
               )#)mainPanel
             )#)SidebarLayout
    )#)TabPanel
  )#)tabsetPanel
)#)fluidpage
)#)ShinyUI