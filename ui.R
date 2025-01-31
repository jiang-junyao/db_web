ui <- {

  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style_1.css"),
      tags$script(src = "scripts.js"),
      tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"),
      tags$script(
        HTML("
        $(document).on('shiny:sessioninitialized', function(event) {
          if (localStorage.getItem('firstVisit') === null) {
            localStorage.setItem('firstVisit', 'true');
            location.reload();
          } else {
            localStorage.removeItem('firstVisit');
          }
        });
      ")
      )
    ),



  navbarPage(
    title = 'scLTdb: single cell lineage tracing database',
    #div(img(src = "jianbian.png",height='46px',width = '620px'),class = 'logo'),
    id = "inTabset",
    bg = "white",
    #----

    tabPanel(
      title = tags$span(
        "Home",
        `data-toggle` = "tooltip",
        `data-placement` = "bottom",
        title = "Go to Home Page",style = "font-size: 22px; font-family: 'Helvetica'",
      ),

      icon = icon('home', lib = 'font-awesome'),
      value = 'Home',
      h1('Database Aims',class = 'custom-h1'),
      #控制
      splitLayout(
        cellWidths = c("49%","8%","43%"),
        div(
          br(),
          br(),
          div(style = "background-color: white; padding: 20px; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;",
              class="responsive-text",
              p("scLTdb is a comprehensive single-cell lineage tracing database with multi-functional modules for the analysis of gene expression, fate outcomes, lineage relationships and potential regulators for cell fate determination. scLTdb includes 106 datasets including three species, 13 tissue sources, 2.7 million cells and 36 scLT technologies."),
              p("scLTdb provides:"),
              p("1. Search and query scLT datasets by species, tissue sources, barcode types, and technologies."),
              p("2. Re-analyze and viualize scLT datasets through three interactive modules, including single cell module, lineage tracing module, and integration module."),
              p("3. Download well-processed scLT datasets in h5ad and rds format."),
              p("4. Online tool to analyze single cell lineage tracing or bulk lineage tracing data."),
              p("5. A step-by-step tutorial for users to play scLTdb. Users can access our tutorial by clicking the ",
                actionLink("tutorial_link", "Tutorial"),
                " button located in the navigation bar."),
              p("Please note that when you click on the 'Explore dataset' button on the Search page, it may take approximately 30 seconds to load the dataset. This delay is caused by the large number of cells in the dataset and the limitations of computational resources. In case you encounter any difficulty loading the dataset, kindly refresh the page."),
          ),


        ),
        div(),
        div(img(src = "overview.png", style = "width: 100%;"))
       ),
      br(),
      h1('Introduction to single cell lineage tracing',class = 'custom-h1'),
      br(),
      div(
        p('In recent years, the next-generation lineage tracing has involved cellular barcoding that uses a large number of synthetic DNA sequences to uniquely label cells, providing quantitative insights into stem cell dynamics and cell fate outcomes. This strategy can be achieved using various barcode types, including integration of exogenous random DNA sequences into the cell genome, recombination of endogenous DNA units, or genome editing-mediated DNA insertions and deletions (INDEL). Cellular barcoding allows researchers to distinguish individual cells based on specific DNA barcode sequences at clonal resolution. Combining cellular barcoding with single-cell genomics, also known as single-cell lineage tracing (scLT), has generated rich datasets that resolve cell fate and transcriptional or epigenetic state of each cell.',class = 'responsive-text'),
        br(),
        div(img(src = "intro1.png",style = "width: 75%;"),class = 'main_figure')
      ) ,
      br(),
      br(),
      div(h1('Statistics',class = 'custom-h1'),
          br(),
          div(class = 'main_figure',
          img(src = "statistics.png", style = "width: 75%; "))
          ),
      br(),
      br(),
      div(
        class = 'contact_text',
        p("Contact:",class = "Contact_bold"),
        p("School of Life Sciences, Westlake University",class = "Contact_bold"),
        p("Weike Pei (correspondence): peiweike@westlake.edu.cn",class = "Contact_bold"),
        p("Junyao Jiang (maintainer): jiangjunyao@westlake.edu.cn",class = "Contact_bold"),
        p("Xing Ye (maintainer): yexing@mail.ustc.edu.cn",class = "Contact_bold")
      )


    ),
# 建立search页面 ----
tabPanel( title = tags$span( "Search", `data-toggle` = "tooltip",style = "font-size: 22px; font-family: 'Helvetica'", `data-placement` = "bottom", title = "Go to Search Page"),
          icon = icon('search',lib = 'font-awesome'),
          value = 'Search',

         fluidRow( column(12,div(class = "container-title", "Dataset overview") ) ),
         fluidRow(
           column(12, div(id = 'searchBox', style = "display: flex; flex-direction: row; margin-bottom: 20px;",
                          textInput("search", label = NULL, placeholder = "Type to search...",width = "100%"),
                          actionButton("searchBtn", label = "", icon = icon("search"), style = "height: 35px;") ) ) ),

         fluidRow(
             column(2, offset = 1,
                     pickerInput("species", "Species", choices = unique(coretable$Species), multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3" ,style = "font-size: 22px; font-family: 'Helvetica'") ) ),
             column(2,
                    pickerInput("tissue", "Tissue source", choices = unique(coretable$`Tissue source`), multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3" ,style = "font-size: 22px; font-family: 'Helvetica'") ) ),
             column(2,
                   pickerInput("technology", "Technology", choices = unique(coretable$Technology), multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3" ,style = "font-size: 22px; font-family: 'Helvetica'") ) ),
             column(2,
                    pickerInput("barcodetype", "Barcode type", choices = unique(coretable$`Barcode type`), multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE,selectedTextFormat = "count > 3" ,style = "font-size: 22px; font-family: 'Helvetica'") ) ) ),
         fluidRow( column(12, div(DT::dataTableOutput('coretable123', width = "100%" ) ) ) ),
         fluidRow( column(12, div(actionButton("go_to_panel", "Select dataset",class = 'jump') ) ) ) ,
         br(),
         br(),
         div(
           class = 'contact_text',
           p("Contact:",class = "Contact_bold"),
           p("School of Life Sciences, Westlake University",class = "Contact_bold"),
           p("Weike Pei (correspondence): peiweike@westlake.edu.cn",class = "Contact_bold"),
           p("Junyao Jiang (maintainer): jiangjunyao@westlake.edu.cn",class = "Contact_bold"),
           p("Xing Ye (maintainer): yexing@mail.ustc.edu.cn",class = "Contact_bold")
         )
         ),
#----

# 建立Results页面----
tabPanel(
  title = tags$span( "Results", `data-toggle` = "tooltip",style = "font-size: 22px; font-family: 'Helvetica'", `data-placement` = "bottom", title = "Go to Results Page"),
         icon = icon('chart-line', lib = 'font-awesome'),
         value = 'Results',

         fluidRow( class = "row-spacing", column(12, div(class="container-title", uiOutput('dynamicTitle')))),

         dashboardPage(  dashboardHeader(title = 'Select a module'),
           dashboardSidebar(
             width = 290,
             sidebarMenu(
               menuItem("Single cell module", tabName = "embedding", icon = icon("search-plus")),
               menuItem("Lineage tracing module", tabName = "main1", icon = icon("dashboard"),
                        menuSubItem(HTML("Fate outcome &<br>&nbsp&nbsp&nbsp&nbsp&nbspLineage relationship"), tabName = "heatmap",icon = icon("chart-bar")),
                        menuSubItem("Barcode statistics", tabName = "barcode", icon = icon("sliders")) ),
               menuItem("Intergrative module", tabName = "main2", icon = icon("th"),
                        menuSubItem("Fate bias deg", tabName = "deg", icon = icon("dna")),
                        menuSubItem("Clone deg", tabName = "deg_size", icon = icon("dna")),
                        menuSubItem(HTML("Visualize gene expression<br>&nbsp&nbsp&nbsp&nbspin different  fate bias"), tabName = "gene_violin", icon = icon("dna")) ) )),
           dashboardBody(
             width = "calc(100% - 220px)",
             style = "background-color: white; ",
             tabItems(
               tabItem(tabName = "embedding",
                  fluidRow(
                    div(h3('Here is the single-cell module of scLTdb, which offers a range of functions to visualize and re-analyze scRNA-seq (Spatial Transcriptome,scATAC-seq,scMethy-seq) data modalities within scLT datasets:'),
                        h3('1.Cell annotation (Pseudotime) embedding: visualize cell type (state) annotaion and pseudotime.'),
                        h3('2.Clone embedding: visualize a set of clones or single clone on single cell embedding.'),
                        h3('3.Cell type (state) number barplot: Visualize cell type (state) number and fraction.'),
                        h3('4.Gene expression: Plot the gene of interest on the embedding plot. Please note that this function does not work for scATAC-seq and scMethy-seq datasets.'),
                        style = 'margin-left:10px;'),
                    br(),
                    div(tableOutput("sampleInfo"
                                    ),class = 'resulttable'
                       ),
                    column(6,
                      div( class="container-subtitle", "Cell annotation (Pseudotime) embedding" ),
                      div(  style = "display: flex; justify-content: space-between;",
                            div(style="display: flex; justify-content: flex-start; width: 100% ",
                                pickerInput("change_svg", label = 'Colored by', choices = c('Cell type (state)','Pseudotime'), selected = 'Cell type (state)', width = '40%') ),
                       div(style="display: flex; justify-content: flex-end; ", selectInput("selectPage", label = NULL, choices = NULL, selected = 1, width = '120px') )),
                      div( #style = "width: 100%; height: 30vw;",
                           uiOutput("pseudotime_or_celltype")
                        )
                      ),
                    column(6,
                      div( class="container-subtitle", "Clone embedding"),
                      div(style = "position: relative; width: 100%;",
                       div( style = "display: flex; justify-content: space-around; align-items: center; width: 100%;",

                            pickerInput( inputId = "select_A_fate_bias", label = 'Select fate bias', choices = NULL, multiple = TRUE,
                                         options = pickerOptions( `size` = 15, actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3"), width = '100%', ),

                            pickerInput( inputId = "select_A_barcode", label = 'Select Clone ID', choices = NULL, multiple = TRUE,
                                         options = pickerOptions( `size` = 15, actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3"), width = '100%' ) ),
                       div(girafeOutput("girafe_output_barcode", width = "100%",height= "100%" ) ) ) ) ),
                  fluidRow(
                    column(6,
                           div( class="container-subtitle", style = "margin-top: 10px;", "Cell type (state) number barplot" ),
                           div(style="display: flex; justify-content: flex-start; width: 100% ",
                               pickerInput("bar_or_pie", label = 'Select picture type:', choices = c('Pie plot','Bar plot'), selected = 'Bar plot', width = '40%') ),
                           div( girafeOutput("barplot_celltype_number", width = "100%",height= "100%") ) ),


                    column(6,
                           div( class="container-subtitle", style = "margin-top: 10px;", "Gene expression" ),
                           div( textInput("gene_expression", "Enter gene name", "ExampleGene"), actionButton("submit", "Submit")  ),
                           div( uiOutput("dynamic_plot_gene") ) )





                    ) ),
               tabItem(tabName = "heatmap",
                       div(h3('Here is the lineage tracing module of scLTdb, which offers a range of functions to visualize and re-analyze lineage tracing (clone) data modalities within scLT datasets:'),
                           h3('This page offers two functions:'),
                           h3('1. Fate outcome: analyze and visualize fate outcomes among different cell types (states). Users can adjust the column order in the heatmap by rolling the bars on the left of the heatmap.'),
                           h3('2. Lineage relationship: Analyze and visualize the similarity between cell types (states) by calculating the Spearman correlation using barcode count. Users can selecte cell types of interested to calculate the lineage relationships by clicking "Select cell type (state)".'),
                           style = 'margin-left:10px;'),
                       br(),
                       fluidRow(style = "height: 70vh;",
                         splitLayout( cellWidths = c("40%", "60%"),
                         div( style='width:100%;height:100%',
                        div( style='width:100%;height:100%',
                          div(style = "font-size: clamp(10px, 1.4vw, 25px); text-align: center;","Custom Cell Name Sorting"),
                          uiOutput("celltype_clone_applysort")
                          ) ),

                         div(
                        div( class="container-subtitle", "Fate outcome"),
                        div(style="display: flex; justify-content: flex-start; width: 100% ",
                            pickerInput("change_normalize_method", label = 'Normalize method:', choices = c('ratio','log'), selected = 'ratio', width = '40%') ),
                        div(plotlyOutput("celltype_clone_heatmap" ,height= "30.5vw",width= "100%" ) ) )
                         )),

                       fluidRow(
                         div(
                        div( class="container-subtitle", "Lineage relationship",style='width:70%'),
                        div(style="display: flex; justify-content: flex-start; width: 100% ",
                            uiOutput("lineage_celltype_sort") ),
                        div(plotlyOutput("lineage_relationship_heatmap" ,height  = "30.5vw",width= "70%" ) ) )
                      )),
               tabItem(tabName = "barcode",
                       div(h3('Here is the lineage tracing module of scLTdb, which offers a range of functions to visualize and re-analyze lineage tracing (clone) data modalities within scLT datasets.'),
                           h3('This page offers four functions:'),
                           h3('1. Unique barcode number in each cell type: bar plot to show unique barcode number in each cell type (state)'),
                           h3('2. Number of cell with barcode in each cell type: bar plot to show number of cell with barcode in each cell type (state)'),
                           h3('3. Clone size statistics: bar plot to show the clone size (cell counts) of a set of clones or single clone'),
                           h3('4. Fate bias summary: visualize clone fate bias in specific cell type (state). In single-cell lineage tracing data analysis, clone fate bias refers to the phenomenon where a particular clone or cell lineage shows a propensity to differentiate into specific cell types or states.'),
                           style = 'margin-left:10px;'),
                       br(),
                       fluidRow( style = "height: 60vh;",
                         column(6,
                                div( class="container-subtitle", style = "margin-top: 10px;", "Unique barcode number in each cell type" ),
                                div( girafeOutput("barplot_barcode_1", width = "100%",height= "100%") ) ),
                         column(6,
                                div( class="container-subtitle", style = "margin-top: 10px;", "Number of cell with barcode in each cell type" ),
                                div( girafeOutput("barplot_barcode_2", width = "100%",height= "100%") ) )
                       ),

                       fluidRow( style = "height: 100vh;",
                                 column(6,
                                        div( class="container-subtitle", "Clone size statistics"),
                                        div( style = "display: flex; justify-content: space-around; align-items: center; width: 100%;",
                                             pickerInput(
                                               inputId = "select_A_fate_bias_size",
                                               label = 'Select fate bias',
                                               choices = NULL,
                                               multiple = TRUE,
                                               options = pickerOptions(
                                                 `size` = 15,
                                                 actionsBox = TRUE,
                                                 liveSearch = TRUE,
                                                 selectedTextFormat = "count > 3" ), width = '100%'),
                                             pickerInput(
                                               inputId = "select_A_clone_size",
                                               label = 'Select Clone ID',
                                               choices = NULL,
                                               multiple = TRUE,
                                               options = pickerOptions(
                                                 `size` = 15,
                                                 actionsBox = TRUE,
                                                 liveSearch = TRUE,
                                                 selectedTextFormat = "count > 3" ), width = '100%')
                                        ),

                                        fluidRow( style = "height: 90%;",
                                                  girafeOutput(outputId = "girafe_output_clone_size", width = "100%", height = "100%")   ) ),
                                 column(6,
                                        div( class="container-subtitle", "Fate bias summary"),
                                        div(style = "display: flex; justify-content: space-around; align-items: center; width: 100%;",
                                          pickerInput(
                                                inputId = "select_A_celltype_fates",
                                                label = 'Select cell type (state)',
                                                choices = NULL,
                                                multiple = FALSE,
                                                options = pickerOptions(
                                                  `size` = 15,
                                                  actionsBox = TRUE,
                                                  liveSearch = TRUE,
                                                  selectedTextFormat = "count > 3"
                                                ),width = '100%'),
                                            pickerInput("bar_or_pie_1", label = 'Select picture type:', choices = c('Pie plot','Bar plot'), selected = 'Bar plot', width = '100%')),

                                        div( girafeOutput(outputId = "girafe_output_celltype_fates", width = "100%", height = "100%")  ) ) ) ),
               tabItem(tabName = "deg",
                       div(h3('Here is the integration module of scLTdb. This module offers the integrative analysis of scRNA-seq (scATAC-seq) and lineage tracing data.'),
                           h3('This page offers two functions:'),
                           h3('1. Fate bias DEGs: These are the set of DEGs found in two distinct cell groups that display contrasting fate biases within a specific cell type (state). To utilize this feature, users are required to first choose a specific cell type (state) and then select a particular fate bias associated with that cell type (state). The outcome of this analysis reveals the extent of transcriptome heterogeneity (DEGs) between the selected cell type (state) with two different fate biases.'),
                           h3('2. Enrichment analysis (gene ontology) for fate bias DEGs.'),
                           h3('Attention! If this dataset have no significant fate bias DEGs, the scLTdb will display the message "No significant DEGs in this datasets". If fate bias DEGs are unable to enrich for any functions in the gene ontology, the scLTdb will display the message "No significant functions in this datasets"'),
                           style = 'margin-left:10px;'),
                       br(),
                       fluidRow( style = "height: 1200px;",
                                 column(6,
                                        uiOutput("imageOutputOrPreset")),
                                 column(6,uiOutput("imageOutputOrPreset_2"))),
                       fluidRow( #style = "height: 800px;",
                         column(6,uiOutput("imageOutputOrPreset_3")),
                         column(6,uiOutput("imageOutputOrPreset_4"))) ),
               tabItem(tabName = "deg_size",
                       div(h3('Here is the integration module of scLTdb. This module offers the integrative analysis of scRNA-seq (scATAC-seq) and lineage tracing data.'),
                           h3('This page offers two functions:'),
                           h3('1. Clone DEGs: DEGs among different clones in specific cell type (state). To utilize this feature, users are required to first choose a specific cell type (state) and then select two clones in this cell type (state).'),
                           h3('2. Enrichment analysis (gene ontology) for clone DEGs.'),
                           h3('Attention! If this dataset have no significant clone DEGs, the scLTdb will display the message "No significant DEGs in this datasets". If clone DEGs are unable to enrich for any functions in the gene ontology, the scLTdb will display the message "No significant functions in this datasets"'),
                           style = 'margin-left:10px;'),
                       br(),
                       fluidRow( style = "height: 800px;",
                                 column(6,
                                        uiOutput("imageOutputOrPreset_size")),
                                 column(6,uiOutput("imageOutputOrPreset_size_2" ))),
                       fluidRow(
                                 column(6,uiOutput("imageOutputOrPreset_size_3")),
                                 column(6,uiOutput("imageOutputOrPreset_size_4"))) ),
               tabItem(tabName = "gene_violin",
                       div(h3('Here is the integration module of scLTdb. This module offers the integrative analysis of scRNA-seq (scATAC-seq) and lineage tracing data.'),
                           h3('This page offers a violin plot to visualize gene expression among different fate biases in a specific cell type. To use this function, users are required to first choose a specific cell type (state) and then enter the gene name in the input box.'),
                           h3('Please note that this function only supports datasets containing the transcriptome modality.'),
                           style = 'margin-left:10px;'),
                       br(),
                       fluidRow( style = "height: 100vh;",
                                 column(12,
                                        div(    pickerInput(
                                          inputId = "select_A_celltype_gene_violin",
                                          label = 'Select cell type (state)',
                                          choices = NULL,
                                          multiple = FALSE,
                                          options = pickerOptions(
                                            `size` = 15,
                                            actionsBox = TRUE,
                                            liveSearch = TRUE,
                                            selectedTextFormat = "count > 3"
                                          ),width = '30%')),
                                        div( textInput("gene_violin", "Enter gene name", "ExampleGene"), actionButton("submit_violin", "Submit")  ),
                                        uiOutput("dynamic_gene_violin"))
                                     ) )


               ))),
  br(),
  br(),
  div(
    class = 'contact_text',
    p("Contact:",class = "Contact_bold"),
    p("School of Life Sciences, Westlake University",class = "Contact_bold"),
    p("Weike Pei (correspondence): peiweike@westlake.edu.cn",class = "Contact_bold"),
    p("Junyao Jiang (maintainer): jiangjunyao@westlake.edu.cn",class = "Contact_bold"),
    p("Xing Ye (maintainer): yexing@mail.ustc.edu.cn",class = "Contact_bold")
  ),
  ),
#----

# 建立Download页面 ----
tabPanel( title = tags$span( "Download", `data-toggle` = "tooltip",style = "font-size: 22px; font-family: 'Helvetica'", `data-placement` = "bottom", title = "Go to Download Page"),
          icon = icon('download',lib = 'font-awesome'),
          value = 'Download',

          fluidRow( column(12,div(class = "container-title", "Dataset overview") ) ),
          fluidRow(
            column(12, div(id = 'searchBox_1', style = "display: flex; flex-direction: row; margin-bottom: 20px;",
                           textInput("search_download", label = NULL, placeholder = "Type to search...",width = "100%"),
                           actionButton("searchBtn_download", label = "", icon = icon("search"), style = "height: 35px;") ) ) ),

          fluidRow(
            column(2, offset = 2,
                   pickerInput("species_download", "Species", choices = unique(coretable1$Species), multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3" ) ) ),
            column(2,
                   pickerInput("tissue_download", "Tissue source", choices = unique(coretable1$`Tissue source`), multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3" ) ) ),
            column(2,
                   pickerInput("technology_download", "Technology", choices = unique(coretable1$Technology), multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3" ) ) ),
            column(2,
                   pickerInput("barcodetype_download", "Barcode type", choices = unique(coretable1$`Barcode type`), multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE,selectedTextFormat = "count > 3" ) ) ) ),
          fluidRow( column(12, div(DT::dataTableOutput('coretable123_D', width = "100%" ) ) ) ),

          fluidRow(
            column(12,
                   div(style = "background-color: white; padding: 20px; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;",
                       class="responsive-text",
                       p("For users who have difficulty downloading datasets from Cowtransfer, we also provide a ZENODO repository for them to access our well-processed single-cell lineage tracing datasets (https://zenodo.org/records/12176634).")
                   ) )),
          br(),
          br(),
          div(
            class = 'contact_text',
            p("Contact:",class = "Contact_bold"),
            p("School of Life Sciences, Westlake University",class = "Contact_bold"),
            p("Weike Pei (correspondence): peiweike@westlake.edu.cn",class = "Contact_bold"),
            p("Junyao Jiang (maintainer): jiangjunyao@westlake.edu.cn",class = "Contact_bold"),
            p("Xing Ye (maintainer): yexing@mail.ustc.edu.cn",class = "Contact_bold")
          ),
          ),
#----



tabPanel( title = tags$span( "Online tool", `data-toggle` = "tooltip", `data-placement` = "bottom",style = "font-size: 22px; font-family: 'Helvetica', sans-serif;", title = "Go to Online tool Page"),
          icon = icon('tools',lib = 'font-awesome'),
          value = 'online_tool',
          fluidRow(
            column(4,div(
              div( class="container-subtitle", "Clone analysis tool" ),
             div( style = "font-family: 'Helvetica', sans-serif;  font-size: clamp(10px, 1.5vw, 22px);",
             HTML("Welcome to the online tool of scLTdb to analyze bulk or single cell lineage tracing data.<br><br>
           You can click the Upload button at the right panel to start analysis. This online tool offers three functions: <br>
           (1) Fate Outcome function: this function is employed to analyze the distribution of clones across various cell types (samples). <br>
           (2) Lineage relationship : this fcuntion is used to calculate the lineage similarity of different cell types (samples). <br>
           (3) Clone fate bias: this function is used to assess the fate bias of a clone towards or against specific clusters.<br><br>
           Please find below the details regarding the demo data for this online tool. To access the demo data, you can simply click on the provided hyperlink. It is worth noting that our online tool supports two types of input formats: tables and matrices. <br>
           Table format: :<a href='https://drctdb.cowtransfer.com/s/2ec687039f504e'>demo data</a> <br>
           matrix format: :<a href='https://drctdb.cowtransfer.com/s/5c17f29dc0df4a'>demo data</a>" ) )) ),
            column(8,div( style = 'height:100vh',
              navbarPage(
                title=div(
                  style = "display: flex; align-items: center; margin-top: 1px; width: 300px; font-size: 22px; font-family: 'Helvetica', sans-serif;",
                  div( uiOutput("fileInputRender"), style = "margin-top: -5px; font-size: 22px; font-family: 'Helvetica', sans-serif;"),
                  div( actionButton("resetButton", "Clear"), style = "margin-top: -45px; font-size: 22px; font-family: 'Helvetica', sans-serif;") ),

                tabPanel(title = span("Fate outcome function",style = "font-size: 16px; font-family: 'Helvetica', sans-serif;"),
                         value = 'fate_outcome_module',
                         div( style = "display: flex; justify-content: space-around; align-items: center; width: 100%;",
                           # div(style="position: relative; width: 20%; height: 100%;",
                           #     uiOutput("tool_celltype_clone_applysort") ),
                           div(style="position: relative; width: 100%; height: 100%;",
                             plotlyOutput('cloneprofile_tools', width = "100%", height = "45vw")) ) ),

                tabPanel(title = span("Lineage relationship",style = "font-size: 16px; font-family: 'Helvetica', sans-serif;"),
                         value = 'lineage_relationship_module',
                         div( style = "display: flex; justify-content: space-around; align-items: center; width: 100%;",
                              # div(style="position: relative; width: 20%; height: 100%;",
                              #     uiOutput("tool_celltype_clone_applysort_2") ),
                              div(style="position: relative; width: 100%; height: 100%;",
                                  plotlyOutput('cell_type_fate_similartiy_tools', width = "100%", height = "45vw")) ) ),

                # tabPanel(title = span("Clone fate bias",style = "font-size: 16px;"),
                #          value = 'clone_fate_bias',
                #          div( style = "position: relative;", uiOutput('fate_bias_summary_tools'),
                #          div( downloadButton("downloadData", "Download Data"), style = "position: absolute; bottom: -60px; right: 10px; z-index: 100;" ) ) )

              )


            ))



          ),


          br(),
          br(),
          div(
            class = 'contact_text',
            p("Contact:",class = "Contact_bold"),
            p("School of Life Sciences, Westlake University",class = "Contact_bold"),
            p("Weike Pei (correspondence): peiweike@westlake.edu.cn",class = "Contact_bold"),
            p("Junyao Jiang (maintainer): jiangjunyao@westlake.edu.cn",class = "Contact_bold"),
            p("Xing Ye (maintainer): yexing@mail.ustc.edu.cn",class = "Contact_bold")
          ),
          ),

# 建立tutorial页面 ----
tabPanel( title = tags$span( "Tutorial", `data-toggle` = "tooltip",style = "font-size: 22px; font-family: 'Helvetica', sans-serif;", `data-placement` = "bottom", title = "Go to Tutorial Page"),
          icon = icon('book',lib = 'font-awesome'),
          value = 'Tutorial',

          fluidRow( style = "height: 100vh;",
            column(12,div(tags$iframe(style="height:100vh; width:100%", src="scLTdb-tutorial.pdf") ) ) ),
          br(),
          br(),
          div(
            class = 'contact_text',
            p("Contact:",class = "Contact_bold"),
            p("School of Life Sciences, Westlake University",class = "Contact_bold"),
            p("Weike Pei (correspondence): peiweike@westlake.edu.cn",class = "Contact_bold"),
            p("Junyao Jiang (maintainer): jiangjunyao@westlake.edu.cn",class = "Contact_bold"),
            p("Xing Ye (maintainer): yexing@mail.ustc.edu.cn",class = "Contact_bold")
          ),

           )
#----









       ),style = "width:80%;")
}
