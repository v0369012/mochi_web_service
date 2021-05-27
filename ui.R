# web version
library(shiny)
library(DT)
library(plotly)
library(shinythemes)
library(shinyFiles)
library(knitr)
library(pingr)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)

# my_qiime_public_ip <- ipify::get_ip()
# my_qiime_local_ip <- "127.0.0.1"
# my_qiime_port <- ":8011"


# Spinner style
spinner_type <- 2
spinner_bg_color <- "white"

# Cores
my_cores <- parallel::detectCores()
suggested_cores <- c()

if(my_cores<=2){
  suggested_cores <- 1
}else{
  suggested_cores <- my_cores-2
}

# if(is.na(ping_port(my_qiime_public_ip, port = "8011")[1])){
#   
#   my_qiime_ip <- my_qiime_local_ip
#   
# }else{
#   my_qiime_ip <- my_qiime_public_ip
# }
# 
# my_qiime_ip_port <- paste0(my_qiime_ip, my_qiime_port)

# Tabpanel style
tabPanel_title_style <- "color: white; font-size: 18px; font-weight: 200;font-family:Arial;"
tabPanel_navmenu_style <- "color: black; font-size: 18px; font-weight: 200;font-family:Arial;"

# denoise btn label
denoise_btn_start_label <- strong("Start!")

shinyUI(
  fluidPage(
    theme = shinytheme("readable"),
    # shinythemes::themeSelector(),
    tags$style(type = 'text/css', '.navbar { background-color: #317EAC;border-color: white;position:fixed;width:100%;}'),
    tags$style("h1 {color: #317EAC}"),
    tags$style("h4 {color: #317EAC}"),
    tags$style("a {color: #317EAC}"),
    
    
    # Define UI for data upload app ----
    titlePanel( 
      # window title
      windowTitle = "MOCHI",
      
      # title = span("MOCHI ",
      #              style = "color: blue; font-size: 50px; font-weight: 900; font-family: Comic Sans MS;",
      #              span(" A 16S rRNA NGS data analytical tool for microbiota",
      #                   style = "color: #009900; font-size: 16px; font-weight: 600; font-family: Comic Sans MS"))
      
      # title = tags$head(tags$link(rel="shortcut icon", 
      #                             href="/mochi_logo.png",
      #                             type = "image/png",
      #                             size = "32x32")),
      
      title = tags$head(tags$link(rel="shortcut icon", 
                                  href="https://mochi.life.nctu.edu.tw/mochi_logo_3.png",
                                  type = "image/png",
                                  size = "64x64")

                        # ,tags$head(span("MOCHI ",
                        #                style = "position:relative;left:15px;bottom:-20px;color: blue; font-size: 50px; font-weight: 900; font-family: Comic Sans MS;",
                        #                span(" A 16S rRNA NGS data analytical tool for microbiota",
                        #                style = "color: #009900; font-size: 16px; font-weight: 600; font-family: Comic Sans MS")
                        #                )
                        #           )
                        )
      
      
    ),
    
    # tags$head(span("MOCHI ",
    #                style = "position:relative;left:15px;bottom:-20px;color: blue; font-size: 50px; font-weight: 900; font-family: Comic Sans MS;",
    #                span(" A 16S rRNA NGS data analytical tool for microbiota",
    #                style = "color: #009900; font-size: 16px; font-weight: 600; font-family: Comic Sans MS")
    #                )
    #           ),
    
    # tags$head(tags$img(src = "https://mochi.life.nctu.edu.tw/mochi_title.png", height = 76, width = 300, style = "position:relative;bottom:-20px;top:50px"),
    #                span(" A 16S rRNA NGS data analytical tool for microbiota",
    #                     style = "position:relative;bottom:-38px;color: #009900; font-size: 16px; font-weight: 600; font-family: Comic Sans MS;top:50px")
    # ),
    
    navbarPage(
      # header = tags$h6("123"),
      collapsible = T,
      fluid = T,
      # id = "first_demo_page",
      # theme = "bootstrap.css",
      
      # tags$style(HTML(".navbar-header {background-color: #317EAC;}")),
      position = "fixed-top",
      
      # TRUE to use a dark background and light text for the navigation bar
      inverse = T,
      
      # title = strong("MOCHI", style = "color: white; font-weight: 800;width: 50px;font-size: 24px;"),
      title = a(img(src = "https://mochi.life.nctu.edu.tw/mochi_title_white.png", width = "250px", height = "100px", style = "position:relative;top: -20px;left:-10px"), 
                href = "https://mochi.life.nctu.edu.tw"),
      # title = span("MOCHI ",
      #              style = "color: white; font-size: 40px; font-weight: 800;padding:10px;text-align:center;line-hight:50px",
      #              ),
      
      
      # Home page
      tabPanel(title = span("Home", style = tabPanel_title_style),
               icon = icon("home", class = "home_icon"),
               tags$style(".home_icon {color:white}"),
               tags$style(".dna_icon {color:white}"), 
               tags$style(".caret {color: white}"), # Sequence preprocessing ">" color
               mainPanel(width = "100%",
                         style = "position:relative;top:-25px;",
                 # uiOutput(outputId = "show_user_id"),
                 withSpinner(ui_element = uiOutput("home_page"),
                             type = spinner_type,
                             color.background = spinner_bg_color)
                 
                 # withMathJax(includeMarkdown("~/text_files/home_page.Rmd"))
                 
                 # withMathJax(includeHTML("~/text_files/home_page.html"))
               )
      ),
      
      
      navbarMenu(title = span("Sequence Preprocessing", 
                              style = tabPanel_title_style),
                 icon = icon("dna", class = "dna_icon"),
                 # menuName = "first_demo_page",
                 
                 
                 
                 # Demultiplexed ----
                 tabPanel(title = span("Step 1. Sequence summary", style = tabPanel_navmenu_style),
                          
                          # fluidRow(
                          sidebarLayout(
                            
                            fluid = T,
                            sidebarPanel(
                              
                              style = "background-color: #317EAC; border: none; border-radius: 5px; margin-left: 0px; width: 500;position: relative;",
                              uiOutput(outputId = "show_job_id"),
                              textInput(inputId = "input_job_id_demux", label = "Nothing") %>% shinyjs::hidden(),
                              
                              
                              # strong("Directory selection", style = "font-size:24px;color:white;top:20px"),
                              strong("Sequence files", 
                                     
                                     style = "font-size:24px;color:white;top:20px"),
                              
                              
                              fileInput(inputId = "seqs_data_upload",
                                        label = span("Please select and upload the sequence files (*.fastq.gz or *.fq.gz)",
                                                     style= "font-size: 20px; font-weight: 300; color: white;"),
                                        multiple = T
                                        # accept = ".fastq.gz"
                                        ) %>% div(),
                              
                             
                              
                              actionButton(inputId = "load_parameter_demux",
                                           label = strong("Example sequences", style = "margin: 5px;font-size: 18px"),
                                           icon = icon("chalkboard-teacher"),
                                           style = "color:#317EAC;background-color:white;") %>% div(),
                              
                              # br(),br(),
                              hr(),
                              strong("Sequence type", style = "font-size:24px;color:white;top:20px"),
                              
                              pickerInput(inputId = "seqs_type",
                                          label = span("Choose the sequence type", 
                                                       style= "font-size: 20px; font-weight: 300; color: white; margin: 0px;"),
                                          choices = c("Single end", "Paired end", "Long read"),
                                          width = "300px"
                              ),
                              
                             div(
                               id = "primer_seqs_bttn",
                              hr(),
                              # p("After the database is selected, we need primer to extract the target region of sequences."),
                              strong("Primer sequences", style = "font-size:24px;color:white"),
                              checkboxInput("checkbox_primer",
                                            label = span("Check this if your sequences are primer-trimmed reads",
                                                         style = "font-size: 14px; font-weight: 300; color: white; margin-top: 5px;"),
                                            value = F),
                              
                              
                                selectInput(inputId = "primer_f", 
                                            label = span("Choose the forward primer sequence", style = "font-size: 20px; font-weight: 300; color: white; margin-top: 5px;"), 
                                            choice = c("8F", "27F", "CC [F]", "341F","357F", "515F", "533F", "16S.1100.F16", "1237F", "other"),
                                            width = "400px"
                                ),
                                
                                selectInput(inputId = "primer_r", 
                                            label = span("Choose the reverse primer sequence", style = "font-size: 20px; font-weight: 300; color: white; margin-top: 5px;"), 
                                            choice = c("519R", "CD [R]", "806R","907R", "1100R", "1391R", "1492R (l)", "1492R (s)", "other"),
                                            width = "400px"
                                ),
                              
                              # uiOutput(outputId = "in_r"),
                                actionButton(inputId = "show_primer", 
                                             label = "Show primer sequences", 
                                             icon = icon("table"),
                                             style = "margin-bottom:10px"
                                ),
                                
                                # p('If your primers could not be found from the primer table, you also can  manually input your primer sequences by choosing',
                                #   strong(' other.'),
                                #   style = "font-size: 16px; font-weight: 300; color: white; margin-top: 5px;"),
                                
                                uiOutput(outputId = "out_f"),
                                uiOutput(outputId = "out_r"),
                             
                              ) %>% shinyjs::hidden(),
                              
                              hr(),
                              strong("Computing setting", style = "font-size:24px;color:white"),
                              textInput(inputId = "n_jobs_demux", 
                                        label = span("Number of threads MOCHI can use", style = "font-size: 18px; font-weight: 300; color: white; margin-top: 5px;"),
                                        value = suggested_cores,
                                        placeholder = "Input number",
                                        width = "300px"),
                              actionButton(inputId = "my_cores_demux", 
                                           label = "Show the number of threads on system",
                                           icon = icon("calculator"))
                              # actionButton(inputId = "Q_cores_demux", 
                              #              label = "What is thread ?", 
                              #              icon = icon("question-circle")
                              #              )
                              
                              
                            ),
                            
                            mainPanel(
                              style="position: relative;margin-top: -20px",
                              # tags$head(tags$style(".modal-dialog{display: flex;justify-content: center;align-items: center;}")),
                              
                              conditionalPanel(
                                
                                # condition = "input.seqs_type == 'Single end'",
                                condition = "input.seqs_type == 'Single end'",
                                
                                # div(
                                column(width = 12,
                                  
                                  
                                  h1("1. Sequence summary (for single end)"),
                                  h4("(1) Summarize the single-end sequences.",style = "color: #317EAC;"),
                                  actionButton(inputId = "demultiplexed_single_ends", 
                                               label = strong("Start!"), 
                                               icon = icon("play-circle"),
                                               style = "margin: 10px; display: inline-block;"
                                  ),
                                  
                                  
                                  
                                 div(
                                    id = "demux_results_view_single",
                                    hr(),
                                    tabsetPanel(
                                      type = "tabs",
                                      tabPanel(
                                        title = span("Sequence counts summary",
                                                     style = "color:#317EAC"),
                                        h4("Forward reads"),
                                        tableOutput("demux_table_single"),
                                        plotlyOutput("demux_table_boxplot_single"),
                                        downloadButton("demux_table_single_dl", 
                                                       "Download the read couts table")
                                      ),
                                      
                                      tabPanel(
                                        title = span("Quality plot",
                                                     style = "color:#317EAC"),
                                        h4("Sequence length summary"),
                                        tableOutput("demux_Q_seven_table_single"),
                                        plotlyOutput("demux_Q_plot_single") %>% withSpinner(type = 2, color.background = "white"),
                                        downloadButton("demux_Q_table_single_dl", 
                                                       "Download the quality score table")
                                      ),
                                      tabPanel(
                                        title = "Log",
                                        tableOutput("demux_parameter_table_single"),
                                        downloadButton("demux_parameter_table_single_dl")
                                      )
                                    )
                                    
                                    
                                  ) %>% shinyjs::hidden(),
                                 
                                 
                                  
                                 
                                  
                                ),
                              ),
                              
                              conditionalPanel(
                                
                                condition = "input.seqs_type == 'Paired end'",
                                
                                column(width = 12,
                               
                                h1("1. Sequence summary (for paired end)"),
                                h4("(1) Summarize the paired-end sequences.",style = "color: #317EAC;"),
                                actionButton(inputId = "demultiplexed_paired_ends", 
                                                                label = strong("Start!"), 
                                                                icon = icon("play-circle"),
                                                                style = "margin: 10px; display: inline-block;"
                                                   ),
                                  # br(),br(),
                                
                                  div(
                                    id = "demux_results_view_paired",
                                    hr(),
                                    tabsetPanel(
                                      type = "tabs",
                                      tabPanel(
                                        title = span("Sequence counts summary",
                                                     style = "color:#317EAC"),
                                        h4("Forward reads"),
                                        tableOutput("demux_table_paired_f"),
                                        plotlyOutput("demux_table_boxplot_paired_f"),
                                        h4("Reverse reads"),
                                        tableOutput("demux_table_paired_r"),
                                        plotlyOutput("demux_table_boxplot_paired_r"),
                                        downloadButton("demux_table_paired_dl", 
                                                       "Download the read couts table")
                                      ),
                                      
                                      tabPanel(
                                        title = span("Quality plot",
                                                     style = "color:#317EAC"),
                                        h4("Forward sequence length summary"),
                                        tableOutput("demux_Q_seven_table_paired_f"),
                                        plotlyOutput("demux_Q_plot_paired_f") %>% withSpinner(type = 2, color.background = "white"),
                                        downloadButton("demux_Q_table_paired_dl_f", 
                                                       "Download the forward read couts table"),
                                        
                                        h4("Reverse sequence length summary"),
                                        tableOutput("demux_Q_seven_table_paired_r"),
                                        plotlyOutput("demux_Q_plot_paired_r") %>% withSpinner(type = 2, color.background = "white"),
                                        downloadButton("demux_Q_table_paired_dl_r", 
                                                       "Download the reverse read couts table")
                                      ), 
                                      tabPanel(
                                        title = "Log",
                                        tableOutput("demux_parameter_table_paired"),
                                        downloadButton("demux_parameter_table_paired_dl")
                                      )
                                    )
                                  ) %>% shinyjs::hidden()
                                  
                               
                                  
                                  
                                 )
                                
                                
                              ),
                              
                              conditionalPanel(
                                condition = "input.seqs_type == 'Long read'",
                                column(width = 12,
                                       h1("1. Sequence summary (for Long read)"),
                                       h4("(1) Summarize the long reads.",style = "color: #317EAC;"),
                                       actionButton(inputId = "demultiplexed_Pacbio", 
                                                    label = strong("Start!"), 
                                                    icon = icon("play-circle"),
                                                    style = "margin: 10px; display: inline-block;"
                                       ),
                                       div(
                                         id = "demux_results_view_Pacbio",
                                         hr(),
                                         tabsetPanel(
                                           type = "tabs",
                                           tabPanel(
                                             title = span("Sequence counts summary",
                                                          style = "color:#317EAC"),
                                             h4("Forward reads"),
                                             tableOutput("demux_table_Pacbio"),
                                             plotlyOutput("demux_table_boxplot_Pacbio"),
                                             downloadButton("demux_table_Pacbio_dl", 
                                                            "Download the read couts table")
                                           ),
                                           
                                           tabPanel(
                                             title = span("Quality plot",
                                                          style = "color:#317EAC"),
                                             h4("Sequence length summary"),
                                             tableOutput("demux_Q_seven_table_Pacbio"),
                                             plotlyOutput("demux_Q_plot_Pacbio") %>% withSpinner(type = 2, color.background = "white"),
                                             downloadButton("demux_Q_table_Pacbio_dl", 
                                                            "Download the quality score table")
                                           ),
                                           tabPanel(
                                             title = "Log",
                                             tableOutput("demux_parameter_table_Pacbio"),
                                             downloadButton("demux_parameter_table_Pacbio_dl")
                                           )
                                         )
                                         
                                         
                                       ) %>% shinyjs::hidden()
                                )
                              ),
                              
                              div(
                                id = "primer_table_hide",
                                hr(),
                                h2("Primer sequences table"),
                                # p('If your primers could not be found from the table, you can manually input your primer sequences by choosing',
                                  p("Select",strong("other")," and enter your primer sequences if you can't  find your primer sequence in this table."),
                                  # strong(' other'), span("."),
                                  style = "font-size: 16px; font-weight: 300; margin-top: 5px;",
                                dataTableOutput(outputId = "primer_seqs_table"),
                                
                                # style = "left:15px;top: 25px;position: relative"
                              ) %>% shinyjs::hidden()
                              
                              ,width = 8
                            )
                          )
                          
                          
                 ),
                 
                 
                 
                 
                 # Sequence denoising ----
                 tabPanel(title = span("Step 2. Sequence denoising", 
                                       style = tabPanel_navmenu_style),
                          
                          sidebarLayout(
                            fluid = T,
                            sidebarPanel(
                             # width = 5,
                             style = "background-color: #317EAC; border: none; border-radius: 5px; margin-left: 0px; width: 700;position: relative;",
                             uiOutput(outputId = "show_job_id_denoise"),
                             uiOutput(outputId = "dada2_parameter"),
                             textInput(inputId = "input_job_id_denoise", label = "Nothing") %>% shinyjs::hidden()
                                         ),
                            
                            mainPanel(
                              width = 8,
                              # style = "margin-top: -20px;position:relative;margin-left:-40px;display: inline-block;",
                              style = "position:relative;margin-top: -20px;",
                              # style = "position: relative;margin-top: -20px;border: solid 2px black;",
                              conditionalPanel(
                                
                                condition = "input.seqs_type == 'Single end'",
                                
                                column(width = 12,
                                
                                h1('2. Sequence denoising (DADA2 1.18.0) for Single end'),

                                h4("(1) Start to denoise."),
                                actionButton(inputId = "denoising_single", 
                                             label = denoise_btn_start_label,
                                             icon = icon("play-circle"),
                                             style = "margin: 10px; display: inline-block;"), 
                                
                                div(
                                  id = "dada2_results_single",
                                  
                                  hr(),
                                  tabsetPanel(
                                    type = "tabs",
                                    
                                    tabPanel(
                                      title = "Summary",
                                      # br(),br(),
                                      h4("Sample read count summary"),
                                      tableOutput("dada2_sample_summary_single"),
                                      h4("Sample summary table"),
                                      dataTableOutput("dada2_sample_table_single"),
                                      downloadButton("dada2_sample_table_single_dl"),
                                      br(),br(),
                                      h4("ASV read count summary"),
                                      tableOutput("dada2_asv_summary_table_single"),
                                      h4("ASV summary table"),
                                      dataTableOutput("dada2_asv_table_single"),
                                      downloadButton("dada2_asv_table_single_dl")
                                    ),
                                    tabPanel(
                                      title = "Filter info",
                                      br(),br(),
                                      dataTableOutput("dada2_filter_table_single"),
                                      downloadButton("dada2_filter_table_single_dl")
                                    ),
                                    tabPanel(
                                      title = "Sequence info",
                                      h4("Sequence Length Statistics"),
                                      tableOutput("dada2_seqs_info_single_1"),
                                      h4("Seven-Number Summary of Sequence Lengths"),
                                      tableOutput("dada2_seqs_info_single_2"),
                                      h4("Sequence table"),
                                      dataTableOutput("dada2_seqs_table_single"),
                                      downloadButton("dada2_seqs_table_single_dl", "Download the sequences as a FASTA file")
                                      ),
                                    tabPanel(
                                      title = "Rarefaction plot",
                                      br(),br(),
                                      plotOutput("rarefaction_plot_single", height = "500px"),
                                      downloadButton("rarefaction_plot_single_dl", "Download the rarefaction plot"),
                                      downloadButton("rarefaction_table_single_dl", "Download the rarefaction table")
                                    ),
                                    
                                    tabPanel(
                                      title = "Table",
                                      br(),br(),
                                      dataTableOutput("dada2_table_single"),
                                      downloadButton("dada2_table_single_dl")
                                    ),
                                    
                                    tabPanel(
                                      title = "Log",
                                      tableOutput("dada2_log_table_single"),
                                      downloadButton("dada2_log_table_single_dl")
                                    )
                                  )
                                ) %>% shinyjs::hidden()
                                
                                
                                  )
                              ),
                              
                              
                              
                              conditionalPanel(
                                
                                condition = "input.seqs_type == 'Paired end'",
                                
                                column(width = 12,
                                
                                uiOutput(outputId = "show_job_id_denoise_paired"),
                                h1('2. Sequence denoising (DADA2 1.18.0) for Paired end'),
                                
                                
                                # br(),br(),
                                h4("(1) Start to denoise."),
                                actionButton(inputId = "denoising_paired", 
                                             label = denoise_btn_start_label,
                                             icon = icon("play-circle"),
                                             style = "margin: 10px; display: inline-block;"), 
                                # br(),br(),
                                # hr(),
                                # strong('Results', style = "font-size: 20px;color: #317EAC"),
                                div(
                                  id = "dada2_results_paired",
                                  hr(),
                                  tabsetPanel(
                                    type = "tabs",
                                    
                                    tabPanel(
                                      title = "Summary",
                                      # br(),br(),
                                      h4("Sample read count summary"),
                                      tableOutput("dada2_sample_summary_paired"),
                                      h4("Sample summary table"),
                                      dataTableOutput("dada2_sample_table_paired"),
                                      downloadButton("dada2_sample_table_paired_dl"),
                                      br(),br(),
                                      h4("ASV read count summary"),
                                      tableOutput("dada2_asv_summary_table_paired"),
                                      h4("ASV summary table"),
                                      dataTableOutput("dada2_asv_table_paired"),
                                      downloadButton("dada2_asv_table_paired_dl")
                                    ),
                                    tabPanel(
                                      title = "Filter info",
                                      br(),br(),
                                      dataTableOutput("dada2_filter_table_paired"),
                                      downloadButton("dada2_filter_table_paired_dl")
                                    ),
                                    tabPanel(
                                      title = "Sequence info",
                                      h4("Sequence Length Statistics"),
                                      tableOutput("dada2_seqs_info_paired_1"),
                                      h4("Seven-Number Summary of Sequence Lengths"),
                                      tableOutput("dada2_seqs_info_paired_2"),
                                      h4("Sequence table"),
                                      dataTableOutput("dada2_seqs_table_paired"),
                                      downloadButton("dada2_seqs_table_paired_dl", "Download the sequences as a FASTA file")
                                    ),
                                    tabPanel(
                                      title = "Rarefaction plot",
                                      br(),br(),
                                      plotOutput("rarefaction_plot_paired", height = "500px"),
                                      downloadButton("rarefaction_plot_paired_dl", "Download the rarefaction plot"),
                                      downloadButton("rarefaction_table_paired_dl", "Download the rarefaction table")
                                    ),
                                    tabPanel(
                                      title = "Table",
                                      br(),br(),
                                      dataTableOutput("dada2_table_paired"),
                                      downloadButton("dada2_table_paired_dl")
                                    ),
                                    tabPanel(
                                      title = "Log",
                                      tableOutput("dada2_log_table_paired"),
                                      downloadButton("dada2_log_table_paired_dl")
                                    )
                                  )
                                  ) %>% shinyjs::hidden()
                                
                                

                                
                              )
                              ),
                              
                              conditionalPanel(
                                
                                condition = "input.seqs_type == 'Long read'",
                                
                                column(width = 12,
                                       
                                       h1('2. Sequence denoising (DADA2 1.18.0) for Long read'),
                                       
                                       h4("(1) Start to denoise."),
                                       actionButton(inputId = "denoising_Pacbio", 
                                                    label = denoise_btn_start_label,
                                                    icon = icon("play-circle"),
                                                    style = "margin: 10px; display: inline-block;"),
                                       
                                       div(
                                         id = "dada2_results_Pacbio",
                                         
                                         hr(),
                                         tabsetPanel(
                                           type = "tabs",
                                           
                                           tabPanel(
                                             title = "Summary",
                                             # br(),br(),
                                             h4("Sample read count summary"),
                                             tableOutput("dada2_sample_summary_Pacbio"),
                                             h4("Sample summary table"),
                                             dataTableOutput("dada2_sample_table_Pacbio"),
                                             downloadButton("dada2_sample_table_Pacbio_dl"),
                                             br(),br(),
                                             h4("ASV read count summary"),
                                             tableOutput("dada2_asv_summary_table_Pacbio"),
                                             h4("ASV summary table"),
                                             dataTableOutput("dada2_asv_table_Pacbio"),
                                             downloadButton("dada2_asv_table_Pacbio_dl")
                                           ),
                                           tabPanel(
                                             title = "Filter info",
                                             br(),br(),
                                             dataTableOutput("dada2_filter_table_Pacbio"),
                                             downloadButton("dada2_filter_table_Pacbio_dl")
                                           ),
                                           tabPanel(
                                             title = "Sequence info",
                                             h4("Sequence Length Statistics"),
                                             tableOutput("dada2_seqs_info_Pacbio_1"),
                                             h4("Seven-Number Summary of Sequence Lengths"),
                                             tableOutput("dada2_seqs_info_Pacbio_2"),
                                             h4("Sequence table"),
                                             dataTableOutput("dada2_seqs_table_Pacbio"),
                                             downloadButton("dada2_seqs_table_Pacbio_dl", "Download the sequences as a FASTA file")
                                           ),
                                           tabPanel(
                                             title = "Rarefaction plot",
                                             br(),br(),
                                             plotOutput("rarefaction_plot_Pacbio", height = "500px"),
                                             downloadButton("rarefaction_plot_Pacbio_dl", "Download the rarefaction plot"),
                                             downloadButton("rarefaction_table_Pacbio_dl", "Download the rarefaction table")
                                           ),
                                           tabPanel(
                                             title = "Table",
                                             br(),br(),
                                             dataTableOutput("dada2_table_Pacbio"),
                                             downloadButton("dada2_table_Pacbio_dl")
                                           ),
                                           tabPanel(
                                             title = "Log",
                                             tableOutput("dada2_log_table_Pacbio"),
                                             downloadButton("dada2_log_table_Pacbio_dl")
                                           )
                                         )
                                       ) %>% shinyjs::hidden()
                                )
                              ),
                              
                              div(
                                id = "primer_table_hide_Pacbio",
                                hr(),
                                h2("Primer sequences table"),
                                # p('If your primers could not be found from the table, you can manually input your primer sequences by choosing',
                                p("Select",strong("other")," and enter your primer sequences if you can't  find your primer sequence in this table."),
                                # strong(' other'), span("."),
                                style = "font-size: 16px; font-weight: 300; margin-top: 5px;",
                                dataTableOutput(outputId = "primer_seqs_table_Pacbio"),
                                
                                # style = "left:15px;top: 25px;position: relative"
                              ) %>% shinyjs::hidden()
                              
                              
                              
                              
                              
                              # htmlOutput(outputId = "denoising"),
                              # textOutput("message"),
                              )
                          )),
                 
                 

                 
                 # Taxonomy classification ----
                 tabPanel(span("Step 3. Taxonomy classification", style = tabPanel_navmenu_style),
                          
                          sidebarLayout(
                            fluid = T,
                            column(width = 4, 
                                   wellPanel(
                                      style = "background-color: #317EAC; border: none; border-radius: 5px; color: white;font-size: 16px;",
                                      # p('First, download the data from database'),
                                      uiOutput(outputId = "show_job_id_taxa"),
                                      textInput(inputId = "input_job_id_taxa", label = "Nothing") %>% shinyjs::hidden(),
                                      strong("Database", style = "font-size:24px;color:white"),
                                      p('Select the reference database for taxonomy classification.', style = "font-size:18px;"),
                                      selectInput(inputId = "select_database", 
                                                  # label = span("Choose the database", style = "font-size:20px"),
                                                  label = "",
                                                  choices = c(),
                                                  width = "300px") %>% div(),
                                      actionButton(inputId = "auto_load_db",
                                                   label = "Auto download database") %>% shinyjs::hidden(),
                                      actionButton(inputId = "load_parameter_taxa",
                                                   label = strong("Example", style = "margin: 5px;font-size: 18px"),
                                                   icon = icon("chalkboard-teacher"),
                                                   style = "color:#317EAC;background-color:white;margin-top:10px") %>% div(),
                                      
                                      
                                      
                                      hr(),
                                      
                                      strong("Reference sequence filtering", style = "font-size:24px;color:white"),
                                      br(),br(),
                                      strong("1. Check primers", style = "color: white;font-size: 20px;"),
                                      p("If incorrect, go to 'Step 1.Sequence summary' to select the correct primer.", style = "font-size:18px;"),
                                      
                                      uiOutput(outputId = "check_primer"),
                                      # uiOutput(outputId = "check_r_primer"),
                                      br(),br(),
                                      strong("2. Filter the reference sequence by length", style = "font-size:20px; margin-top:15px"),
                                      # p("Filter the reference sequences based on length.", style = "font-size:18px;"),
                                      textInput(inputId = "min_length", 
                                                label = span("Minimum length",
                                                          style = "font-size:20px;"),
                                                # label = "Give the minimum length to retain",
                                                placeholder = "Input number",
                                                value = 0,
                                                width = "400px"),
                                      # tippy::tippy_this(elementId = "info_min", tooltip = "The default value is minimun length of denoised-sequences", placement = "right"),
                                      # p("Shorter sequenceses are discarded. Set to zero to disable min length filtering.", 
                                      #   style = "font-size: 16px;margin-top:-3px"),
                                      
                                      
                                      textInput(inputId = "max_length", 
                                                label = span("Maximum length",
                                                          style = "font-size:20px;"),
                                                # label = "Give the maximum length to retain",
                                                placeholder = "Input number",
                                                value = 0,
                                                width = "400px"),
                                      # tippy::tippy_this(elementId = "info_max", tooltip = "The default value is maximum length of denoised-sequences", placement = "right"),
                                      # p("Longer sequenceses are discarded. Set to zero to disable max length filtering.",
                                      #   style = "font-size: 14px;margin-top:-3px"),
                                      actionButton("filter_ref_info",
                                                   "learn more",
                                                   icon = icon("question-circle")),
                                      
                                      hr(),
                                      strong("Computing setting", style = "font-size:24px;color:white"),
                                      textInput(inputId = "n_jobs", 
                                                label = "Number of threads MOCHI can use",
                                                value = suggested_cores,
                                                placeholder = "Input number",
                                                width = "300px")
                                      
                                      
                            ) # wellPanel
                            ), # collumn
                            
                            mainPanel(
                              h1("3. Taxonomy classification", 
                                 style = "color: #317EAC;margin-top: 0px;"),
                              h4("(1) Classify taxonomy"),
                              actionButton(inputId = "start_training", 
                                           label = strong("Start!"),
                                           icon = icon("play-circle"),
                                           style = "margin: 10px; display: inline-block;"
                              ),
                              
                              
                              div(
                                id = "taxa_results_view",
                               
                                hr(),
                                h4("(2) Inspect the taxonomy classification result.", 
                                      style = "margin-top: 25px"),
                                tabsetPanel(
                                  type = "tabs",
                                  tabPanel(
                                    title = "Taxonomy result",
                                    br(),br(),
                                    dataTableOutput("taxonomy_classificatio_table"),
                                    downloadButton("taxonomy_classificatio_table_dl")
                                    
                                  ),
                                  tabPanel(
                                    title = "Log",
                                    tableOutput("taxacls_log_table"),
                                    downloadButton("log_file_taxonomy_classification")
                                  )
                                )
                                
                                  # style = "margin-top: 10px;"
                              ) %>% shinyjs::hidden(),
                             
                              
                              
                              div(
                                id = "taxa_results_download",
                                hr(),
                                h4("(3) Download the files for the next step.",
                                   style = "margin-top: 25px"),
                                downloadButton(outputId = "taxatable_download",
                                               label = "The taxonomic table  ",
                                               style = "margin-left: 0px"),
                                
                                downloadButton(outputId = "table_dada2_download",
                                               label = "The ASVs table  "),
                                
                                
                                tippy::tippy_this(elementId = "table_dada2_download",
                                                  tooltip = "<p style='text-align: left;margin:2px'>amplicon sequence variant (ASV) table, a higher-resolution analogue of the traditional OTU table</p>",
                                                  allowHTML = TRUE,
                                                  placement = "bottom"),
                                
                                downloadButton(outputId = "rep_seq_dada2_download",
                                               label = "The seqs data  ")
                              ) %>% shinyjs::hidden(),
                              
                              # br(),br(),
                              # hr(),
                              # h4('Example output for taxonomy classification'),
                              # actionButton(inputId = "view_taxonomy_example",
                              #              label = "Taxonomy result",
                              #              style = "margin-left: 10px",
                              #              onclick = paste0("window.open('http://",
                              #                               "mochi.life.nctu.edu.tw",
                              #                               "/example_files/taxonomy_analysis/data/index.html",
                              #                               "')")
                              # ),
                              

                              # uiOutput(outputId = "mk_taxa"), 
                              # dataTableOutput(outputId = "taxonomy_output"), 
                              width = 8)
                          ) # sidebarLayout
                          ) # tabPanel
                 
      ), # navbarmenu
      
      
      
      
    # Taxonomy Analysis ----
      tabPanel(title = span("Taxonomy Analysis", style = tabPanel_title_style),
               
               icon = icon("chart-bar", class = "chart-bar_icon"),
               tags$style(".chart-bar_icon {color: white}"),
               
               sidebarLayout(
                 fluid = T,
                 
                 sidebarPanel(
                   style = "background-color: #317EAC; border: none; border-radius: 5px; color: white;font-size: 20px;",
                   # strong("Metadata", style = "font-size:20px;color:white"),
                   # span("Upload the metadata (1st column name must be"), strong('#SampleID'),span(")"),
                   fileInput(inputId = "sample_data", 
                             label = p(HTML("<b>Upload the metadata file</b>"),span(shiny::icon("info-circle"),id = "info_metadata")),
                             multiple = F,
                             accept = ".tsv"),
                   tippy::tippy_this(elementId = "info_metadata", 
                                     tooltip = HTML("<p>1st column name must be <b>SampleID</b></p>"), 
                                     placement = "right",
                                     allowHTML =T),
                   
                   fileInput(inputId = "taxonomic_table", 
                             label = p(HTML("<b>Upload the taxonomic table file </b>"),span(shiny::icon("info-circle"), id = "info_taxatable")),
                             multiple = F,
                             accept = ".qza"),
                   tippy::tippy_this(elementId = "info_taxatable", tooltip = "Downloaded from taxonomy classification", placement = "right"),
                   
                   # span("Upload the ASVs table file (Download from Taxonomy Analysis)"),
                   fileInput(inputId = "table_dada2_upload", 
                             label = p(HTML("<b>Upload the ASVs table file </b>"),span(shiny::icon("info-circle"), id = "info_ASVs")),
                             multiple = F,
                             accept = ".qza"),
                   tippy::tippy_this(elementId = "info_ASVs", tooltip = "Downloaded from taxonomy classification", placement = "right"),
                   checkboxInput(inputId = "18S", label = p(HTML("<b>18S rRNA</b>"),span(shiny::icon("info-circle"), id = "18S_check"))),
                   tippy::tippy_this(elementId = "18S_check", tooltip = "Select this checkbox if the sequences are 18S rRNA.", placement = "right"),
                   actionButton(inputId = "TA_start", 
                                label = strong("Start!"), 
                                icon = icon("play-circle")
                   ),
                   actionButton("TA_reset", "reset", icon = icon("trash")),
                   actionButton("TA_example",
                                strong("Example files", style = "margin: 5px;font-size: 18px"),
                                icon = icon("chalkboard-teacher"),
                                style = "color:#317EAC;background-color:white;margin-top:10px") %>% div(),
                   
            
                   width = 3),
                 
                 
                 
                 
                 
                 
                 # Main panel for displaying outputs ----
                 mainPanel(
                   useShinyjs(),
                   shinyBS::bsAlert("sample_data_alert"),
                   shinyBS::bsAlert("taxatable_alert"),
                   shinyBS::bsAlert("seq_alert"),
                   
                   tabsetPanel(type = "tabs",
                               
                               tabPanel(title = 'Taxonomic table',
                                        icon = icon(name = "table"),
                                        
                                        useShinyjs(),
                                        
                                        div(
                                          id = "taxatable_ui",
                                          radioButtons(inputId = "metadata1", 
                                                       label = "Choose the group", 
                                                       choices = " ", 
                                                       inline = T),
                                          withSpinner(
                                            dataTableOutput(outputId = "contents"), 
                                            type = 2, 
                                            color.background = "white"),
                                          downloadButton(outputId = "downloadTaxaTable", 
                                                         label = "Download Taxonomic table"),
                                          style = "margin-top:10px"
                                        ) %>% shinyjs::hidden(),
                                        
                                        
                                        tags$head(
                                          tags$style(HTML("
                                                                      .shiny-output-error-validation {
                                                                        color: red;
                                                                        font-size: 24px;
                                                                        font-weight: 600;
                                                                        margin-top: 40px;
                                                                        }
                                                                    "))
                                        )
                                        # tableOutput(outputId = "taxatable_summary")
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                               ),
                               
                               tabPanel(title = "Taxonomic barplot",
                                        icon = icon(name = "chart-bar"),
                                        
                                        div(
                                          id = "taxabarplot_ui",
                                          withSpinner(
                                            plotlyOutput(outputId = "barplot", 
                                                         height = "600px"), 
                                            type = spinner_type, 
                                            color.background = spinner_bg_color),
                                          
                                          textOutput(outputId="word_metadata_NA_1") %>% shinyjs::hidden(),  
                                          selectInput(inputId = "select_level_bar", 
                                                      label = "Choose the level", 
                                                      choices = c("Kingdom","Phylum","Class","Order","Family","Genus","Species") 
                                          ),
                                          sliderInput(inputId = "integer", 
                                                      label = "Top N:",
                                                      min = 1, 
                                                      max = 30,
                                                      value = 10,
                                                      ticks = F),
                                          radioButtons(inputId = "metadata_barplot", 
                                                       label = "Choose the group",
                                                       choices = " ",
                                                       inline = T),
                                          downloadButton(outputId = "download_barplot", 
                                                         label = "Download barplot")
                                          
                                        ) %>% shinyjs::hidden()
                               ),
                               
                               tabPanel(title = "Taxonomic heatmap", 
                                        icon = icon(name = "th"),
                                        div(
                                          id = "taxaheatmap_ui",
                                          withSpinner(
                                            plotlyOutput(outputId = "crimeplot", 
                                                         height = "600px"), 
                                            type = spinner_type, 
                                            color.background = spinner_bg_color
                                          ),
                                          textOutput(outputId="word_metadata_NA_2"),
                                          selectInput(inputId = "select_level_hm", 
                                                      label = "Choose the level", 
                                                      choices = c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
                                          ),
                                          radioButtons(inputId = "metadata_hm", 
                                                       label = "Choose the group",
                                                       choices = " ",
                                                       inline = T),
                                          downloadButton(outputId = "downloadHMmatrix", 
                                                         label = "Download Heatmap matrix")
                                        ) %>% shinyjs::hidden(),
                                        
                               ),
                               
                               tabPanel(title = "Krona", 
                                        icon = icon(name = "chart-pie"),
                                        
                                        div(
                                          id = "krona_ui",
                                          withSpinner(
                                            uiOutput(outputId = "krona_output"),
                                            type = spinner_type,
                                            color.background = spinner_bg_color
                                          ),
                                          
                                          downloadButton(outputId ="download_krona", 
                                                         label = "Download Krona results (.zip)")
                                        ) %>% shinyjs::hidden(),
                                        
                                        
                               ),
                               
                               
                               tabPanel(title = "Alpha diversity",
                                        icon = icon(name = "info"),
                                        div(
                                          id = "alpha_ui",
                                          h3("Table"),
                                          withSpinner(
                                            dataTableOutput(outputId = "contents2"), 
                                            type = spinner_type, 
                                            color.background = spinner_bg_color),
                                          
                                          downloadButton(outputId = "downloadAlpha", 
                                                         label = "Download Alpha Diversity Table"),
                                          hr(),
                                          h3("Boxplot"),
                                          selectInput(inputId = "select_diversity", 
                                                      label = "Choose the index", 
                                                      choices = c("Richness", "Chao1", "ACE", 
                                                                  "Shannon_diverstiy", "Simpson_diversity", "InvSimpson_diversity",
                                                                  "Shannon_evenness", "Simpson_evenness", "Goods_coverage")),
                                          radioButtons(inputId = "select_stat", 
                                                       label = "Choose the statistic method", 
                                                       choices = c("ANOVA", "Kruskal-Wallis test"), 
                                                       inline = T),
                                          radioButtons(inputId = "metadata_alpha", 
                                                       label = "Choose the group",
                                                       choices = " ",
                                                       inline = T),
                                          
                                          # plotOutput("alpha_boxplot"),
                                          withSpinner(
                                            plotOutput(outputId = "alpha_boxplot"), 
                                            type = spinner_type, 
                                            color.background = spinner_bg_color),
                                          textOutput(outputId="word_metadata_NA_3"),
                                          downloadButton(outputId = "downloadAlphaBoxPlot", 
                                                         label = "Download Alpha Diversity Boxplot"),
                                          div(
                                            id = "post_hoc_ui",
                                            hr(),
                                          h3("Post hoc analysis"),
                                          textOutput(outputId = "post_test_type"),
                                          tags$head(
                                            tags$style("#post_test_type{color: black;
                                                               font-size: 20px;
                                                                       }"
                                            )
                                          ),
                                          withSpinner(
                                            tableOutput(outputId = "post_test"), 
                                            type = spinner_type, 
                                            color.background = spinner_bg_color),
                                          downloadButton(outputId = "Alpha_posttest_DL", 
                                                         label = "Download Alpha Diversity statistical result")
                                          )
                                          ,
                                          br(),br()
                                          
                                        ) %>% shinyjs::hidden(),
                                        
                                        
                                        
                               ),
                               
                               tabPanel(title = "Beta diversity",
                                        icon = icon("info"),
                                        div(
                                          id = "beta_ui",
                                          h3("Beta diversity heatmap (Bray-Curtis)"),
                                          withSpinner(
                                            plotlyOutput(outputId = "beta_dsmx_hm", 
                                                         height = "600px"), 
                                            type = spinner_type, 
                                            color.background = spinner_bg_color),
                                          downloadButton(outputId = "download_beta_dsmx_hm", 
                                                         label = "Download Beta Diversity distance matrix data"),
                                          
                                          hr(),
                                          h3("Dimension reduction plot"),
                                          withSpinner(
                                            plotOutput(outputId = "betaplot", 
                                                       height = "600px"), 
                                            type = spinner_type, 
                                            color.background = spinner_bg_color),
                                          # textOutput(outputId="word_metadata_NA_4"),
                                          radioButtons(inputId = "sep", 
                                                       label = "Choose the method",
                                                       choices = c("PCA","PCoA","NMDS"), 
                                                       inline = T),
                                          checkboxInput(inputId = "beta_cluster", 
                                                        label = "Clustering"),
                                          checkboxInput(inputId = "beta_showID", 
                                                        label = "Show sample ID",value = F),
                                          textOutput(outputId = "NMDS_stress"),
                                          radioButtons(inputId = "metadata_beta", 
                                                       label = "Choose the group",
                                                       choices = " ",
                                                       inline = T),
                                          downloadButton(outputId = "downloadBetaPlot", 
                                                         label = "Download Beta Diversity Plot"),
                                          hr(),
                                          
                                          h3("Statistical analysis"),
                                          column(
                                            width = 4,
                                            textOutput(outputId = "Permanova_title"), 
                                            tags$style(type="text/css", "#Permanova_title {font-size: 15px; font-weight: bold;}"),
                                            #dataTableOutput("permanova_table"),
                                            withSpinner(
                                              tableOutput(outputId = "permanova_table"), 
                                              type = spinner_type, 
                                              color.background = spinner_bg_color),
                                            downloadButton(outputId = "download_permanova", 
                                                           label = "Download PERMANOVA table"),
                                            hr()
                                          ),
                                          
                                          
                                          column(width = 4,
                                                 textOutput(outputId = "ANOSIM_title"), 
                                                 tags$style(type="text/css", "#ANOSIM_title {font-size: 15px; font-weight: bold;}"),
                                                 withSpinner(
                                                   tableOutput(outputId = "ANOSIM_table"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 downloadButton(outputId = "download_ANOSIM", 
                                                                label = "Download ANOSIM table"),
                                                 hr()
                                          ),
                                          
                                          
                                          column(width = 4,
                                                 textOutput(outputId = "MRPP_title"), 
                                                 tags$style(type="text/css", "#MRPP_title {font-size: 15px; font-weight: bold;}"),
                                                 withSpinner(
                                                   tableOutput(outputId = "MRPP_table"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 downloadButton(outputId = "download_MRPP", 
                                                                label = "Download MRPP table"),
                                                 hr()
                                          ),
                                          
                                          hr(),
                                          # h3("Pair"),
                                          # h4("Permanova"),
                                          
                                          column(width = 4, 
                                                 textOutput(outputId = "Permanova_pair_title"), 
                                                 tags$style(type="text/css", "#Permanova_pair_title {font-size: 15px; font-weight: bold;}"), 
                                                 withSpinner(
                                                   tableOutput(outputId = "permanova_pair_table"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 downloadButton(outputId = "download_permanova_pair", 
                                                                label = "Download pairwise PERMANOVA table")
                                          ),
                                          # h4("ANOSIM"),
                                          
                                          column(width = 4, 
                                                 textOutput(outputId = "ANOSIM_pair_title"), 
                                                 tags$style(type="text/css", "#ANOSIM_pair_title {font-size: 15px; font-weight: bold;}"),
                                                 withSpinner(
                                                   tableOutput(outputId = "ANOSIM_pair_table"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 downloadButton(outputId = "download_ANOSIM_pair", 
                                                                label = "Download pairwise ANOSIM table")
                                          ),
                                          # h4("MRPP"),
                                          
                                          column(width = 4, 
                                                 textOutput(outputId = "MRPP_pair_title"), 
                                                 tags$style(type="text/css", "#MRPP_pair_title {font-size: 15px; font-weight: bold;}"),
                                                 withSpinner(
                                                   tableOutput(outputId = "MRPP_pair_table"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 downloadButton(outputId = "download_MRPP_pair", 
                                                                label = "Download pairwise MRPP table")
                                          )
                                        ) %>% shinyjs::hidden(),
                                        
                                        
                               ),
                               
                               
                               tabPanel(title = "Phylogenetic diversity",
                                        icon = icon("code-branch"),
                                        div(
                                          id = "phylo_ui",
                                          strong("Phylogenetic diversity is a measure of diversity that take the genetic distance between species into consideration.",
                                                 style = "font-size: 18px"),
                                          br(),br(),
                                          # strong("The pipeline uses the"),
                                          # a("mafft", href="https://en.wikipedia.org/wiki/MAFFT", target="_blank"),
                                          # strong("program to perform a multiple sequence alignment of the sequences."),
                                          # br(),br(), 
                                          # fileInput(inputId = "table_dada2_upload", 
                                          #           label = "Upload the feature table (Download from Taxonomy Anlysis)",
                                          #           multiple = F,
                                          #           accept = ".qza", 
                                          #           width = "40%"),
                                          fileInput(inputId ="rep_seq_dada2_upload", 
                                                    # label = "Upload the sequences data (Download from Taxonomy Analysis)",
                                                    label = p(HTML("<b >Upload the sequence file </b>"),span(shiny::icon("info-circle"),id = "info_seqs_forPhylo")),
                                                    multiple = F,
                                                    accept = ".qza",
                                                    width = "40%"),
                                          tippy::tippy_this(elementId = "info_seqs_forPhylo", tooltip = "Downloaded from taxonomy classification", placement = "right"),
                                          # div(downloadButton(outputId = "example_rep_seqs",
                                          #                    label = span("Seqs_forPhylo_example.qza"),
                                          #     style = "margin: 5px;color: #317EAC")),
                                          # br(),br(),
                                          textInput(inputId = "sampling_depth", 
                                                    label = "sampling depth"),
                                          actionButton(inputId = "Q_sampling_depth", 
                                                       label = "learn more", 
                                                       icon = icon("question-circle")
                                          ),
                                          
                                          br(),br(),
                                          textInput(inputId = "threads_phylogenetic", 
                                                    label = "Number of threads MOCHI can use", 
                                                    value = suggested_cores),
                                          # p("The number of threads to use for multithreaded process. The default value is all threads-2."),
                                          p("The default value is (number of threads on the system -2)."),
                                          
                                          hr(),
                                          actionButton(inputId = "phylogenetic_tree", 
                                                       label = strong("Start!"),
                                                       icon = icon("play-circle")
                                          ),
                                          actionButton("phylo_example",
                                                       strong("example files", style = "margin: 5px;font-size: 18px"),
                                                       icon = icon("chalkboard-teacher"),
                                                       style = "color:#317EAC;background-color:white;margin-top:10px") %>% div()
                                          , style = "margin-top: 10px;"
                                        ) %>% shinyjs::hidden(),
                                        
                                        
                                        div(
                                          id = "phylo_output_ui",
                                          hr(),
                                          # textOutput(outputId = "word_phylo_tree"),
                                          h3("Faith PD table"),
                                          withSpinner(
                                            dataTableOutput(outputId = "contents4"), 
                                            type = spinner_type, 
                                            color.background = spinner_bg_color
                                          ),
                                          downloadButton(outputId = "download_faithPD_table",
                                                         label = "Download Faith PD table"),
                                          hr(),
                                          
                                          h3("Faith PD boxplot"),
                                          withSpinner(
                                            plotOutput(outputId = "faith_PD_boxplot"),
                                            type = spinner_type, 
                                            color.background = spinner_bg_color
                                          ),
                                          
                                          radioButtons(inputId = "select_stat_phylo", 
                                                       label = "Choose the statistic method", 
                                                       choices = c("ANOVA", "Kruskal-Wallis test"), 
                                                       inline = T),
                                          radioButtons(inputId = "metadata_phylo_alpha", 
                                                       label = "Choose the group", 
                                                       choices = " ",
                                                       inline = T),
                                          downloadButton(outputId = "download_faithPD_boxplot", 
                                                         label = "Download Faith PD boxplot"),
                                          div(
                                            id = "post_hoc_ui_phylo",
                                            hr(),
                                          h3("Post hoc analysis"),
                                          textOutput(outputId = "post_test_type_phylo"),
                                          tags$head(tags$style("#post_test_type_phylo{color: black;
                                                               font-size: 20px;
                                                                                     }"
                                          )
                                          ),
                                          
                                          withSpinner(
                                            tableOutput(outputId = "post_test_phylo"),
                                            type = spinner_type, 
                                            color.background = spinner_bg_color
                                          ),
                                          
                                          downloadButton(outputId = "download_faithPD_posttest", 
                                                         label = "Download Faith PD post hoc result")
                                          )
                                          ,
                                          hr(),
                                          
                                          h3("Heatmap of UniFrac distance"),
                                          withSpinner(
                                            plotlyOutput(outputId = "unif_dm_hm", 
                                                         height = "600px"),
                                            type = spinner_type, 
                                            color.background = spinner_bg_color
                                          ),
                                          radioButtons(inputId = "UnW_or_W", 
                                                       label = "Choose the method",
                                                       choices = c("Unweighted", "Weighted"), 
                                                       inline = T),
                                          downloadButton(outputId = "download_unif_dm", 
                                                         label = "Download UniFrac distance matrix"),
                                          hr(),
                                          
                                          h3("Dimension reduction plot of UniFrac distance"),
                                          withSpinner(
                                            plotOutput(outputId = "unW_unif_ordination", 
                                                       height = "600px"),
                                            type = spinner_type, 
                                            color.background = spinner_bg_color
                                          ),
                                          radioButtons(inputId = "UnW_or_W_phylo", 
                                                       label = "Choose the method",
                                                       choices = c("Unweighted", "Weighted"), 
                                                       inline = T),
                                          radioButtons(inputId = "ordination_phylo", 
                                                       label = "Choose the ordination method",
                                                       choices = c("PCoA", "NMDS"), 
                                                       inline = T),
                                          checkboxInput(inputId = "phylo_cluster", 
                                                        label = "Clustering"),
                                          checkboxInput(inputId = "phylo_showID", 
                                                        label = "Show sample ID",value = F),
                                          radioButtons(inputId = "metadata_phylo_beta", 
                                                       label = "Choose the group", 
                                                       choices = " ", 
                                                       inline = T),
                                          downloadButton(outputId = "download_unif_plot", 
                                                         label = "Download UniFrac plot"),
                                          
                                          hr(),
                                          
                                          h3("Statistical analysis"),
                                          column(
                                            width = 4,
                                            textOutput(outputId = "Permanova_title_phylo"), 
                                            tags$style(type="text/css", "#Permanova_title_phylo {font-size: 15px; font-weight: bold;}"),
                                            #dataTableOutput("permanova_table"),
                                            withSpinner(
                                              tableOutput(outputId = "permanova_table_phylo"), 
                                              type = spinner_type, 
                                              color.background = spinner_bg_color),
                                            downloadButton(outputId = "download_permanova_phylo", 
                                                           label = "Download PERMANOVA table"),
                                            hr()
                                          ),
                                          
                                          
                                          column(width = 4,
                                                 textOutput(outputId = "ANOSIM_title_phylo"), 
                                                 tags$style(type="text/css", "#ANOSIM_title_phylo {font-size: 15px; font-weight: bold;}"),
                                                 withSpinner(
                                                   tableOutput(outputId = "ANOSIM_table_phylo"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 downloadButton(outputId = "download_ANOSIM_phylo", 
                                                                label = "Download ANOSIM table"),
                                                 hr()
                                          ),
                                          
                                          
                                          column(width = 4,
                                                 textOutput(outputId = "MRPP_title_phylo"), 
                                                 tags$style(type="text/css", "#MRPP_title_phylo {font-size: 15px; font-weight: bold;}"),
                                                 withSpinner(
                                                   tableOutput(outputId = "MRPP_table_phylo"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 downloadButton(outputId = "download_MRPP_phylo", 
                                                                label = "Download MRPP table"),
                                                 hr()
                                          ),
                                          
                                          hr(),
                                          # h3("Pair"),
                                          # h4("Permanova"),
                                          
                                          column(width = 4, 
                                                 textOutput(outputId = "Permanova_pair_title_phylo"), 
                                                 tags$style(type="text/css", "#Permanova_pair_title_phylo {font-size: 15px; font-weight: bold;}"), 
                                                 withSpinner(
                                                   tableOutput(outputId = "permanova_pair_table_phylo"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 downloadButton(outputId = "download_permanova_pair_phylo", 
                                                                label = "Download pairwise PERMANOVA table")
                                          ),
                                          # h4("ANOSIM"),
                                          
                                          column(width = 4, 
                                                 textOutput(outputId = "ANOSIM_pair_title_phylo"), 
                                                 tags$style(type="text/css", "#ANOSIM_pair_title_phylo {font-size: 15px; font-weight: bold;}"),
                                                 withSpinner(
                                                   tableOutput(outputId = "ANOSIM_pair_table_phylo"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 downloadButton(outputId = "download_ANOSIM_pair_phylo", 
                                                                label = "Download pairwise ANOSIM table")
                                          ),
                                          # h4("MRPP"),
                                          
                                          column(width = 4, 
                                                 textOutput(outputId = "MRPP_pair_title_phylo"), 
                                                 tags$style(type="text/css", "#MRPP_pair_title_phylo {font-size: 15px; font-weight: bold;}"),
                                                 withSpinner(
                                                   tableOutput(outputId = "MRPP_pair_table_phylo"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 downloadButton(outputId = "download_MRPP_pair_phylo", 
                                                                label = "Download pairwise MRPP table")
                                          )
                                          
                                        ) %>% shinyjs::hidden(),
                                        
                                        
                               ),
                               
                               tabPanel(title = "ANCOM",
                                        icon = icon("window-maximize"),
                                        div(
                                          id = "ancom_ui",
                                          strong("ANCOM (Analysis of composition of microbiomes) is used for comparing the composition of microbiomes in two or more populations.",
                                                 style = "font-size: 18px"),
                                          br(),br(),
                                          radioButtons(inputId = "metadata_ANCOM", 
                                                       label = "Select an attribute comparison.", 
                                                       choices = " ", 
                                                       inline = T),
                                          selectInput(inputId = "ANCOM_level", 
                                                      label = "Choose the level", 
                                                      choices = c("Phylum","Class","Order","Family","Genus","Species") 
                                          ),
                                          actionButton(inputId = "ANCOM_start", 
                                                       label = strong("Start!"),
                                                       icon = icon("play-circle")
                                                       ),
                                          # actionButton(inputId = "load_parameter_ANCOM",
                                          #              label = strong("Demo", style = "margin: 5px;font-size: 18px"),
                                          #              icon = icon("chalkboard-teacher"),
                                          #              style = "color:#317EAC;background-color:white;margin-top:10px") %>% div(),
                                          
                                          # textOutput(outputId = "word_ANCOM"),
                                          hr(),
                                          style = "margin-top:10px"
                                        ) %>% shinyjs::hidden(),
                                        
                                        div(
                                          id = "ancom_output_ui",
                                          uiOutput(outputId = "word_ancom_plotly"),
                                          withSpinner(
                                            plotlyOutput(outputId = "ancom_plotly"), 
                                            type = spinner_type, 
                                            color.background = spinner_bg_color
                                          ),
                                          
                                          uiOutput(outputId = "annotation_ancom"),
                                          hr(),
                                          uiOutput(outputId = "word_ancom_table"),
                                          
                                          withSpinner(
                                            dataTableOutput(outputId = "ancom_sig"),
                                            type = spinner_type, 
                                            color.background = spinner_bg_color
                                          ),
                                          downloadButton(outputId = "ancom_table_download",
                                                         label = "Download the ANCOM result table (Contain all species)")
                                          
                                          ) %>% shinyjs::hidden()
                                          

                                        
                                        
                               )
                               
                   )
                   , width = 9)
               )
               
      ),
      
      
      
    # Function Analysis -------------
      tabPanel(title = span("Function Analysis", style = tabPanel_title_style),
               
               icon = icon("project-diagram", class = "caret_icon"),
               tags$style(".caret_icon {color: white}"),
               
               sidebarLayout(
                 fluid = T,
                 sidebarPanel(
                   style = "background-color: #317EAC; border: none; border-radius: 5px; color: white;font-size: 20px;",

                   
                   fileInput(inputId = "sample_data_FA", 
                             label = p(HTML("<b>Upload the metadata file</b>"),
                                       span(shiny::icon("info-circle"),
                                            id = "info_metadata_FA")),
                             multiple = F,
                             accept = ".tsv"),
                   tippy::tippy_this(elementId = "info_metadata_FA", 
                                     tooltip = HTML("<p>1st column name must be <b>SampleID</b></p>"), 
                                     placement = "right",
                                     allowHTML = TRUE),
                   
                   fileInput(inputId = "taxonomic_table_FA", 
                             label = p(HTML("<b>Upload the taxonomic table file </b>"),
                                       span(shiny::icon("info-circle"),
                                            id = "info_taxatable_FA")),
                             multiple = F,
                             accept = ".qza"),
                   tippy::tippy_this(elementId = "info_taxatable_FA", tooltip = "Downloaded from Taxonomy classification", placement = "right"),
                   
                   actionButton(inputId = "function_analysis", 
                                label = strong("Start!"), 
                                icon = icon("play-circle")
                                ),
                   
                   actionButton("FA_reset", "reset", icon = icon("trash")),
                   
                   actionButton(inputId = "function_info",
                                label = "learn more",
                                icon = icon("question-circle"),
                                style = "margin-top:10px") %>% div(),
                   
                   actionButton("FA_example",
                                strong("Example files", style = "margin: 5px;font-size: 18px"),
                                icon = icon("chalkboard-teacher"),
                                style = "color:#317EAC;background-color:white;margin-top:10px") %>% div(),
                   width = 3),
                 
                 mainPanel(
                   useShinyjs(),
                   shinyBS::bsAlert("sample_data_alert_FA"),
                   shinyBS::bsAlert("taxatable_alert_FA"),
                   tabsetPanel(type="tabs",
                               tabPanel(title = "Function annotation table",
                                        icon = icon("table"),
                                        div(
                                          id = "func_table_ui",
                                          h3("Summary"),
                                          uiOutput(outputId = "function_report"),
                                          hr(),
                                          h3("Function annotation table"),
                                          withSpinner(
                                            dataTableOutput(outputId = "func_table_BY_sampleid",
                                                            height = "auto"), 
                                            type = spinner_type, 
                                            color.background = spinner_bg_color
                                          ),
                                          downloadButton(outputId = "func_table_ID", 
                                                         label = "Download the function annotation table")
                                        ) %>% shinyjs::hidden()
                               ),
                               # tabPanel(title = "Function Tale (by Species Name)", 
                               #          icon = icon("table"),
                               #          textOutput(outputId = "word_FA2"),
                               #          dataTableOutput(outputId = "func_table_BY_speciesname"),
                               #          downloadButton(outputId = "func_table_Sp", 
                               #                         label = "Download")),
                               tabPanel(title = "Function barplot",
                                        icon = icon("th"),
                                        div(
                                          id="func_barplot_ui",
                                        h3("Function barplot"),
                                        withSpinner(
                                          plotlyOutput(outputId = "Function_barplot", 
                                                   height = "800px"), 
                                          type = spinner_type, 
                                          color.background = spinner_bg_color
                                        ),
                                        radioButtons(inputId = "metadata_FA", 
                                                     label = "Choose the group", 
                                                     choices = " ",
                                                     inline = T),
                                        downloadButton(outputId = "FA_plot_download",
                                                       label = "Download the function barplot")
                                        ) %>% shinyjs::hidden()
                               )
                   ), width = 9)
               )),
      
     # Tutorial--------------
      tabPanel(title = span("Tutorial", style = tabPanel_title_style),
               
               icon = icon("question-circle", class = "nav_qc_icon"),
               tags$style(".nav_qc_icon {color: white}"),
               
               mainPanel(
                 width = 12,

                 tags$iframe(style="height: 800px; width:90%; scrolling=yes;margin:0 100px", id = "ch2",
                             src=paste0("https://mochi.life.nctu.edu.tw/MOCHI_Tutorial_Ch_2_Web_new.pdf")

                 )
                 
               )
               
               
      ),
      
      
    
    # Download local version ----
    tabPanel(
      title = span("MOCHI Installation", style = tabPanel_title_style),
      icon = icon("cloud-download-alt", class = "local_icon"),
      tags$style(".local_icon {color: white}"),
      # sidebarLayout(position = "left",
      #               fluid = T,
      #   sidebarPanel(
      #     width = 3,
      #     style = "background-color: #317EAC; border: none; border-radius: 5px; color: white;width:250px;",
      #     h2("Download"),
      #     downloadButton(outputId = "download_yml", 
      #                    label = "docker-compose.yml")
      #   ),
        mainPanel(
          width = 12,
          div(
            strong("MOCHI Installation", style = "font-size:24px;color:white;margin-bottom:10px"),
            p("We provide ", strong("MOCHI local version"),"to let users use MOCHI on their own server.", style = "color: white;font-size:16px",
              " Follow the below steps to install the local version."),
            # p("Download docker-compose.yml and follow the installation tutorial.", style = "color: white"),
            downloadButton(outputId = "download_yml", 
                           label = "docker-compose.yml"),
            style = "background-color: #317EAC; border: none; border-radius: 5px; width: 650px;position: relative;padding:20px;margin-left:100px"
          ),
          
          br(),br(),
          # strong("Installation tutorial", style = "font-size:24px;"),
          tags$iframe(style="height:800px; width:90%; scrolling=yes;margin:0 100px", id = "ch1",
                      src="https://mochi.life.nctu.edu.tw/MOCHI_Tutorial_Local.pdf")
                      # src="http://140.113.83.24:3811/test.pdf") %>% span()
          
        )
      # )
      
    ),
      # user results ----
      # tabPanel(title = span("User Results", style = tabPanel_title_style),
      #          icon = icon("user", class = "user_icon"),
      #          tags$style(".user_icon {color: white}"),
      #          
      #          sidebarLayout(
      #            fluid = T,
      #           column(width = 4,
      #             # wellPanel(
      #              style = "position:relative;background-color: #317EAC; border: none; border-radius: 5px; color: white;font-size: 20px;width:500px;padding:10px",
      #              # strong("Inspect your results", style = "font-size:20px;color:white"),
      #              strong("Show the results", style = "font-size:24px;color:white;top:20px"),
      #              p("Your results will be deleted after two weeks."),
      #              textInput(inputId = "job_id",
      #                        label = strong("Job ID", style = "font-size:20px;color:white"),
      #                        placeholder = "Input your job id",
      #                        width = "200px"),
      #              actionButton(inputId = "submit_id",
      #                           label = "Submit",
      #                           icon = icon("chevron-circle-up"))
      #            # )
      #            ),
      #            mainPanel(
      #              # width = 7,
      #              # style = "position: relative;",
      #              column(
      #                width = 12,
      #                # wellPanel(
      #                  style = "background-color: white;",
      #                  uiOutput(outputId = "users_results_download",inline = T) %>% shinyjs::hidden()
      #                # )
      #                
      #              )
      #              
      #                
      #                  
      #                               
      #            )
      #          )
      #          ),
    
    # Demo ----
    tabPanel(title = span("Demo", style = tabPanel_title_style),
             icon = icon("arrow-alt-circle-right", class = "arrow-alt-circle-right_icon"),
             tags$style(".arrow-alt-circle-right_icon {color: white}"),
             
             sidebarLayout(
               fluid = T,
               column(width = 4,
                      # wellPanel(
                      style = "position:relative;background-color: #317EAC; border: none; border-radius: 5px; color: white;font-size: 20px;width:500px;padding:10px",
                      # strong("Inspect your results", style = "font-size:20px;color:white"),
                      strong("Demo", style = "font-size:24px;color:white;top:20px"),
                      selectInput(inputId = "select_dataset", 
                                  label = "Select a dataset", 
                                  choices = c("Single end", "Paired end", "Long read") 
                      ),
                      selectInput(inputId = "select_module", 
                                  label = "Select a module", 
                                  choices = c("Sequence preprocessing", "Taxonomy analysis", "Function analysis") 
                      ),
                      selectInput(inputId = "select_module_step", 
                                  label = "Select a step", 
                                  choices = c("Step 1. Sequence summary", "Step 2. Sequence denoising", "Step 3. Taxonomy classification"), 
                                  width = "350px"
                      ) %>% shinyjs::hidden()
                      
               ),
               mainPanel(
                 # single end
                 conditionalPanel(
                   #single1
                   condition = "input.select_dataset == 'Single end' & input.select_module == 'Sequence preprocessing' & input.select_module_step == 'Step 1. Sequence summary'",
                   
                   column(width = 12,
                          h1("1. Sequence summary (for single end)"),
                          
                          div(
                            id = "demux_single_demo",
                            hr(),
                            tabsetPanel(
                              type = "tabs",
                              tabPanel(
                                title = span("Sequence counts summary",
                                             style = "color:#317EAC"),
                                h4("Forward reads"),
                                tableOutput("demux_table_single_demo"),
                                plotlyOutput("demux_table_boxplot_single_demo") %>% withSpinner(type = 2, color.background = "white"),
                                downloadButton("demux_table_single_demo_dl", 
                                               "Download the read couts table")
                              ),
                              
                              tabPanel(
                                title = span("Quality plot",
                                             style = "color:#317EAC"),
                                h4("Sequence length summary"),
                                tableOutput("demux_Q_seven_table_single_demo"),
                                plotlyOutput("demux_Q_plot_single_demo") %>% withSpinner(type = 2, color.background = "white"),
                                downloadButton("demux_Q_table_single_demo_dl", 
                                               "Download the quality score table")
                              ),
                              tabPanel(
                                title = "Log",
                                tableOutput("demux_parameter_table_single_demo"),
                                downloadButton("demux_parameter_table_single_demo_dl")
                              )
                            )
                            
                            
                          )
                   )
                 ),
                 conditionalPanel(
                   #single2
                   condition = "input.select_dataset == 'Single end' & input.select_module == 'Sequence preprocessing' & input.select_module_step == 'Step 2. Sequence denoising'",
                   
                   column(width = 12,
                          h1('2. Sequence denoising (DADA2 1.18.0) for Single end'),
                          div(
                            id = "dada2_results_single_demo",
                            hr(),
                            tabsetPanel(
                              type = "tabs",
                              
                              tabPanel(
                                title = "Summary",
                                # br(),br(),
                                h4("Sample read count summary"),
                                tableOutput("dada2_sample_summary_single_demo"),
                                h4("Sample summary table"),
                                dataTableOutput("dada2_sample_table_single_demo"),
                                downloadButton("dada2_sample_table_single_demo_dl"),
                                br(),br(),
                                h4("ASV read count summary"),
                                tableOutput("dada2_asv_summary_table_single_demo"),
                                h4("ASV summary table"),
                                dataTableOutput("dada2_asv_table_single_demo"),
                                downloadButton("dada2_asv_table_single_demo_dl")
                              ),
                              tabPanel(
                                title = "Filter info",
                                br(),br(),
                                dataTableOutput("dada2_filter_table_single_demo"),
                                downloadButton("dada2_filter_table_single_demo_dl")
                              ),
                              tabPanel(
                                title = "Sequence info",
                                h4("Sequence Length Statistics"),
                                tableOutput("dada2_seqs_info_single_1_demo"),
                                h4("Seven-Number Summary of Sequence Lengths"),
                                tableOutput("dada2_seqs_info_single_2_demo"),
                                h4("Sequence table"),
                                dataTableOutput("dada2_seqs_table_single_demo"),
                                downloadButton("dada2_seqs_table_single_demo_dl", "Download the sequences as a FASTA file")
                              ),
                              tabPanel(
                                title = "Rarefaction plot",
                                br(),br(),
                                plotOutput("rarefaction_plot_single_demo", height = "500px"),
                                downloadButton("rarefaction_plot_single_demo_dl", "Download the rarefaction plot"),
                                downloadButton("rarefaction_table_single_demo_dl", "Download the rarefaction table")
                              ),
                              tabPanel(
                                title = "Table",
                                br(),br(),
                                dataTableOutput("dada2_table_single_demo"),
                                downloadButton("dada2_table_single_demo_dl")
                              ),
                              tabPanel(
                                title = "Log",
                                tableOutput("dada2_log_table_single_demo"),
                                downloadButton("dada2_log_table_single_demo_dl")
                              )
                            )
                          )
                   )
                 ),
                 conditionalPanel(
                   #single3
                   condition = "input.select_module == 'Sequence preprocessing' & input.select_module_step == 'Step 3. Taxonomy classification'",
                   
                   column(width = 12,
                          
                          h1("3. Taxonomy classification", 
                             style = "color: #317EAC;margin-top: 0px;"),
                          
                          div(
                            id = "taxa_results_view_demo",
                            
                            hr(),
                            h4("Inspect the taxonomy classification result.", 
                               style = "margin-top: 25px"),
                            tabsetPanel(
                              type = "tabs",
                              tabPanel(
                                title = "Taxonomy result",
                                br(),br(),
                                dataTableOutput("taxonomy_classificatio_table_demo"),
                                downloadButton("taxonomy_classificatio_table_demo_dl")
                                
                              ),
                              tabPanel(
                                title = "Log",
                                tableOutput("taxacls_log_table_demo"),
                                downloadButton("log_file_taxonomy_classification_demo")
                              )
                            )
                            
                           
                          ),
                          
                          
                          
                          div(
                            id = "taxa_results_download_demo",
                            hr(),
                            h4("(3) Download the files for the next step.",
                               style = "margin-top: 25px"),
                            downloadButton(outputId = "taxatable_download_demo",
                                           label = "The taxonomic table  ",
                                           style = "margin-left: 0px"),
                            
                            downloadButton(outputId = "table_dada2_download_demo",
                                           label = "The ASVs table  "),
                            
                            
                            tippy::tippy_this(elementId = "table_dada2_download_demo",
                                              tooltip = "<p style='text-align: left;margin:2px'>amplicon sequence variant (ASV) table, a higher-resolution analogue of the traditional OTU table</p>",
                                              allowHTML = TRUE,
                                              placement = "bottom"),
                            
                            downloadButton(outputId = "rep_seq_dada2_download_demo",
                                           label = "The seqs data  ")
                          )
                   )
                 ),
                 
                 # Taxonomy analysis demo
                 conditionalPanel(
                   
                   condition = "input.select_module == 'Taxonomy analysis'",
                   
                   column(width = 12,
                          tabsetPanel(type = "tabs",
                                      
                                      tabPanel(title = 'Taxonomic table',
                                               icon = icon(name = "table"),
                                               
                                               useShinyjs(),
                                               
                                               div(
                                                 id = "taxatable_ui_demo",
                                                 radioButtons(inputId = "metadata1_demo", 
                                                              label = "Choose the group", 
                                                              choices = " ", 
                                                              inline = T),
                                                 withSpinner(
                                                   dataTableOutput(outputId = "contents_demo"), 
                                                   type = 2, 
                                                   color.background = "white"),
                                                 downloadButton(outputId = "downloadTaxaTable_demo", 
                                                                label = "Download Taxonomic table"),
                                                 style = "margin-top:10px"
                                               ),
                                               
                                               
                                               tags$head(
                                                 tags$style(HTML("
                                                                      .shiny-output-error-validation {
                                                                        color: red;
                                                                        font-size: 24px;
                                                                        font-weight: 600;
                                                                        margin-top: 40px;
                                                                        }
                                                                    "))
                                               )
                                               # tableOutput(outputId = "taxatable_summary")
                                               
                                               
                                               
                                               
                                               
                                               
                                               
                                      ),
                                      
                                      tabPanel(title = "Taxonomic barplot",
                                               icon = icon(name = "chart-bar"),
                                               
                                               div(
                                                 id = "taxabarplot_ui_demo",
                                                 withSpinner(
                                                   plotlyOutput(outputId = "barplot_demo", 
                                                                height = "600px"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 
                                                 textOutput(outputId="word_metadata_NA_1_demo") %>% shinyjs::hidden(),  
                                                 selectInput(inputId = "select_level_bar_demo", 
                                                             label = "Choose the level", 
                                                             choices = c("Kingdom","Phylum","Class","Order","Family","Genus","Species") 
                                                 ),
                                                 sliderInput(inputId = "integer_demo", 
                                                             label = "Top N:",
                                                             min = 1, 
                                                             max = 30,
                                                             value = 10,
                                                             ticks = F),
                                                 radioButtons(inputId = "metadata_barplot_demo", 
                                                              label = "Choose the group",
                                                              choices = " ",
                                                              inline = T),
                                                 downloadButton(outputId = "download_barplot_demo", 
                                                                label = "Download barplot")
                                                 
                                               )
                                      ),
                                      
                                      tabPanel(title = "Taxonomic heatmap", 
                                               icon = icon(name = "th"),
                                               div(
                                                 id = "taxaheatmap_ui_demo",
                                                 withSpinner(
                                                   plotlyOutput(outputId = "crimeplot_demo", 
                                                                height = "600px"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color
                                                 ),
                                                 textOutput(outputId="word_metadata_NA_2_demo"),
                                                 selectInput(inputId = "select_level_hm_demo", 
                                                             label = "Choose the level", 
                                                             choices = c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
                                                 ),
                                                 radioButtons(inputId = "metadata_hm_demo", 
                                                              label = "Choose the group",
                                                              choices = " ",
                                                              inline = T),
                                                 downloadButton(outputId = "downloadHMmatrix_demo", 
                                                                label = "Download Heatmap matrix")
                                               ),
                                               
                                      ),
                                      
                                      tabPanel(title = "Krona", 
                                               icon = icon(name = "chart-pie"),
                                               
                                               div(
                                                 id = "krona_ui_demo",
                                                 withSpinner(
                                                   uiOutput(outputId = "krona_output_demo"),
                                                   type = spinner_type,
                                                   color.background = spinner_bg_color
                                                 ),
                                                 
                                                 downloadButton(outputId ="download_krona_demo", 
                                                                label = "Download Krona results (.zip)")
                                               ),
                                               
                                               
                                      ),
                                      
                                      
                                      tabPanel(title = "Alpha diversity",
                                               icon = icon(name = "info"),
                                               div(
                                                 id = "alpha_ui_demo",
                                                 h3("Table"),
                                                 withSpinner(
                                                   dataTableOutput(outputId = "contents2_demo"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 
                                                 downloadButton(outputId = "downloadAlpha_demo", 
                                                                label = "Download Alpha Diversity Table"),
                                                 hr(),
                                                 h3("Boxplot"),
                                                 selectInput(inputId = "select_diversity_demo", 
                                                             label = "Choose the index", 
                                                             choices = c("Richness", "Chao1", "ACE", 
                                                                         "Shannon_diverstiy", "Simpson_diversity", "InvSimpson_diversity",
                                                                         "Shannon_evenness", "Simpson_evenness", "Goods_coverage")),
                                                 radioButtons(inputId = "select_stat_demo", 
                                                              label = "Choose the statistic method", 
                                                              choices = c("ANOVA", "Kruskal-Wallis test"), 
                                                              inline = T),
                                                 radioButtons(inputId = "metadata_alpha_demo", 
                                                              label = "Choose the group",
                                                              choices = " ",
                                                              inline = T),
                                                 
                                                 # plotOutput("alpha_boxplot"),
                                                 withSpinner(
                                                   plotOutput(outputId = "alpha_boxplot_demo"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 textOutput(outputId="word_metadata_NA_3_demo"),
                                                 downloadButton(outputId = "downloadAlphaBoxPlot_demo", 
                                                                label = "Download Alpha Diversity Boxplot"),
                                                 div(
                                                   id = "post_hoc_ui_demo",
                                                   hr(),
                                                 h3("Post hoc analysis"),
                                                 textOutput(outputId = "post_test_type_demo"),
                                                 tags$head(
                                                   tags$style("#post_test_type_demo{color: black;
                                                               font-size: 20px;
                                                                       }"
                                                   )
                                                 ),
                                                 withSpinner(
                                                   tableOutput(outputId = "post_test_demo"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 downloadButton(outputId = "Alpha_posttest_DL_demo", 
                                                                label = "Download Alpha Diversity statistical result")
                                                 )
                                                 ,
                                                 br(),br()
                                                 
                                               ),
                                               
                                               
                                               
                                      ),
                                      
                                      tabPanel(title = "Beta diversity",
                                               icon = icon("info"),
                                               div(
                                                 id = "beta_ui",
                                                 h3("Beta diversity heatmap (Bray-Curtis)"),
                                                 withSpinner(
                                                   plotlyOutput(outputId = "beta_dsmx_hm_demo", 
                                                                height = "600px"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 downloadButton(outputId = "download_beta_dsmx_hm_demo", 
                                                                label = "Download Beta Diversity distance matrix data"),
                                                 
                                                 hr(),
                                                 h3("Dimension reduction plot"),
                                                 withSpinner(
                                                   plotOutput(outputId = "betaplot_demo", 
                                                              height = "600px"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color),
                                                 # textOutput(outputId="word_metadata_NA_4"),
                                                 radioButtons(inputId = "sep_demo", 
                                                              label = "Choose the method",
                                                              choices = c("PCA","PCoA","NMDS"), 
                                                              inline = T),
                                                 checkboxInput(inputId = "beta_cluster_demo", 
                                                               label = "Clustering"),
                                                 checkboxInput(inputId = "beta_showID_demo", 
                                                               label = "Show sample ID",value = F),
                                                 textOutput(outputId = "NMDS_stress_demo"),
                                                 radioButtons(inputId = "metadata_beta_demo", 
                                                              label = "Choose the group",
                                                              choices = " ",
                                                              inline = T),
                                                 downloadButton(outputId = "downloadBetaPlot_demo", 
                                                                label = "Download Beta Diversity Plot"),
                                                 hr(),
                                                 
                                                 h3("Statistical analysis"),
                                                 column(
                                                   width = 4,
                                                   textOutput(outputId = "Permanova_title_demo"), 
                                                   tags$style(type="text/css", "#Permanova_title_demo {font-size: 15px; font-weight: bold;}"),
                                                   #dataTableOutput("permanova_table"),
                                                   withSpinner(
                                                     tableOutput(outputId = "permanova_table_demo"), 
                                                     type = spinner_type, 
                                                     color.background = spinner_bg_color),
                                                   downloadButton(outputId = "download_permanova_demo", 
                                                                  label = "Download PERMANOVA table"),
                                                   hr()
                                                 ),
                                                 
                                                 
                                                 column(width = 4,
                                                        textOutput(outputId = "ANOSIM_title_demo"), 
                                                        tags$style(type="text/css", "#ANOSIM_title_demo {font-size: 15px; font-weight: bold;}"),
                                                        withSpinner(
                                                          tableOutput(outputId = "ANOSIM_table_demo"), 
                                                          type = spinner_type, 
                                                          color.background = spinner_bg_color),
                                                        downloadButton(outputId = "download_ANOSIM_demo", 
                                                                       label = "Download ANOSIM table"),
                                                        hr()
                                                 ),
                                                 
                                                 
                                                 column(width = 4,
                                                        textOutput(outputId = "MRPP_title_demo"), 
                                                        tags$style(type="text/css", "#MRPP_title_demo {font-size: 15px; font-weight: bold;}"),
                                                        withSpinner(
                                                          tableOutput(outputId = "MRPP_table_demo"), 
                                                          type = spinner_type, 
                                                          color.background = spinner_bg_color),
                                                        downloadButton(outputId = "download_MRPP_demo", 
                                                                       label = "Download MRPP table"),
                                                        hr()
                                                 ),
                                                 
                                                 hr(),
                                                 # h3("Pair"),
                                                 # h4("Permanova"),
                                                 
                                                 column(width = 4, 
                                                        textOutput(outputId = "Permanova_pair_title_demo"), 
                                                        tags$style(type="text/css", "#Permanova_pair_title_demo {font-size: 15px; font-weight: bold;}"), 
                                                        withSpinner(
                                                          tableOutput(outputId = "permanova_pair_table_demo"), 
                                                          type = spinner_type, 
                                                          color.background = spinner_bg_color),
                                                        downloadButton(outputId = "download_permanova_pair_demo", 
                                                                       label = "Download pairwise PERMANOVA table")
                                                 ),
                                                 # h4("ANOSIM"),
                                                 
                                                 column(width = 4, 
                                                        textOutput(outputId = "ANOSIM_pair_title_demo"), 
                                                        tags$style(type="text/css", "#ANOSIM_pair_title_demo {font-size: 15px; font-weight: bold;}"),
                                                        withSpinner(
                                                          tableOutput(outputId = "ANOSIM_pair_table_demo"), 
                                                          type = spinner_type, 
                                                          color.background = spinner_bg_color),
                                                        downloadButton(outputId = "download_ANOSIM_pair_demo", 
                                                                       label = "Download pairwise ANOSIM table")
                                                 ),
                                                 # h4("MRPP"),
                                                 
                                                 column(width = 4, 
                                                        textOutput(outputId = "MRPP_pair_title_demo"), 
                                                        tags$style(type="text/css", "#MRPP_pair_title_demo {font-size: 15px; font-weight: bold;}"),
                                                        withSpinner(
                                                          tableOutput(outputId = "MRPP_pair_table_demo"), 
                                                          type = spinner_type, 
                                                          color.background = spinner_bg_color),
                                                        downloadButton(outputId = "download_MRPP_pair_demo", 
                                                                       label = "Download pairwise MRPP table")
                                                 )
                                               ),
                                               
                                               
                                      ),
                                      
                                      
                                      tabPanel(title = "Phylogenetic diversity",
                                               icon = icon("code-branch"),
                                               
                                              div(
                                                 id = "phylo_output_ui_demo",
                                                 hr(),
                                                 # textOutput(outputId = "word_phylo_tree"),
                                                 h3("Faith PD table"),
                                                 withSpinner(
                                                   dataTableOutput(outputId = "contents4_demo"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color
                                                 ),
                                                 downloadButton(outputId = "download_faithPD_table_demo",
                                                                label = "Download Faith PD table"),
                                                 hr(),
                                                 
                                                 h3("Faith PD boxplot"),
                                                 withSpinner(
                                                   plotOutput(outputId = "faith_PD_boxplot_demo"),
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color
                                                 ),
                                                 
                                                 radioButtons(inputId = "select_stat_phylo_demo", 
                                                              label = "Choose the statistic method", 
                                                              choices = c("ANOVA", "Kruskal-Wallis test"), 
                                                              inline = T),
                                                 radioButtons(inputId = "metadata_phylo_alpha_demo", 
                                                              label = "Choose the group", 
                                                              choices = " ",
                                                              inline = T),
                                                 downloadButton(outputId = "download_faithPD_boxplot_demo", 
                                                                label = "Download Faith PD boxplot"),
                                                 div(
                                                   id = "post_hoc_ui_phylo_demo",
                                                   hr(),
                                                   h3("Post hoc analysis"),
                                                   textOutput(outputId = "post_test_type_phylo_demo"),
                                                   tags$head(tags$style("#post_test_type_phylo_demo{color: black;
                                                               font-size: 20px;
                                                                                     }"
                                                   )
                                                   ),
                                                   
                                                   withSpinner(
                                                     tableOutput(outputId = "post_test_phylo_demo"),
                                                     type = spinner_type, 
                                                     color.background = spinner_bg_color
                                                   ),
                                                   
                                                   downloadButton(outputId = "download_faithPD_posttest_demo", 
                                                                  label = "Download Faith PD post hoc result")
                                                 )
                                                 ,
                                                 hr(),
                                                 
                                                 h3("Heatmap of UniFrac distance"),
                                                 withSpinner(
                                                   plotlyOutput(outputId = "unif_dm_hm_demo", 
                                                                height = "600px"),
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color
                                                 ),
                                                 radioButtons(inputId = "UnW_or_W_demo", 
                                                              label = "Choose the method",
                                                              choices = c("Unweighted", "Weighted"), 
                                                              inline = T),
                                                 downloadButton(outputId = "download_unif_dm_demo", 
                                                                label = "Download UniFrac distance matrix"),
                                                 hr(),
                                                 
                                                 h3("Dimension reduction plot of UniFrac distance"),
                                                 withSpinner(
                                                   plotOutput(outputId = "unW_unif_ordination_demo", 
                                                              height = "600px"),
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color
                                                 ),
                                                 radioButtons(inputId = "UnW_or_W_phylo_demo", 
                                                              label = "Choose the method",
                                                              choices = c("Unweighted", "Weighted"), 
                                                              inline = T),
                                                 radioButtons(inputId = "ordination_phylo_demo", 
                                                              label = "Choose the ordination method",
                                                              choices = c("PCoA", "NMDS"), 
                                                              inline = T),
                                                 checkboxInput(inputId = "phylo_cluster_demo", 
                                                               label = "Clustering"),
                                                 checkboxInput(inputId = "phylo_showID_demo", 
                                                               label = "Show sample ID",value = F),
                                                 radioButtons(inputId = "metadata_phylo_beta_demo", 
                                                              label = "Choose the group", 
                                                              choices = " ", 
                                                              inline = T),
                                                 downloadButton(outputId = "download_unif_plot_demo", 
                                                                label = "Download UniFrac plot"),
                                                 
                                                 hr(),
                                                 
                                                 h3("Statistical analysis"),
                                                 column(
                                                   width = 4,
                                                   textOutput(outputId = "Permanova_title_phylo_demo"), 
                                                   tags$style(type="text/css", "#Permanova_title_phylo_demo {font-size: 15px; font-weight: bold;}"),
                                                   #dataTableOutput("permanova_table"),
                                                   withSpinner(
                                                     tableOutput(outputId = "permanova_table_phylo_demo"), 
                                                     type = spinner_type, 
                                                     color.background = spinner_bg_color),
                                                   downloadButton(outputId = "download_permanova_phylo_demo", 
                                                                  label = "Download PERMANOVA table"),
                                                   hr()
                                                 ),
                                                 
                                                 
                                                 column(width = 4,
                                                        textOutput(outputId = "ANOSIM_title_phylo_demo"), 
                                                        tags$style(type="text/css", "#ANOSIM_title_phylo_demo {font-size: 15px; font-weight: bold;}"),
                                                        withSpinner(
                                                          tableOutput(outputId = "ANOSIM_table_phylo_demo"), 
                                                          type = spinner_type, 
                                                          color.background = spinner_bg_color),
                                                        downloadButton(outputId = "download_ANOSIM_phylo_demo", 
                                                                       label = "Download ANOSIM table"),
                                                        hr()
                                                 ),
                                                 
                                                 
                                                 column(width = 4,
                                                        textOutput(outputId = "MRPP_title_phylo_demo"), 
                                                        tags$style(type="text/css", "#MRPP_title_phylo_demo {font-size: 15px; font-weight: bold;}"),
                                                        withSpinner(
                                                          tableOutput(outputId = "MRPP_table_phylo_demo"), 
                                                          type = spinner_type, 
                                                          color.background = spinner_bg_color),
                                                        downloadButton(outputId = "download_MRPP_phylo_demo", 
                                                                       label = "Download MRPP table"),
                                                        hr()
                                                 ),
                                                 
                                                 hr(),
                                                 # h3("Pair"),
                                                 # h4("Permanova"),
                                                 
                                                 column(width = 4, 
                                                        textOutput(outputId = "Permanova_pair_title_phylo_demo"), 
                                                        tags$style(type="text/css", "#Permanova_pair_title_phylo_demo {font-size: 15px; font-weight: bold;}"), 
                                                        withSpinner(
                                                          tableOutput(outputId = "permanova_pair_table_phylo_demo"), 
                                                          type = spinner_type, 
                                                          color.background = spinner_bg_color),
                                                        downloadButton(outputId = "download_permanova_pair_phylo_demo", 
                                                                       label = "Download pairwise PERMANOVA table")
                                                 ),
                                                 # h4("ANOSIM"),
                                                 
                                                 column(width = 4, 
                                                        textOutput(outputId = "ANOSIM_pair_title_phylo_demo"), 
                                                        tags$style(type="text/css", "#ANOSIM_pair_title_phylo_demo {font-size: 15px; font-weight: bold;}"),
                                                        withSpinner(
                                                          tableOutput(outputId = "ANOSIM_pair_table_phylo_demo"), 
                                                          type = spinner_type, 
                                                          color.background = spinner_bg_color),
                                                        downloadButton(outputId = "download_ANOSIM_pair_phylo_demo", 
                                                                       label = "Download pairwise ANOSIM table")
                                                 ),
                                                 # h4("MRPP"),
                                                 
                                                 column(width = 4, 
                                                        textOutput(outputId = "MRPP_pair_title_phylo_demo"), 
                                                        tags$style(type="text/css", "#MRPP_pair_title_phylo_demo {font-size: 15px; font-weight: bold;}"),
                                                        withSpinner(
                                                          tableOutput(outputId = "MRPP_pair_table_phylo_demo"), 
                                                          type = spinner_type, 
                                                          color.background = spinner_bg_color),
                                                        downloadButton(outputId = "download_MRPP_pair_phylo_demo", 
                                                                       label = "Download pairwise MRPP table")
                                                 )
                                                 
                                               ),
                                               
                                               
                                      ),
                                      
                                      tabPanel(title = "ANCOM",
                                               icon = icon("window-maximize"),
                                               div(
                                                 id = "ancom_ui_demo",
                                                 strong("ANCOM (Analysis of composition of microbiomes) is used for comparing the composition of microbiomes in two or more populations.",
                                                        style = "font-size: 18px"),
                                                 br(),br(),
                                                 radioButtons(inputId = "metadata_ANCOM_demo", 
                                                              label = "Select an attribute comparison.", 
                                                              choices = " ", 
                                                              inline = T),
                                                 selectInput(inputId = "ANCOM_level_demo", 
                                                             label = "Choose the level", 
                                                             choices = c("Phylum","Class","Order","Family","Genus","Species") 
                                                 ),
                                                 actionButton(inputId = "ANCOM_start_demo", 
                                                              label = strong("Start!"),
                                                              icon = icon("play-circle")
                                                 ),
                                           
                                                 hr(),
                                                 style = "margin-top:10px"
                                               ) %>% shinyjs::hidden(),
                                               
                                               div(
                                                 id = "ancom_output_ui_demo",
                                                 uiOutput(outputId = "word_ancom_plotly_demo"),
                                                 withSpinner(
                                                   plotlyOutput(outputId = "ancom_plotly_demo"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color
                                                 ),
                                                 
                                                 uiOutput(outputId = "annotation_ancom_demo"),
                                                 hr(),
                                                 uiOutput(outputId = "word_ancom_table_demo"),
                                                 
                                                 withSpinner(
                                                   dataTableOutput(outputId = "ancom_sig_demo"),
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color
                                                 ),
                                                 downloadButton(outputId = "ancom_table_download_demo",
                                                                label = "Download the ANCOM result table (Contain all species)")
                                                 
                                               )
                                               
                                               
                                               
                                               
                                      )
                                      
                          )
                   )
                 ),
                 
                 conditionalPanel(
                   #single5
                   condition = "input.select_module == 'Function analysis'",
                   
                   column(width = 12,
                          tabsetPanel(type="tabs",
                                      tabPanel(title = "Function annotation table",
                                               icon = icon("table"),
                                               div(
                                                 id = "func_table_ui_demo",
                                                 h3("Summary"),
                                                 uiOutput(outputId = "function_report_demo"),
                                                 hr(),
                                                 h3("Function annotation table"),
                                                 withSpinner(
                                                   dataTableOutput(outputId = "func_table_BY_sampleid_demo",
                                                                   height = "auto"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color
                                                 ),
                                                 downloadButton(outputId = "func_table_ID_demo", 
                                                                label = "Download the function annotation table")
                                               )
                                      ),
                                     
                                      tabPanel(title = "Function barplot",
                                               icon = icon("th"),
                                               div(
                                                 id="func_barplot_ui_demo",
                                                 h3("Function barplot"),
                                                 withSpinner(
                                                   plotlyOutput(outputId = "Function_barplot_demo", 
                                                                height = "800px"), 
                                                   type = spinner_type, 
                                                   color.background = spinner_bg_color
                                                 ),
                                                 radioButtons(inputId = "metadata_FA_demo", 
                                                              label = "Choose the group", 
                                                              choices = " ",
                                                              inline = T),
                                                 downloadButton(outputId = "FA_plot_download_demo",
                                                                label = "Download the function barplot")
                                               )
                                      )
                          )
                   )
                 ),
                 
                 
                 
                 # Paired end
                 conditionalPanel(
                   #Paired1
                   condition = "input.select_dataset == 'Paired end' & input.select_module == 'Sequence preprocessing' & input.select_module_step == 'Step 1. Sequence summary'",
                   
                   column(width = 12,
                          h1("1. Sequence summary (for paired end)"),
                          
                          div(
                            id = "demux_paired_demo",
                            hr(),
                            tabsetPanel(
                              type = "tabs",
                              tabPanel(
                                title = span("Sequence counts summary",
                                             style = "color:#317EAC"),
                                h4("Forward reads"),
                                tableOutput("demux_table_paired_f_demo"),
                                plotlyOutput("demux_table_boxplot_paired_f_demo") %>% withSpinner(type = 2, color.background = "white"),
                                h4("Reverse reads"),
                                tableOutput("demux_table_paired_r_demo"),
                                plotlyOutput("demux_table_boxplot_paired_r_demo") %>% withSpinner(type = 2, color.background = "white"),
                                downloadButton("demux_table_paired_demo_dl", 
                                               "Download the read couts table")
                              ),
                              
                              tabPanel(
                                title = span("Quality plot",
                                             style = "color:#317EAC"),
                                h4("Forward sequence length summary"),
                                tableOutput("demux_Q_seven_table_paired_f_demo"),
                                plotlyOutput("demux_Q_plot_paired_f_demo") %>% withSpinner(type = 2, color.background = "white"),
                                downloadButton("demux_Q_table_paired_demo_dl_f", 
                                               "Download the forward read couts table"),
                                
                                h4("Reverse sequence length summary"),
                                tableOutput("demux_Q_seven_table_paired_r_demo"),
                                plotlyOutput("demux_Q_plot_paired_r_demo") %>% withSpinner(type = 2, color.background = "white"),
                                downloadButton("demux_Q_table_paired_demo_dl_r", 
                                               "Download the reverse read couts table")
                              ), 
                              tabPanel(
                                title = "Log",
                                tableOutput("demux_parameter_table_paired_demo"),
                                downloadButton("demux_parameter_table_paired_demo_dl")
                              )
                            )
                          )
                   )
                 ),
                 conditionalPanel(
                   #Paired2
                   condition = "input.select_dataset == 'Paired end' & input.select_module == 'Sequence preprocessing' & input.select_module_step == 'Step 2. Sequence denoising'",
                   
                   column(width = 12,
                          h1('2. Sequence denoising (DADA2 1.18.0) for Paired end'),
                          div(
                            id = "dada2_results_paired_demo",
                            hr(),
                            tabsetPanel(
                              type = "tabs",
                              
                              tabPanel(
                                title = "Summary",
                                # br(),br(),
                                h4("Sample read count summary"),
                                tableOutput("dada2_sample_summary_paired_demo"),
                                h4("Sample summary table"),
                                dataTableOutput("dada2_sample_table_paired_demo"),
                                downloadButton("dada2_sample_table_paired_demo_dl"),
                                br(),br(),
                                h4("ASV read count summary"),
                                tableOutput("dada2_asv_summary_table_paired_demo"),
                                h4("ASV summary table"),
                                dataTableOutput("dada2_asv_table_paired_demo"),
                                downloadButton("dada2_asv_table_paired_demo_dl")
                              ),
                              tabPanel(
                                title = "Filter info",
                                br(),br(),
                                dataTableOutput("dada2_filter_table_paired_demo"),
                                downloadButton("dada2_filter_table_paired_demo_dl")
                              ),
                              tabPanel(
                                title = "Sequence info",
                                h4("Sequence Length Statistics"),
                                tableOutput("dada2_seqs_info_paired_1_demo"),
                                h4("Seven-Number Summary of Sequence Lengths"),
                                tableOutput("dada2_seqs_info_paired_2_demo"),
                                h4("Sequence table"),
                                dataTableOutput("dada2_seqs_table_paired_demo"),
                                downloadButton("dada2_seqs_table_paired_demo_dl", "Download the sequences as a FASTA file")
                              ),
                              tabPanel(
                                title = "Rarefaction plot",
                                br(),br(),
                                plotOutput("rarefaction_plot_paired_demo", height = "500px"),
                                downloadButton("rarefaction_plot_paired_demo_dl", "Download the rarefaction plot"),
                                downloadButton("rarefaction_table_paired_demo_dl", "Download the rarefaction table")
                              ),
                              tabPanel(
                                title = "Table",
                                br(),br(),
                                dataTableOutput("dada2_table_paired_demo"),
                                downloadButton("dada2_table_paired_demo_dl")
                              ),
                              tabPanel(
                                title = "Log",
                                tableOutput("dada2_log_table_paired_demo"),
                                downloadButton("dada2_log_table_paired_demo_dl")
                              )
                            )
                          )
                   )
                 ),
                 
                 
                 
                 # Long read
                 conditionalPanel(
                   #Long read 1
                   condition = "input.select_dataset == 'Long read' & input.select_module == 'Sequence preprocessing' & input.select_module_step == 'Step 1. Sequence summary'",
                   
                   column(width = 12,
                          h1("1. Sequence summary (for Long read)"),
                          
                          div(
                            id = "demux_Pacbio_demo",
                            hr(),
                            tabsetPanel(
                              type = "tabs",
                              tabPanel(
                                title = span("Sequence counts summary",
                                             style = "color:#317EAC"),
                                h4("Forward reads"),
                                tableOutput("demux_table_Pacbio_demo"),
                                plotlyOutput("demux_table_boxplot_Pacbio_demo") %>% withSpinner(type = 2, color.background = "white"),
                                downloadButton("demux_table_Pacbio_demo_dl", 
                                               "Download the read couts table")
                              ),
                              
                              tabPanel(
                                title = span("Quality plot",
                                             style = "color:#317EAC"),
                                h4("Sequence length summary"),
                                tableOutput("demux_Q_seven_table_Pacbio_demo"),
                                plotlyOutput("demux_Q_plot_Pacbio_demo") %>% withSpinner(type = 2, color.background = "white"),
                                downloadButton("demux_Q_table_Pacbio_demo_dl", 
                                               "Download the quality score table")
                              ),
                              tabPanel(
                                title = "Log",
                                tableOutput("demux_parameter_table_Pacbio_demo"),
                                downloadButton("demux_parameter_table_Pacbio_demo_dl")
                              )
                            )
                            
                            
                          )
                   )
                 ),
                 conditionalPanel(
                   #Long read 2
                   condition = "input.select_dataset == 'Long read' & input.select_module == 'Sequence preprocessing' & input.select_module_step == 'Step 2. Sequence denoising'",
                   
                   column(width = 12,
                          
                          h1('2. Sequence denoising (DADA2 1.18.0) for Long read'),
                          div(
                            id = "dada2_results_Pacbio_demo",
                            hr(),
                            tabsetPanel(
                              type = "tabs",
                              
                              tabPanel(
                                title = "Summary",
                                # br(),br(),
                                h4("Sample read count summary"),
                                tableOutput("dada2_sample_summary_Pacbio_demo"),
                                h4("Sample summary table"),
                                dataTableOutput("dada2_sample_table_Pacbio_demo"),
                                downloadButton("dada2_sample_table_Pacbio_demo_dl"),
                                br(),br(),
                                h4("ASV read count summary"),
                                tableOutput("dada2_asv_summary_table_Pacbio_demo"),
                                h4("ASV summary table"),
                                dataTableOutput("dada2_asv_table_Pacbio_demo"),
                                downloadButton("dada2_asv_table_Pacbio_demo_dl")
                              ),
                              tabPanel(
                                title = "Filter info",
                                br(),br(),
                                dataTableOutput("dada2_filter_table_Pacbio_demo"),
                                downloadButton("dada2_filter_table_Pacbio_demo_dl")
                              ),
                              tabPanel(
                                title = "Sequence info",
                                h4("Sequence Length Statistics"),
                                tableOutput("dada2_seqs_info_Pacbio_1_demo"),
                                h4("Seven-Number Summary of Sequence Lengths"),
                                tableOutput("dada2_seqs_info_Pacbio_2_demo"),
                                h4("Sequence table"),
                                dataTableOutput("dada2_seqs_table_Pacbio_demo"),
                                downloadButton("dada2_seqs_table_Pacbio_demo_dl", "Download the sequences as a FASTA file")
                              ),
                              tabPanel(
                                title = "Rarefaction plot",
                                br(),br(),
                                plotOutput("rarefaction_plot_Pacbio_demo", height = "500px"),
                                downloadButton("rarefaction_plot_Pacbio_demo_dl", "Download the rarefaction plot"),
                                downloadButton("rarefaction_table_Pacbio_demo_dl", "Download the rarefaction table")
                              ),
                              tabPanel(
                                title = "Table",
                                br(),br(),
                                dataTableOutput("dada2_table_Pacbio_demo"),
                                downloadButton("dada2_table_Pacbio_demo_dl")
                              ),
                              tabPanel(
                                title = "Log",
                                tableOutput("dada2_log_table_Pacbio_demo"),
                                downloadButton("dada2_log_table_Pacbio_demo_dl")
                              )
                            )
                          )
                   )
                 )
                 
                 
                 
                 
                 
                 
                 
                 
                 
               )
             )
    )
      

      
    ), # navbarPage
    
    # footer -----------
    
    tags$footer(
      tags$span(
        tags$a("Molecular Bioinformatics Lab,", href = "https://fullofbeans.nctu.edu.tw/?page_id=333&lang=en", style = "color: white", target = "_blank"), 
        " National Yang Ming Chiao Tung University, Taiwan 300, R.O.C.,", " last updated on 05/27/2021"),
      # tags$a(href="https://ibs.nctu.edu.tw/faculty/%E9%99%B3%E4%BA%AD%E5%A6%8F/", tags$span("Contact us!"), 
      #        target = "_blank",
      #        class="externallink", 
      #        style = "color: white; padding-left: 100px;"), 
      
      style = "
                       font-family: Arial;
                       position:fixed;
                       text-align:center;
                       left: 0px;
                       right: 0px;
                       bottom: 0px;
                       z-index:1000;  
                      
                       color: white;
                       padding: 5px;
                       background-color: #043B70;
                       background-color: #317EAC;
                       border-top: solid 1px white;
                       "
    ),
    
    tags$style("@media screen and (min-width: 611px) {
               footer {
               height: 30px;
               }
    }"),
    tags$style("@media screen and (max-width: 611px) {
               footer {
               height: 60px;
               }
    }"),
    
    # avoid from navbar and footer cover contents
    tags$body(
      style = "position:relative;",
      tags$style("@media screen and (min-width: 1600px) {
                         body {
                           position:relative;
                           margin-top: 60px;
                           margin-bottom: 30px;
                                             }
                 }"
      ),
      tags$style("@media screen and (max-width: 1600px) {
                         body {
                           position:relative;
                           margin-top: 125px;
                           margin-bottom: 30px;
                                             }
                 }"
      ),
      tags$style("@media screen and (max-width: 1333px) {
                         body {
                           position:relative;
                           margin-top: 195px;
                           margin-bottom: 30px;
                                             }
                 }"
                 ),
      tags$style("@media screen and (max-width: 767px) {
                         body {
                           position:relative;
                           margin-top: 60px;
                           margin-bottom: 30px;
                                             }
                 }"
      ),
      tags$style("@media screen and (max-width: 611px) {
                         body {
                           position:relative;
                           margin-top: 60px;
                           margin-bottom: 60px;
                                             }
                 }"
      )
      
      # tags$style("@media screen and (max-height: 900px) {
      #                    #ch2 {
      #                      height: 300px
      #                                        }
      #            }"
      # )
    )
    
    
  ) # fluidPage
) # shinyUI

