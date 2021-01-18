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
                              # h4("Please choose the directory containing sequences data",
                              # h4("Please upload the sequences data",
                              #    style = "color: white; left:5px"),
                              # shinyFiles::shinyDirButton(id = 'dirs', 
                              #                            label = 'Select the directory', 
                              #                            title = 'Please select a directory',
                              #                            style = "margin: 2px;", 
                              #                            icon = icon("folder")
                              #                            ),
                              
                              fileInput(inputId = "seqs_data_upload",
                                        label = span("Please select and upload the sequence files (*.fastq.gz or *.fq.gz)",
                                                     style= "font-size: 20px; font-weight: 300; color: white;"),
                                        multiple = T
                                        # accept = ".fastq.gz"
                                        ) %>% div(),
                              
                              # HTML('<div class="file-loading" style = "font-size: 20px; font-weight: 300; color: white;background-color: #317EAC;">
                              #       <input id="seqs_data_upload_1" style = "font-size: 20px; font-weight: 300; color: #317EAC;background-color: white;" multiple type="file" class="file" data-allowed-file-extensions="["gz"]">
                              #       </div>'),
                              
                              
                              
                              
                              
                              # p("Download the demo sequences", 
                              #   style= "font-size: 20px; font-weight: 300; color: white; margin: 5px;"),
                              # downloadButton(outputId = "seqs_demo_download",
                              #                label = "Download"),
                              # downloadButton(outputId = "seqs_demo_download",
                              #                label = span("Demo seqs", style ="font-weight: 800"),
                              #                style = "color:#317EAC;background-color:white;"
                              # ),
                              
                              actionButton(inputId = "load_parameter_demux",
                                           label = strong("Demo", style = "margin: 5px;font-size: 18px"),
                                           icon = icon("chalkboard-teacher"),
                                           style = "color:#317EAC;background-color:white;") %>% div(),
                              
                              # br(),br(),
                              hr(),
                              strong("Sequence type", style = "font-size:24px;color:white;top:20px"),
                              
                              pickerInput(inputId = "seqs_type",
                                          label = span("Choose the sequence type", 
                                                       style= "font-size: 20px; font-weight: 300; color: white; margin: 0px;"),
                                          choices = c("Single end", "Paired end"),
                                          width = "300px"
                              ),
                              
                             
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
                             
                              
                              hr(),
                              strong("Computing setting", style = "font-size:24px;color:white"),
                              textInput(inputId = "n_jobs_demux", 
                                        label = span("Number of threads MOCHI can use", style = "font-size: 18px; font-weight: 300; color: white; margin-top: 5px;"),
                                        value = my_cores-2,
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
                                  
                                  # h2("1. Sequences summary (for single end)", 
                                  #    style = "color: #317EAC;margin-top: 0px;"),
                                  # h4("(1) Summarize the single-end sequences.", 
                                  #    style = "color: #317EAC;",
                                  #    span(
                                  #      actionButton(inputId = "demultiplexed_single_ends", 
                                  #                   label = strong("Start!"), 
                                  #                   icon = icon("play-circle"),
                                  #                   style = "margin: 10px; display: inline-block;")
                                  #    ),
                                  #    
                                  #    
                                  # ),
                                  h1("1. Sequence summary (for single end)"),
                                  h4("(1) Summarize the single-end sequences.",style = "color: #317EAC;"),
                                  actionButton(inputId = "demultiplexed_single_ends", 
                                               label = strong("Start!"), 
                                               icon = icon("play-circle"),
                                               style = "margin: 10px; display: inline-block;"
                                  ),
                                  
                                  
                                  
                                  
                                  
                                  # br(),br(),
                                  div(
                                    id = "demux_results_view_single",
                                    h4('(2) Inspect the result.'),
                                    uiOutput("show_demux_single_bttn"),
                                    
                                    span("On the", strong('View'), "webpage, you can download the result by right click" ,strong('Save as ...'),
                                         style = "position: relative; top:5px;")
                                  ) %>% shinyjs::hidden(),
                                  
                                  # span("Go to", strong('User results'), ", you can inspect the result by your user id."),
                                  
                                  
                                  
                                  # tags$a(href = 'https://140.113.239.95:8055', class = "btn", icon("download"), 'Download file.'),
                                  
                                  
                                  
                                  br(),br(),
                                  # hr(),
                                  h4('Example output for sequence summary'),
                                  actionButton(inputId = "demux_example_single", 
                                               label = "Example for single end", 
                                               # icon = icon("link"),
                                               style = "top:20px; margin: 10px; display: inline-block;",
                                               onclick = paste0("window.open('http://", 
                                                                "mochi.life.nctu.edu.tw",
                                                                "/example_files/demux_single/data/index.html",
                                                                "')")
                                  ),
                                  # tags$hr(class="A",
                                  #         tags$style(
                                  #           "hr.A{border: 3px solid #317EAC;}"
                                  #         )
                                  # ),
                                  
                                # ), style = "border: none;"
                                ),
                              ),
                              
                              conditionalPanel(
                                
                                condition = "input.seqs_type == 'Paired end'",
                                
                                column(width = 12,
                                # div(
                                  # h2("1. Sequences summary (for paired end)", 
                                  #    style = "color: #317EAC;margin-top: 0px;"),
                                  # h4("(1) Summarize the paired-end sequences.",
                                  #    style = "color: #317EAC;",
                                  #    span(
                                  #      actionButton(inputId = "demultiplexed_paired_ends", 
                                  #                   label = strong("Start!"), 
                                  #                   icon = icon("play-circle"),
                                  #                   style = "margin: 10px; display: inline-block;"
                                  #      )
                                  #      
                                  #    ),
                                  #    
                                  # ),
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
                                    h4('(2) Inspect the result.'),
                                    uiOutput("show_demux_paired_bttn"),
                                    
                                    span("On the", strong('View'), "webpage, you can download the result by right click" ,strong('Save as ...'),
                                         style = "position: relative; top:5px;")
                                  ) %>% shinyjs::hidden(),
                                  
                                # span("Go to", strong('User results'), ", you can inspect the result by your user id."),
                                  

                                  br(),br(),
                                  # hr(),
                                  h4('Example output for sequence summary'),
                                  actionButton(inputId = "demux_example_paired", 
                                               label = "Example for paired end", 
                                               # icon = icon("link"),
                                               style = "top:20px; margin: 10px; display: inline-block;", 
                                               onclick = paste0("window.open('http://", 
                                                                "mochi.life.nctu.edu.tw",
                                                                "/example_files/demux_paired/data/index.html",
                                                                "')")
                                               ),
                                  br(),br(),
                                  # tags$hr(class="A",
                                  #         tags$style(
                                  #           "hr.A{border: 3px solid #317EAC;}"
                                  #         )
                                  # ),
                                  
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
                                
                                h1('2. Sequence denoising (DADA2) for Single end'),
                                # strong("Sequences", style = "font-size:20px;color: #317EAC;"),
                                # splitLayout(cellWidths = "300px",
                                #             
                                #             textInput(inputId = "trim_left_single", 
                                #                       label = "The starting position to trim the sequences",
                                #                       placeholder = "Input number",
                                #                       value = 0),
                                #             textInput(inputId = "trunc_len_single", 
                                #                       label = "The ending position to trim the sequences",
                                #                       placeholder = "Input number",
                                #                       value = 0)
                                # )
                                # ,
                                # p("Reads that are shorter than the ending position will be discarded."),
                                # p("If ending position is 0, no truncation or length filtering will be performed."),
                                # 
                                # strong("Quality score", style = "font-size:20px;color: #317EAC"),
                                # textInput(inputId = "qvalue_single", 
                                #           label = "Reads are truncated at the position of a quality score less than or equal to this value.",
                                #           placeholder = "Input number",
                                #           value = 0,
                                #           width = "600px"),
                                # p("If the resulting read is then shorter than ending position, it is discarded."),
                                # br(),br(),
                                # 
                                # strong("Chimeric reads filter", style = "font-size:20px;color: #317EAC"),
                                # textInput(inputId = "chimera_single", 
                                #           label = "The minimum fold-change value", 
                                #           value = 1),
                                # actionButton(inputId = "word_chimera_single", 
                                #              label = "What is this ?",
                                #              icon = icon("question-circle")
                                #              ),
                                #              
                                # 
                                # 
                                # 
                                # # textOutput(outputId = "message_thread_single_position"),
                                # br(),br(),
                                # 
                                # strong("Training error model", style = "font-size:20px;color: #317EAC"),
                                # textInput(inputId = "n_reads_single", 
                                #           label = "The number of reads for training error model", 
                                #           value = format(1000000, scientific = F)),
                                # actionButton(inputId = "Q_learn_reads_single", 
                                #              label = "What's this ?", 
                                #              icon = icon("question-circle")
                                # ),
                                # # textOutput(outputId = "message_learn_single_position"),
                                # br(),br(),
                                # 
                                # strong("Computing setting", style = "font-size:20px;color: #317EAC"),
                                # textInput(inputId = "threads_single", 
                                #           label = "Input the number of threads", 
                                #           value = suggested_cores),
                                # # actionButton(inputId = "my_cores_single", 
                                # #              label = "Show the threads of this server.") %>% div(),
                                # # actionButton(inputId = "Q_cores_single", 
                                # #              label = "What is thread ?", 
                                # #              icon = icon("question-circle")
                                # # ) %>% div(),
                                # 
                                # br(),br(),
                                # 
                                # strong("Integrating the metadata", style = "font-size:20px;color: #317EAC"),
                                # fileInput(inputId = "sample_data_single",
                                #           label = "Upload the metadata (1st column name must be '#SampleID'')",
                                #           multiple = F,
                                #           accept = ".tsv",
                                #           width = "500px"),
                                # div(
                                #   span("This input is"),
                                #   strong(" optional"),
                                #   span(".If metadata is provided, the results would have metadata information."),
                                # style = "margin-top:-15px"),
                                
                                # br(),br(),
                                h4("(1) Start to denoise."),
                                actionButton(inputId = "denoising_single", 
                                             label = denoise_btn_start_label,
                                             icon = icon("play-circle"),
                                             style = "margin: 10px; display: inline-block;"), 
                                # br(),br(),
                                # hr(),
                                # strong('Results', style = "font-size: 20px;color: #317EAC"),
                                div(
                                  id = "dada2_results_single",
                                  h4('(2) Inspect the sequence denoising result.'),
                                  uiOutput("dada2_single_results_bttn"),
                                  style = "margin-top: 10px;margin-bottom: 25px"
                                ) %>% shinyjs::hidden(),
                                
                                br(),br(),
                                # hr(),
                                # strong('Examples', style = "font-size: 20px;color: #317EAC"),
                                h4('Example output for sequence denoising',
                                   style = ""),
                                div(
                                    actionButton(inputId = "table_dada2_single_example",
                                                 label = "Summary table",
                                                 icon = NULL,
                                                 onclick = paste0("window.open('http://",
                                                                  "mochi.life.nctu.edu.tw",
                                                                  "/example_files/denoising_table/data/index.html",
                                                                  "')")
                                                 ),

                                    actionButton(inputId = "rep_dada2_single_example",
                                                 label = "Seqs info",
                                                 icon = NULL,
                                                 onclick = paste0("window.open('http://",
                                                                  "mochi.life.nctu.edu.tw",
                                                                  "/example_files/denoising_rep_seqs/data/index.html",
                                                                  "')")
                                                 ),

                                    actionButton(inputId = "stats_dada2_single_example",
                                                 label = "Filter info",
                                                 icon = NULL,
                                                 onclick = paste0("window.open('http://",
                                                                  "mochi.life.nctu.edu.tw",
                                                                  "/example_files/denoising_stats/data/index.html",
                                                                  "')")
                                                 ),
                                    actionButton(inputId = "rarefaction_single_example",
                                                 label = "Alpha rarefaction",
                                                 icon = NULL,
                                                 onclick = paste0("window.open('http://",
                                                                  "mochi.life.nctu.edu.tw",
                                                                  "/example_files/alpha-rarefaction_example/data/index.html",
                                                                  "')")
                                                 ),
                                    # textOutput(outputId = "word_denoising_single_position")
                                    )
                                  )
                              ),
                              
                              
                              
                              conditionalPanel(
                                
                                condition = "input.seqs_type == 'Paired end'",
                                
                                column(width = 12,
                                
                                uiOutput(outputId = "show_job_id_denoise_paired"),
                                h1('2. Sequence denoising (DADA2) for Paired end'),
                                
                                # strong("Forward sequences", style = "font-size:20px;color: #317EAC"),
                                # splitLayout(cellWidths = "300px",
                                #             
                                #             textInput(inputId = "trim_left_f_paired", 
                                #                       label = "The starting position to trim",
                                #                       placeholder = "Input number",
                                #                       value = 0
                                #             ),
                                #             textInput(inputId = "trunc_len_f_paired", 
                                #                       label = "The ending position to trim",
                                #                       placeholder = "Input number",
                                #                       value = 0
                                #             )
                                # ),
                                # 
                                # strong("Reverse sequences", style = "font-size:20px;color: #317EAC"),
                                # splitLayout(cellWidths = "300px",
                                #             
                                #             textInput(inputId = "trim_left_r_paired", 
                                #                       label = "The starting position to trim",
                                #                       placeholder = "Input number",
                                #                       value = 0),
                                #             textInput(inputId = "trunc_len_r_paired", 
                                #                       label = "The ending position to trim",
                                #                       placeholder = "Input number",
                                #                       value = 0)
                                # ),
                                # p("Reads that are shorter than the ending position will be discarded."),
                                # p("If ending position is 0, no truncation or length filtering will be performed."),
                                # 
                                # strong("Quality score", style = "font-size:20px;color: #317EAC"),
                                # textInput(inputId = "qvalue_paired", 
                                #           label = "Reads are truncated at the position of a quality score less than or equal to this value.",
                                #           placeholder = "Input number",
                                #           value = 0,
                                #           width = "600px"),
                                # p("If the resulting read is then shorter than ending position, it is discarded."),
                                # 
                                # br(),br(),
                                # 
                                # strong("Chimeric reads filter", style = "font-size:20px;color: #317EAC"),
                                # textInput(inputId = "chimera_paired", 
                                #           label = "The minimum fold-change value", 
                                #           value = 1),
                                # actionButton(inputId = "word_chimera_paired", 
                                #              label = "What is this ?",
                                #              icon = icon("question-circle")
                                # ),
                                # 
                                # br(),br(),
                                # 
                                # strong("Training error model", style = "font-size:20px;color: #317EAC"),
                                # textInput(inputId = "n_reads_paired", 
                                #           label = "The number of reads for training error model", 
                                #           value = format(1000000, scientific = F)),
                                # actionButton(inputId = "Q_learn_reads_paired", 
                                #              label = "What's this ?", 
                                #              icon = icon("question-circle")
                                # ),
                                # # textOutput(outputId = "message_learn_paired"),
                                # 
                                # br(),br(),
                                # 
                                # strong("Computing setting", style = "font-size:20px;color: #317EAC"),
                                # textInput(inputId = "threads_paired", 
                                #           label = "Input the number of threads", 
                                #           value = suggested_cores),
                                # # actionButton(inputId = "my_cores_paired", 
                                # #              label = "Show the threads of this server.") %>% div(),
                                # # actionButton(inputId = "Q_cores_paired", 
                                # #              label = "What is thread ?", 
                                # #              icon = icon("question-circle")) %>% div(),
                                # br(),br(),
                                # 
                                # strong("Integrating the metadata", style = "font-size:20px;color: #317EAC"),
                                # fileInput(inputId = "sample_data_paired",
                                #           label = "Upload the metadata (1st column name must be '#SampleID')",
                                #           multiple = F,
                                #           accept = ".tsv",
                                #           width = "500px"),
                                # 
                                # div(
                                #   span("This input is"),
                                #   strong(" optional"),
                                #   span(".If metadata is provided, the results would have metadata information."),
                                #   style = "margin-top:-15px"),
                                
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
                                  h4('(2) Inspect the sequence denoising result.'),
                                  uiOutput("dada2_paired_results_bttn"),
                                  style = "margin-top: 10px;margin-bottom: 25px"
                                  ) %>% shinyjs::hidden(),

                                br(),br(),
                                # hr(),
                                # strong('Examples', style = "font-size: 20px;color: #317EAC"),
                                h4('Example output for sequence denoising'),
                                div(
                                  actionButton(inputId = "table_dada2_paired_example",
                                               label = "Summary table",
                                               icon = NULL,
                                               onclick = paste0("window.open('http://",
                                                                "mochi.life.nctu.edu.tw",
                                                                "/example_files/denoising_table/data/index.html",
                                                                "')")
                                               ),
                                  actionButton(inputId = "rep_dada2_paired_example",
                                               label = "Seqs info",
                                               icon = NULL,
                                               onclick = paste0("window.open('http://",
                                                                "mochi.life.nctu.edu.tw",
                                                                "/example_files/denoising_rep_seqs/data/index.html",
                                                                "')")
                                               ),

                                  actionButton(inputId = "stats_dada2_paired_example",
                                               label = "Filter info",
                                               icon = NULL,
                                               onclick = paste0("window.open('http://",
                                                                "mochi.life.nctu.edu.tw",
                                                                "/example_files/denoising_stats/data/index.html",
                                                                "')")
                                               ),
                                  actionButton(inputId = "rarefaction_paired_example",
                                               label = "Alpha rarefaction",
                                               icon = NULL,
                                               onclick = paste0("window.open('http://",
                                                                "mochi.life.nctu.edu.tw",
                                                                "/example_files/alpha-rarefaction_example/data/index.html",
                                                                "')")
                                               ),

                                  # textOutput(outputId = "word_denoising_paired_position")
                                )
                              )
                              )
                              
                              
                              
                              
                              
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
                                                   label = strong("Demo", style = "margin: 5px;font-size: 18px"),
                                                   icon = icon("chalkboard-teacher"),
                                                   style = "color:#317EAC;background-color:white;margin-top:10px") %>% div(),
                                      
                                      # p('If the database have been updated, you need to download the latest data by yourself.'),
                                      # actionButton(inputId = "database_tutorial", 
                                      #              label = "How to download the data", 
                                      #              icon = icon("question")
                                      # ),
                                      
                                      # hr(),
                                      # p("After the database is selected, we need primer to extract the target region of sequences."),
                                      # selectInput(inputId = "primer_f",
                                      #             label = "Choose the forward primer sequences",
                                      #             choice = c("8F", "27F", "CC [F]", "357F", "515F", "533F", "16S.1100.F16", "1237F", "other"),
                                      #             width = "300px"
                                      # ),
                                      # selectInput(inputId = "primer_r",
                                      #             label = "Choose the reverse primer sequences",
                                      #             choice = c("519R", "CD [R]", "907R", "1100R", "1391R", "1492R (l)", "1492R (s)", "other"),
                                      #             width = "300px"
                                      # ),
                                      # actionButton(inputId = "show_primer",
                                      #              label = "Show the primer table",
                                      #              icon = icon("table")
                                      # ),
                                      # br(), br(),
                                      # p('If your primers could not be found in the selections, you also can  manually input your primer sequences by choosing',
                                      #   strong(' other.'),
                                      #   style = "font-size: 14px"),
                                      # 
                                      # uiOutput(outputId = "out_f"),
                                      # uiOutput(outputId = "out_r"),
                                      
                                      hr(),
                                      # textInput(inputId = "trunc_length", 
                                      #           label = "Give the trunc length you want",
                                      #           placeholder = "Input number",
                                      #           width = "300px"),
                                      strong("Reference sequence filtering", style = "font-size:24px;color:white"),
                                      br(),br(),
                                      strong("1. Check primers", style = "color: white;font-size: 20px;"),
                                      p("If incorrect, go to 'Sequence summary' to select the correct primer.", style = "font-size:18px;"),
                                      # p(HTML("<b>1. Check your primers</b>"), span(shiny::icon("info-circle"), id = "info_check")
                                      #   , style = "font-size:20px;font-weight:700; margin-top:15px"),
                                      # tippy::tippy_this(elementId = "info_check", tooltip = "If incorrect, go to Sequences summary to change", placement = "right"),
                                      
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
                                                value = my_cores-2,
                                                placeholder = "Input number",
                                                width = "300px")
                                      # br(),br(),
                                      # actionButton(inputId = "start_training", 
                                      #              label = strong("Start!"),
                                      #              icon = icon("play-circle")
                                      #              ),
                                      # textOutput(outputId = "word_training"),
                                      
                                      # strong("Result", style = "font-size: 20px"),
                                      # div(actionButton(inputId = "view_taxa",
                                      #              label = "Taxonomy classification result",
                                      #              onclick = paste0("window.open('http://", 
                                      #                               my_qiime_ip, my_qiime_port,
                                      #                               "/taxonomy_unzip/new_dirname/data/index.html",
                                      #                               "')"),
                                      #              icon = icon("eye")),
                                      #     style = "margin-top: 10px;margin-bottom: 10px"
                                      #     ),
                                      
                                      # div(downloadButton(outputId = "taxatable_download", 
                                      #                    label = "Download the taxonomic table for next step.",
                                      #                    style = "margin-bottom: 10px")
                                      #     ),
                                      # div(downloadButton(outputId = "table_dada2_download", 
                                      #                    label = "Download the ASVs table for next step.",
                                      #                    style = "margin-bottom: 10px")
                                      # ),
                                      # 
                                      # tippy::tippy_this(elementId = "table_dada2_download",
                                      #                   tooltip = "<p style='text-align: left;margin:2px'>amplicon sequence variant (ASV) table, a higher-resolution analogue of the traditional OTU table</p>",
                                      #                   allowHTML = TRUE,
                                      #                   placement = "right"),
                                      # 
                                      # div(downloadButton(outputId = "rep_seq_dada2_download", 
                                      #                    label = "Download the seqs data for next step.",
                                      #                    style = "margin-bottom: 10px")
                                      #     ),
                                      # p("amplicon sequence variant (ASV) table, a higher-resolution analogue of the traditional OTU table.",
                                      #   style = "font-size: 14px;"),
                                      # div(strong('Example', style = "font-size: 20px")),
                                      # actionButton(inputId = "view_taxonomy_example",
                                      #              label = "How does the taxonomy result look?",
                                      #              style = "margin-bottom: 10px;margin-top: 10px",
                                      #              onclick = paste0("window.open('http://",
                                      #                               my_qiime_ip, my_qiime_port,
                                      #                               "/example_files/taxonomy_analysis/data/index.html",
                                      #                               "')")
                                      #              )
                                      
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
                                h4("(2) Inspect the taxonomy classification result.", 
                                   style = "margin-top: 25px"),
                                uiOutput(outputId = "taxa_view_bttn"),
                                # span("On the", strong('View'), "webpage, you can download the result by right click" ,strong('Save as ...'),
                                #      style = "position: relative; top:5px;"),
                                  style = "margin-top: 10px;"
                              ) %>% shinyjs::hidden(),
                              # span("Go to", strong('User results'), ", you can inspect the result by your user id."),
                              
                              
                              div(
                                id = "taxa_results_download",
                                h4("(3) Download the files for the next step.",
                                   style = "margin-top: 25px"),
                                downloadButton(outputId = "taxatable_download",
                                               label = "The taxonomic table  ",
                                               style = "margin-left: 10px"),
                                
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
                              hr(),
                              h4('Example output for taxonomy classification'),
                              actionButton(inputId = "view_taxonomy_example",
                                           label = "Taxonomy result",
                                           style = "margin-left: 10px",
                                           onclick = paste0("window.open('http://",
                                                            "mochi.life.nctu.edu.tw",
                                                            "/example_files/taxonomy_analysis/data/index.html",
                                                            "')")
                              ),
                              

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
                   # tags$style(".tooltip {position:relative;bottom:20px}"),
                   
                   # div(
                   #   span("Upload the metadata (1st column name must be"), 
                   #   strong('#SampleID'),
                   #   span(")"),
                   # style = "font-size: 16px"),
                   # textOutput(outputId="word_metadata_samecol"),
                   
                   # span("Upload the taxonomic table file (Download from Taxonomy Analysis)"),
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
                   actionButton("TA_demo",
                                strong("Demo", style = "margin: 5px;font-size: 18px"),
                                icon = icon("chalkboard-teacher"),
                                style = "color:#317EAC;background-color:white;margin-top:10px") %>% div(),
                   
                   # Download example button
                   # hr(),
                   # p('Download the example files', style = "font-weight:700"),
                   # div(downloadButton(outputId = "downloadMetaData", 
                   #                label = span("Metadata_example.tsv"),
                   #     style = "margin: 5px;color: #317EAC")),
                   # div(downloadButton(outputId = "downloadData", 
                   #                label = span("Taxonomic_table_example.qza"),
                   #     style = "margin: 5px;color: #317EAC")),
                   # div(downloadButton(outputId = "example_feature_table",
                   #                label = span("ASVs_table_example.qza"),
                   #     style = "margin: 5px;color: #317EAC")),
                   # div(downloadButton(outputId = "example_rep_seqs",
                   #                    label = "Seqs_forPhylo_example.qza"),
                   #     style = "margin: 5px;"),
                   
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
                                                                  "Shannon_diverstiy", "Simpon_diversity", "InvSimpson_diversity",
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
                                                         label = "Download Alpha Diversity statistical result"),
                                          br(),br()
                                          
                                        ) %>% shinyjs::hidden(),
                                        
                                        
                                        
                               ),
                               
                               tabPanel(title = "Beta diversity",
                                        icon = icon("info"),
                                        div(
                                          id = "beta_ui",
                                          h3("Beta diversity table (Bray-Curtis)"),
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
                                                           label = "Download permanova table"),
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
                                                                label = "Download permanova pair table")
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
                                                                label = "Download ANOSIM pair table")
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
                                                                label = "Download MRPP pair table")
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
                                          actionButton("phylo_demo",
                                                       strong("Demo", style = "margin: 5px;font-size: 18px"),
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
                                                         label = "Download Faith PD post hoc result"),
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
                                                           label = "Download PerMANOVA table"),
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
                                                                label = "Download PerMANOVA pair table")
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
                                                                label = "Download ANOSIM pair table")
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
                                                                label = "Download MRPP pair table")
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
                                          )
                                          
                                          ) %>% shinyjs::hidden(),
                                          downloadButton(outputId = "ancom_table_download",
                                                         label = "Download the ANCOM result table (Contain all species)") %>% shinyjs::hidden()

                                        
                                        
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
                   # a("FAPROTAX", 
                   #   href ="https://pages.uoregon.edu/slouca/LoucaLab/archive/FAPROTAX/lib/php/index.php", 
                   #   target="_blank",
                   #   style = "font-weight: 700; color:white;"), 
                   # span("is a database that maps prokaryotic clades (e.g. genera or species) to established metabolic or other ecologically relevant functions"),
                   # br(),br(),
                   
                   fileInput(inputId = "sample_data_FA", 
                             label = p(HTML("<b>Upload the metadata file</b>"),
                                       span(shiny::icon("info-circle"),
                                            id = "info_metadata_FA")),
                             multiple = F,
                             accept = ".tsv"),
                   tippy::tippy_this(elementId = "info_metadata_FA", 
                                     tooltip = HTML("<p>1st column name must be <b>#SampleID</b></p>"), 
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
                   
                   actionButton("FA_demo",
                                strong("Demo", style = "margin: 5px;font-size: 18px"),
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
                 # uiOutput("tutorial"),
                 # htmlOutput("tutorial")
                 # tabsetPanel(type="tabs",
                 #             # tabPanel(title="Installation",
                 #             #          withMathJax(includeMarkdown("/home/imuser/text_files/tutorial_install.Rmd"))
                 #             #          ),
                 #             tabPanel(title="Sequence Preprocessing",
                 #                      h2("The tutorial of Sequences preprocessing"),
                 #                      selectInput(inputId = "seq_choice_tutorial",
                 #                                  label = "Choose the process",
                 #                                  choices = c("Step 1. Sequence summary", "Step 2. Sequence denoising", "Step 3. Taxonomy classification")),
                 #                      hr(),
                 #                      conditionalPanel(
                 #                        condition = "input.seq_choice_tutorial == 'Step 1. Sequence summary'",
                 #                        withMathJax(includeMarkdown("/home/imuser/text_files/tutorial_seq_1.Rmd")
                 #                                    ),
                 #                      ),
                 #                      conditionalPanel(
                 #                        condition = "input.seq_choice_tutorial == 'Step 2. Sequence denoising'",
                 #                        withMathJax(includeMarkdown("/home/imuser/text_files/tutorial_seq_2.Rmd")
                 #                        ),
                 #                      ),
                 #                      conditionalPanel(
                 #                        condition = "input.seq_choice_tutorial == 'Step 3. Taxonomy classification'",
                 #                        withMathJax(includeMarkdown("/home/imuser/text_files/tutorial_seq_3.Rmd")
                 #                        ),
                 #                      )
                 #                      ),
                 #             tabPanel(title="Taxonomy Analysis",
                 #                      withMathJax(includeMarkdown("/home/imuser/text_files/tutorial_DA.Rmd"))
                 #                      ),
                 #             tabPanel(title="Function Analysis",
                 #                      withMathJax(includeMarkdown("/home/imuser/text_files/tutorial_func.Rmd"))
                 #                      )
                 #                      
                 #             )
                 tags$iframe(style="height: 800px; width:90%; scrolling=yes;margin:0 100px", id = "ch2",
                             src=paste0("https://mochi.life.nctu.edu.tw/MOCHI_Tutorial_Ch_2_Web_new.pdf")
                             # src=paste0("http://", "140.113.83.24:8011", "/MOCHI_Tutorial_Ch_2.pdf")
                             # HTML('<div class="tutorial_ch2"> 
                             #       <iframe src="https://mochi.life.nctu.edu.tw/MOCHI_Tutorial_Ch_2.pdf" style="height:800px; width:90%; scrolling=yes;margin:0 100px>
                             #       </iframe>
                             #       </div>')
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
                      src="https://mochi.life.nctu.edu.tw/MOCHI_Tutorial_Ch_1.pdf")
                      # src="http://140.113.83.24:3811/test.pdf") %>% span()
          
        )
      # )
      
    ),
      # user results ----
      tabPanel(title = span("User Results", style = tabPanel_title_style),
               icon = icon("user", class = "user_icon"),
               tags$style(".user_icon {color: white}"),
               
               sidebarLayout(
                 fluid = T,
                column(width = 4,
                  # wellPanel(
                   style = "position:relative;background-color: #317EAC; border: none; border-radius: 5px; color: white;font-size: 20px;width:500px;padding:10px",
                   # strong("Inspect your results", style = "font-size:20px;color:white"),
                   strong("Show the results", style = "font-size:24px;color:white;top:20px"),
                   p("Your results will be deleted after two weeks."),
                   textInput(inputId = "job_id",
                             label = strong("Job ID", style = "font-size:20px;color:white"),
                             placeholder = "Input your job id",
                             width = "200px"),
                   actionButton(inputId = "submit_id",
                                label = "Submit",
                                icon = icon("chevron-circle-up"))
                 # )
                 ),
                 mainPanel(
                   # width = 7,
                   # style = "position: relative;",
                   column(
                     width = 12,
                     # wellPanel(
                       style = "background-color: white;",
                       uiOutput(outputId = "users_results_download",inline = T) %>% shinyjs::hidden()
                     # )
                     
                   )
                   
                     
                       
                                    
                 )
               )
               )
      

      
    ), # navbarPage
    
    # footer -----------
    
    tags$footer(
      tags$span(
        tags$a("Molecular Bioinformatics Lab", href = "https://fullofbeans.nctu.edu.tw/?page_id=333&lang=en", style = "color: white", target = "_blank"), 
        ", National Chiao Tung University, Taiwan 300, R.O.C."),
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

