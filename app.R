## Title: CoV.FunGeST, version Beta 0.1 Version (March 18, 2021)
## Author: Sandrine Imbeaud
## Contact: sandrine.imbeaud@inserm.fr
## FUNctional GEnomics of Solid Tumors - FunGeST
## INSERM U1138, EQ28
## Centre de Recherche des Cordeliers, 15 rue de l'Ecole de Médecine, 75006 Paris
## http://zucmanlab.com/our-lab-fungest/


# Load Package ----

library(tidyverse)
library(knitr)
library(ggtree)
library(tidytree)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(treeio)
library(g3viz)
library(janitor)
library(lubridate)
library(rcartocolor)
library(ggtext)
library(shadowtext)
library(plotly)


# Define UI ----Warning: E
ui <-  fluidPage(shinyjs::useShinyjs(),

titlePanel("COVID datasets"),
    sidebarLayout(
       sidebarPanel(width = 3,
          fluidRow(offset=0,
               column(12,


      # Input: Select separator ----
      radioButtons(inputId = 'sep', label = 'Separator',
                   choices = c(Comma=',',Semicolon=';',Tab='\t'), selected = '\t'),

      # Input: Input variant annotation (separator)as nexstrain format) ----
      prettySwitch(inputId = "Idwithvariant",
                   label = "including variants",
                   status = "success",
                   value = TRUE,
                   fill = TRUE),
      
      ## Input: Select a file ----
       fileInput("file1", "Input your annotation file", multiple = FALSE,
                 accept = c(".txt",".csv"),
                 width = NULL, buttonLabel = "Browse...",
                 placeholder = "No file selected"),

       selectInput("series", "Choose Annotation:", choices=c()),
                conditionalPanel(condition = "input.series == 'Check variants'",
                                 selectInput(inputId = "variantlist",label = "Select your Variants :",
                                             choices = c(), width="80%", multiple = TRUE),
                                 awesomeRadio(
                                   inputId = "modevariants",
                                   label = "Mode of selection",
                                   choices = c("OR", "AND"),
                                   selected = "AND",
                                   inline = TRUE,
                                   checkbox = TRUE
                                 ),
                                 tags$hr()
                      ),
       fileInput("file2", width='80%',
                 label="Input your tree formatted file",accept=c(".newick", ".nw")),

       knobInput(
         inputId = "height",
         label = "Select size of the plot:",
         value = 25,
         min = 0,
         max = 100,
         displayPrevious = TRUE,
         width="100px",
         lineCap = "round",
         fgColor = "#428BCA",
         inputColor = "#428BCA"
       ),

       pickerInput(
         inputId = "plot_type",
         label = "Choose a dendogram type",
         choices = c("rectangular","slanted","fan","circular",
                "radial","unrooted","equal_angle","daylight"),
         select=c("rectangular"),
         options = list(
           list(title = "Please Select plot type"))
       )
       ))),

   mainPanel(width = 9,
      tabsetPanel(type="tab",
                  tabPanel("CoV clades timeline", plotOutput("p.clades")),
                  tabPanel("CoV clades distribution",
                           column(2,
                                  pickerInput("subset_CoV","Zoom on:",choices = c())),
                           tags$hr(),
                           tags$hr(),
                           tags$hr(),
                           tags$hr(),
                           plotlyOutput("p.dist.clades"),
                           plotlyOutput("p.dist.other"),
                           plotlyOutput("p.dist.other2"),
                           plotlyOutput("p.dist.other3"),
                           ),
                  tabPanel("tree plot",
                           fluidRow(
                            column(6,
                                    tags$hr(),
                                   conditionalPanel(condition = "input.series != 'Check variants'",
                                    selectInput("series2", "Highlight a specific annotation:", choices=c(),  multiple = TRUE))),
                                    plotOutput("p.tree"))),
                  tabPanel("annot",
                           fluidRow(
                             column(12,
                                        div(style="display:inline-block; float:right",downloadButton('download',"Download the data")),
                                    dataTableOutput("my_table")))),
                  tabPanel("variants",
                           fluidRow(
                             column(12,
                                    downloadButton('download2',"Download the data"),
                                    dataTableOutput("my_variants")))),
                  tabPanel("GISAID", 
                           wellPanel(
                             radioButtons(inputId = "submit", "Submit to GISAID", c("yes", "no"), selected = "no", inline = TRUE)
                           ),
                           conditionalPanel(condition = "input.submit == 'no'"),
                           conditionalPanel(condition = "input.submit == 'yes'",                 
                                    div(style="display:inline-block; float:right",downloadButton('downloadtosubmit',"Download to submit GISAID")),
                                    div(style = "font-size: 12px",textInput("Submitter", "Type Submitter name", value = "")),
                                    div(style = "font-size: 12px",textInput("Location", "Type Location (Continent/Country/Region/City)", value = "Europe / France / Ile-de-France / Paris")),
                                    div(style = "font-size: 12px",textInput("SubLab", "Type Submitting lab", value = "")),
                                    div(style = "font-size: 12px",textInput("SubAddress", "Type Submitting Address lab", value = "")),
                                    div(style = "font-size: 12px",textInput("Authors", "Type list of Authors", value = "")),
                                    tags$br(),
                                    div(style = "font-size: 12px",textInput("Type", "Type of virus", value = "betacoronavirus")),
                                    div(style = "font-size: 12px",textInput("Host", "Specify Host", value = "Human")),
                                    div(style = "font-size: 12px",textInput("Fasta", "Type FASTA filename", value = "all_sequences.fasta")),
                                    tags$br(),
                                    div(style = "font-size: 12px",selectInput("Lab", "Select column that describes Originating lab", choices=c(),  multiple = FALSE)),
                                    div(style = "font-size: 12px",selectInput("Address", "Select column that describes Address lab", choices=c(),  multiple = FALSE)),
                                    div(style = "font-size: 12px",textInput("Prefix", "Type common prefix for all sequences", value = "hCoV-19/France")),
                                    div(style = "font-size: 12px",selectInput("Suffix", "Select column that describes sequence name", choices=c(), selected="name", multiple = FALSE)),
                                    div(style = "font-size: 12px",selectInput("Status", "Select column that describes Patient status", choices=c(),  multiple = FALSE)),
                                    div(style = "font-size: 12px",textInput("Passage", "Type Passage details/history", value = "Original")),
                                    div(style = "font-size: 12px",textInput("Seq", "Specify Sequencing technology", value = "Illumina Miseq")),
                                    div(style = "font-size: 12px",textInput("Assembly", "Specify Assembly method", value = "Geneious Prime 2021.0.3")),
                                    div(style = "font-size: 12px",textInput("Coverage", "Specify coverage", value = "")),
                                    )),
                  tabPanel("about",
                           fluidRow(
                               column(
                                 tags$br(),  tags$br(),
                            h4("About"),
                            h5("This dashboard shows recent sequencing data of the COVID-19 pandemic (source: HEGP, France)"),
                            h5("The latest sequencing data on the COVID-19 are regularly uploaded and displayed as
                             timeline, summary tables and phylogenic trees"),
                            h5("Beta 0.1 Version (March 18, 2021)"),

                            tags$br(),
                            h4("Sources"),
                            h5("PharmD PhD David Veyer"),
                            h5("PharmD PhD Hélène Péré"),
                            h5("MD Nicolas Robillard"),
                            h5("Unité de Virologie, Service de Microbiologie @, HEGP, Paris, France"),
                            tags$br(),
                            h5("MD Maxime Wack"),
                            h5("Département d'Informatique Médicale, Biostatistiques et Santé Publique @, HEGP, Paris, France"),
                             tags$br(),
                           h4("Developer"), "PhD Sandrine Imbeaud | Bioinformatics @",
                  tags$a(href = "http://zucmanlab.com/", "FunGeST lab"), "|",
                  tags$a(href = "http://www.crc.jussieu.fr/index.html", "Centre de Recherche des Cordeliers"), "|",
                  tags$a(href = "https://www.inserm.fr/", "Inserm"), "|",
                  tags$a(href = "https://github.com/FunGeST", "Github"),
                           h5("Contact: sandrine.imbeaud(@)inserm.fr"),
                  width = 12,
                  style = "padding-left: 20px; padding-right: 20px; padding-bottom: 40px; margin-top: -15px;"),
                  width = 12,
                  style = "padding: 15px"
                  ))

))))

server<-function(session, input,output){
  options(shiny.maxRequestSize=40*1024^2)


  data1 <- reactive({
    validate(need(input$file1,""))
    #req(input$file2)
    if (is.null(input$file1)) {
      return(NULL)
    } else {
    # ext <- tools::file_ext(input$file1$datapath)
    # validate(need(ext == "csv", "Please upload a csv file"))
   df= read.delim(input$file1$datapath, header=T, sep=input$sep, na.strings="")
   tmp=NULL
   tmp=grep("date_prel", colnames(df))
   if (length(tmp)>0) {df[,tmp]=as.Date(df[,tmp], "%m/%d/%Y")}

   tmp=NULL
   tmp=grep("sexe", colnames(df))
   if (length(tmp)>0) {colnames(df)[tmp]="gender"}
   
   tmp=NULL
   tmp=grep("virus", colnames(df))
   if (length(tmp)==0) {virus="sars_cov_2";df=cbind(df, as.character(virus)); colnames(df)[ncol(df)]="virus"}
   
   tmp=NULL
   tmp=grep("clade", colnames(df))
   if (length(tmp)==0) {clade=NA; df=cbind(df, as.character(clade)); colnames(df)[ncol(df)]="clade"}
   
   tmp=NULL
   tmp=grep("pangolin_lineage", colnames(df))
   if (length(tmp)==0) {pangolin_lineage=NA; df=cbind(df, as.character(pangolin_lineage)); colnames(df)[ncol(df)]="pangolin_lineage"}
   
   tmp=NULL
   tmp=grep("UH", colnames(df))
   if (length(tmp)==0) {UH=NA; df=cbind(df,as.character(UH)); colnames(df)[ncol(df)]="UH"}
  
   tmp=NULL
   tmp=grep("nb_labo", colnames(df))
   if (length(tmp)==0) {nb_labo=NA; df=cbind(df,as.character(nb_labo)); colnames(df)[ncol(df)]="nb_labo"}
   print(head(tmp))
   print(head(df))
   #print(str(df))
   
   return(df)

}

  })

observe({
  updateSelectInput(session,"series",choices=c(colnames(data1())[-1],"Check variants"))
  updatePickerInput(session,"subset_CoV",choices=c(colnames(data1())[-1]), selected="virus")
  updateSelectInput(session,"Lab",choices=c(colnames(data1())), selected="")
  updateSelectInput(session,"Suffix",choices=c(colnames(data1())), selected="name")
  updateSelectInput(session,"Status",choices=c(colnames(data1())), selected="")
  updateSelectInput(session,"Address",choices=c(colnames(data1())), selected="")
  })

  data3 <- reactive({
    validate(need(input$file2,""))
    #req(input$file1)
    if (is.null(input$file2)) {
      return(NULL)
    } else {
    ext <- tools::file_ext(input$file2$datapath)
    validate(need(ext %in% c("newick", "nw"), "Please upload a newick file"))
    tree=treeio::read.newick(input$file2$datapath)
    return(tree)}
  })

  my_Idwithvariant<- reactive({
    input$Idwithvariant
  })
  
  my_submission<- reactive({
    input$submit
  })

   my_series <- reactive({
    input$series
   })

observe({
      if(my_series()!="") {
        if(my_series()!="Check variants") {
          updateSelectInput(session,"series2",choices=data1() %>% select(my_series()), selected="")}}
    })

   my_series2 <- reactive({
     input$series2
   })

   my_height = reactive({
    input$height
   })

   my_plot = reactive({
     input$plot_type
   })

   my_variantlist <- reactive({
     input$variantlist
   })

   my_subset_CoV<- reactive({
     input$subset_CoV
   })

   my_mode<- reactive({
     input$modevariants
   })

   my_submit_dta <- reactiveVal()
   gg.tree= reactiveVal()

 ## format GISAID submission table
   my_Submitter <- reactive({  input$Submitter })
   my_Location <- reactive({  input$Location })
   my_SubLab<- reactive({  input$SubLab })
   my_SubAddress<- reactive({  input$SubAddress })
   my_Authors<- reactive({  input$Authors })
   my_Type<- reactive({  input$Type })
   my_Host<- reactive({  input$Host })
   my_Fasta<- reactive({  input$Fasta })
   my_Lab<- reactive({  input$Lab })
   my_Address<- reactive({  input$Address })
   my_Prefix<- reactive({  input$Prefix })
   my_Suffix<- reactive({  input$Suffix })
   my_Status<- reactive({  input$Status })
   my_Passage<- reactive({  input$Passage })
   my_Seq<- reactive({  input$Seq })
   my_Assembly<- reactive({  input$Assembly })
   my_Coverage<- reactive({  input$Coverage })
   
   
   observe({
     if(my_submission() == "yes") {
       if (my_submission()!='') {t1=NULL; t1=grep(my_Suffix(), colnames(data1()));suffix.data=data1()[,t1]}
       if (my_submission()=='') {suffix.data=NA}
       
       if (my_Status()!='') {t2=NULL; t2=grep(my_Status(), colnames(data1())); status.data=data1()[,t2]}
       if (my_Status()=='') {status.data=NA}
     
       if (my_Lab()!='') {t3=NULL; t3=grep(my_Lab(), colnames(data1()));lab.data=data1()[,t3]}
       if (my_Lab()=='') {lab.data=NA}
       
       if (my_Address()!='') {t4=NULL;  t4=grep(my_Address(), colnames(data1())); address.data=data1()[,t4]}
       if (my_Address()=='') {address.data=NA}
       gisaid_table=cbind(data1(), suffix.data, status.data, lab.data, address.data)
       print(head(gisaid_table))
     
       tp=gisaid_table %>%
       select(name, gender,date_prel,DDN,suffix.data, status.data, lab.data, address.data)  %>%
      #  # filter(name!="MN908947.3") %>%
      #  # filter(!grepl("control|Control", nb_labo)) %>%
      mutate(givendate=year(as.Date(date_prel, "%m/%d/%Y"))) %>%
      mutate(DDN=year(as.Date(as.character(DDN),format="%Y")))%>%
      mutate('Patient age' = givendate - DDN) %>%
      #  # mutate(UH=ifelse(is.na(UH), "HEGP - Laboratoire de Virologie", UH)) %>%
      mutate(prefix.data=my_Prefix())  %>%
      mutate(Submitter=my_Submitter(),
               'FASTA filename'=my_Fasta(),
               'Virus name'=paste0(prefix.data,"/",suffix.data),
                Type= my_Type(),
               'Passage details/history'=my_Passage(),
                Location=my_Location(),
                Host=my_Host(),
                gender=gsub("F","Female", gsub("M", "Male", gender)),
               'Patient status'=status.data,
               'Sequencing technology'=my_Seq(),
               'Assembly method'= my_Assembly(),
               'Originating lab'=lab.data,
               Address=address.data,
               '(Submitting)Address'=my_SubAddress(),
               'Submitting lab'=my_SubLab(),
               Coverage=my_Coverage(),
               Authors=my_Authors()
               ) %>%
            mutate('Additional location information'=NA, 'Additional host information'=NA, 'Sampling Strategy'=NA,
               'Specimen source'=NA, 'Outbreak'=NA,'Last vaccinated'=NA,'Treatment'=NA,
               'Sample ID given by the originating laboratory'=NA,'Sample ID given by the submitting laboratory'=NA,
               'Comment'=NA,"Comment Icon"=NA) %>%
       mutate(gender=ifelse(is.na(gender), "unknown", gender)) %>%
       rename('Collection date'=date_prel, Gender=gender)
       
       tp=tp[,c("name","Submitter","FASTA filename","Virus name","Type","Passage details/history",
      "Collection date","Location","Additional location information", "Host", "Additional host information",
      "Sampling Strategy", "Gender","Patient age","Patient status",
      "Specimen source", "Outbreak","Last vaccinated", "Treatment","Sequencing technology","Assembly method",
      "Coverage","Originating lab","Address","Sample ID given by the originating laboratory","Submitting lab",
      "(Submitting)Address","Sample ID given by the submitting laboratory","Authors","Comment","Comment Icon")]

      my_submit_dta(tp)

     }})





  ## Table of annotations --------
  output$my_table <- renderDataTable({data1()},
                                     options = list(orderClasses = TRUE,
                                     LengthMenu = c(5, 25, 50, 100,250),
                                     pageLength =10))

  ## Save annotations
  output$download <- downloadHandler(
    filename = function(){"seq_results.txt"},
    content = function(fname){
      write.table(data1(), fname, row.names=FALSE, sep ="\t")
    })


  output$downloadtosubmit <- downloadHandler(
    filename = function(){"tosubmit_GISAID.txt"},
    content = function(fname){
      write.table(my_submit_dta(), fname, row.names=FALSE, sep ="\t")
    })

  ## Table of variants --------
observe({
    if(my_Idwithvariant() == TRUE) {
      var.tab = reactive({

      data1() %>%
        tidyr::unite("z", aaSubstitutions:aaDeletions, sep=",", remove = FALSE, na.rm=TRUE) %>%
        separate_rows(z, sep=",") %>%
        separate(z, c("Domain", "Value"), ":", extra = "merge") %>%
        dplyr::mutate(Variant_Classification ="Missense_Mutation") %>%
        dplyr::mutate(AA.pos = parseProteinChange(Value, Variant_Classification)) %>%
        arrange(Domain, AA.pos) %>%
        dplyr::rename(Variant=Value) %>%
        select(name,nb_labo,virus,clade,pangolin_lineage,Domain,Variant,AA.pos) %>%
        as.data.frame()

      # var.tab2=data1() %>% separate_rows(aaDeletions, sep=",") %>%
      #       separate(aaDeletions, c("Domain", "Value"), ":", extra = "merge") %>%
      #       dplyr::mutate(Variant_Classification ="Missense_Mutation") %>%
      #       dplyr::mutate(AA.pos = parseProteinChange(Value, Variant_Classification)) %>%
      #       arrange(Domain, AA.pos) %>%
      #       dplyr::rename(Variant=Value) %>%
      #       select(name,code,virus,clade,pangolin_lineage,Domain,Variant,AA.pos) %>%
      #       filter(!is.na(Variant)) %>%
      #       as.data.frame()
      #
      # var_tab=rbind.fill(var.tab1,var.tab2) %>% arrange(Domain, AA.pos)

          })
        #

        #   df0 = df0 %>%
        #     arrange(Domain, AA.pos)



        var=unique(paste0(var.tab()$Domain,":",var.tab()$Variant))
        #print(var)
        var=gsub(":NA","", var)
        updateMultiInput(session,"variantlist",choices=var)


        output$my_variants <- renderDataTable({
        var.tab()
         }, options = list(orderClasses = TRUE,
                 LengthMenu = c(5, 25, 50, 100,250),
                 pageLength =10))

      output$download2 <- downloadHandler(
      filename = function(){"var_results.txt"},
      content = function(fname){
        write.table(var.tab(), fname, row.names=FALSE, sep="\t")
      })
}
  })




observe({output$p.tree <- renderPlot({

  if(my_Idwithvariant() == FALSE) {
    if (my_series() == "Check variants") {
      annot= data1() %>%
        dplyr::rename(seq=name) %>%
        as.data.frame() %>%
        mutate(cat="no annot provided") %>%
        select(seq, cat)
      colnames(annot)=c("seq","cat")}}

  if(my_Idwithvariant() == TRUE) {
  if (my_series() == "Check variants") {
     if (my_mode()=="OR") {
       mut=paste(my_variantlist(), collapse = "|")
       #print(mut)

       annot= data1() %>%
         dplyr::rename(seq=name) %>%
         as.data.frame() %>%
         mutate(cat = ifelse(grepl(mut, aaSubstitutions), "yes",
                          ifelse(grepl(mut, aaDeletions), "yes","no"))) %>%
         select(seq, cat)
       colnames(annot)=c("seq","cat")}

    if (my_mode()=="AND") {
      df=data1() %>%
        tidyr::unite("z", aaSubstitutions:aaDeletions, sep=",", remove = FALSE, na.rm=TRUE) %>%
        select(name, z) %>%
        rowwise() %>%
        filter(all(sapply(my_variantlist(), function(pat) grepl(pat, z))))
       my_name=df$name

       annot= data1() %>%
         dplyr::rename(seq=name) %>%
         as.data.frame() %>%
         mutate(cat = ifelse(seq %in% my_name, "yes","no")) %>%
         select(seq, cat)
       colnames(annot)=c("seq","cat")}

    }

  if (my_series() != "Check variants") {

         if (is.null(my_series2())) {
            annot= data1() %>%
            dplyr::rename(seq=name) %>%
            as.data.frame() %>%
            select(seq, my_series())
            colnames(annot)=c("seq","cat")
           }

         ## in order to hightlight subannotation in tree
         if (!is.null(my_series2())) {
            annot= data1() %>%
            dplyr::rename(seq=name) %>%
            as.data.frame()

            # select column with series annotation to extract modalities
            col=grep(my_series(), colnames(annot))
            annot$cat=ifelse(annot[,col] %in% my_series2(), as.character(annot[,col]), NA)
            annot =annot %>%  select(seq, cat)}
  }
}


    data3() %>%
      as_tibble %>%
      dplyr::mutate(label=gsub("_L001_R_001\\(trimmed\\)","",label)) %>%
      #left_join(annot) %>%
      as.treedata   -> tree
      print(tree)


    print(my_plot())

    p <- ggtree(tree, layout=my_plot())
    #p <- ggtree(tree)
    gg.tree = p %<+% annot + geom_tiplab(aes(fill = cat),
                                       color = "black", # color for label font
                                       geom = "label",  # labels not text
                                       label.padding = unit(0.15, "lines"), # amount of padding around the labels
                                       label.size = 0) + # size of label border) +
                              #geom_tree(layout=my_plot()) +
                              theme_tree2(legend.title = element_blank(), # no title
                                          legend.key = element_blank())

    print(gg.tree)},
            height=as.numeric(my_height()*2500/50))

  }) ## end of plot tree


observe({output$p.clades <- renderPlot({
  ## refer to https://medium.com/epfl-extension-school/from-static-to-animated-time-series-the-tidyverse-way-d696eb75f2fa
  ## formatting cumulative count

  annot = data1()%>%
    dplyr::mutate(date_prel=as.Date(date_prel, "%m/%d/%Y")) %>%
    dplyr::mutate(date=ymd(date_prel))  %>%
    filter(!is.na(date)) %>%
    arrange(date) %>%
    dplyr::mutate(days_since_first = (date-first(date))) %>%
    select(name, days_since_first, clade, date) %>%
    group_by(clade, days_since_first, date) %>%
    summarise(cases = n())

  annot2 = annot %>%
    group_by(clade) %>%
    dplyr::mutate(cum_cases = cumsum(cases)) %>%
    dplyr::mutate(clade = fct_infreq(clade))

  annot3 = annot2 %>%
    dplyr::mutate(color_label = forcats::fct_collapse(clade,
                                                      "black" = "19A",
                                                      "black" = "19B",
                                                      "#6699CC" = "20A",
                                                      "#661100" = "20A.EU2",
                                                      "#882255" = "20B",
                                                      "#CC503E" = "20C",
                                                      "#999933" = "20D",
                                                      "#44AA99" = "20E (EU1)",
                                                      "#CC503E" = "20F",
                                                      "#1D6996" = "20G",
                                                      "#332288" = "20H/501Y.V2",
                                                      "#332288" = "20H (Beta, V2)",
                                                      "#117733" = "20I/501Y.V1",
                                                      "#117733" = "20I (Alpha, V1)",
                                                      "#DDCC77" = "20J/501Y.V3",
                                                      "#DDCC77" = "20J (Gamma, V3)",
                                                      "#855C75" = "21A (Delta)",
                                                      "#855C75" = "21B (Kappa)",
                                                      "#855C75" = "21C (Epsilon)",
                                                      "#855C75" = "21D (Eta)",
                                                      "#888888" = "21E (Theta)",
                                                      "#D63D32" = "21F (Iota)", other_level = "grey90"),
                  color_label = fct_relevel(color_label, "grey90")) %>%
    arrange(color_label) %>%
    #dplyr::mutate(clade = fct_inorder(clade)) %>%
    dplyr::mutate(clade_label = ifelse(color_label == "grey90", "", as.character(clade))) %>%
    as.data.frame()

  ## Set label
  annotations <- annot3 %>%
    group_by(clade) %>%
    filter(days_since_first == max(days_since_first))
  #mutate(label_clade = ifelse(clade %in% levels(clade)[1:12],
  #                              as.character(clade), ""))


  ## plot ----
  min <- as.Date(min(annot3$date))
  max <- as.Date(max(annot3$date)+4)
  p=ggplot(data = annot3,
           mapping = aes(x = date, #days_since_first,
                         y = cum_cases,
                         color = color_label,
                         group = clade)) +
    #add the lines to the chart
    geom_line(size = 0.7, alpha =0.9,
              mapping = aes(linetype = "1")) +
    # set the x axis to have a little more space for annotations
    scale_x_date(date_breaks = "weeks", date_labels = "%b %d,%Y", limits = c(min, max)) +
    #scale_x_continuous(limits = c(0, max(annot3$days_since_first) +20))+
    # set the y axis to be log10
    scale_y_log10(expand = expansion(add = c(0,0.1)),
                  #breaks=c(10, 50, 100,  200, 500)
    ) +
    theme_minimal() +
    scale_color_identity()+
    shadowtext::geom_shadowtext(data = annotations,
                                mapping = aes(x = date, #days_since_first,
                                              y = cum_cases,
                                              label = clade_label),
                                hjust=+0.5, vjust = 0, bg.color = "white") +
    # customise the theme a bit more
    theme(
      axis.text.x=element_text(angle=60, hjust=1),
      panel.grid.minor = element_blank(),
      # remove legend
      legend.position = "none",
      # change margin size
      plot.margin = margin(3,15,3,3,"mm"),
      # set the caption to be written as HTML code
      plot.caption = ggtext::element_markdown()
    ) +
    # allow points to be plotted outside of plot area
    coord_cartesian(clip = "on") +
    # customise all the labels in the chart
    labs(x = "Sampling Date",
         y = "Cumulative Number of Cases",
         title = "Total number of COVID-19 cases",
         #subtitle =  "[day0 = 23th Novembre 2020]",
         caption = "<span style = 'font-size:8pt;color:#888888'>Data Source: HEGP </span>")
  #p= p+transition_reveal(days_since_first)

  print(p)

})
})  ## end of plot timeline

observe({output$p.dist.clades <- renderPlotly({
  ## https://ramikrispin.github.io/coronavirus_dashboard/
   ## distribution of clades
 if (is.null(my_subset_CoV())) {
  clade_all = data1()%>%
      #filter(!(name=="MN908947.3" |  nb_labo=="control_neg" | nb_labo=="control_pos"	| nb_labo=="Control_pos"))  %>%
        select(name, clade) %>%
        group_by(clade) %>%
        summarise(cases = n()) %>%
        rename(labels = clade) %>%
        as.data.frame()
   }

  if (!is.null(my_subset_CoV())) {

  clade_all = data1()%>%
    #filter(name!="MN908947.3" & nb_labo!="control_neg" & nb_labo!="control_pos"	& nb_labo!="Control_pos")  %>%
    select(name, my_subset_CoV()) %>%
    rename(labels = 2) %>%
    group_by(labels) %>%
    summarise(cases = n()) %>%
    as.data.frame()
  #print(head(data1()))
  }

    #print(clade_all)

    p0 = plotly::plot_ly(
      data = clade_all,
      type= "treemap",
      values = ~cases,
      labels= ~ labels,
      parents=  "sars_cov_v2",
      domain = list(column=0),
      name = "Confirmed",
      textinfo="label+value+percent parent"
    )

    print(p0)


})
}) ## end of plot distribution


observe({output$p.dist.other3 <- renderPlotly({

  if (!is.null(my_subset_CoV())) {
    ALL = data1()%>%
      #filter(name!="MN908947.3" & nb_labo!="control_neg" & nb_labo!="control_pos"	& nb_labo!="Control_pos")  %>%
      select(name, my_subset_CoV(), clade) %>%
      rename(labels = 2) %>%
      filter(!is.na(labels) & labels!="") %>%
      group_by(labels, clade) %>%
      summarise(cases = n()) %>%
      as.data.frame()

 #print(head(ALL))
p1 = plotly::plot_ly(
  data = ALL %>% dplyr::filter(clade == "19A"),
  type= "treemap",
  values = ~cases,
  labels= ~ labels,
  parents=  ~clade,
  domain = list(column=0),
  name = "Confirmed",
  textinfo="label+value+percent parent"
)
p1 = p1 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(clade == "19B"),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=1),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p1 = p1 %>%  plotly::layout(grid=list(columns=2, rows=1))
  }
  
})})
  
observe({output$p.dist.other2 <- renderPlotly({

  if (!is.null(my_subset_CoV())) {
    ALL = data1()%>%
      #filter(name!="MN908947.3" & nb_labo!="control_neg" & nb_labo!="control_pos"	& nb_labo!="Control_pos")  %>%
      select(name, my_subset_CoV(), clade) %>%
      rename(labels = 2) %>%
      filter(!is.na(labels) & labels!="") %>%
      group_by(labels, clade) %>%
      summarise(cases = n()) %>%
      as.data.frame()
    
p2 = plotly::plot_ly(
  data = ALL %>% dplyr::filter(grepl("20A", clade)),
  type= "treemap",
  values = ~cases,
  labels= ~ labels,
  parents=  ~clade,
  domain = list(column=0),
  name = "Confirmed",
  textinfo="label+value+percent parent"
)
p2 = p2 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(grepl("20B", clade)),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=1),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p2 = p2 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(grepl("20C", clade)),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=2),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p2 = p2 %>%
  plotly::add_trace(data = ALL %>% dplyr::filter(grepl("20D", clade)),
                    type= "treemap",
                    values = ~cases,
                    labels= ~ labels,
                    parents=  ~clade,
                    domain = list(column=3),
                    name = "Active",
                    textinfo="label+value+percent parent"
  )
p2 = p2 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(grepl("20E", clade)),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=4),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p2 = p2 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(grepl("20F", clade)),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=5),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p2 = p2 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(grepl("20G", clade)),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=6),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p2 = p2 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(grepl("20H", clade)),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=7),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p2 = p2 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(grepl("20I", clade)),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=8),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p2 = p2 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(grepl("20J", clade)),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=9),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p2 = p2 %>%  plotly::layout(grid=list(columns=10, rows=1))
}
})})


observe({output$p.dist.other <- renderPlotly({
  
if (!is.null(my_subset_CoV())) {
  ALL = data1()%>%
    #filter(name!="MN908947.3" & nb_labo!="control_neg" & nb_labo!="control_pos"	& nb_labo!="Control_pos")  %>%
    select(name, my_subset_CoV(), clade) %>%
    rename(labels = 2) %>%
    filter(!is.na(labels) & labels!="") %>%
    group_by(labels, clade) %>%
    summarise(cases = n()) %>%
    as.data.frame()
  
p3 = plotly::plot_ly(
  data = ALL %>% dplyr::filter(grepl("21A", clade)),
  type= "treemap",
  values = ~cases,
  labels= ~ labels,
  parents=  ~clade,
  domain = list(column=0),
  name = "Confirmed",
  textinfo="label+value+percent parent"
)
p3 = p3 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(grepl("21B", clade)),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=1),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p3 = p3 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(grepl("21C", clade)),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=2),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p3 = p3 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(grepl("21D", clade)),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=3),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p3 = p3 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(grepl("21E", clade)),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=4),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p3 = p3 %>%
  plotly::add_trace( data = ALL %>% dplyr::filter(grepl("21F", clade)),
                     type= "treemap",
                     values = ~cases,
                     labels= ~ labels,
                     parents=  ~clade,
                     domain = list(column=5),
                     name = "Active",
                     textinfo="label+value+percent parent"
  )
p3 = p3 %>%  plotly::layout(grid=list(columns=6, rows=1))
}

})
}) ## end of plot distribution 2



 }  ## end of server


# Run the app ----

shinyApp(ui = ui, server = server)
