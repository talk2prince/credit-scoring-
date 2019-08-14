library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(rhandsontable)
library(plotly)
# hypothesis_DF <- data.frame(Crisp.ID = c(1,2),UID = c("CM1-1","CM1-2"),Description = c("Desc1","Desc2"),Status = c("High","Medium"),Created.By = c("ABC","XYZ"),Created.On = c("2019-04-10","2019-04-11"))
# hypothesis_DF$Created.On <- as.Date(hypothesis_DF$Created.On)
# hypothesis_DF$Created.By <- as.character(hypothesis_DF$Created.By)
# hypothesis_DF$Description <- as.character(hypothesis_DF$Description)
# hypothesis_DF$Status <- as.character(hypothesis_DF$Status)
# hypothesis_DF$UID <- as.character(hypothesis_DF$UID)
# hypothesis_DF$Crisp.ID <- as.integer(hypothesis_DF$Crisp.ID)
trend_data <- read.csv("trend_data.csv")
colnames(trend_data)[1] <- "Industry.Name"
hypothesis_DF <- read.csv("hypothesis.csv",nrows = 3)
colnames(hypothesis_DF)[1] <- "Hypothesis"
data_schedule <- read.csv("data_schedule.csv")
colnames(data_schedule)[1] <- "Job Name"
job_performance <- read.csv("job_performance.csv")
colnames(job_performance)[1] <- "Job Name"
credit_master_data1 <- read.csv("loan_cut2.csv")
library(scales)
library(plyr)
library(tidyverse)
library(plotly)
library(lazyeval)
library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
library(funModeling)
library(scales)
library(tidyverse)
library(corrplot)
library(GGally)
library(caret)
library(pROC)
library(gbm)
library(choroplethr)
library(choroplethrMaps)
library(microbenchmark)
library(doParallel)
library(e1071)
# libraries_used <-
#   c("lazyeval", "readr","plyr" ,"dplyr", "readxl", "ggplot2",
#     "funModeling", "scales", "tidyverse", "corrplot", "GGally", "caret",
#     "pROC", "gbm", "choroplethr", "choroplethrMaps",
#     "microbenchmark", "doParallel", "e1071")
# lapply(libraries_used, require, character.only = TRUE)
loans<-read.csv('loan_cut.csv')


ui <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidPage(
      tabBox(width = 12,
      tabPanel(title = "CRISP Modeler",
      boxPlus(width = 12,collapsible = F,closable = F,
        h3(strong('Business Understanding')),
        panel(heading = 'Business Problem Definition',status = 'primary',
              tabsetPanel(
                tabPanel(
                  title = h4('Main'),
                  textAreaInput(width = '850px',height = '150px',inputId = 'main_description_credit',label = '',value = 'Development of credit scoring models, that would help business in assignment of Credit Limits, suggesting Security (Security Deposits, Bank Guarantees) in a way that STC benefits in
(i) increasing the revenues
(ii) minimizing the defaults
(iii) deploying auditable policies')
                ),
                tabPanel(
                  title = h4('Notes'),
                  textAreaInput(width = '850px',height = '150px',inputId = 'notes_description_credit',label = '',value = "15-Jan-2019
Calvin Smith -
question: can we add 'visibility in audit trail' (for model deployment) to the analytics scope ?
context: I met the CRO today to explain the context of our POC. She is excited about how 'PD next gen' will help
her improve the organization's evaluation of risk. A recent audit gave a 4/10 with 

10-Jan-2019
Michael Gilborn -
  question: can we increase Revenues in our biggest vertical 'Oil & Gas Producers', over the next 2 quarters ?
  context: The BU head is concerned about us continuously losing market share in this lucrative vertical, over the past year. However, our
marketing funds are limited. Susan, the 'credit control manager' believes we are under-weighted in this vertical, and is open to increase the
exposure.")
                )
              )        
        ),
        panel(heading = 'Hypothesis Master',status = 'primary',
              fluidRow(
                column(
                  width = 12,
                  actionBttn("edit_rhandsontable_credit",label = "Edit Table",icon = icon("fas fa-edit"),no_outline = T,color = "primary",style = "gradient",size = "sm"),
                  actionBttn("new_rhandsontable_credit",label = "New Row",icon = icon("fas fa-plus-circle"),no_outline = T,color = "success",style = "gradient",size = "sm"),
                  br()
                  
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  rHandsontableOutput("hypothesis_table_credit")
                )
              )
            )
       ),
    boxPlus(width = 12,collapsible = F,closable = F,
            h3(strong('Data Understanding')),
    panel(heading = 'Import Data',status = 'primary',
          fluidRow(
            column(
              width=4,
              fileInput("choose_file_credit",label = "Choose File",placeholder = "credit.csv")
            ),
            column(
              width = 4,
              br(),
              actionBttn('submit_credit',"Submit",color = 'default',size = 'sm',no_outline = TRUE,style='gradient')
            ),
            column(
              width = 4,
              br(),
              actionBttn('show_credit_data',"Show Data",color = 'default',size = 'sm',no_outline = TRUE,style='gradient')
            )
          ),
          conditionalPanel(condition = 'input.show_credit_data%2==1',
                           boxPlus(
                             width = 12,closable = F,collapsible = F,
                             fluidRow(
                               column(
                                 width = 12,
                                 br(),
                                 h4('Dataset File'),
                                 dataTableOutput('credit_data'),style = "height:500px; overflow-y: scroll;"
                               )
                               )
                           )
                           )
  ),
  conditionalPanel(condition = 'input.submit_credit%2==1',
                   
                   shinyWidgets::panel(heading = 'Filter Data',status = 'primary',
                                       fluidRow(
                                         column(
                                           width = 12,
                                           #tags$style(type="text/css", HTML("#ms>*{float: left; margin-right: 15px; height: 20px;} #test {height: 20px;}"))
                                           prettyCheckboxGroup(inputId = 'attribute_group_credit',label = 'Attributes',choices = colnames(credit_master_data1),inline = TRUE,selected = NULL,status = 'primary')
                                           
                                         ) ),
                                       fluidRow(
                                         column(
                                           width = 4,
                                           actionBttn(inputId = 'apply_filter_credit',label = 'Apply',size = 'sm',no_outline = TRUE,style='gradient')
                                           
                                         ),
                                         column(
                                           width = 4,
                                           actionBttn(inputId = 'clear_filter_credit',label = 'Clear',size = 'sm',no_outline = TRUE,style='gradient')
                                           
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           br(),
                                           dataTableOutput('filter_credit_table')
                                         )
                                         
                                       )
                                       ),
                   panel(heading = 'Exploratory Data Analysis',status = 'primary',
                         # fluidRow(
                         #   column(
                         #     width = 12,
                         #     plotlyOutput("loan_amount_box_plot")
                         #   )
                         # ),
                         # fluidRow(
                         #   column(
                         #     width = 12,
                         #     plotlyOutput("grade_subgrade_plot")
                         #   )
                         # ),
                         fluidRow(
                           column(
                             width = 12,
                             plotlyOutput("years_in_buisness")
                           )
                         ),
                         fluidRow(
                           column(
                             width = 12,
                             plotlyOutput("grade_wise_bar_chart")
                           )
                         ),
                         fluidRow(
                           column(
                             width = 12,
                             plotOutput('credit_scoring_plot1')
                           )
                         ),
                         fluidRow(
                           column(
                             width = 12,
                             plotOutput('credit_scoring_plot2')
                           )
                         ),
                         fluidRow(
                           column(
                             width = 12,
                             plotOutput('credit_scoring_plot3')
                           )
                         ),
                         fluidRow(
                           column(
                             width = 12,
                             plotOutput('credit_scoring_plot4')
                           )
                         ),
                         fluidRow(
                           column(
                             width = 6,
                             selectInput("select_trend_industry","Select Industry",trend_data$Industry.Name)
                           )
                         ),
                         fluidRow(
                           column(
                             width = 12,
                             plotlyOutput("trend_R_AR_NPL")
                           )
                         )
                    )
          )
),
boxPlus(
  width = 12,closable = F,collapsible = F,
  panel(heading = 'Model Building',status = 'primary',
        h3('Resampling'),
        fluidRow(
          column(
            width = 4,
            selectInput('method_selection','Method:',choices = c('cv','repeatedcv','LOOCV','LGOCV','BOOT'),selected = 'cv')  
          ),
          column(
            width = 4,
            textInput('test_train','Train/Test Ratio')  
          ),
          column(
            width = 4,
            selectInput('Model_Type','Model Type',c('Decision Tree','Log Reg','Random Forest'))
          )
          
        ),
        fluidRow(
          column(
            width = 4,
            textInput('number_times',"k value(k fold)")
          ),
          column(
            width = 4,
            br(),
            actionBttn(inputId = 'apply_resample',label = 'Apply',size = 'sm',no_outline = TRUE,style='gradient')
          )
        ),
        fluidRow(
          
          column(
            tags$style(type="text/css", '#result_resample tfoot {display:none;}'),
            
            h3('Resampling Result'),
            width = 12,
            br(),
            dataTableOutput('result_resample'),
            br()
          )
        ),
        fluidRow(
          column(
            width = 12,
            #plotOutput('rf_varimp')
            img(src = "var_imp_rf.PNG",height = 400, width = 600)
            
            
          )
        )
        
)),



boxPlus(
  width = 12,closable = F,collapsible = F,
  h3(strong("Model Performance")),
  fluidRow(
    column(
      width = 12,
      img(src = "thredhold_50_new.PNG",height = 350, width = 600),
      br()
    )
  ),
  # fluidRow(
  #   column(
  #     width = 12,
  #     img(src = "log_reg_comp.PNG",height = 350, width = 750)
  #   )
  #   
  # ),
  # fluidRow(
  #   column(
  #     width = 12,
  #     img(src = "model_rpart.png",height = 350, width = 750)
  #   )
  # ),
  fluidRow(
    column(
      width = 12,
      img(src ="without_ensemble.PNG" ,height = 350, width = 600)
      
    )
  )
  # fluidRow(
  #   column(
  #     width = 12,
  #     img(src = "model_rpart3.png",height = 350, width = 750)
  #     
  #   )
  # ),
  # fluidRow(
  #   column(
  #     width = 12,
  #     img(src = "model_rpart2.png",height = 350, width = 750)
  #   )
  # )
),

boxPlus(
  width = 12,closable = F,collapsible = F,
  panel(heading = 'Model Comparison',status = 'primary',
        br(),
        prettyCheckboxGroup(inputId = 'xy',inline = T,'Models:',choices = c('glm_1','glm_2','rpart','ensemble'),selected = c('glm_1','glm_2','rpart','ensemble'),status = 'primary'),
        br(),
        img(src = "model_comp_final.PNG",height = 400, width = 600)
        
        
        )
  
),

boxPlus(
  width = 12,closable = F,collapsible = F,
  shinyWidgets::panel(heading = 'Model Evaluation',status = 'primary',
                      rHandsontableOutput('model_result_output'),
                      br(),
                      h3('Result'),
                      tags$style(type="text/css", '#rf_output tfoot {display:none;}'),
                      dataTableOutput('rf_output')
  )
)
),
tabPanel(
  title = "Experiment",
  boxPlus(width = 12,collapsible = F,closable = F,
          h3(strong("Design of Experiment")),
          fluidRow(
            column(width = 8,
                   HTML("<b>Type of Experiment</b>: Response Surface Design"),
                   br(),
                   HTML("<b>Design Type</b>: Hybrid Design"),
                   br(),
                   br(),
                   h4(strong("Test v/s Control Strategy")),
                   HTML("<font color='green'><b>OBJECTIVE</b></font>"),
                   br(),
                   HTML('Maximize ARPU,CU<br>Minimize Default rate,assuming a fixed "Total Credit"'),
                   br(),
                   br(),
                   HTML("<font color='green'><b>TEST v/s CONTROL</b></font>"),
                   br(),
                   HTML("<b>TEST:</b> 10% of customers in each of the 3 segments (Oil and Gas, Mining, Chemicals)<br><b>CONTROL:</b> 90% of customers in those 3 segments"),
                   br(),
                   br(),
                   HTML("<font color='green'><b>TIME PERIOD</b></font>"),
                   br(),
                   HTML("<b>Pre Test:</b> 6 Months(Dec 1,2017-Apr 30,2018<br>"),
                   HTML("<b>Warm Up period:</b> 1 Month(May 1-31,2018)<br>"),
                   HTML("<b>Post test:</b> 6 Months(June 1,2018-Nov 30,2018)")
            ),
            column(
              width = 4,
              boxPlus(width = 12,closable = F,collapsible = F,
                      HTML("<font color='green'><b>DEPENDENT VARIABLES</b></font><br>"),
                      HTML("Default Rate<br>ARPU<br>Credit Utilization(CU)<br>Total Credit Limit(TCL)"),
                      br(),
                      br(),
                      HTML("<font color='green'><b>INDEPENDENT VARIABLES</b></font><br>"),
                      HTML("Credit Limit(CL)<br>"),
                      HTML("Security Deposit(BG)")
              )
            )
          )
  ),
  boxPlus(width = 12,closable = F,collapsible = F,
          h3(strong("Analysis of Experiment")),
          h4(strong("Highlights")),
          uiOutput("highlights_credit_scoring_app"),
          fluidRow(
            
            column(
              h3('Impact on Dependent Variables'),
              width = 4,
              selectInput("select_factor_graph","Dependent Variables",c("ALL","Default Rate","ARPU","Credit Utilization","Revenue"))
            ),
            column(
              width = 4,
              br(),
              br(),
              br(),
              selectInput("select_industry_graph","Select Segment",c("ALL","Oil And Gas Industry"))
            ),
            column(
              width = 1,
              br(),
              br(),
              br(),
              checkboxInput("test_check",label = "Test",value = T)
            ),
            column(
              width = 1,
              br(),
              br(),
              br(),
              checkboxInput("control_check",label = "Control",value = F)
            )
          ),
          fluidRow(
            column(
              width = 8,
              plotlyOutput("usage_trend_plotly")
            ),
            column(
              width = 4,
              h5(strong("A/B Testing")),
              rHandsontableOutput("A_B_testing_table")
            )
          )
    )
),
tabPanel(
 title = "Data",
 h3(strong("Job Scheduler")),
 rHandsontableOutput("job_schedule_table"),
 h3(strong("Job Performance")),
 rHandsontableOutput("job_performance_table")
)
)
)
)
)
server <- function(input, output, session) {
  react_credit <- reactiveValues()
  react_credit$hypothesis_DF <- hypothesis_DF
  react_credit$editing <- T
  
  observeEvent(input$edit_rhandsontable_credit,{
    react_credit$editing <- F
  })
  
  observeEvent(input$new_rhandsontable_credit,{
    react_credit$hypothesis_DF <- rbind(react_credit$hypothesis_DF,data.frame(Hypothesis = nrow(react_credit$hypothesis_DF)+1,Description = c(""),Priority = c("")))
  })
  
  output$hypothesis_table_credit <- renderRHandsontable({
    rhandsontable(react_credit$hypothesis_DF,rowHeaders = F,readOnly = react_credit$editing,selectCallback = T) %>%
      hot_cols(manualColumnResize = T)
  })
  
  output$credit_data<-renderDataTable({
    credit_master_data1
  })
  
  observeEvent(input$apply_filter_credit,{ 
    output$filter_credit_table<-renderDataTable({
      credit_master_data1[,c(input$attribute_group_credit),drop = FALSE]
    })
})
  
  empty<-data.frame(warning = c("Please select some attribute"))
  observe(
    if(input$clear_filter_credit >0){
      shinyWidgets::updatePrettyCheckboxGroup(session=session,inputId = 'attribute_group_credit',label = 'Attributes',choices = colnames(credit_master_data1),inline = TRUE,selected = NULL)
      output$filter_credit_table<-renderDataTable({
        empty
      },escape = TRUE)
    }
    
  )
  
  observeEvent(input$submit_credit,{
    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "csv submitted successfully",
      type = "success"
    )
  })
  
  observeEvent(input$apply_filter_credit,{
    output$loan_amount_box_plot <- renderPlotly({
      plot_ly(x =credit_master_data1$Loan.Amount, type = "box",name = "Credit Amount") %>%
        add_trace(x = credit_master_data1$funded_amnt,name = "Funded Amount") %>%
        #add_trace(x = loan1$Net.Profit,name = "Net Profit",boxpoints = FALSE) %>%
        config(displayModeBar = F)
    })
    
    output$grade_subgrade_plot <- renderPlotly({
      temp <- credit_master_data1 %>% group_by(Industry) %>% summarise(Net.Profit = sum(Net.Profit))
      plot_ly(x = temp$Net.Profit,y = factor(temp$Industry,levels = temp$Industry), name = "Net Profit v/s Industry",type = 'bar') %>%
        layout(yaxis = list(title = 'Industry'),xaxis = list(title = 'Net Profit'), barmode = 'stack')
    })
    
    output$years_in_buisness <- renderPlotly({
      temp <- credit_master_data1 %>% filter(employ!="Unemployed")
      ggplotly(ggplot(temp, aes(x = factor(employ))) +
                 geom_bar(aes(y = (..count..)/sum(..count..)), fill = "dodgerblue3") +
                 scale_y_continuous(labels = percent) +
                 labs(x = "Years in Business", y = "") +
                 ggtitle("Years in Business") +
                 theme_classic(base_family = "Gill Sans MT"))
    })
    
    output$grade_wise_bar_chart <- renderPlotly({
      ggplotly(ggplot(credit_master_data1, aes(x = factor(sub_grade))) +
                 geom_bar(aes(y = (..count..)/sum(..count..)), fill = "dodgerblue3") +
                 scale_y_continuous(labels = percent) +
                 labs(x = "Industry Sub Grade", y = "") +
                 ggtitle("Industry Sub Grade") +
                 theme_classic(base_family = "Gill Sans MT"))
    })
    
    ###Model EDA(Prince)
    meta_loans <- funModeling::df_status(loans, print_results = FALSE)
    #knitr::kable(meta_loans)
    meta_loans <-
      meta_loans %>%
      mutate(uniq_rat = unique / nrow(loans))
    
    
    # meta_loans %>%
    #   select(variable, unique, uniq_rat) %>%
    #   mutate(unique = unique, uniq_rat = scales::percent(uniq_rat)) %>%
    #   knitr::kable()
    
    
    chr_to_num_vars <- 
      c("annual_inc_joint", "mths_since_last_major_derog", "open_acc_6m",
        "open_il_12m", "open_il_24m", "mths_since_rcnt_il",
        "total_bal_il", "il_util", "open_rv_12m", "open_rv_24m",
        "max_bal_bc", "all_util", "total_rev_hi_lim", "total_cu_tl",
        "inq_last_12m", "dti_joint", "inq_fi", "tot_cur_bal", "tot_coll_amt")
    
    
    loans <-
      loans %>%
      mutate_at(.funs = funs(as.numeric), .vars = chr_to_num_vars)
    
    chr_to_date_vars <- 
      c("issue_d", "last_pymnt_d", "last_credit_pull_d",
        "next_pymnt_d", "earliest_cr_line", "next_pymnt_d")
    
    # loans %>%
    #   select_(.dots = chr_to_date_vars) %>%
    #   str()
    # head(unique(loans$next_pymnt_d))
    
    
    # for (i in chr_to_date_vars){
    #   print(head(unique(loans[, i])))
    # }
    
    
    # meta_loans %>% 
    #   select(variable, q_na) %>% 
    #   filter(variable %in% chr_to_date_vars)
    
    convert_date <- function(x){
      as.Date(paste0("01-", x), format = "%d-%b-%Y")
    } 
    
    loans <-
      loans %>%
      mutate_at(.funs = funs(convert_date), .vars = chr_to_date_vars)
    
    
    num_vars <- 
      loans %>% 
      sapply(is.numeric) %>% 
      which() %>% 
      names()
    
    meta_loans %>%
      select(variable, p_zeros, p_na, unique) %>%
      filter_(~ variable %in% num_vars) %>%
      knitr::kable()
    
    
    na_to_zero_vars <-
      c("mths_since_last_delinq", "mths_since_last_record",
        "mths_since_last_major_derog")
    
    loans <- 
      loans %>%
      mutate_at(.vars = na_to_zero_vars, .funs = funs(replace(., is.na(.), 0)))
    
    
    meta_loans <- funModeling::df_status(loans, print_results = FALSE)
    meta_loans <-
      meta_loans %>%
      mutate(uniq_rat = unique / nrow(loans))
    
    
    # knitr::kable(meta_loan_stats[,1:2])
    # 
    # dplyr::setdiff(colnames(loans), meta_loan_stats$LoanStatNew)
    # 
    # dplyr::setdiff(meta_loan_stats$LoanStatNew, colnames(loans))
    # 
    
    default_vars <- c("loan_status", "times_delinq_2yrs", "acc_now_delinq")
    #purrr::map(.x = loans[, default_vars], .f = base::unique)
    
    
    # loans %>%
    #    group_by(loan_status) %>%
    #    summarize(count = n(), rel_count = count/nrow(loans)) %>%
    #    knitr::kable()
    
    
    defaulted <- 
      c("Default", 
        "Does not meet the credit policy. Status:Charged Off", 
        "In Grace Period", 
        "Late (16-30 days)", 
        "Late (31-120 days)")
    
    
    loans <-
      loans %>%
      mutate(default = ifelse(!(loan_status %in% defaulted), FALSE, TRUE))
    
    
    vars_to_remove <- 
      c("annual_inc_joint", "dti_joint", "policy_code", "id", "member_id",
        "emp_title", "url", "desc", "title", "open_acc_6m", "open_il_6m", 
        "open_il_12m", "open_il_24m", "mths_since_rcnt_il", "total_bal_il", 
        "il_util", "open_rv_12m", "open_rv_24m", "max_bal_bc", "all_util",
        "total_rev_hi_lim", "inq_fi", "total_cu_tl", "inq_last_12m",
        "verification_status_joint", "next_pymnt_d")
    
    loans <- loans %>% select(-one_of(vars_to_remove))
    
    vars_to_remove <- 
      c("sub_grade", "loan_status")
    
    loans <- loans %>% select(-one_of(vars_to_remove))
    
    set.seed(6438)
    
    train_index <- 
      caret::createDataPartition(y = loans$default, times = 1, 
                                 p = .8, list = FALSE)
    
    train <- loans[train_index, ]
    test <- loans[-train_index, ]
    
    #income_vars <- c('Net.Profit')
    
    output$credit_scoring_plot1<-renderPlot({
      loan_amount_vars <- c("Loan.Amount", "funded_amnt", "funded_amnt_inv")
      train %>%
        select_(.dots = loan_amount_vars) %>%
        gather_("variable", "value", gather_cols = loan_amount_vars) %>%
        ggplot(aes(x = value)) +
        facet_wrap(~ variable, scales = "free_x", ncol = 3) +
        geom_histogram()
      
    }) 
    ###########################################################
    categorical_vars <- 
      c("term", "grade", "sub_grade", "emp_title", "home_ownership",
        "verification_status", "loan_status", "purpose", "zip_code",
        "addr_state", "application_type", "policy_code")
    
    give_count <- 
      stat_summary(fun.data = function(x) return(c(y = median(x)*1.06,
                                                   label = length(x))),
                   geom = "text")
    
    # see http://stackoverflow.com/questions/19876505/boxplot-show-the-value-of-mean
    give_mean <- 
      stat_summary(fun.y = mean, colour = "darkgreen", geom = "point", 
                   shape = 18, size = 3, show.legend = FALSE)
    output$credit_scoring_plot2<-renderPlot({
      train %>%
        ggplot(aes(grade, Loan.Amount)) +
        geom_boxplot(fill = "white", colour = "darkblue", 
                     outlier.colour = "red", outlier.shape = 1) +
        give_count +
        give_mean +
        scale_y_continuous(labels = comma) +
        facet_wrap(~ default) +
        labs(title="Credit Exposure by Customer Segment", x = "Customer Segment", y = "Credit Amount \n")
      
    })
    
    output$credit_scoring_plot3<-renderPlot({
      train %>%
        ggplot(aes(grade, int_rate)) +
        geom_boxplot(fill = "white", colour = "darkblue", 
                     outlier.colour = "red", outlier.shape = 1) +
        give_count +
        give_mean +
        scale_y_continuous(labels = comma) +
        labs(title="BG% of Credit Limit by Customer Segment", x = "Customer Segment", y = "BG% of Credit Limit \n") +
        facet_wrap(~ term)
      
    })
    ##########round off certain values in the data frame########
    
    #########################################################
    
    
    
    output$credit_scoring_plot4<-renderPlot({
      train %>%
        ggplot(aes(home_ownership, int_rate)) +
        geom_boxplot(fill = "white", colour = "darkblue", 
                     outlier.colour = "red", outlier.shape = 1) +
        give_count +
        give_mean +
        scale_y_continuous(labels = comma) +
        facet_wrap(~ default) +
        labs(title="BG% of Credit Limit by Years in Business", x = "Ownership Status", y = "BG% of Credit Limit \n")
      
    })
    
    output$trend_R_AR_NPL <- renderPlotly({
      data <- trend_data %>% filter(Industry.Name == input$select_trend_industry)
      plot_ly(x = ~c(1:6),y = ~as.numeric(data[,2:7]),type = "scatter",mode="lines+markers",name = "Revenue") %>%
        add_trace(y = ~as.numeric(data[,8:13]),name='ARPU',mode="lines+markers") %>%
        add_trace(y = ~as.numeric(data[14:19]),mode="lines+markers",name = '2 Plus') %>%
        add_trace(y = ~as.numeric(data[20:25]),mode="lines+markers",name = 'NPL') %>%
        add_trace(y = ~as.numeric(data[26:31]),mode="lines+markers",name = 'Exposure') %>%
        layout(xaxis = list(title = "Months"),
               yaxis = list (title = "Value"))
    })
    
  })
  
  output$job_schedule_table <- renderRHandsontable({
    rhandsontable(data_schedule,rowHeaders = F)
  })
  
  output$job_performance_table <- renderRHandsontable({
    rhandsontable(job_performance,rowHeaders = F)
  })
  ###read resample csv#########
  model_rpart_resample<-read.csv('model_rpart_resample.csv')
  model_glm_resample<-read.csv('glm_resample.csv')
  observeEvent(input$apply_resample,{
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    if(input$Model_Type=='Decision Tree'){
      output$result_resample<-renderDataTable({
        round_df(model_rpart_resample,2)
      })
    } else if(input$Model_Type=='Log Reg'){
      output$result_resample<-renderDataTable({
        model_glm_resample
      })} else{
        model_rf_resample<-read.csv('model_rf_resample.csv')
        output$result_resample<-renderDataTable({
          model_rf_resample
        })
      }
    
  })
  
  #####audit log#############
  femb_transaction<-data.frame('User_Id' = c('1','',''),'TimeStamp' = c('16-04-2019','',''),'Data' = c('Credit Master','',''),'Resampling' = c('cv','',''),'Model' = c('Decision Tree','Random Forest','Log Reg'))
  
  output$model_result_output<-renderRHandsontable({
    rhandsontable(femb_transaction,selectCallback = TRUE,readOnly = FALSE)
  })
  
  
  ##########audit result##############
  observeEvent(input$model_result_output_select,{
    x<-input$model_result_output_select$data[[
      input$model_result_output_select$select$r]][[input$model_result_output_select$select$c]]
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    if(x=='Random Forest'){
      output$rf_output<-renderDataTable({
        rf_result_audit<-read_csv('rf_result_audit.csv')
        round_df(rf_result_audit,2)
        
      })
    } else if(x=='Decision Tree'){
      output$rf_output<-renderDataTable({
        round_df(model_rpart_resample,2)
      })
    } else{
      output$rf_output<-renderDataTable({
       glm_audit<-read.csv('glm_audit.csv')
       glm_audit
        
      })
    }
    
  })
  
  
  ############experiment section server part#############
  reactive_segment <- reactive({
    if(input$select_factor_graph == "Default Rate"){
      data <- data.frame(y=c(4.103854,4.383606,4.398768,4.491266,4.507749,4.639080,5.114451,5.222690,5.598410,5.651167,5.670449,5.738636))
      if(input$select_industry_graph == "Oil And Gas Industry"){
        data$z <- c(3.584346,3.789241,3.859750,4.124409,4.374216,4.405562,4.537156,4.807478,4.886510,5.281706,5.336524,5.445266)
      }
    } else if(input$select_factor_graph == "ARPU"){
      data <- data.frame(y=c(4.216499,4.212156,4.001005,4.372928,4.148409,4.461895,4.384196,4.703115,4.821305,4.904724,5.086668,5.381816))
      if(input$select_industry_graph == "Oil And Gas Industry"){
        data$z <- c(3.742648,3.521495,3.422149,3.326777,3.325025,3.266952,3.085232,
                    4.440322,5.870581,5.945315,6.830541,6.830977)
      }
      if(input$control_check==TRUE){
        data$x <- c(3.157389,3.375353,3.488789,3.162514,3.376899,3.261265,3.099659,3.509837,3.671078,3.730525,3.961676,3.965740)
      }
    } else if(input$select_factor_graph == "Credit Utilization"){
      data <- data.frame(y=c(4.912920,4.866310,4.573682,4.541766,4.151439,4.065029,
                             4.329235,
                             4.409956,4.521052,4.759651,4.852883,5.306346
      ))
      if(input$select_industry_graph == "Oil And Gas Industry"){
        data$z <- c(5.395429,5.392682,5.363218,4.856534,4.698452,4.312768,4.172526,
                    3.882125,4.114165,5.295350,6.333192,6.738853)
      }
      if(input$control_check==TRUE){
        data$x <- c(4.393310,4.187057,4.020066,3.813201,3.791668,3.711848,
                    3.732727,
                    3.614288,4.285452,4.039116,3.738070,3.748030)
      }
    } else if(input$select_factor_graph == "Revenue"){
      data <- data.frame(y=c(4.066550,4.538065,4.573958,4.728676,4.866642,
                             5.218351,
                             5.501152,5.600647,5.914712,6.166533,6.170922,6.609056
      ))
      if(input$select_industry_graph == "Oil And Gas Industry"){
        data$z <- c(4.577351,4.415113,4.051558,3.684440,3.536977,3.064498,3.036109,
                    3.331605,3.972900,4.383904,5.545141,5.568065)
      }
    } else if(input$select_factor_graph == "ALL"){
      data <- data.frame(y1=c(4.103854,4.383606,4.398768,4.491266,4.507749,4.639080,5.114451,5.222690,5.598410,5.651167,5.670449,5.738636))
      data <- cbind(data,data.frame(y2=c(4.216499,4.212156,4.001005,4.372928,4.148409,4.461895,4.384196,4.703115,4.821305,4.904724,5.086668,5.381816)))
      data <- cbind(data,data.frame(y3=c(4.912920,4.866310,4.573682,4.541766,4.151439,4.065029,
                                         4.329235,
                                         4.409956,4.521052,4.759651,4.852883,5.306346)))
      data <- cbind(data,data.frame(y4=c(4.066550,4.538065,4.573958,4.728676,4.866642,
                                         5.218351,
                                         5.501152,5.600647,5.914712,6.166533,6.170922,6.609056)))
    }
    data$a <- runif(12,1,50)
    data
  })
  
  output$usage_trend_plotly <- renderPlotly({
    temp <- c("Dec 2017","Jan 2018","Feb 2018","Mar 2018","Apr 2018","May 2018","Jun 2018","Jul 2018","Aug 2018","Sep 2018","Oct 2018","Nov 2018")
    temp <- factor(temp,levels = temp)
    if(ncol(reactive_segment())==2){
      plot_ly(x=temp,y=reactive_segment()$y,type = 'scatter',mode = 'lines+markers',name = "Test") %>%
        add_trace(x=temp[5],y=c(0:15),line = list(dash = 'dash',color = "black"),mode = 'lines',showlegend=F) %>%
        add_trace(x=temp[6],y=c(0:15),line = list(dash = 'dash',color = "black"),mode = 'lines',showlegend=F) %>%
        layout(yaxis = list(range = c(0,15),title = "%change")) %>% config(displayModeBar = F)  
    } else if(ncol(reactive_segment())==5){
      plot_ly(x=temp,y=reactive_segment()$y1,type = 'scatter',mode = 'lines+markers',name = "Default Rate") %>%
        add_trace(y=reactive_segment()$y2,mode = 'lines+markers',name = "ARPU") %>%
        add_trace(y=reactive_segment()$y3,mode = 'lines+markers',name = "Credit Utilization") %>%
        add_trace(y=reactive_segment()$y4,mode = 'lines+markers',name = "Revenue") %>%
        add_trace(x=temp[5],y=c(0:15),line = list(dash = 'dash',color = "black"),mode = 'lines',showlegend=F) %>%
        add_trace(x=temp[6],y=c(0:15),line = list(dash = 'dash',color = "black"),mode = 'lines',showlegend=F) %>%
        layout(yaxis = list(range = c(0,15),title = "%change")) %>% config(displayModeBar = F)
    } else if(ncol(reactive_segment())==3 & input$control_check==FALSE){
      plot_ly(x=temp,y=reactive_segment()$y,type = 'scatter',mode = 'lines+markers',name = "Test") %>%
        add_trace(y=reactive_segment()$z,mode = 'lines+markers',name = "Segment") %>%
        add_trace(x=temp[5],y=c(0:15),line = list(dash = 'dash',color = "black"),mode = 'lines',showlegend=F) %>%
        add_trace(x=temp[6],y=c(0:15),line = list(dash = 'dash',color = "black"),mode = 'lines',showlegend=F) %>%
        layout(yaxis = list(range = c(0,15),title = "%change")) %>% config(displayModeBar = F)
    } else if(ncol(reactive_segment())==3 & input$control_check==TRUE){
      plot_ly(x=temp,y=reactive_segment()$y,type = 'scatter',mode = 'lines+markers',name = "Test") %>%
        add_trace(y=reactive_segment()$x,mode = 'lines+markers',name = "Control") %>%
        add_trace(x=temp[5],y=c(0:15),line = list(dash = 'dash',color = "black"),mode = 'lines',showlegend=F) %>%
        add_trace(x=temp[6],y=c(0:15),line = list(dash = 'dash',color = "black"),mode = 'lines',showlegend=F) %>%
        layout(yaxis = list(range = c(0,15),title = "%change")) %>% config(displayModeBar = F)
    } else if(ncol(reactive_segment())==4 & input$control_check==TRUE){
      plot_ly(x=temp,y=reactive_segment()$y,type = 'scatter',mode = 'lines+markers',name = "Test") %>%
        add_trace(y=reactive_segment()$z,mode = 'lines+markers',name = "Segment") %>%
        add_trace(y=reactive_segment()$x,mode = 'lines+markers',name = "Control") %>%
        add_trace(x=temp[5],y=c(0:15),line = list(dash = 'dash',color = "black"),mode = 'lines',showlegend=F) %>%
        add_trace(x=temp[6],y=c(0:15),line = list(dash = 'dash',color = "black"),mode = 'lines',showlegend=F) %>%
        layout(yaxis = list(range = c(0,15),title = "%change")) %>% config(displayModeBar = F)
    }
  })
  
  output$highlights_credit_scoring_app <- renderUI({
    socialBox(
      width = 12, closable = F, collapsible = F,
      src = 'https://cdn4.iconfinder.com/data/icons/alphabet-3/500/ABC_alphabet_letter_font_graphic_language_text_H-512.png', 
      title = strong('HIGHLIGHTS'), subtitle = 'Credit Scoring',
      comments = tagList(
        boxComment(
          width = 12,
          src = "https://image.flaticon.com/icons/png/128/511/511121.png", #src
          title =  HTML("<font color='grey'>CU</font>"), #productTitle
          date = "Sept 1, 2018",#productPrice
          #priceColor = "warning", #priceColor
          HTML(
            "<b>At an aggregate level, CU has seen a <font color='green'>increase of 3.7%</font> (2.1% vs Control), basis start of Experiment window (June 1, 2018),with a high degree of statistical significance. The associated p-value is 0.03</b>"
          )
        ),
        boxComment(
          width = 12,
          src = "https://image.flaticon.com/icons/png/128/511/511121.png", #src
          title =  HTML("<font color='grey'>CU</font>"), #productTitle
          date = "Oct 1, 2018",#productPrice
          #priceColor = "warning", #priceColor
          HTML(
            '<b>For "Oil & Gas industry", CU has seen a <font color="green">increase of 4.3%</font> (4.1% vs Control) , basis start of Experiment window (June 1, 2018),with a high degree of statistical significance. The associated p-value is 0.01</b>'
          )
        ),
        boxComment(
          width = 12,
          src = "https://image.flaticon.com/icons/png/128/511/511121.png", #src
          title =  HTML("<font color='grey'>CU</font>"), #productTitle
          date = "Nov 1, 2018",#productPrice
          #priceColor = "warning", #priceColor
          HTML(
            '<b>For "Oil & Gas industry", CU has seen a <font color="green">increase of 7.3%</font> (6.2% vs Control), basis start of Experiment window (June 1, 2018),with a <font color="blue">very high</font> degree of statistical significance. The associated p-value is 0.0008
            </b>'
          )
          ),
        boxComment(
          width = 12,
          src = "https://image.flaticon.com/icons/png/128/511/511121.png", #src
          title =  HTML("<font color='grey'>Default Rates</font>"), #productTitle
          date = "Nov 1, 2018",#productPrice
          #priceColor = "warning", #priceColor
          HTML(
            '<b>At the aggregate level, there is no difference in %chg in "Default rates" from a statistical significance perspective. The associated p-value is 0.12
            </b>'
          )
          ),
        boxComment(
          width = 12,
          src = "https://image.flaticon.com/icons/png/128/511/511121.png", #src
          title =  HTML("<font color='grey'>ARPU</font>"), #productTitle
          date = "Nov 1, 2018",#productPrice
          #priceColor = "warning", #priceColor
          HTML(
            '<b>At the aggregate level, ARPU has seen a <font color="green">increase of 2.8%</font> (3.1% vs Control), basis start of Experiment window (June 1, 2018), with a <font color="blue">very high</font> degree of statistical significance. The associated p-value is 0.003
            </b>'
          )
          ),
        boxComment(
          width = 12,
          src = "https://image.flaticon.com/icons/png/128/511/511121.png", #src
          title =  HTML("<font color='grey'>ARPU</font>"), #productTitle
          date = "Nov 1, 2018",#productPrice
          #priceColor = "warning", #priceColor
          HTML(
            '<b>For "Oil & Gas industry", ARPU has seen a <font color="green">increase of 9.3%</font> (6.2% vs Control), basis start of Experiment window (June 1, 2018),with a <font color="blue">very high</font> degree of statistical significance. The associated p-value is 0.0008
            </b>'
          )
          )
        ))
  })
  
  output$A_B_testing_table <- renderRHandsontable({
    data <- data.frame(x = c("","Pre","Post"),Test = c("Test",round(reactive_segment()$a[1:2],digits = 2)),Control = c("Post",round(reactive_segment()$a[3:4],digits = 2)))
    rhandsontable(data,rowHeaders = F,colHeaders = F) %>% hot_cols(colWidths = 100) %>% hot_rows(rowHeights = 100)
  })
 
}

shinyApp(ui, server)