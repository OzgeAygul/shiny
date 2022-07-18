library(shiny)
library(shinythemes)
library(DT)
library(knitr)
library(psych) 
library(summarytools) 
library(ggplot2)
library(dplyr)
library(corrgram)
library(corrplot)
#library(caret)
library(pROC)

data <- read.csv("data.csv")

data_tra <- data %>%
  mutate(diagnosis = case_when(
    (diagnosis == "B") ~ 1,
    (diagnosis == "M") ~ 0
  ))

data_tra <- subset(data_tra, select = -c(1,33))

set.seed(1234)
train <- sample(nrow(data_tra), .7*nrow(data_tra))
data_tra.train <- data_tra[train,]
data_tra.validate <- data_tra[-train,]


model <- glm(diagnosis ~.,family="binomial",data=data_tra.train)


model_back <- step(model)
fitted.results <- predict(model_back,newdata=subset(data_tra.validate,select=-1),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != data_tra.validate$diagnosis)

tab = table(data_tra.validate$diagnosis,fitted.results)




res = glm(diagnosis ~ ., data_tra, family = "binomial")


ui <- fluidPage(theme = shinytheme("united"),
                titlePanel("Breast Cancer Detection using Machine Learning "),
                navbarPage("Get Started",
                           tabPanel(icon("home"),
                                    fluidRow(column(
                                               br(),
                                               tags$p("This application is written for analyzing famous dataset called 'Breast Cancer Wisconsin (Diagnostic)' and also run a logictic regression.",style="text-align:justify;color:black;background-color:grey;padding:15px;border-radius:10px"),
                                               br(), 
                                               tags$p("There are total 30 variables that might be related to class of diagnosis. In the data, there are total 569 cases with 357 benign and 212 malignant.
                                                 The goal is to perform data analysis, feature selection and logistic regression model to predict if a tumor is benign or malignant.",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                                               width = 8)),
                                    hr(),
                                    tags$style(".fa-database {color:#E87722}"),
                                    h3(tags$p(em("Dataset"),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                                    fluidRow(column(datatable(data, filter = 'top', options = list(
                                             pageLength = 5,scrollX = TRUE)), width=12)),
                                    hr(),
                                    tags$p(em("Developed by"),br("Ozge Aygul"),style="text-align:center; font-family: times")),
                           
                          tabPanel("Data Exploration",
                                   tabsetPanel(
                                     tabPanel("Data Overlook",
                                      navlistPanel("Data Manipulation",
                                               tabPanel(title = "Variable Types",
                                                         verbatimTextOutput("factor_data"),
                                                        fluidRow(column(br(),
                                                                 tags$p("We can see that all independent variables we are going to use are numeric. However, dependent variable is factor with levels M and B. So, we need to convert it to numeric as 0 and 1.",style="color:black"),
                                                                 br(),width = 12, style="background-color:lavender;border-left:8px solid blue"))),
                                               tabPanel(title = "Missing Values",
                                                        fluidRow(column(verbatimTextOutput("missing_data"), width = 10)),
                                                        fluidRow(column(br(),
                                                                        tags$p("Above table shows the total missing values in every column of a data frame"),
                                                                        br(),
                                                                        tags$p("We can see that there is no missing values expect from variable X. So, we do not include X in the analysis.",style="color:black"),
                                                                        br(),width = 12,style="background-color:lavender;border-left:8px solid blue"))),
                                              "Descriptive Statictics",
                                              tabPanel(title = 'Summary',
                                                       verbatimTextOutput("summary_data"),
                                                       fluidRow(column(br(),
                                                                       tags$p("Table above illustrates almost all features of variables. We can see how they are distributed by looking percentiles.", style="color:black"),
                                                                       br(), width = 12, style="background-color:lavender;border-left:8px solid blue"))),
                                              tabPanel(title = "By Diagnosis",
                                                       verbatimTextOutput("des_anal_group"),
                                                       fluidRow(column(br(),
                                                                       tags$p("By looking this table, the relation between variables and diagnosis become clear.", style="color:black"),
                                                                       br(),
                                                                       tags$p("For example; compare the area_mean for both diagnosis. We can argue that big tumors are more likely to be malign.", style="color:black"),
                                                                       br(), width = 12, style="background-color:lavender;border-left:8px solid blue"))))),
                                      
                                    tabPanel("Univariate Data Analysis",
                                               br(),
                                               fluidRow(column(
                                                 br(),
                                                 tags$p("Univariate analysis is the examination of one variables at a time. The goal is to be familiar with the distribtion of variables.",style="color:black;text-align:justify"),
                                                 br(),
                                               width=8,style="background-color:lavender;border-radius: 10px")),
                                               hr(),
                                               selectInput("IndependentVariable",tags$p("Please select an independent variable to plot:",style="color:black; text-align:center"), choices=c("radius_mean"=1,"texture_mean"=2,"perimeter_mean"=3,"area_mean"=4,"smoothness_mean"=5,
                                                                                                                                                                                    "compactness_mean"= 6, "concavity_mean" = 7, "concave.points_mean" = 8, "symmetry_mean"=9,
                                                                                                                                                                                    "fractal_dimension_mean" = 10, "radius_se" = 11, "texture_se" = 12, "perimeter_se" = 13,
                                                                                                                                                                                     "area_se" = 14, "smoothness_se" = 15, "compactness_se" = 16, "concavity_se" = 17,
                                                                                                                                                                                     "concave.points_se" = 18, "symmetry_se" = 19, "fractal_dimension_se" = 20, "radius_worst" = 21,
                                                                                                                                                                                     "texture_worst" = 22, "perimeter_worst" = 23, "area_worst" = 24, "smoothness_worst" = 25,
                                                                                                                                                                                    "compactness_worst" = 26, "concavity_worst" = 27, "concave.points_worst" = 28,
                                                                                                                                                                                    "symmetry_worst" = 29, "fractal_dimension_worst" = 30)),
                                               mainPanel(
                                                       fluidRow(
                                                                column(plotOutput("boxplots"),width=6),
                                                                column(br(),
                                                                tags$p("Boxplots are a way of displaying the distribution of data based on a five number summary ('minimum', first quartile (Q1), median, third quartile (Q3), and 'maximum'.
                                                                  It can tell you about your outliers and what their values are. 
                                                                    It can also tell you if your data is symmetrical, how tightly your data is grouped, and if and how your data is skewed.",style="color:black"),
                                                                    br(),width = 6,style="background-color:lavender;border-left:8px solid blue"))
                                                       )
                                              ),
                                     tabPanel("Bivariate Data Analysis",
                                              br(),
                                              fluidRow(column(
                                                br(),
                                                tags$p("Bivariate analysis is the examination of two variables.",style="color:black;text-align:justify"),
                                                br(),
                                                width=8,style="background-color:lavender;border-radius: 10px")),
                                                fluidRow(
                                                  fluidRow(
                                                  column(2,plotOutput("hist1")),
                                                  column(2, plotOutput("hist2")),
                                                  column(2, plotOutput("hist3")),
                                                  column(2, plotOutput("hist4")),
                                                  column(2, plotOutput("hist5"))),
                                                  fluidRow(column(2,plotOutput("hist6")),
                                                           column(2, plotOutput("hist7")),
                                                           column(2, plotOutput("hist8")),
                                                           column(2, plotOutput("hist9")),
                                                           column(2, plotOutput("hist10"))),
                                                  column(br(),
                                                         tags$p("Histograms shows how distribution of variables (associated with mean measures) changes with malign and belign tumors", style="color:black"),
                                                         br(), width = 6, style="background-color:lavender;border-left:8px solid blue"))
                                                
                                              ),
                                      tabPanel("Multivariate Data Analysis",
                                               br(),
                                               fluidRow(column(
                                                 br(),
                                                 tags$p("Multivariate analysis is the examination of correlation of variables. In the dataset, there are 3 types of measurement: mean, se and worst. We will examine them separetly.",style="color:black;text-align:justify"),
                                                 br(),
                                                 width=8,style="background-color:lavender;border-radius: 10px")),
                                               navlistPanel("Measures",
                                                 tabPanel(title = "Mean",
                                                        fluidRow(column(8,plotOutput("multi1")),
                                                                   br(),
                                                                column(4,
                                                                          tags$p("There is a strong correlation between perimeter, area and radius.", style="color:black"),
                                                                          br(), style="background-color:lavender;border-left:8px solid blue"))
                                                          ),
                                                 tabPanel(title = ".se",
                                                          plotOutput("multi2")
                                                          ),
                                                 tabPanel(title = "worst",
                                                          plotOutput("multi3")
                                                          )
                                               )
                                               )
                                      )),
                           tabPanel("Model Run",
                                    tabsetPanel(
                                    tabPanel("Logictic Regression",
                                             br(),
                                             fluidRow(column(
                                               br(),
                                               tags$p("Logistic regression models the probabilities for classification problems with two possible outcomes.
                                                      It's an extension of the linear regression model for classification problems.
                                                       Instead of fitting a straight line or hyperplane, the logistic regression model uses the logistic function to squeeze the output of a linear equation between 0 and 1. 
                                                      The logistic function is defined as:",style="color:black;text-align:justify"),
                                               withMathJax(),
                                               tags$p('$$logictic(\\eta) = \\frac{1}{1 + exp(-\\eta)}$$',style="color:black;border:1px solid black;background-color:white;font-size:50px"),
                                               
                                               br(),
                                               width=8,style="background-color:lavender;border-radius: 10px"),
                                               br(),
                                               column(tags$p("And the function looks like this:",style="color:black"),
                                                      br(),
                                                      tags$p(tags$img(src = "https://christophm.github.io/interpretable-ml-book/images/logistic-function-1.png",width="250px",height="200px",style="border: 1px solid black; margin-left: 100px")),
                                                      width=4,style="background-color:lavender;border-radius: 10px")
                                             ),
                                             navlistPanel("Feature Selection",
                                                          tabPanel(title = "All variables",
                                                          br(),
                                                          fluidRow(column(8, verbatimTextOutput("anova1"))),
                                                          fluidRow(column(br(),
                                                                          tags$p("Here, we should look how adding variables makes it better in comparasion
                                                                                 to model with just intercept.
                                                                                 We can evaluate this by looking difference in residual deviance.", style="color:black"),
                                                                          br(),
                                                                          tags$p("Adding radius_mean, texture_mean and perimeter_mean results in drop in residual deviance ", style="color:black"),
                                                                          br(), width = 12, style="background-color:lavender;border-left:8px solid blue"))
                                                          ),
                                                          tabPanel(title = "Backward Selection",
                                                                   fluidRow(br(),
                                                                            column(10, 
                                                                                   tags$p("Stepwise regression is a procedure we can use to build a regression model from a set of predictor
                                                                                          variables by entering and removing predictors in a stepwise manner into the model until there is no 
                                                                                          statistically valid reason to enter or remove any more. Here, we use backward selection.", style="color:black"),
                                                                                   style="background-color:lavender;border-radius: 10px")),
                                                                   fluidRow(br(),
                                                                            verbatimTextOutput("anova2")))
                                                          )
                                             ),

                                    tabPanel("Evaluation",
                                             fluidRow(br(),
                                                       column(6,
                                                              tags$p("There are a variety of method to eveluate model performance.
                                                                     One way is to split the data into test and train, and calculate average accuracy in the test data", style = "color:black"),
                                                              style="background-color:lavender;border-radius: 10px"),
                                                      column(6,
                                                             tags$p("The last step is to plot ROC curve and calculate area under curve(AUC) as a performance metric", style = "color:black"),
                                                             style="background-color:lavender;border-radius: 10px")),
                                             fluidRow(br(),
                                                      column(6,tags$p("Here how it works:",style="color:black"),
                                                             br(),
                                                             tags$p(tags$img(src = "https://miro.medium.com/max/1400/1*AOwsTgJnh_b_oLIAaAn8Bg.png",
                                                                             width="550px",height="200px",style="border: 1px solid black; margin-left: 100px")),
                                                             style="background-color:lavender;border-radius: 10px"),
                                                      column(6, tags$p("Here how it can be interpreted:", style = "color:black"),
                                                             br(),
                                                             tags$p(tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/1/13/Roc_curve.svg",
                                                                             width="550px",height="200px",style="border: 1px solid black; margin-left: 100px")),
                                                             style="background-color:lavender;border-radius: 10px")),
                                                             
                                                    hr(),
                                             fluidRow(br(),
                                                      column(6, tags$p("When data is randomly splitted 70% to train and 30% of test,
                                                                       the mean accuracy is around 93% in the backward model."),
                                                             br(),
                                                             tags$p("Let's visualize it by confusion matrix!")
                                                             )
                                                      ),
                                             fluidRow(br(),
                                                     column(6, plotOutput("conf_mat")),
                                                     column(6, plotOutput("roc_curve"))
                                             ),
                                             fluidRow(br(),
                                                      column(6, tags$p("Model predicts malign tumors as malign 54 times, benign tumors as benign 105 times. There are total 12 misprediction in 171 cases.", style="color:black"),
                                                      br(), style="background-color:lavender;border-left:8px solid blue"),
                                                      column(6, tags$p("AUC should be greater than 0.5 (ideally equal to 1) to have a good model. In our case, it is around 0.92.
                                                                       To conclude, class of tumor is predicted using logistic regression. Backward selection is done to eliminate some features out of 30.
                                                                       It is shown that final model performs good.", style="color:black"),
                                                             br(), style="background-color:lavender;border-left:8px solid blue")
                                                      )
                                             )
                           ))

                            ))


server <- function(input, output) {
  # Boxplot
  output$boxplot <- renderPlot({
    boxplot(data)
  }, height=700, width=1000)
  
  
  output$summary_data <- renderPrint(dfSummary(subset(data, select = c(3:32))))
  
  output$des_anal_group <- renderPrint(describeBy(subset(data, select = c(3:32)), group=data$diagnosis, fast=TRUE))
  
  
  output$factor_data <- renderPrint(str(data))
  
  output$missing_data <- renderPrint(kable(as.table(sapply(data, function(x) sum(is.na(x))))))
  
  
  output$boxplots <- renderPlot({
    if(input$IndependentVariable == 1){boxplot(data$radius_mean)}
    if(input$IndependentVariable == 2){boxplot(data$texture_mean)}
    if(input$IndependentVariable == 3){boxplot(data$perimeter_mean)}
    if(input$IndependentVariable == 4){boxplot(data$area_mean)}  
    if(input$IndependentVariable == 5){boxplot(data$smoothness_mean)}
    if(input$IndependentVariable == 6){boxplot(data$compactness_mean)}  
    if(input$IndependentVariable == 7){boxplot(data$concavity_mean)}
      if(input$IndependentVariable == 8){boxplot(data$concave.points_mean)}
      if(input$IndependentVariable == 9){boxplot(data$symmetry_mean)}  
      if(input$IndependentVariable == 10){boxplot(data$fractal_dimension_mean)}
      if(input$IndependentVariable == 11){boxplot(data$radius_se)}  
      if(input$IndependentVariable == 12){boxplot(data$texture_se)}  
      if(input$IndependentVariable == 13){boxplot(data$perimeter_se)}  
      if(input$IndependentVariable == 14){boxplot(data$area_se)}  
     if(input$IndependentVariable == 15){boxplot(data$smoothness_se)}  
      if(input$IndependentVariable == 16){boxplot(data$compactness_se)}  
      if(input$IndependentVariable == 17){boxplot(data$concavity_se)}  
      if(input$IndependentVariable == 18){boxplot(data$concave.points_se)}  
      if(input$IndependentVariable == 19){boxplot(data$symmetry_se)}  
      if(input$IndependentVariable == 20){boxplot(data$fractal_dimension_se)}  
      if(input$IndependentVariable == 21){boxplot(data$radius_worst)} 
      if(input$IndependentVariable == 22){boxplot(data$texture_worst)}
      if(input$IndependentVariable == 23){boxplot(data$perimeter_worst)}
      if(input$IndependentVariable == 24){boxplot(data$area_worst)}
      if(input$IndependentVariable == 25){boxplot(data$smoothness_worst)} 
     if(input$IndependentVariable == 26){boxplot(data$compactness_worst)}
      if(input$IndependentVariable == 27){boxplot(data$concavity_worst)}
      if(input$IndependentVariable == 28){boxplot(data$concave.points_worst)}
      if(input$IndependentVariable == 29){boxplot(data$symmetry_worst)}
      if(input$IndependentVariable == 30){boxplot(data$fractal_dimension_worst)}},height=600, width=600)
  
 
 
  output$hist1 <- renderPlot({ggplot(data=data, aes(x=radius_mean, fill=diagnosis))+geom_density(alpha=.3)},height=400, width=400)
  output$hist2 <- renderPlot(ggplot(data=data, aes(x=texture_mean, fill=diagnosis))+geom_density(alpha=.3),height=400, width=400)
  output$hist3 <- renderPlot(ggplot(data=data, aes(x=perimeter_mean, fill=diagnosis))+geom_density(alpha=.3),height=400, width=400)
  output$hist4 <- renderPlot(ggplot(data=data, aes(x=area_mean, fill=diagnosis))+geom_density(alpha=.3),height=400, width=400)
  output$hist5 <- renderPlot(ggplot(data=data, aes(x=smoothness_mean, fill=diagnosis))+geom_density(alpha=.3),height=400, width=400)
  output$hist6 <- renderPlot(ggplot(data=data, aes(x=compactness_mean, fill=diagnosis))+geom_density(alpha=.3),height=400, width=400)
  output$hist7 <- renderPlot(ggplot(data=data, aes(x=concavity_mean, fill=diagnosis))+geom_density(alpha=.3),height=400, width=400)
  output$hist8 <- renderPlot(ggplot(data=data, aes(x=concave.points_mean, fill=diagnosis))+geom_density(alpha=.3),height=400, width=400)
  output$hist9 <- renderPlot(ggplot(data=data, aes(x=symmetry_mean, fill=diagnosis))+geom_density(alpha=.3),height=400, width=400)
  output$hist10 <- renderPlot(ggplot(data=data, aes(x=fractal_dimension_mean, fill=diagnosis))+geom_density(alpha=.3),height=400, width=400)
  
  output$multi1 <- renderPlot(corrgram(data[2:11], order = TRUE,
                            upper.panel = panel.pie), height = 800, width = 800)
  
  output$multi2 <- renderPlot(corrgram(data[12:21], order = TRUE,
                            upper.panel = panel.pie), height = 800, width = 800)
  
  output$multi3 <- renderPlot(corrgram(data[22:31], order = TRUE,
                            upper.panel = panel.pie), height = 800, width = 800)

  output$anova1 <- renderPrint(anova(model, test="Chisq"))
  output$anova2 <- renderPrint(anova(model_back, test = "Chisq"))
  
  output$conf_mat <- renderPlot(fourfoldplot(tab, color = c("cyan", "pink"),
                                             conf.level = 0, margin = 1, main = "Confusion Matrix"))
  
  output$roc_curve <- renderPlot(plot(roc(data_tra.validate$diagnosis, fitted.results, direction="<"),
                                      col="yellow", lwd=3, main="ROC Curve",print.auc=TRUE))
  
  }

shinyApp(ui, server)




