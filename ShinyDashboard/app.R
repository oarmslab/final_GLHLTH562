## app.R ##

###################################################################################### Library

library(shiny)
library(shinydashboard)
library(tidyverse)
library(openxlsx)
library(dataReporter)
library(ggplot2)
library(patchwork)
library(googlesheets4)

###################################################################################### Read and prepare data

gs4_deauth()
df <- read_sheet("https://docs.google.com/spreadsheets/d/1hJmd9kSLRlzEK97cWLCc15yR1k9fWRhLK5ljzqlmVvw/edit?usp=sharing")

# Replace "xml:space="preserve">" with an empty string "" in columns Q30_1 through Q30_6

df[, c("Q30_1", "Q30_2", "Q30_3", "Q30_4", "Q30_5", "Q30_6")] <- lapply(
  df[, c("Q30_1", "Q30_2", "Q30_3", "Q30_4", "Q30_5", "Q30_6")],
  function(x) gsub("xml:space=\"preserve\">", "", x)
)

# remove the numbers in parenthesis in front of each observation
df$Q30_1 <- gsub("\\(\\d+\\)\\s+", "", df$Q30_1)
df$Q30_2 <- gsub("\\(\\d+\\)\\s+", "", df$Q30_2)
df$Q30_3 <- gsub("\\(\\d+\\)\\s+", "", df$Q30_3)
df$Q30_4 <- gsub("\\(\\d+\\)\\s+", "", df$Q30_4)
df$Q30_5 <- gsub("\\(\\d+\\)\\s+", "", df$Q30_5)
df$Q30_6 <- gsub("\\(\\d+\\)\\s+", "", df$Q30_6)
df$PPGENDER <- gsub("\\(\\d+\\)\\s+", "", df$PPGENDER)
df$PPHHHEAD <- gsub("\\(\\d+\\)\\s+", "", df$PPHHHEAD)
df$PPMSACAT <- gsub("\\(\\d+\\)\\s+", "", df$PPMSACAT)
df$PPREG4 <- gsub("\\(\\d+\\)\\s+", "", df$PPREG4)
df$LGBT <- gsub("\\(\\d+\\)\\s+", "", df$LGBT)
df$MARRIED <- gsub("\\(\\d+\\)\\s+", "", df$MARRIED)
df$NUM_CHILD_HH <- gsub("\\(\\d+\\)\\s+", "", df$NUM_CHILD_HH)
df$Q16 <- gsub("\\(\\d+\\)\\s+", "", df$Q16)
df$PPETHM <- gsub("\\(\\d+\\)\\s+", "", df$PPETHM)
df$PPEDUCAT <- gsub("\\(\\d+\\)\\s+", "", df$PPEDUCAT)

# removing everything but the numbers in columns PPINCIMP and PPETHM

#df$PPINCIMP <- gsub("\\((\\d+)\\).*", "\\1", df$PPINCIMP) # This leaves the numbers (codes)
df$PPINCIMP <- gsub("^[^ ]*\\s", "", df$PPINCIMP) # This leaves the income categories
#df$PPETHM <- gsub("[^0-9]+", "", df$PPETHM)
#df$PPEDUCAT <- gsub("\\D*(\\d+)\\D*", "\\1", df$PPEDUCAT)

# simplifying the responses of Q16 to only No and Yes

df$Q16 <- gsub(".*(No|Yes).*", "\\1", df$Q16)

# remove space at the end of "Most of the time" and "Some of the time"

df$Q30_1 <- trimws(df$Q30_1)
df$Q30_2 <- trimws(df$Q30_2)
df$Q30_3 <- trimws(df$Q30_3)
df$Q30_4 <- trimws(df$Q30_4)
df$Q30_5 <- trimws(df$Q30_5)
df$Q30_6 <- trimws(df$Q30_6)


# changing the responses of the questionnaire to the respective numbers


df <- df %>%
  mutate(Q30_1 = recode(Q30_1, "None of the time" = 0, "A little of the time" = 1,
                        "Some of the time" = 2, "Most of the time" = 3,
                        "All of the time" =4),
         Q30_2 = recode(Q30_2, "None of the time" = 0, "A little of the time" = 1,
                        "Some of the time" = 2, "Most of the time" = 3,
                        "All of the time" =4),
         Q30_3 = recode(Q30_3, "None of the time" = 0, "A little of the time" = 1,
                        "Some of the time" = 2, "Most of the time" = 3,
                        "All of the time" =4),
         Q30_4 = recode(Q30_4, "None of the time" = 0, "A little of the time" = 1,
                        "Some of the time" = 2, "Most of the time" = 3,
                        "All of the time" =4),
         Q30_5 = recode(Q30_5, "None of the time" = 0, "A little of the time" = 1,
                        "Some of the time" = 2, "Most of the time" = 3,
                        "All of the time" =4),
         Q30_6 = recode(Q30_6, "None of the time" = 0, "A little of the time" = 1,
                        "Some of the time" = 2, "Most of the time" = 3,
                        "All of the time" =4))

# Adding up all the values for K-6 questions

df$sum_Q30 <- rowSums(df[, c("Q30_1", "Q30_2", "Q30_3", "Q30_4", "Q30_5", "Q30_6")])

# creating dummy variable determining if the observation experience psychological distress or not
# 1 = score is 13 or higher (psychological distress), 0 = score is lower than 13 (no psychological distress)

df$score_final <- ifelse(df$sum_Q30 >= 13, 1, 0)

df$score_class <- ifelse(df$score_final == 0, "low to moderate distress","high risk of psychological distress")
#Adding the new variable "score_class" which mentions the class of each participant.

# Creating factor levels

df$PPETHM <- factor(df$PPETHM,

                    levels = c("White, Non-Hispanic", "Black, Non-Hispanic", "Other, Non-Hispanic",
                               "Hispanic", "2+ Races, Non-Hispanic"))

df$PPEDUCAT <- factor(df$PPEDUCAT,

                      levels = c("Less than high school", "High school",

                                 "Some college", "Bachelor's degree or higher"))


# Renaming variables

names(df)[names(df) == "PPAGE"] <- "Age"
names(df)[names(df) == "PPETHM"] <- "Ethnicity"
names(df)[names(df) == "PPEDUCAT"] <- "Level_of_education"
names(df)[names(df) == "PPGENDER"] <- "Gender"
names(df)[names(df) == "PPINCIMP"] <- "Income_category"
names(df)[names(df) == "PPMSACAT"] <- "Metropolitan_area"
names(df)[names(df) == "PPREG4"] <- "Region"
names(df)[names(df) == "MARRIED"] <- "Married"
names(df)[names(df) == "NUM_CHILD_HH"] <- "Number_of_children"
names(df)[names(df) == "PPHHHEAD"] <- "Head_of_household"

##################################################################################### Write functions and define attributes

# Function
plot_df <- function(catg0){
  if (catg0 == "LGBT" | catg0 == "Married" | catg0 == "Head_of_household" | catg0 == "Metropolitan_area" | catg0 == "Region" |
      catg0 == "Level_of_education"| catg0 == "Gender"){
    catg <- sym(catg0)

    df %>%
      select(!!catg, score_class) %>%
      remove_missing() %>%

      group_by(!!catg, score_class) %>%
      summarise(total = length(score_class)) %>%

      group_by(!!catg) %>%
      mutate(sum = sum(total)) %>%
      mutate(proption = (total/sum)*100) %>%
      filter(score_class == "high risk of psychological distress") %>%

      ggplot(aes(x = fct_reorder(!!catg, proption), y = proption, fill = !!catg)) +
      geom_col() +
      theme(legend.position = "none") +
      ylab("Percentage of individuals with \nhigh risk of ppsychological distress") +
      xlab(case_when(catg0 == "LGBT" ~ "LGBT",
                     catg0 == "Married" ~ "Marital Status",
                     catg0 == "Head_of_household" ~ "House Hold Head Satus",
                     catg0 == "Metropolitan_area" ~ "Area (Metropolitan or Non metropolitan)",
                     catg0 == "Region" ~ "Region",
                     catg0 == "Level_of_education" ~ "Level of Education",
                     catg0 == "Gender" ~ "Gender")) +
      labs(title = paste("High Risk Psychological Distress Rates Across The US",
                         case_when(catg0 == "LGBT" ~ "by LGBT",
                                   catg0 == "Married" ~ "by Marital Status",
                                   catg0 == "Head_of_household" ~ "by House Hold Head Satus",
                                   catg0 == "Metropolitan_area" ~ "by Area (Metropolitan or Non metropolitan)",
                                   catg0 == "Region" ~ "by Region",
                                   catg0 == "Level_of_education" ~ "by Level of Education",
                                   catg0 == "Gender" ~ "by Gender")
      ))

  }else if (catg0 == "Ethnicity" | catg0 == "Income_category" | catg0 == "Number_of_children"){

    catg <- sym(catg0)

    df %>%
      select(!!catg, score_class) %>%
      remove_missing() %>%

      group_by(!!catg, score_class) %>%
      summarise(total = length(score_class)) %>%

      group_by(!!catg) %>%
      mutate(sum = sum(total)) %>%
      mutate(proption = (total/sum)*100) %>%
      filter(score_class == "high risk of psychological distress") %>%

      ggplot(aes(x = fct_reorder(!!catg, proption), y = proption, color = !!catg)) +
      geom_point(size = 3.5) +
      geom_segment(aes(x = !!catg, xend = !!catg, y = proption, yend = 0)) +
      theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.5)) +
      theme(legend.position = "none") +
      ylab("Percentage of individuals with \nhigh risk of psychological distress") +
      xlab(case_when(catg0 == "Ethnicity" ~ "Race/Ethnicity",
                     catg0 == "Income_category" ~ "Income Level",
                     catg0 == "Number_of_children" ~ "Number of Children in The Household")) +
      labs(title = paste("High Risk Psychological Distress Rates Across The US",
                         case_when(catg0 == "Ethnicity" ~ "by Race/Ethnicity",
                                   catg0 == "Income category" ~ "by Income Level",
                                   catg0 == "Number of children" ~ "by Number of Children in The Household")
      ))

  }else{

    catg <- sym(catg0)

    df %>%
      select(!!catg, score_class) %>%
      remove_missing() %>%

      group_by(!!catg, score_class) %>%
      summarise(total = length(score_class)) %>%

      group_by(!!catg) %>%
      mutate(sum = sum(total)) %>%
      mutate(proption = (total/sum)*100) %>%
      filter(score_class == "high risk of psychological distress") %>%


      ggplot(aes(x = !!catg, y = proption)) +
      geom_point() +
      theme(legend.position = "none") +
      ylab("Percentage of individuals with \nhigh risk of psychological distress") +
      xlab("Age") +
      labs(title = paste("High Risk Psychological Distress Rates Across The US by Age"))

  }
}


AttributeChoices = c("LGBT", "Gender", "Married","Head_of_household","Metropolitan_area","Region","Level_of_education",
                     "Ethnicity","Number_of_children","Age")

###################################################################################### User interface


ui <- dashboardPage(
  dashboardHeader(title = "Risk of Psychological Distress in the US"),
  dashboardSidebar(
    selectInput("catg0", label = "Variable ~ Select any of the variables to see how risk of psychological distress
                is distributed across its levels",
                choices = c("LGBT", "Gender", "Married","Head_of_household","Metropolitan_area","Region","Level_of_education",
                            "Ethnicity","Income_category","Number_of_children","Age"), selected = "Gender")

  ),
  dashboardBody(

    fluidRow(box(plotOutput("Risk_Iic_Cat")),

             box(plotOutput("Risk"))),


    fluidRow(box(sidebarPanel(
      checkboxGroupInput(inputId = "vars", label = "Select variables",
                         choices = AttributeChoices, selected = NULL))),

      box(tableOutput("logistic_regression"))

    )
  )
)

####################################################################################### Server

server <- function(input, output) {

  output$Risk_Iic_Cat <- renderPlot({
    df_score_class <- df %>%
      select(score_class) %>%
      remove_missing() %>%

      group_by(score_class) %>%
      summarise(total = length(score_class))

    df_score_class <- df_score_class %>%
      mutate(sum = sum(total)) %>%
      mutate(proption = (total/sum)*100)


    ggplot(data = df_score_class, aes(x = score_class, y = total)) +
      geom_col(fill = "darkgrey") +
      geom_text(label = (round(df_score_class$proption, 2)), vjust = -0.5) +
      geom_text(label = "%", vjust = -0.5, hjust = -1.5) +
      theme_minimal() +
      theme(legend.position = "none") +
      ylab("Number of individuals") +
      xlab("Psychological distres level") +
      labs(title = "Risk of Psychological Distress in the US")
  })

  output$Risk <- renderPlot({

    plot_df(input$catg0)

  })

  observe({
    selected_vars <- input$vars
    if (length(selected_vars) == 0) {
      return(NULL)
    } else {
      formula <- paste("score_final ~", paste(selected_vars, collapse = " + "))
      model <- glm(formula = formula, data = df, family = 'binomial')

    }

    output$logistic_regression <- renderTable({
      # Creating a table such that the logistic regression results are displayed in a user friendly manner

      levels <- broom::tidy(model)
      levels$estimate <- exp(levels$estimate)
      levels <- levels %>% mutate(lower = estimate - 2*std.error,
                                  upper = estimate + 2*std.error)

      exp_confint <- apply(confint(model), 2, exp)

      # create a data frame with the coefficient and confidence interval values
      result <- data.frame(term = levels$term,
                           estimate = levels$estimate,
                           lower = exp_confint[, 1],
                           upper = exp_confint[, 2])

      # print the resulting table
      result

    })
  })

}

shinyApp(ui, server)
