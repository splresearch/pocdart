#PROMIS ADULT ANXIETY SF4 V1
#DESCRIPTORS FOR SETTING RANGES, VALUES
Descriptors <- list(Intro = c("as within normal limits for anxiety","as mild anxiety",
                              "as moderate anxiety","as severe anxiety","as extreme anxiety"),
                    Recent = c(", improved from last survey ",", unchanged from last survey ",", deteriorated from last survey "),
                    Admission = c("and improved compared to day",
                                  "and at the same level as on the day",
                                  "and deteriorated compared to day"),
                    Max = c("Patient is below maximum value","Patient is at the maximum value"),
                    emr_recent = c("improved from last survey ","unchanged from last survey ","deteriorated from last survey "),
                    emr_adm = c("improved compared to admission",
                                "and at the same level as on the admission",
                                "deteriorated compared to admission")
)
promis_flip <- NA
Ranges <- list(Intro = c(54,59,69,90),
               Recent = c(-5,5),
               Admission = c(-5,5))

#REDCAP FIELDS
fields <- c('promisanx4v1_t','promisanx4v1_1','promisanx4v1_2','promisanx4v1_3','promisanx4v1_4')
fields_all <- c('promisanx4v1_t','promisanx4v1_1','promisanx4v1_2','promisanx4v1_3','promisanx4v1_4')
display_term <- "_t"

Percentage <- NA
#GRAPHING VALUES
assay_min <- 40.3
assay_max <- 81.6
mass_display <- "N"
max_type <- "worse"
score_type <- "T-Score"
relevant_questions <- list("Question" = NA,"Variable" = NA)

#DAILY REPORTING VALUES
Rows <- 5
Label <- "PROMIS Adult Anxiety SF4"
Questions <- c("Never","Rarely","Sometimes","Often","Almost Always")
names(Questions) <- c(1,2,3,4,5)
Context <- c("Not at all","Only a little","Somewhat","Quite a bit","Totally")
names(Context) <- c(0,1,2,3,4)
Contextual <- "!"
ContextualList <- list("!" = Context)

total.Frame <- data.frame(Begin = c(0,55,60,70), 
                          Result = c("<i>normal</i>","mild","<strong>moderate</strong>","<strong><i>SEVERE</i></strong>"),
                          Color = c("<span class='green-result'>normal</span>","<span class='yellow-result'>mild</span>",
                                    "<span class='orange-result'>moderate</span>","<span class='red-result'>severe</span>"),
                          unFormattedResult <- c('normal','mild','moderate','severe'))
Titles <- c("PROMIS Anxiety T Score:","Fearful","Focus","Overwhelm","Uneasy")
titles_full <- c("PROMIS Anxiety T Score","I felt fearful","I found it hard to focus on anytihng other than my anxiety","My worries overwhelmed me","I felt uneasy")
#TO BE COPIED VARIABLES
assay_q1 <- 'promisanx4v1_1'

#TABLE VARIABLES
table_title <- 'PROMIS Adult Anxiety SF4'

assay_label <- '<font size = 5><strong>Anxiety</strong></font><br><font size = 3><i>(PROMIS)</i></font>'
assay_label_unformatted <- "<strong>Anxiety</strong> <i>(PROMIS)</i>"
assay_label_alternate <- "Anxiety <i>(PROMIS)</i>"
assay_range <- "<i>(higher score=more anxiety, 20-54: normal, 55-59: mild,60-69: moderate,70-90: severe)</i>"