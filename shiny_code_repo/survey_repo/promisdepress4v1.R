#PROMIS ADULT DEPRESSION SF4 V1
#DESCRIPTORS FOR SETTING RANGES, VALUES
Descriptors <- list(Intro = c("as within normal limits for depression","as mild depression",
                              "as moderate depression","as severe depression","as extreme depression"),
                    Recent = c(", improved from last survey ",", unchanged from last survey ",", deteriorated from last survey "),
                    Admission = c("and improved compared to day",
                                  "and at the same level as on the day",
                                  "and deteriorated compared to day"),
                    Max = c("Patient is below maximum value","Patient is at the maximum value"),
                    emr_recent = c("improved from last survey ","unchanged from last survey ","deteriorated from last survey "),
                    emr_adm = c("improved compared to admission",
                                "and at the same level as on the admission",
                                "deteriorated compared to admission"))

Ranges <- list(Intro = c(54,59,69,90),
               Recent = c(-5,5),
               Admission = c(-5,5))
promis_flip <- NA
#REDCAP FIELDS
fields <- c('promisdepress4v1_t','promisdepress4v1_1','promisdepress4v1_2','promisdepress4v1_3','promisdepress4v1_4')
fields_all <- c('promisdepress4v1_t','promisdepress4v1_1','promisdepress4v1_2','promisdepress4v1_3','promisdepress4v1_4')
display_term <- "_t"

#GRAPHING VALUES
assay_min <- 41.0
assay_max <- 79.4
mass_display <- "N"
Percentage <- NA
max_type <- "worse"
score_type <- "T-Score"
relevant_questions <- list("Question" = NA,"Variable" = NA)

#DAILY REPORTING VALUES
Rows <- 5
Label <- "PROMIS Adult Depression SF4"
Questions <- c("Never","Rarely","Sometimes","Often","Almost Always")
names(Questions) <- c(1,2,3,4,5)
Context <- c("Not at all","Only a little","Somewhat","Quite a bit","Totally")
names(Context) <- c(0,1,2,3,4)
Contextual <- "!"
ContextualList <- list("!" = Context)

total.Frame <- data.frame(Begin = c(0,55,60,70), Result = c("<i>normal</i>","mild","<strong>moderate</strong>","<strong><i>SEVERE</i></strong>"))
Titles <- c("PROMIS Depression T Score:","Worthless:","Helpless:","Depressed:","Hopeless:")

#TO BE COPIED VARIABLES
assay_q1 <- 'promisdepress4v1_1'

#TABLE VARIABLES
table_title <- 'PROMIS Adult Depression SF4'


assay_label <- '<font size = 5><strong>Depression</strong></font><br><font size = 3><i>(PROMIS)</i></font>'
assay_label_unformatted <- "<strong>Depression</strong> <i>(PROMIS)</i>"
assay_range <- "<i>(higher score=more depression, 20-55: normal, 55-60: mild,60-70: moderate,70+: severe)</i>"