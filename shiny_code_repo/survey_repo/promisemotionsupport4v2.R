#PROMIS ADULT Emotional Support SF4 V1
#DESCRIPTORS FOR SETTING RANGES, VALUES
Descriptors <- list(Intro = c("as extreme impairment for emotional support", "as severe impairment for emotional support","as moderate impairment for emotional support",
                              "as mild impairment for emotional support","as within normal range for emotional support"),
                    Recent = c(", deteriorated from last survey ",", unchanged from last survey ",", improved from last survey "),
                    Admission = c("and deteriorated compared to day",
                                  "and at the same level as on the day",
                                  "and improved compared to day"),
                    Max = c("Patient is below maximum value","Patient is at the maximum value"),
                    emr_recent = c("deteriorated from last survey ","unchanged from last survey ","improved from last survey "),
                    emr_adm = c("deteriorated compared to admission",
                                "and at the same level as on the admission",
                                "improved compared to admission"))

Ranges <- list(Intro = c(31,41,56,90),
               Recent = c(-5,5),
               Admission = c(-5,5))
promis_flip <- "Y"
#REDCAP FIELDS
fields <- c('promisemotionsupport4v2_t','promisemotionsupport4v2_1','promisemotionsupport4v2_2',
            'promisemotionsupport4v2_3','promisemotionsupport4v2_4')
fields_all <- c('promisemotionsupport4v2_t','promisemotionsupport4v2_1','promisemotionsupport4v2_2',
                'promisemotionsupport4v2_3','promisemotionsupport4v2_4')
display_term <- "_t"

#GRAPHING VALUES
assay_min <- 25.7
assay_max <- 62.0
mass_display <- "N"
Percentage <- NA
max_type <- "better"
score_type <- "T-Score"
relevant_questions <- list("Question" = NA,"Variable" = NA)

#DAILY REPORTING VALUES
Rows <- 5
Label <- "PROMIS Emotional Support SF4"
Questions <- c("Never","Rarely","Sometimes","Usually","Always")
names(Questions) <- c(1,2,3,4,5)
Context <- c("Not at all","Only a little","Somewhat","Quite a bit","Totally")
names(Context) <- c(0,1,2,3,4)
Contextual <- "!"
ContextualList <- list("!" = Context)

total.Frame <- data.frame(Begin = c(0,30,40,55), Result = c("<strong><i>SEVERE</i></strong>","<strong>moderate</strong>","mild","<i>normal</i>"))
Titles <- c("PROMIS Emotional Supp. T Score:","Listen:","Confide:","Appreciated:","Converse:")

#TO BE COPIED VARIABLES
assay_q1 <- 'promisemotionsupport4v2_1'

#TABLE VARIABLES
table_title <- 'PROMIS Emotional Support SF4'

assay_label <- '<font size = 5><strong>Emotional Support</strong></font><br><font size = 3><i>(PROMIS)</i></font>'
assay_label_unformatted <- "<strong>Emotional Support</strong> <i>(PROMIS)</i>"
assay_range <- "<i>(higher score=more Emotional Support, 20-30: severe impairment, 31-40: moderate impairment,41-55: mild impairment,56+: normal)</i>"