#PROMIS Anger SF5v1.1
#DESCRIPTORS FOR SETTING RANGES, VALUES
Descriptors <- list(Intro = c("as within normal limits for anger","as mild anger",
                              "as moderate anger","as severe anger","as extreme anger"),
                    Recent = c(", deteriorated from last survey ",", unchanged from last survey ",", improved from last survey "),
                    Admission = c("and deteriorated compared to day",
                                  "and at the same level as on the day",
                                  "and improved compared to day"),
                    Max = c("Patient is below maximum value","Patient is at the maximum value"),
                    emr_recent = c("<u>improved</u> from last survey ","unchanged from last survey ","<u>deteriorated</u> from last survey "),
                    emr_adm = c("<u>improved</u> compared to admission",
                                "and at the same level as on the admission",
                                "<u>deteriorated</u> compared to admission"))

Ranges <- list(Intro = c(54,59,69,90),
               Recent = c(-5,5),
               Admission = c(-5,5))
promis_flip <- NA
#REDCAP FIELDS
fields <- c('promisanger5v1_t','promisanger5v1_1','promisanger5v1_2',
            'promisanger5v1_3','promisanger5v1_4','promisanger5v1_5')
fields_all <- c('promisanger5v1_t','promisanger5v1_1','promisanger5v1_2',
                'promisanger5v1_3','promisanger5v1_4','promisanger5v1_5')
display_term <- "_t"

#GRAPHING VALUES
assay_min <- 32.9
assay_max <- 82.9
mass_display <- "N"
Percentage <- NA
max_type <- "worse"
score_type <- "T-Score"
relevant_questions <- list("Question" = NA,"Variable" = NA)

#DAILY REPORTING VALUES
Rows <- 6
Label <- "PROMIS Anger SF5"
Questions <- c("Never","Rarely","Sometimes","Often","Always")
names(Questions) <- c(1,2,3,4,5)
Context <- c("Not at all","Only a little","Somewhat","Quite a bit","Totally")
names(Context) <- c(0,1,2,3,4)
Contextual <- "!"
ContextualList <- list("!" = Context)

total.Frame <- data.frame(Begin = c(0,55,60,70), Result = c("<i>normal</i>","mild","<strong>moderate</strong>","<strong><i>SEVERE</i></strong>"))
Titles <- c("PROMIS Anger T Score:","Irritated:","Angry:","Ready to Explode:","Grouchy:","Annoyed:")

#TO BE COPIED VARIABLES
assay_q1 <- 'promisanger5v1_1'

#TABLE VARIABLES
table_title <- 'PROMIS Anger SF5'

assay_label <- '<font size = 5><strong>Anger</strong></font><br><font size = 3><i>(PROMIS)</i></font>'
assay_label_unformatted <- "<strong>Anger</strong> <i>(PROMIS)</i>"
assay_range <- "<i>(higher score=more anger, 20-54: normal, 55-59: mild,60-69: moderate,70+: severe)</i>"