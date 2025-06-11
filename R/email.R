
library(curl)

## constructor for email address (name + address). Should be used for
## sender, recipient, cc, etc.

email <- function(name, address)
{
    stopifnot(length(name) == 1)
    stopifnot(length(address) == 1)
    structure(list(name = name, address = address),
              class = "email")
}

username <- function(e)
{
    address <- 
        if (inherits(e, "email")) e$address
        else as.character(e)
    a <- strsplit(address, "@", fixed = TRUE)[[1]]
    stopifnot(length(a) == 2)
    a[[1]]
}

server <- function(hostname, ssl = "force", username = NULL)
{
    PWD <- Sys.getenv("EMAILPWD") # must be defined --- otherwise fail
    if (!nzchar(PWD))
        stop("Password not available: please define the EMAILPWD environment variable")
    structure(list(smtp_server = hostname, use_ssl = ssl,
                   username = username, # will use sender email if NULL
                   password = PWD),
              class = "smtpserver")
}


## FIXME: need more flexible way to format row. Usually two types: (a)
## individual course with breakup, and (b) subject wise totals.

message_body <- function(data, rollno, ..., ALL_COURSES, STUDENTS)
{
    .rollno <- fixRollNumber(rollno)
    if (is.null(data$RollNo)) data$RollNo <- data$RollNumber
    ## data must have columns CourseCode and RollNo
    stopifnot(all(c("CourseCode", "RollNo") %in% names(data)))
    data$RollNo <- fixRollNumber(data$RollNo)
    d <- subset(data, RollNo == .rollno)
    stopifnot(nrow(d) == 1)
    courseName <- ALL_COURSES[d$CourseCode, "CourseName"] # not currently used
    studentName <- STUDENTS[.rollno, "StudentName"]
    studentEmail <- STUDENTS[.rollno, "StudentEmailISI"]
    d$Attendance <- NULL
    d$CourseCode <- NULL
    d$RollNumber <- NULL
    d$RollNo <- NULL
    d$Name <- NULL
    msgBody <- sprintf("<p>Scores for %s: %s &lt;%s&gt;</p>\n",
                       .rollno, studentName, studentEmail)
    msgBody <- c("\n\n<html><body>",
                 msgBody,
                 "<pre>",
                 ## capture.output(print(knitr::kable(data.frame(score = unlist(d))))),
                 capture.output(print(data.frame(score = unlist(d)))),
                 "\n</pre>",
                 "\n</body></html>\n\n")
    structure(paste(msgBody, collapse = "\n"),
              to = email(studentName, studentEmail),
              ...,
              class = "msgbody")
}


message_header <- function(x, ...)
{
    hattr <- c(x, list(...))
    anames <- names(hattr)
    ## We will only process special names: from, to, cc, bcc, subject
    ## from / to / subject are mandatory and must be unique.
    ## cc / bcc / are optional and can be repeated
    stopifnot(sum(anames == "from") == 1); from <- hattr[["from"]]
    stopifnot(sum(anames == "to") == 1); to <- hattr[["to"]]
    stopifnot(sum(anames == "subject") == 1); subject <- hattr[["subject"]]
    if (!inherits(from, "email")) stop("'from' must be an 'email' object")
    if (!inherits(to, "email")) stop("'from' must be an 'email' object")
    if (length(subject) != 1) stop("'subject' must be a character string")
    CONTENT_TYPE <- "Content-Type: text/html; charset=UTF-8"
    FROM <- sprintf("From: %s <%s>", from$name, from$address)
    SENDER <- from$address
    TO <- sprintf("To: %s <%s>", to$name, to$address)
    RECIPIENTS <- to$address
    SUBJECT <- sprintf("Subject: %s", subject)
    CC <- character(0)
    for (i in seq_along(hattr)) {
        if (anames[[i]] == "cc" && inherits(hattr[[i]], "email")) {
            CC <- c(CC, sprintf("%s <%s>", hattr[[i]]$name, hattr[[i]]$address))
            RECIPIENTS <- c(RECIPIENTS, hattr[[i]]$address)
        }
    }
    if (length(CC)) CC <- sprintf("CC: %s", paste(CC, collapse = ", "))
    BCC <- character(0)
    for (i in seq_along(hattr)) {
        if (anames[[i]] == "bcc" && inherits(hattr[[i]], "email")) {
            BCC <- c(BCC, sprintf("%s <%s>", hattr[[i]]$name, hattr[[i]]$address))
            RECIPIENTS <- c(RECIPIENTS, hattr[[i]]$address)
        }
    }
    if (length(BCC)) BCC <- sprintf("BCC: %s", paste(BCC, collapse = ", "))
    header <- c(FROM, TO, CONTENT_TYPE, CC, BCC, SUBJECT) |> paste(collapse = "\n")
    structure(header,
              sender = SENDER,
              recipients = RECIPIENTS)
}

