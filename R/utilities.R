
require(jsonlite)

### Processing tools to convert, split, or merge CSV files

unrowname <- function(x) { rownames(x) <- NULL; x }

fixRollNumber <- function(x, check.dup = TRUE)
{
    x <- gsub("-", "", toupper(x), fixed = TRUE)
    if (anyDuplicated(x)) stop("duplicated RollNo")
    x
}

getLoc <- function(var, default)
{
    ans <- Sys.getenv(var)
    if (nzchar(ans)) ans else default
}

## basic validation checks

import_csv <- function(...)
{
    read.csv(file.path(...), comment.char = "#", check.names = FALSE)
}

validateSetup <- function(COURSEDIR = NULL,
                          courses = c("MSTAT.csv", "MSQE.csv", "BSDS.csv"))
{
    ## setup source folders
    BASEDIR <<- getLoc("BASEDIR", ".")
    if (is.null(COURSEDIR))
        COURSEDIR <<- file.path(BASEDIR, "CSV", "Courses")
    INSTRDIR <<- file.path(BASEDIR, "CSV", "Instructors")
    STUDENTDIR <<- file.path(BASEDIR, "CSV", "Students")
    SCOREDIR <<- file.path(BASEDIR, "CSV", "Scores")
    OUTDIR <<- file.path(BASEDIR, "Output")
    if (!dir.exists(OUTDIR)) dir.create(OUTDIR)

    ## read common information (make these package data)

    ALL_COURSES <<- do.call(rbind,
                            lapply(file.path(COURSEDIR, courses), import_csv))

    ## add implicit course names for backpapers
    BP_COURSES <- within(ALL_COURSES,
    {
        CourseCode <- paste0(CourseCode, "_BP")
        CourseName <- paste0(CourseName, " (Backpaper)")
    })
    ALL_COURSES <<- rbind(ALL_COURSES, BP_COURSES)
    ## TODO: similar for non-credit courses if necessary

    INSTRUCTORS <<- import_csv(INSTRDIR, "Instructors.csv")
    rownames(INSTRUCTORS) <<- INSTRUCTORS$InstructorCode

    if (anyDuplicated(ALL_COURSES$CourseCode)) stop("duplicated CourseCode")
    if (anyDuplicated(ALL_COURSES$CourseName)) stop("duplicated CourseName")
    rownames(ALL_COURSES) <<- ALL_COURSES$CourseCode
    STUDENTS <<- combineStudents(STUDENTDIR)
}


## Create a single data frame containing all students listed under STUDENTDIR

combineStudents <- function(DIR)
{
    slist <- lapply(list.files(DIR, recursive = TRUE, full.names = TRUE),
                    import_csv)
    d <- do.call(rbind, slist)
    d$RollNo <- fixRollNumber(d$RollNo)
    rownames(d) <- d$RollNo
    d
}


## Split scores of multiple courses in a single CSV file into
## course-specific CSV files. This should normally not be done, and is
## needed only for one-time processing of historical data. Output file
## names are of the form SESSIONID-CourseCode.csv. Check and stop
## before overwriting existing files (this may happen when data from
## common courses such as Optimization Techniques) are split over
## separate files (say for MSQE and MSTAT).

splitScores <- function(session, infile = "combined.csv", debug = TRUE)
{
    inloc <- file.path(BASEDIR, "CSV", "Scores", session, infile)
    if (file.exists(inloc))
        combined <- import_csv(inloc)
    else
        stop("Input file ", inloc, " not found.")
    session_outdir <- file.path(OUTDIR, session)
    if (!dir.exists(session_outdir)) dir.create(session_outdir)
    stopifnot(names(combined)[[1]] == "RollNo")
    codes <- names(combined)[-1]
    code_prefix <- strsplit(codes, "_", fixed = TRUE) |> sapply("[", 1)
    ## check that codes are all valid
    if (!all(code_prefix %in% ALL_COURSES$CourseCode))
        stop("Invalid CourseCode found: ", paste(code_prefix, collapse = ", "))
    if (debug) print(cbind(ALL_COURSES[code_prefix, ], NOTE = codes))
    ## Loop over CourseCode-s and write one file for each
    exportScores <- function(code) {
        ## Extract scores for specific code. Add student name for readability
        dcode <- na.omit(combined[c("RollNo", code)])
        d <- with(dcode,
                  data.frame(RollNo = RollNo,
                             Name = STUDENTS[RollNo, "StudentName"]))
        d[[code]] <- dcode[[code]] # Total score
        outfile <- file.path(session_outdir, paste0(code, ".csv"))
        ## str(d, give.attr = FALSE)
        str(outfile)
        if (file.exists(outfile))
            stop(outfile, " already exists. Aborting export")
        else
            write.csv(d, file = outfile, row.names = FALSE, quote = FALSE)
    }
    lapply(codes, exportScores)
    invisible()
}


courseDetails <- function(session)
{
    SESSIONDIR <- file.path(BASEDIR, "CSV", "Scores", session)
    d <- import_csv(SESSIONDIR, "instructors.csv")
    ## FIXME: Do we need some way to override course name, e.g., for
    ## special topics courses?

    d <- within(d,
    {
        id <- strsplit(CourseCode, "_") |> sapply("[", 1)
        CourseCode <- factor(CourseCode, levels = unique(CourseCode)) # no sorting
        CourseName <- ALL_COURSES[id, "CourseName"]
        ShortName <- ALL_COURSES[id, "ShortName"]
        ShortName <- ifelse(is.na(ShortName), as.character(CourseCode), ShortName)
        InstructorName <- INSTRUCTORS[InstructorCode, "InstructorName"]
    })

    ## combine multiple instructors
    s <- split(d, ~ CourseCode)
    s <- 
        lapply(s,
           function(x) {
               stopifnot(length(unique(x$CourseCode)) == 1)
               stopifnot(length(unique(x$CourseName)) == 1)
               with(x,
                    data.frame(CourseCode = unique(CourseCode),
                               CourseName = unique(CourseName),
                               ShortName = unique(ShortName),
                               InstructorName = paste(InstructorName, collapse = " + ")))
           })
    do.call(rbind, s)
}



## Combine scores in multiple courses in a session into a single CSV
## file for all students in a 'batch'. A batch is usually defined by
## the first four letters of the RollNo, but may need to be
## supplemented if there are repeat students. This is controlled by
## the 'batchRE' argument, which can be a vector. Normally, this
## should just be a single string such as '^MQ21' or even just 'MQ21'

combineScores <- function(session, batchRE, courseDetails = TRUE)
{
    keep <- character()
    for (pattern in batchRE)
        keep <- append(keep, grep(pattern, STUDENTS$RollNo, value = TRUE))
    ## 'keep' now has all roll numbers we wish to include. Next, we
    ## need to read in all scores for this session, and create a table
    ## of RollNo by course
    SESSIONDIR <- file.path(BASEDIR, "CSV", "Scores", session)
    score_files <- list.files(SESSIONDIR, full.names = TRUE)
    sessionDetails <- courseDetails(session)
    ## sanity check: should end with .csv
    fext <- tools::file_ext(score_files)
    if (!all(fext == "csv"))
        stop("Unexpected file extension::",
             unique(fext) |> paste(collapse = ", "),
             "::")
    course_codes <- gsub("[\\.]csv$", "", basename(score_files))
    ## str(course_codes)
    ## retain only those whose first part is a valid course code
    ccorig <- sapply(strsplit(course_codes,
                              "_",
                              fixed = TRUE),
                     "[", 1)
    ok <- ccorig %in% ALL_COURSES$CourseCode
    course_codes <- course_codes[ok]
    ccorig <- ccorig[ok]
    ## Reorder according to the order in
    ## SESSIONDIR/instructors.csv. i.e.,
    ## sessionDetails$CourseCode. Note that 'course_codes' can contain
    ## _BP suffixes, whereas sessionDetails$CourseCode (and ccorig) will not.
#    course_codes <- 
#    print(course_codes)

    ## match ccorig with sessionDetails$CourseCode
    p <- order(match(ccorig, sessionDetails$CourseCode))
    course_codes <- course_codes[p]
    ccorig <- ccorig[p]

    processCourse <- function(code) {
        d <- import_csv(SESSIONDIR, paste0(code, ".csv"))
        d$RollNo <- fixRollNumber(d$RollNo)
        rownames(d) <- d$RollNo
        scores <- d[keep, "Total"]
        ## str(scores)
        if (all(is.na(scores))) # none of these students took this course
            NULL
        else
            structure(scores,
                      dim = c(length(keep), 1),
                      dimnames = list(keep, code))
    }
    slist <- sapply(course_codes, processCourse, simplify = FALSE)
    ## str(slist)
    slist <- slist[!sapply(slist, is.null)]
    ## sort in order of courses given in session-specific 
    ## slist <- slist[sort(names(slist))] # alphabetical sorting - not a good idea
    score_matrix <- do.call(cbind, slist)

    ## TODO:
    ##
    ## Add a Total column, but this needs to handle backpapers (which
    ## is OK if we use the convention of _BP to indicate backpaper
    ## scores), and omit any non-credit courses. So far we have no
    ## conventions for NC courses (other that ProgIntro, which is
    ## always NC).

    agg <- computeTotal(score_matrix)

    ## TODO: go through _BP columns present and combine them with
    ## original scores to obtain updated scores; drop _BP column. We
    ## want to still record the original and backpaper scores
    ## separately; let's do this (as a character string) as follows: let's say
    ## - original and BP scores are (17, 41), then we should have "41_(17, 41)"
    ## - original and BP scores are (41, 17), then we should have "41_(41, 17)"

    cscore <-  matrix("", nrow(score_matrix), ncol(score_matrix))
    dimnames(cscore) <- dimnames(score_matrix)
    cscore[] <- as.character(score_matrix)
    bp <- grep("_BP$", colnames(score_matrix), value = TRUE)
    if (length(bp)) {
        for (code in bp) {
            ocode <- gsub("_BP$", "", code)
            if (!(ocode %in% colnames(score_matrix)))
                stop("CourseCode not found: ", ocode)
            wbp <- is.finite(score_matrix[, code]) # others did not give backpaper
            if (any(score_matrix[wbp, code] > 45)) stop("backpaper score exceeds 45.")
            cscore[ wbp, ocode ] <-
                sprintf("%g_(%g, %g)", pmax(score_matrix[wbp, ocode], score_matrix[wbp, code]),
                        score_matrix[wbp, ocode], score_matrix[wbp, code])
        }
    }
    ## drop _BP columns
    
    cscore <- cscore[ , !grepl("_BP$", colnames(score_matrix)), drop = FALSE]
    
    ## We return a (character) matrix here instead of a data frame (to
    ## simplify DataTable input). But one consequnce of that is the
    ## JSON discards column names. So provide that separately in the
    ## return value when exporting to JSON.
    ans <- list(data = cbind(RollNo = rownames(score_matrix),
                             Name = STUDENTS[rownames(score_matrix), "StudentName"],
                             cscore, Total = agg))
    rownames(ans$data) <- NULL
    ans$header <- colnames(ans$data)

    ## Optionally add attributes giving full course names and
    ## instructors
    if (courseDetails)
        ans$courseDetails <-
            subset(sessionDetails,
                   CourseCode %in% names(slist))
    ## str(ans)
    ans
}


## Take a data frame with RollNo (as row names) and courses (as column
## names), and calculate a total column. This needs to handle
## backpapers and non-credit courses. Backpaper scores are in separate
## files with suffix _BP. Noncredit scores are negative, except for
## 'ProgIntro' which is always noncredit.

## Algo: drop 'ProgIntro' and all negative scores. For any column
## ending in BP, drop it after taking max of it with the corresponding
## column after dropping the _BP suffix.

computeTotal <- function(x)
{
    print(x)
    if (!inherits(x, "data.frame")) x <- as.data.frame(x)
    x$ProgIntro <- NULL
    bp <- grep("_BP$", colnames(x), value = TRUE)
    if (length(bp)) {
        for (code in bp) {
            ## update
            wna <- is.na(x[[code]])
            x[[code]][ wna ] <- 0
            ocode <- gsub("_BP$", "", code)
            if (is.null(x[[ocode]]))
                stop("CourseCode not found in pre-backpaper score matrix: ", ocode)
            x[[ocode]] <- pmax(x[[ocode]], x[[code]])
            x[[code]] <- NULL
        }
    }
    x <- data.matrix(x)
    countCourses <- function(s) sum(is.finite(s) & s >= 0)
    ncourse <- apply(x, 1, countCourses)
    if (any(ncourse != 5))
    {
        message("FIXME - Students with number of scores different from 5:")
        ## str(x)
        xIncomplete <- x[ncourse != 5, , drop = FALSE]
        ## str(xIncomplete)
        rownames(xIncomplete) <-
            sprintf("%s (%s)",
                    rownames(xIncomplete),
                    STUDENTS[rownames(xIncomplete), "StudentName"])
        ## n5courses <- ncourse[ncourse != 5]
        ## names(n5courses) <-
        ##     STUDENTS[names(n5courses), "StudentName"]
        ## print(n5courses)
        print(xIncomplete)
    }
    rowTotal <- function(s) {
        k <- sum(is.finite(s) & s >= 0)
        if (k < 5) {
            print(s)
            stop("Expected 5 valid scores, found ", k)
        }
        if (k > 5) {
            print(s)
            warning("Expected 5 valid scores, found ", k, ". Keeping top 5")
            s <- sort(s[is.finite(s)], decreasing = TRUE)[1:5]
        }
        sum(s[is.finite(s) & s >= 0])
    }
    apply(x, 1, rowTotal)
}


## To generate transcripts, we need info on all sessions for a
## student. One option is to do this for all students in a batch, and
## the export to JSON and do the rest using Javascript. We will still
## require that the relevant sessions be specified explicitly (so that
## repeat candidates get their correct sessions)

combineSessions <- function(..., batchRE, pretty = TRUE)
{
    slist <- sapply(c(...), combineScores,
                    batchRE = batchRE,
                    courseDetails = TRUE,
                    simplify = FALSE)
    courseList <- lapply(slist, "[[", "courseDetails")
    courseList <- do.call(rbind, courseList)
    rownames(courseList) <- NULL
    ans <- list(scores = lapply(slist, "[", c("data", "header")),
                courses = tapply(courseList, ~CourseCode,
                                 FUN = "[", c("CourseName", "InstructorName", "ShortName"),
                                 simplify = FALSE) #|> do.call(what = rbind)
                )
    toJSON(ans, matrix = "rowmajor", pretty = pretty)
}

## Current session only

makeSessionHTML <- function(scoredata, batch, session)
{
    outfile <- file.path(OUTDIR, sprintf("%s-%s.html", batch, session))
    message("\n----------\nWriting ", outfile, "\n---------\n")
    template <- readLines(file.path(BASEDIR, "html", "templates", "session-batch.html"))
    template <- gsub("{{BATCH}}", batch, template, fixed = TRUE)
    template <- gsub("{{SESSION}}", session, template, fixed = TRUE)
    template <- gsub("{{SCOREDATA}}", scoredata, template, fixed = TRUE)
    cat(template, file = outfile, sep = "\n")
}


## Other utilities

import_score <- function(file, attendance = 0, all = FALSE)
{
    course <- tools::file_path_sans_ext(basename(file))
    d <- import_csv(file)
    if (is.null(d$Attendance)) d$Attendance <- attendance
    if (is.null(d$RollNo)) d$RollNo <- d$RollNumber
    d <- d[!is.na(d$Total),
           if (all) TRUE else c("CourseCode", "RollNo", "Total", "Attendance")]
    d$RollNo <- fixRollNumber(d$RollNo)
    ## ans <- cbind(Course = course, d)
    ## rownames(ans) <- ans$RollNo
    ## ans
    rownames(d) <- d$RollNo
    d
}

collectAllScores <- function(session)
{
    SESSIONDIR <- file.path(BASEDIR, "CSV", "Scores", session)
    score_files <- list.files(SESSIONDIR, full.names = TRUE)
    score_files <- score_files[tools::file_ext(score_files) == "csv" &
                               startsWith(basename(score_files), session)]
    sessionDetails <- courseDetails(session)
    ## str(sessionDetails)
    names(score_files) <- tools::file_path_sans_ext(basename(score_files))
    sapply(score_files,
           import_score,
           simplify = FALSE) |> do.call(what = rbind)
}

