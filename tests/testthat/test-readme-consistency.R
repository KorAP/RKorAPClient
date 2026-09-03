# Consistency checks of Readme.md against the package. These do not query
# anything and run in milliseconds, unlike the documentation prompting tests in
# test-readme-against-llm.R. They guard against the Readme drifting away from
# the package, which misleads readers and, since generated code is copied from
# it verbatim, LLMs as well.
#
# Readme.md is in .Rbuildignore, so it is absent from the built package and
# these tests skip when checking a tarball.

readmePath <- function() {
  for (path in c("Readme.md", "../Readme.md", "../../Readme.md")) {
    if (file.exists(path)) {
      return(path)
    }
  }
  NULL
}

skip_without_readme <- function() {
  skip_if_not(!is.null(readmePath()), "Readme.md not found (checking a built package?)")
}

newsPath <- function() {
  for (path in c("NEWS.md", "../NEWS.md", "../../NEWS.md")) {
    if (file.exists(path)) {
      return(path)
    }
  }
  NULL
}

# All R code blocks of the Readme, as character vectors of their lines
readmeRCodeBlocks <- function() {
  readme <- readLines(readmePath(), warn = FALSE)
  starts <- grep("^```[rR]$", readme)
  ends <- grep("^```$", readme)

  blocks <- list()
  for (start in starts) {
    end <- ends[ends > start][1]
    if (!is.na(end) && end > start + 1) {
      blocks[[length(blocks) + 1]] <- readme[(start + 1):(end - 1)]
    }
  }
  blocks
}

# Every call in an expression, as a list of calls
callsIn <- function(expression) {
  found <- list()
  walk <- function(x) {
    if (is.call(x)) {
      found[[length(found) + 1]] <<- x
      for (i in seq_along(x)) {
        element <- tryCatch(x[[i]], error = function(e) NULL)
        if (!is.null(element)) walk(element)
      }
    }
  }
  walk(expression)
  found
}

readmeCalls <- function() {
  calls <- list()
  for (block in readmeRCodeBlocks()) {
    parsed <- tryCatch(parse(text = paste(block, collapse = "\n")), error = function(e) NULL)
    if (is.null(parsed)) next
    for (expression in parsed) calls <- c(calls, callsIn(expression))
  }
  calls
}

# Parameter names of a function of this package. S4 methods whose signature
# differs from the generic are wrapped in a .local function by methods::
# setMethod(), whose formals are the ones actually documented and called.
packageFunctionParameters <- function(name) {
  method <- tryCatch(getMethod(name, "KorAPConnection"), error = function(e) NULL)
  if (is.null(method)) {
    plain <- tryCatch(get(name, envir = asNamespace("RKorAPClient")), error = function(e) NULL)
    return(if (is.function(plain)) names(formals(plain)) else NULL)
  }

  body <- body(method)
  isRematched <- is.call(body) && identical(body[[1]], as.name("{")) &&
    length(body) > 1 && is.call(body[[2]]) &&
    identical(body[[2]][[1]], as.name("<-")) &&
    identical(body[[2]][[2]], as.name(".local"))

  names(formals(if (isRematched) eval(body[[2]][[3]]) else method))
}

test_that("all R code blocks in the Readme parse", {
  skip_without_readme()

  blocks <- readmeRCodeBlocks()
  expect_gt(length(blocks), 0)

  for (block in blocks) {
    code <- paste(block, collapse = "\n")
    parsed <- tryCatch(parse(text = code), error = function(e) e)
    expect_false(
      inherits(parsed, "error"),
      label = paste0(
        "Readme code block starting with '", block[1], "' does not parse: ",
        if (inherits(parsed, "error")) conditionMessage(parsed) else ""
      )
    )
  }
})

test_that("every function called in the Readme exists", {
  skip_without_readme()

  exports <- getNamespaceExports("RKorAPClient")
  called <- unique(vapply(
    Filter(function(call) is.name(call[[1]]), readmeCalls()),
    function(call) as.character(call[[1]]),
    character(1)
  ))
  expect_gt(length(called), 0)

  # functions of this package must exist, others may come from any package the
  # examples load and are only checked if they can be resolved at all
  ours <- called[called %in% exports | grepl("KorAP|korap", called)]
  for (name in ours) {
    expect_true(
      name %in% exports,
      label = paste0("Function '", name, "()' used in the Readme is exported")
    )
  }

  # functions of the packages the Readme itself loads count as resolvable
  declared <- unique(vapply(
    Filter(
      function(call) {
        is.name(call[[1]]) &&
          as.character(call[[1]]) %in% c("library", "require") && length(call) > 1
      },
      readmeCalls()
    ),
    function(call) as.character(call[[2]]),
    character(1)
  ))
  installed <- declared[vapply(declared, requireNamespace, logical(1), quietly = TRUE)]
  skip_if_not(
    length(installed) == length(declared),
    paste0(
      "Readme loads packages that are not installed here: ",
      paste(setdiff(declared, installed), collapse = ", ")
    )
  )
  fromDeclared <- unlist(lapply(installed, getNamespaceExports))

  unknown <- called[!vapply(called, exists, logical(1)) & !(called %in% fromDeclared)]
  expect_equal(
    unknown, character(0),
    label = paste0(
      "Functions used in the Readme that cannot be resolved: ",
      paste(unknown, collapse = ", ")
    )
  )
})

test_that("every argument named in Readme calls exists in the function", {
  skip_without_readme()

  exports <- getNamespaceExports("RKorAPClient")
  checked <- 0

  for (call in readmeCalls()) {
    if (!is.name(call[[1]])) next
    name <- as.character(call[[1]])
    if (!(name %in% exports)) next

    parameters <- packageFunctionParameters(name)
    if (is.null(parameters) || "..." %in% parameters) {
      # a function taking ... accepts anything, so only known ones are of use
      parameters <- c(parameters, names(call)[-1])
    }

    given <- names(call)[-1]
    given <- given[nzchar(given)]
    for (argument in given) {
      checked <- checked + 1
      expect_true(
        argument %in% parameters,
        label = paste0("Argument '", argument, "' of ", name, "(), as used in the Readme,")
      )
    }
  }

  expect_gt(checked, 0)
})

test_that("versions mentioned in the Readme have a NEWS.md section", {
  skip_without_readme()
  skip_if_not(!is.null(newsPath()), "NEWS.md not found")

  readme <- paste(readLines(readmePath(), warn = FALSE), collapse = "\n")
  news <- readLines(newsPath(), warn = FALSE)

  claimed <- unique(regmatches(
    readme,
    gregexpr("RKorAPClient [0-9]+\\.[0-9]+[0-9.]*", readme)
  )[[1]])
  claimed <- sub("^RKorAPClient ", "", claimed)

  for (version in claimed) {
    expect_true(
      any(grepl(paste0("^# .*(^| )", gsub(".", "\\.", version, fixed = TRUE), "( |$)"), news)),
      label = paste0(
        "Version ", version, ", mentioned in the Readme, has a NEWS.md section, and thus"
      )
    )
  }
})
