# Helper function to find README.md file in current or parent directories
find_readme_path <- function() {
  readme_paths <- c("Readme.md", "../Readme.md", "../../Readme.md")
  for (path in readme_paths) {
    if (file.exists(path)) {
      return(path)
    }
  }
  return(NULL)
}

# Helper function to read README content
read_readme_content <- function() {
  readme_path <- find_readme_path()
  if (is.null(readme_path)) {
    return(NULL)
  }
  readme_content <- readLines(readme_path)
  paste(readme_content, collapse = "\n")
}

# Helper function to call OpenAI API
call_openai_api <- function(prompt, max_tokens = 500, temperature = 0.1) {
  library(httr2)
  library(jsonlite)

  tryCatch({
    response <- request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(list(
        model = "gpt-4.1-mini",
        messages = list(
          list(role = "user", content = prompt)
        ),
        max_tokens = max_tokens,
        temperature = temperature
      )) |>
      req_retry(max_tries = 3) |>
      req_perform()

    # Parse the response
    result <- response |> resp_body_json()
    result$choices[[1]]$message$content
  }, error = function(e) {
    if (grepl("429", as.character(e))) {
      skip("OpenAI API rate limit exceeded - please try again later or check your API key/credits")
    } else if (grepl("401", as.character(e))) {
      skip("OpenAI API authentication failed - please check your OPENAI_API_KEY")
    } else {
      stop(paste("OpenAI API error:", as.character(e)))
    }
  })
}

# KorAP URL for testing
KORAP_URL <- "https://korap.ids-mannheim.de/instance/wiki"

# Helper function to create README-guided prompt
create_readme_prompt <- function(task_description, specific_task) {
  readme_text <- read_readme_content()
  if (is.null(readme_text)) {
    stop("README.md not found")
  }

  paste0(
    "You are an expert R programmer. Based on the following README documentation for the RKorAPClient package, ",
    task_description, "\n\n",
    "IMPORTANT: Use the KorAP URL '", KORAP_URL, "' as the KorAPUrl parameter in KorAPConnection.\n\n",
    "README Documentation:\n",
    readme_text,
    "\n\nTask: ", specific_task,
    "\n\nProvide only the R code without explanations."
  )
}

# Helper function to extract R code from markdown code blocks
extract_r_code <- function(response_text) {
  # Remove markdown code blocks if present
  code <- gsub("```[rR]?\\n?", "", response_text)
  code <- gsub("```\\n?$", "", code)
  # Remove leading/trailing whitespace
  trimws(code)
}

# Helper function to test code syntax
test_code_syntax <- function(code) {
  tryCatch({
    parse(text = code)
    TRUE
  }, error = function(e) {
    cat("Syntax error:", as.character(e), "\n")
    FALSE
  })
}

# Helper function to run code if RUN_LLM_CODE is set
run_code_if_enabled <- function(code, test_name) {
  if (nzchar(Sys.getenv("RUN_LLM_CODE")) && Sys.getenv("RUN_LLM_CODE") == "true") {
    cat("Running generated code for", test_name, "...\n")
    tryCatch({
      result <- eval(parse(text = code))
      cat("Code executed successfully. Result type:", class(result), "\n")
      if (is.data.frame(result)) {
        cat("Result dimensions:", nrow(result), "rows,", ncol(result), "columns\n")
        if (nrow(result) > 0) {
          cat("First few rows:\n")
          print(head(result, 3))
        }
      } else {
        cat("Result preview:\n")
        print(result)
      }
      return(TRUE)
    }, error = function(e) {
      cat("Runtime error:", as.character(e), "\n")
      return(FALSE)
    })
  } else {
    cat("Skipping code execution (set RUN_LLM_CODE=true to enable)\n")
    return(NA)
  }
}

test_that("GPT-4.1 mini can solve frequency query task with README guidance", {
  skip_if_not(nzchar(Sys.getenv("OPENAI_API_KEY")), "OPENAI_API_KEY not set")
  skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

  # Create the prompt with README context and task
  prompt <- create_readme_prompt(
    "write R code to perform a frequency query for the word 'Deutschland' across multiple years (2010-2015). The code should use the RKorAPClient package and return a data frame with year and frequency columns.",
    "Write R code to query frequency of 'Deutschland' from 2010-2015 using RKorAPClient."
  )

  # Call OpenAI API
  generated_response <- call_openai_api(prompt, max_tokens = 500)
  generated_code <- extract_r_code(generated_response)

  # Basic checks on the generated code
  expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
  expect_true(grepl("frequencyQuery", generated_code), "Generated code should include frequencyQuery")
  expect_true(grepl("Deutschland", generated_code), "Generated code should include the search term 'Deutschland'")
  expect_true(grepl("201[0-5]", generated_code), "Generated code should include years 2010-2015")
  expect_true(grepl(KORAP_URL, generated_code, fixed = TRUE), "Generated code should include the specified KorAP URL")

  # Check that the generated code contains essential RKorAPClient patterns
  expect_true(grepl("\\|>", generated_code) || grepl("%>%", generated_code),
              "Generated code should use pipe operators")

  # Test code syntax
  syntax_valid <- test_code_syntax(generated_code)
  expect_true(syntax_valid, "Generated code should be syntactically valid R code")

  # Print the generated code for manual inspection
  cat("Generated code:\n", generated_code, "\n")

  # Run the code if RUN_LLM_CODE is set
  execution_result <- run_code_if_enabled(generated_code, "frequency query")
  if (!is.na(execution_result)) {
    expect_true(execution_result, "Generated code should execute without runtime errors")
  }
})

test_that("GPT-4.1 mini can solve collocation analysis task with README guidance", {
  skip_if_not(nzchar(Sys.getenv("OPENAI_API_KEY")), "OPENAI_API_KEY not set")
  skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

  # Create the prompt for collocation analysis
  prompt <- create_readme_prompt(
    "write R code to perform a collocation analysis for the lemma 'setzen'. The code should use the RKorAPClient package's collocationAnalysis function.",
    "Write R code to perform collocation analysis for 'setzen' using RKorAPClient."
  )

  # Call OpenAI API
  generated_response <- call_openai_api(prompt, max_tokens = 500)
  generated_code <- extract_r_code(generated_response)

  # Basic checks on the generated code
  expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
  expect_true(grepl("collocationAnalysis", generated_code), "Generated code should include collocationAnalysis")
  expect_true(grepl("setzen", generated_code), "Generated code should include the search term 'setzen'")
  expect_true(grepl("auth", generated_code), "Generated code should include auth() for collocation analysis")
  expect_true(grepl(KORAP_URL, generated_code, fixed = TRUE), "Generated code should include the specified KorAP URL")

  # Check for collocation analysis parameters
  expect_true(grepl("leftContextSize|rightContextSize", generated_code),
              "Generated code should include context size parameters")

  # Test code syntax
  syntax_valid <- test_code_syntax(generated_code)
  expect_true(syntax_valid, "Generated code should be syntactically valid R code")

  # Print the generated code for manual inspection
  cat("Generated collocation analysis code:\n", generated_code, "\n")

  # Run the code if RUN_LLM_CODE is set
  execution_result <- run_code_if_enabled(generated_code, "collocation analysis")
  if (!is.na(execution_result)) {
    expect_true(execution_result, "Generated code should execute without runtime errors")
  }
})

test_that("GPT-4.1 mini can solve corpus query task with README guidance", {
  skip_if_not(nzchar(Sys.getenv("OPENAI_API_KEY")), "OPENAI_API_KEY not set")
  skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

  # Create the prompt for corpus query
  prompt <- create_readme_prompt(
    "write R code to perform a simple corpus query for 'Hello world' and fetch all results. The code should use the RKorAPClient package.",
    "Write R code to query 'Hello world' and fetch all results using RKorAPClient."
  )

  # Call OpenAI API
  generated_response <- call_openai_api(prompt, max_tokens = 300)
  generated_code <- extract_r_code(generated_response)

  # Basic checks on the generated code
  expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
  expect_true(grepl("corpusQuery", generated_code), "Generated code should include corpusQuery")
  expect_true(grepl("Hello world", generated_code), "Generated code should include the search term 'Hello world'")
  expect_true(grepl("fetchAll", generated_code), "Generated code should include fetchAll")
  expect_true(grepl(KORAP_URL, generated_code, fixed = TRUE), "Generated code should include the specified KorAP URL")

  # Check that the generated code follows the README example pattern
  expect_true(grepl("\\|>", generated_code) || grepl("%>%", generated_code),
              "Generated code should use pipe operators")

  # Test code syntax
  syntax_valid <- test_code_syntax(generated_code)
  expect_true(syntax_valid, "Generated code should be syntactically valid R code")

  # Print the generated code for manual inspection
  cat("Generated corpus query code:\n", generated_code, "\n")

  # Run the code if RUN_LLM_CODE is set
  execution_result <- run_code_if_enabled(generated_code, "corpus query")
  if (!is.na(execution_result)) {
    expect_true(execution_result, "Generated code should execute without runtime errors")
  }
})
