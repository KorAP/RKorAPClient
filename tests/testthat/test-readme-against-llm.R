# Models the documentation is prompted with. Cheap, fast models are used on
# purpose: the tasks are simple, and if such a model cannot follow the Readme,
# that says something about the Readme, which is what is being tested here.
# Override with a comma separated list in RKORAP_LLM_MODELS, e.g. to add
# OpenAI's cheapest models, "gpt-5-nano" or "gpt-5.6-luna".
defaultLlmModels <- c(
  "gemini-3.5-flash-lite",
  "claude-sonnet-5",
  "z-ai/glm-5.3-flash" # via OpenRouter, see openRouterProvider() below
)

llmModels <- function() {
  configured <- Sys.getenv("RKORAP_LLM_MODELS", unset = NA_character_)
  if (is.na(configured)) {
    return(defaultLlmModels)
  }
  # set but empty means no models at all, which is how these tests are kept out
  # of the pipeline job that runs everything else
  models <- trimws(strsplit(configured, ",", fixed = TRUE)[[1]])
  models[nzchar(models)]
}

# Provider and API key environment variable belonging to a model id
llmProvider <- function(model) {
  if (grepl("^gpt-", model, ignore.case = TRUE)) {
    list(name = "openai", keyVar = "OPENAI_API_KEY")
  } else if (grepl("^claude-", model, ignore.case = TRUE)) {
    list(name = "claude", keyVar = "ANTHROPIC_API_KEY")
  } else if (grepl("^gemini-", model, ignore.case = TRUE)) {
    list(name = "gemini", keyVar = "GOOGLE_API_KEY")
  } else if (grepl("^hf:", model, ignore.case = TRUE)) {
    stop(paste(
      "The Synthetic API was replaced by OpenRouter, so", model,
      "is no longer supported - use z-ai/glm-5.3-flash instead"
    ))
  } else if (grepl("/", model, fixed = TRUE)) {
    # OpenRouter model ids are author/model. Its endpoint is OpenAI compatible,
    # but tidyllm's openai provider does not allow for a custom base url, so
    # these are queried directly (see below).
    list(
      name = "openrouter",
      keyVar = "OPENROUTER_API_KEY",
      baseUrl = "https://openrouter.ai/api/v1"
    )
  } else {
    stop(paste(
      "Unsupported model:", model,
      "- supported are gpt-*, claude-*, gemini-* and author/model (OpenRouter)"
    ))
  }
}

# Models that go through tidyllm, as opposed to those queried directly
usesTidyllm <- function(model) {
  is.null(llmProvider(model)$baseUrl)
}

# OpenRouter serves one model from a couple of dozen hosts and would pick one by
# availability, which the tests cannot use: the hosts differ in quantization and
# answer a documentation prompt in anything between one second and seven
# minutes. So the hosts are named, and everything outside the list is ruled out,
# which keeps a change in the generated code about the Readme rather than about
# which machine answered.
#
# OpenRouter works the list from the front and moves on whenever a host
# declines. Baseten answers in about a second and serves every request, as long
# as the OpenRouter account holds a Baseten key of its own, under Settings ->
# Integrations. Without one, OpenRouter reaches the hosts through pools it
# shares between all of its users, and those are exhausted often enough to cost
# about one prompt in eight. The rest of the list carries those: Relace takes
# some fifteen seconds and is the cheapest of the hosts, Z.ai tens of seconds
# and has always answered. Relace serves the model in fp4 rather than fp8,
# which is why it is not the first choice, although it is the fastest of the
# hosts on shared capacity.
#
# Override with RKORAP_OPENROUTER_PROVIDER, comma separated, in that order.
openRouterProviders <- function() {
  configured <- Sys.getenv(
    "RKORAP_OPENROUTER_PROVIDER",
    unset = "baseten/fp8,relace/fp4,z-ai/fp8"
  )
  hosts <- trimws(strsplit(configured, ",", fixed = TRUE)[[1]])
  as.list(hosts[nzchar(hosts)])
}

# Helper function to skip if the API key of the given model is not available
skip_if_no_api_key <- function(model) {
  keyVar <- llmProvider(model)$keyVar
  skip_if_not(
    nzchar(Sys.getenv(keyVar)),
    paste0("No API key for ", model, " found (need ", keyVar, ")")
  )
}

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

  # Find the line with "## Installation" and truncate before it
  installation_line <- grep("^## Installation", readme_content, ignore.case = TRUE)
  if (length(installation_line) > 0) {
    readme_content <- readme_content[1:(installation_line[1] - 1)]
  }

  paste(readme_content, collapse = "\n")
}

# Helper function to call an OpenAI compatible endpoint that tidyllm cannot be
# pointed at, because its openai provider takes no custom base url
call_openai_compatible_api <- function(prompt, model, temperature, provider) {
  body <- list(
    model = model,
    temperature = temperature,
    messages = list(list(role = "user", content = prompt))
  )
  if (provider$name == "openrouter") {
    body$provider <- list(
      order = openRouterProviders(),
      allow_fallbacks = FALSE
    )
  }
  response <- httr2::request(paste0(provider$baseUrl, "/chat/completions")) |>
    httr2::req_auth_bearer_token(Sys.getenv(provider$keyVar)) |>
    httr2::req_headers(
      # attributes the requests to the package in the OpenRouter activity log
      "HTTP-Referer" = "https://github.com/KorAP/RKorAPClient",
      "X-Title" = "RKorAPClient documentation prompting tests"
    ) |>
    httr2::req_body_json(body) |>
    # every named host can be out of capacity at once, which passes as a 429
    httr2::req_retry(max_tries = 5) |>
    # generous, since the last hosts of the list take minutes rather than seconds
    httr2::req_timeout(300) |>
    httr2::req_perform()

  httr2::resp_body_json(response)$choices[[1]]$message$content
}

# Helper function to call LLM API using tidyllm
call_llm_api <- function(prompt, model, max_tokens = 500, temperature = 0.1) {
  cat("Calling LLM API with model:", model, "\n")
  # Only print prompt up to the beginning of README content
  readme_start <- regexpr("README Documentation:", prompt, fixed = TRUE)
  if (readme_start > 0) {
    prompt_preview <- substr(prompt, 1, readme_start - 1)
    cat("Prompt (up to README):\n", prompt_preview, "\n")
  } else {
    cat("Prompt:\n", prompt, "\n")
  }
  tryCatch(
    {
      provider <- llmProvider(model)

      if (!usesTidyllm(model)) {
        call_openai_compatible_api(
          prompt,
          model = model,
          temperature = temperature,
          provider = provider
        )
      } else {
        # Use tidyllm unified API
        result <- tidyllm::llm_message(prompt) |>
          tidyllm::chat(
            .provider = switch(provider$name,
              openai = tidyllm::openai(),
              claude = tidyllm::claude(),
              gemini = tidyllm::gemini()
            ),
            .model = model,
            .temperature = temperature,
            .max_tries = 3
          )

        # Extract the reply text
        tidyllm::get_reply(result)
      }
    },
    error = function(e) {
      message <- as.character(e)
      # Conditions of the account rather than of the documentation: these must
      # not turn a documentation test red
      if (grepl("429", message)) {
        skip("LLM API rate limit exceeded - please try again later or check your API key/credits")
      } else if (grepl("401|403", message)) {
        skip(paste0(
          "LLM API authentication failed - please check ",
          llmProvider(model)$keyVar
        ))
      } else if (grepl("402|credit balance|billing|quota|insufficient", message, ignore.case = TRUE)) {
        skip(paste0("No credits available for ", model, ": ", message))
      } else if (grepl("404", message) && llmProvider(model)$name == "openrouter") {
        # none of the named hosts is serving the model at the moment
        skip(paste0(
          "OpenRouter has no endpoint for ", model, " at ",
          paste(openRouterProviders(), collapse = ", ")
        ))
      } else {
        stop(paste("LLM API error:", message))
      }
    }
  )
}

# Helper function to create README-guided prompt
#
# Shaped the way a user would reasonably prompt, since what is tested here is
# also what we recommend: the documentation first, the task once and at the end,
# and the expectations stated rather than left to a persona ("you are an expert
# R programmer" moves the wording of an answer, not its correctness).
create_readme_prompt <- function(task_description) {
  readme_text <- read_readme_content()
  if (is.null(readme_text)) {
    stop("README.md not found")
  }

  paste0(
    "The following is the README of the R package RKorAPClient.\n\n",
    readme_text,
    "\n\nTask, based on that documentation: ", task_description,
    "\n\nWrite clear, idiomatic tidyverse code, in the style of the README's",
    " own examples. Answer with a single R code block and nothing else."
  )
}

# Helper function to extract R code from markdown code blocks
extract_r_code <- function(response_text) {
  # Asked for code alone, a model may still explain itself around it, or offer a
  # second way of doing the same in a block of its own. Only the first block is
  # what was asked for; stripping the fences and keeping everything else puts
  # the prose in between into the code, where it does not parse.
  block <- stringr::str_match(response_text, "(?s)```[^\\n]*\\n(.*?)```")[1, 2]
  trimws(if (is.na(block)) response_text else block)
}

test_that("the first code block is what is taken from a reply", {
  fenced <- function(...) paste(c(...), collapse = "\n")

  expect_equal(
    extract_r_code(fenced("```r", "corpusStats(kco)", "```")),
    "corpusStats(kco)"
  )
  # prose around the block does not belong to the code
  expect_equal(
    extract_r_code(fenced("Here you are:", "```R", "corpusStats(kco)", "```", "Hope that helps!")),
    "corpusStats(kco)"
  )
  # a second block, offered as an alternative, would not parse together with the
  # sentence introducing it
  expect_equal(
    extract_r_code(fenced(
      "```r", "corpusStats(kco)", "```",
      "Or, as a data frame:",
      "```r", "corpusStats(kco, as.df = TRUE)", "```"
    )),
    "corpusStats(kco)"
  )
  # a reply that took "only the R code" literally has no fences to look for
  expect_equal(extract_r_code("corpusStats(kco)"), "corpusStats(kco)")
})

# Helper function to test code syntax
test_code_syntax <- function(code) {
  tryCatch(
    {
      parse(text = code)
      TRUE
    },
    error = function(e) {
      cat("Syntax error:", as.character(e), "\n")
      FALSE
    }
  )
}

# Helper function to run code if RUN_LLM_CODE is set
run_code_if_enabled <- function(code, test_name) {
  if (nzchar(Sys.getenv("RUN_LLM_CODE")) && Sys.getenv("RUN_LLM_CODE") == "true") {
    cat("Running generated code for", test_name, "...\n")
    tryCatch(
      {
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
      },
      error = function(e) {
        cat("Runtime error:", as.character(e), "\n")
        return(FALSE)
      }
    )
  } else {
    cat("Skipping code execution (set RUN_LLM_CODE=true to enable)\n")
    return(NA)
  }
}

for (model in llmModels()) {
  test_that(paste(model, "can solve frequency query task with README guidance"), {
    # Skip if offline
    skip_if_offline()

    # Skip if no API keys are set
    skip_if_no_api_key(model)

    # tidyllm is only suggested, so the tests must not fail without it
    if (usesTidyllm(model)) skip_if_not_installed("tidyllm")

    # Check for README file
    skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

    # Create the prompt with README context and task
    prompt <- create_readme_prompt(
      "write R code to perform a frequency query for the word 'Demokratie' across the past three years. The code should use the RKorAPClient package and return a data frame."
    )

    # Call LLM API
    generated_response <- call_llm_api(prompt, model, max_tokens = 500)
    generated_code <- extract_r_code(generated_response)

    # Basic checks on the generated code
    expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
    expect_true(grepl("frequencyQuery", generated_code), "Generated code should include frequencyQuery")
    expect_true(grepl("Demokratie", generated_code), "Generated code should include the search term 'Demokratie'")
    last_year <- as.numeric(format(Sys.Date(), "%Y")) - 1

    expect_true(grepl("Date in", generated_code), "Generated code should vc restriction on years")

    # Check that the generated code contains essential RKorAPClient patterns
    # expect_true(grepl("\\|>", generated_code) || grepl("%>%", generated_code), "Generated code should use pipe operators")

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


  test_that(paste(model, "can solve collocation analysis task with README guidance"), {
    # Skip if offline
    skip_if_offline()

    # Skip if no API keys are set
    skip_if_no_api_key(model)

    # tidyllm is only suggested, so the tests must not fail without it
    if (usesTidyllm(model)) skip_if_not_installed("tidyllm")

    # Check for README file
    skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

    # Create the prompt for collocation analysis
    prompt <- create_readme_prompt(
      paste("Write R code to perform a collocation analysis for the lemma 'leverage' based on the current English Wikipedia Corpus using default parameters", "and show the three highest collocates according to their log dice score.
  ")
    )

    # Call LLM API
    generated_response <- call_llm_api(prompt, model, max_tokens = 500)
    generated_code <- extract_r_code(generated_response)

    # Basic checks on the generated code
    expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
    expect_true(grepl("collocationAnalysis", generated_code), "Generated code should include collocationAnalysis")
    # both ways of asking for the lemma are correct: the annotation layer in the
    # query, as the Readme shows it, or collocationAnalysis' lemmatizeNodeQuery,
    # which builds the same query from a plain word
    expect_true(grepl("leverage", generated_code), "Generated code should include the node 'leverage'")
    expect_true(
      grepl("tt/l=leverage", generated_code) ||
        grepl("lemmatizeNodeQuery\\s*=\\s*T", generated_code),
      "Generated code should search for the lemma, via tt/l= or lemmatizeNodeQuery = TRUE"
    )
    # expect_true(grepl("auth", generated_code), "Generated code should include auth() for collocation analysis")
    expect_true(grepl("instance/english", generated_code, fixed = TRUE), "Generated code should include the specified KorAP URL")

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

  test_that(paste(model, "can solve corpus query task with README guidance"), {
    # Skip if offline
    skip_if_offline()

    # Skip if no API keys are set
    skip_if_no_api_key(model)

    # tidyllm is only suggested, so the tests must not fail without it
    if (usesTidyllm(model)) skip_if_not_installed("tidyllm")

    # Check for README file
    skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

    # Create the prompt for corpus query
    prompt <- create_readme_prompt(
      "write R code to perform a simple corpus query for 'Hello world' and fetch all results. The code should use the RKorAPClient package."
    )

    # Call LLM API
    generated_response <- call_llm_api(prompt, model, max_tokens = 300)
    generated_code <- extract_r_code(generated_response)

    # Basic checks on the generated code
    expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
    expect_true(grepl("corpusQuery", generated_code), "Generated code should include corpusQuery")
    expect_true(grepl("Hello world", generated_code), "Generated code should include the search term 'Hello world'")
    expect_true(grepl("fetchAll", generated_code), "Generated code should include fetchAll")

    # Check that the generated code follows the README example pattern
    expect_true(
      grepl("\\|>", generated_code) || grepl("%>%", generated_code),
      "Generated code should use pipe operators"
    )

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

  test_that(paste(model, "can solve corpus size task with README guidance"), {
    skip_if_offline()
    skip_if_no_api_key(model)
    if (usesTidyllm(model)) skip_if_not_installed("tidyllm")
    skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

    prompt <- create_readme_prompt(
      "write R code that reports how many tokens the virtual corpus of newspaper texts published since 2020 contains."
    )

    generated_code <- extract_r_code(call_llm_api(prompt, model, max_tokens = 300))

    expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
    expect_true(grepl("corpusStats", generated_code), "Generated code should include corpusStats")
    expect_true(grepl("vc", generated_code), "Generated code should restrict to a virtual corpus")
    expect_true(test_code_syntax(generated_code), "Generated code should be syntactically valid R code")

    cat("Generated corpus size code:\n", generated_code, "\n")
  })

  test_that(paste(model, "can solve text metadata task with README guidance"), {
    skip_if_offline()
    skip_if_no_api_key(model)
    if (usesTidyllm(model)) skip_if_not_installed("tidyllm")
    skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

    prompt <- create_readme_prompt(
      "write R code that retrieves all metadata KorAP holds for the text with the sigle WPD17/L79/98721."
    )

    generated_code <- extract_r_code(call_llm_api(prompt, model, max_tokens = 300))

    expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
    expect_true(grepl("textMetadata", generated_code), "Generated code should include textMetadata")
    expect_true(grepl("WPD17/L79/98721", generated_code, fixed = TRUE), "Generated code should include the text sigle")
    expect_true(test_code_syntax(generated_code), "Generated code should be syntactically valid R code")

    cat("Generated text metadata code:\n", generated_code, "\n")
  })

  test_that(paste(model, "can solve association score task with README guidance"), {
    skip_if_offline()
    skip_if_no_api_key(model)
    if (usesTidyllm(model)) skip_if_not_installed("tidyllm")
    skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

    prompt <- create_readme_prompt(
      paste(
        "write R code that computes association scores for the word 'Grund' together with each of the",
        "collocates 'triftiger' and 'guter', without searching for collocates first."
      )
    )

    generated_code <- extract_r_code(call_llm_api(prompt, model, max_tokens = 300))

    expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
    expect_true(grepl("collocationScoreQuery", generated_code), "Generated code should include collocationScoreQuery")
    expect_true(grepl("triftiger", generated_code), "Generated code should include the collocate 'triftiger'")
    expect_true(grepl("guter", generated_code), "Generated code should include the collocate 'guter'")
    expect_true(test_code_syntax(generated_code), "Generated code should be syntactically valid R code")

    cat("Generated association score code:\n", generated_code, "\n")
  })

  test_that(paste(model, "can solve result caching task with README guidance"), {
    skip_if_offline()
    skip_if_no_api_key(model)
    if (usesTidyllm(model)) skip_if_not_installed("tidyllm")
    skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

    prompt <- create_readme_prompt(
      paste(
        "write R code for an R Markdown document that reports how often 'Ameisenplage' occurs,",
        "in a way that does not query the server again every time the document is knitted."
      )
    )

    generated_code <- extract_r_code(call_llm_api(prompt, model, max_tokens = 300))

    expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
    expect_true(grepl("cacheAs", generated_code), "Generated code should keep the result with cacheAs")
    expect_true(grepl("Ameisenplage", generated_code), "Generated code should include the search term")
    expect_true(test_code_syntax(generated_code), "Generated code should be syntactically valid R code")

    cat("Generated result caching code:\n", generated_code, "\n")
  })

  test_that(paste(model, "can solve labelled corpora task with README guidance"), {
    skip_if_offline()
    skip_if_no_api_key(model)
    if (usesTidyllm(model)) skip_if_not_installed("tidyllm")
    skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

    prompt <- create_readme_prompt(
      paste(
        "write R code that reports how many tokens the newspaper texts published before 2010 and",
        "those published since 2010 contain, with the two rows labelled 'before' and 'since'."
      )
    )

    generated_code <- extract_r_code(call_llm_api(prompt, model, max_tokens = 300))

    expect_true(grepl("corpusStats", generated_code), "Generated code should include corpusStats")
    # the labels come from the names of the vc vector, not from a column added afterwards
    expect_true(
      grepl("(vc\\s*=\\s*)?c\\(\\s*[`\"']?before[`\"']?\\s*=", generated_code),
      "Generated code should name the virtual corpora in the vc vector"
    )
    expect_true(test_code_syntax(generated_code), "Generated code should be syntactically valid R code")

    cat("Generated labelled corpora code:\n", generated_code, "\n")
  })

  # The code of the following two tasks cannot reasonably be executed in a test:
  # authorization needs a browser flow or a token for restricted data, and a
  # multi-VC collocation analysis runs for minutes. Only the generated code is
  # inspected, which is the point anyway: can the Readme be followed?

  test_that(paste(model, "can solve authorization task with README guidance"), {
    skip_if_offline()
    skip_if_no_api_key(model)
    if (usesTidyllm(model)) skip_if_not_installed("tidyllm")
    skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

    prompt <- create_readme_prompt(
      paste(
        "write R code that authorizes the application so that it also receives KWIC snippets from",
        "corpora with restricted licenses, and then queries 'Ameisenplage' including those snippets."
      )
    )

    generated_code <- extract_r_code(call_llm_api(prompt, model, max_tokens = 300))

    expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
    expect_true(
      grepl("auth\\(|accessToken", generated_code),
      "Generated code should authorize via auth() or an accessToken"
    )
    expect_true(
      grepl("metadataOnly\\s*=\\s*FALSE", generated_code),
      "Generated code should set metadataOnly = FALSE to receive KWIC snippets"
    )
    expect_true(test_code_syntax(generated_code), "Generated code should be syntactically valid R code")

    cat("Generated authorization code:\n", generated_code, "\n")
  })

  test_that(paste(model, "can solve multi-VC comparison task with README guidance"), {
    skip_if_offline()
    skip_if_no_api_key(model)
    if (usesTidyllm(model)) skip_if_not_installed("tidyllm")
    skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

    prompt <- create_readme_prompt(
      paste(
        "write R code that compares the collocates of 'Kritik' between newspaper texts published before 2010",
        "and those published since 2010, and shows those collocates that are attested in both, ordered by how",
        "differently they are associated."
      )
    )

    generated_code <- extract_r_code(call_llm_api(prompt, model, max_tokens = 500))

    expect_true(grepl("collocationAnalysis", generated_code), "Generated code should include collocationAnalysis")
    # the labels of the comparison columns come from the names of the vc vector,
    # which may just as well be built before the call rather than inside it
    expect_true(
      grepl("vc\\s*=\\s*c\\(\\s*[A-Za-z.`\"']", generated_code) ||
        grepl("c\\(\\s*[`\"']?[A-Za-z.][A-Za-z0-9._]*[`\"']?\\s*=[^=]", generated_code),
      "Generated code should pass a named vector of virtual corpora"
    )
    # one row per collocate and vc, so the comparison needs to be reduced
    expect_true(
      grepl("label", generated_code) || grepl("distinct", generated_code),
      "Generated code should reduce the result to one row per collocate, via label or distinct()"
    )
    # imputed scores describe presence/absence rather than a measured contrast
    expect_true(grepl("imputed", generated_code), "Generated code should take the imputed flag into account")
    expect_true(test_code_syntax(generated_code), "Generated code should be syntactically valid R code")

    cat("Generated multi-VC comparison code:\n", generated_code, "\n")
  })
}
