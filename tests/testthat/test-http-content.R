test_that("the public-content contract also holds for synthetic annotations and collocations", {
  withWorkflowFixture("workflow-content", function(state) {
    checkKorapContentWorkflow(suppressMessages(fixtureKorapConnection()))
  })
})
