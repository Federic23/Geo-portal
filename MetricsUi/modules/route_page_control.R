weight_content_visible <- reactiveVal(TRUE)
metric_content_visible <- reactiveVal(FALSE)
overall_metric_content_visible <- reactiveVal(FALSE)

observeEvent(input$CsvFormatButton, {
  metric_content_visible(TRUE)
  overall_metric_content_visible(FALSE)
  weight_content_visible(FALSE)
})

observeEvent(input$progressionButton, {
  metric_content_visible(FALSE)
  overall_metric_content_visible(TRUE)
  weight_content_visible(FALSE)
})

observeEvent(input$CSVHistoryButton, {
  metric_content_visible(FALSE)
  overall_metric_content_visible(FALSE)
  weight_content_visible(TRUE)
})


