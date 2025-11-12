# pop-up to show factors that have not been considered
modal_confirm <- modalDialog(
  factors_not_included,
  title = "Factors considered but not included",
  footer = tagList(
    actionButton("ok", "OK", class = "btn btn-success")
  )
) 

observeEvent(input$ksurv_factors, {
  showModal(modal_confirm)
})
observeEvent(input$ok, {
  removeModal()
})
# reset the radioGroupButtons when the reset button is clicked
observeEvent(input$ksurv_reset, {
  updateRadioGroupButtons(inputId = "ksurv_age", selected = character(0))
  updateRadioGroupButtons(inputId = "ksurv_pkd", selected = character(0))
  updateRadioGroupButtons(inputId = "ksurv_waiting_time", selected = character(0))
  updateRadioGroupButtons(inputId = "ksurv_prevtx", selected = character(0))
})
