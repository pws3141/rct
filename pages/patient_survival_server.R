# pop-up to show factors that have not been considered
modal_confirm <- modalDialog(
  factors_not_included,
  title = "Factors considered but not included",
  footer = tagList(
    actionButton("ok", "OK", class = "btn btn-success")
  )
) 

observeEvent(input$psurv_factors, {
  showModal(modal_confirm)
})
observeEvent(input$ok, {
  removeModal()
})
# reset the radioGroupButtons when the reset button is clicked
observeEvent(input$psurv_reset, {
  updateRadioGroupButtons(inputId = "psurv_age", selected = character(0))
  updateRadioGroupButtons(inputId = "psurv_ethnic", selected = character(0))
  updateRadioGroupButtons(inputId = "psurv_pkd", selected = character(0))
  updateRadioGroupButtons(inputId = "psurv_waiting_time", selected = character(0))
})
