doAddRem_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("doAddRem"), "Choose what to do (**)",
                 choices = list("Add polygon (**)" = 'addMask',
                                "Remove polygon (**)" = 'remMAsk'), inline = TRUE)
  )
}

doAddRem_MOD <- function(input, output, session) {
  reactive({

  })
}

doAddRem_MAP <- function(map, session) {

}
