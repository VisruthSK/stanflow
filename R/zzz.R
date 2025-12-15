.onAttach <- function(...) {
  wrapped_startup(core_attach_message())
  # TODO: could check to see if stanflow is loaded to suppress backend message on relibrary
  wrapped_startup(backends_attach_message())
}
