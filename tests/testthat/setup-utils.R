setup({
  pbapply::pboptions(type = "none")
  do.call(options, pizzarr_option_defaults)
})
teardown({
  do.call(options, pizzarr_option_defaults)
})
