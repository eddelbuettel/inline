library(inline)

code <- "
      int i;
      for (i = 0; i < *n; i++)
        x[i] = x[i]*x[i];
"
quadfn <- cfunction(signature(n = "integer", x = "numeric"), code,
  language = "C", convention = ".C")

res_known <- list(n = 5L, x = c(1, 4, 9, 16, 25))
expect_identical(quadfn(5, 1:5), res_known)

# Saving and restoring the function removes the pointer to the DLL
quadfn_path <- file.path(tempdir(), "quadfn.rda")
save(quadfn, file = quadfn_path)
rm(quadfn)
load(quadfn_path)
expect_error(quadfn(5, 1:5), "NULL value passed as symbol address")

# The DLL is removed by garbage collection
gc()
expect_false(file.exists(environment(quadfn)$libLFile))

# So we recreate the function and move the DLL to a user defined location
quadfn <- cfunction(signature(n = "integer", x = "numeric"), code,
  language = "C", convention = ".C")
moveDLL(quadfn, name = "testname", directory = tempdir())
expect_identical(quadfn(5, 1:5), res_known)

expect_error(
  moveDLL(quadfn, name = "testname", directory = tempdir()),
  "DLL .* in use")

expect_error(
  moveDLL(quadfn, name = "testname", directory = tempdir(), unload = TRUE),
  "Failed to copy")

expect_error(
  moveDLL(quadfn, name = "testname", directory = tempdir(), unload = TRUE,
  overwrite = TRUE),
  "file can not be copied both 'from' and 'to'")

# We recreate the function to have a new temporary DLL name
quadfn <- cfunction(signature(n = "integer", x = "numeric"), code,
  language = "C", convention = ".C")

expect_identical(quadfn(5, 1:5), res_known)

# Now the new path is taken and loaded, but we can unload and overwrite
moveDLL(quadfn, name = "testname", directory = tempdir(), unload = TRUE,
  overwrite = TRUE)
expect_identical(quadfn(5, 1:5), res_known)

# Now the DLL is not always removed by garbage collection in normal use
if (interactive()) {
  gc()
  expect_true(file.exists(environment(quadfn)$libLFile))
}
# But we still get the pointer removed when saving and restoring
save(quadfn, file = quadfn_path)
rm(quadfn)
load(quadfn_path)
expect_error(quadfn(5, 1:5), "NULL value passed as symbol address")

# So we recreate the function again, move the DLL, write and restore
quadfn <- cfunction(signature(n = "integer", x = "numeric"), code,
  language = "C", convention = ".C")
moveDLL(quadfn, name = "testname", directory = tempdir(), unload = TRUE,
  overwrite = TRUE)
writeCFunc(quadfn, quadfn_path)
quadfn_reloaded <- readCFunc(quadfn_path)
expect_identical(quadfn_reloaded(5, 1:5), res_known)
