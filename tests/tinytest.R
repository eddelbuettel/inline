
if (requireNamespace("tinytest", quietly=TRUE) &&
  utils::packageVersion("tinytest") >= "1.1.0") {

  ## Set a seed to make the tests deterministic
  set.seed(42)

  tinytest::test_package("inline")
}
