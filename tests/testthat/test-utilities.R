
test_that("ConvertCommaSeparatedStringToVector",
          {
              expect_equal(ConvertCommaSeparatedStringToVector("a, a "), c("a","a"))
              expect_equal(ConvertCommaSeparatedStringToVector("a; a ", split = ";"), c("a","a"))
              expect_equal(ConvertCommaSeparatedStringToVector("a; a ;   a ", split = c(";")), c("a","a", "a"))

              # non-breaking white space
              xx <- "NET, Total, SUM, 1901-1910"
              expect_equal(nchar(ConvertCommaSeparatedStringToVector(xx)), c(3,5,3,9))
          })

test_that("ConvertCommaSeparatedStringToVector with text qualifier",
{
    expect_equal(ConvertCommaSeparatedStringToVector("alfa, \"bravo\", charlie",
                                                     text.qualifier = "\""),
                                                     c("alfa", "bravo", "charlie"))
    expect_equal(ConvertCommaSeparatedStringToVector("alfa, \"bravo, char\"lie",
                                                     text.qualifier = "\""),
                                                     c("alfa", "\"bravo", "char\"lie"))
    expect_equal(ConvertCommaSeparatedStringToVector("\",alfa\", \"br , a , vo\", \"char,lie,\"",
                                                     text.qualifier = "\""),
                                                     c(",alfa", "br , a , vo", "char,lie,"))
})

test_that("Trim white space",
          {
              expect_equal(TrimLeadingWhitespace("        Big dog         "), "Big dog         ")
              expect_equal(TrimTrailingWhitespace("        Big dog         "), "        Big dog")
              expect_equal(TrimWhitespace("        Big dog         "), "Big dog")
          })
