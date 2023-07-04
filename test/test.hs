import Test.Tasty (defaultMain, testGroup)
import Ch03.ChapterTest

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "Category Theory for Programmers"
        [ Ch03.ChapterTest.chapterTests
        ]
    )
