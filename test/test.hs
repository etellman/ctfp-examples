import Ch03.ChapterTest
import Ch04.ChapterTest
import Ch05.ChapterTest
import Ch07.ChapterTest
import Ch08.ChapterTest
import Ch09.ChapterTest
import Ch10.ChapterTest
import Ch12.ChapterTest
import Ch13.ChapterTest
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "Category Theory for Programmers"
        [ Ch03.ChapterTest.chapterTests,
          Ch04.ChapterTest.chapterTests,
          Ch05.ChapterTest.chapterTests,
          Ch07.ChapterTest.chapterTests,
          Ch08.ChapterTest.chapterTests,
          Ch09.ChapterTest.chapterTests,
          Ch10.ChapterTest.chapterTests,
          Ch12.ChapterTest.chapterTests,
          Ch13.ChapterTest.chapterTests
        ]
    )
