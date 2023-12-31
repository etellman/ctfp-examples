import Ch03.ChapterTest
import Ch04.ChapterTest
import Ch05.ChapterTest
import Ch07.ChapterTest
import Ch08.ChapterTest
import Ch09.ChapterTest
import Ch10.ChapterTest
import Ch12.ChapterTest
import Ch13.ChapterTest
import Ch14.ChapterTest
import Ch15.ChapterTest
import Ch18.ChapterTest
import Ch21.ChapterTest
import Ch22.ChapterTest
import Ch23.ChapterTest
import Ch24.ChapterTest
import Lib.LibTest
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "Category Theory for Programmers"
        [ Lib.LibTest.libTests,
          Ch03.ChapterTest.chapterTests,
          Ch04.ChapterTest.chapterTests,
          Ch05.ChapterTest.chapterTests,
          Ch07.ChapterTest.chapterTests,
          Ch08.ChapterTest.chapterTests,
          Ch09.ChapterTest.chapterTests,
          Ch10.ChapterTest.chapterTests,
          Ch12.ChapterTest.chapterTests,
          Ch13.ChapterTest.chapterTests,
          Ch14.ChapterTest.chapterTests,
          Ch15.ChapterTest.chapterTests,
          Ch18.ChapterTest.chapterTests,
          Ch21.ChapterTest.chapterTests,
          Ch22.ChapterTest.chapterTests,
          Ch23.ChapterTest.chapterTests,
          Ch24.ChapterTest.chapterTests
        ]
    )
