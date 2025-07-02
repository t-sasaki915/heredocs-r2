{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Text.HeredocSpec (heredocSpec) where

import           Test.Hspec      (Spec, describe, it, shouldBe)

import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import qualified Data.Text       as Text

import           Text.Heredoc    (heredoc)

data Gender = Male | Female | NewHalf deriving Show
data Person = Person String Int Gender deriving Show
data Person' = Person' { name :: String, age :: Int, sex :: Gender }

heredocSpec :: Spec
heredocSpec =
    describe "heredoc" $ do
        it "should process normal text 1" $
            [heredoc|Hello,World|] `shouldBe` "Hello,World"

        it "should process normal text 2" $
            [heredoc|
 Hello,World
 |] `shouldBe` "\n Hello,World\n "

        it "should interpolate variable 1" $
            let x = 42 in
                [heredoc|
The number is ${show x}.
|] `shouldBe` "\nThe number is 42.\n"

        it "should interpolate variable 2" $
            let name' = "Katsutoshi" :: Text in
                [heredoc|Hello, ${Text.unpack name'} san!|] `shouldBe` "Hello, Katsutoshi san!"

        it "should interpolate variable 3" $
            let name' = "Katsutoshi" :: Text in
                [heredoc|Hello, ${name'} (Text) san!|] `shouldBe` "Hello, Katsutoshi (Text) san!"

        it "should interpolate variable 4" $
            let name' = "Katsutoshi" :: ByteString in
                [heredoc|Hello, ${name'} (ByteString) san!|] `shouldBe` "Hello, Katsutoshi (ByteString) san!"

        it "should process $let syntax 1" $
            [heredoc|
Question
  $let x = 42
    ${show x} + 2 = ${show $ x + 2}.
    ${show x} * 2 = ${show $ x * 2}.
    ${show x} ^ 2 = ${show $ x ^ 2}.
|] `shouldBe` "\nQuestion\n  42 + 2 = 44.\n  42 * 2 = 84.\n  42 ^ 2 = 1764.\n"

        it "should process $let syntax 2" $
            [heredoc|
Question
  $let x = 42
    $let y = "Katsutoshi"
      ${y}(${show $ x+3}).
|] `shouldBe` "\nQuestion\n  Katsutoshi(45).\n"

        it "should process $maybe syntax 1" $
            let mu = Just "katsutoshi" in
                [heredoc|
$maybe u <- mu
  Hello ${u} san
$nothing
  Bye
|] `shouldBe` "\nHello katsutoshi san\n"

        it "should process $maybe syntax 2" $
            let mu = Nothing in
                [heredoc|
$maybe u <- mu
  Hello ${u} san
$nothing
  Bye
|] `shouldBe` "\nBye\n"

        it "should process $maybe syntax 3" $
            let ma = Nothing :: Maybe Int in
                [heredoc|
$maybe a<-ma
  ${show a}
|] `shouldBe` "\n"

        it "should process $maybe syntax 4" $
            let mu = Just "katsutoshi"
                ma = Just 45 in
                    [heredoc|
$maybe u <- mu
  $maybe a <- ma
    ${u}(${show a})
|] `shouldBe` "\nkatsutoshi(45)\n"

        it "should process $maybe syntax 5" $
            let mu = Just "katsutoshi"
                ma = Nothing :: Maybe Int in
                    [heredoc|
$maybe u <- mu
  $maybe a <- ma
    ${u}(${show a})
  $nothing
    ${u}(age not found)
|] `shouldBe` "\nkatsutoshi(age not found)\n"

        it "should process $if syntax 1" $
            [heredoc|
$if True
  OK
$else
  NG
|] `shouldBe` "\nOK\n"

        it "should process $if syntax 2" $
            [heredoc|
$if 1==1
  OK
|] `shouldBe` "\nOK\n"

        it "should process $if syntax 3" $
            [heredoc|
$if 1==2
  OK
|] `shouldBe` "\n"

        it "should process $if syntax 4" $
            [heredoc|
$if 1==2
$else
  False
|] `shouldBe` "\nFalse\n"

        it "should process $if syntax 5" $
            let mu = Just "katsutoshi"
                b = True in
                    [heredoc|
Hello
  $maybe u <- mu
   $if b
     OK => ${u}
   $else
     NG
|] `shouldBe` "\nHello\n  OK => katsutoshi\n"

        it "should process $if syntax 6" $
            let mu = Just "katsutoshi" in
                [heredoc|
Hello
  $maybe u <- mu
    $if u=="katsutoshi"
      OK => ${u}
    $else
      NG
|] `shouldBe` "\nHello\n  OK => katsutoshi\n"

        it "should process $if syntax 7" $
            let b = True in
                [heredoc|
Hello
  $if b
    $maybe x <- Just "katsutoshi"
      OK => ${x}.
    $nothing
      Ooops.
  $else
|] `shouldBe` "\nHello\n  OK => katsutoshi.\n"

        it "should process $maybe <- syntax 1" $
            [heredoc|
$maybe _ <- Just 1
  OK
$nothing
  NG
|] `shouldBe` "\nOK\n"

        it "should process $maybe <- syntax 2" $
            [heredoc|
$maybe p@(Person _ age' _) <- Just (Person "katsutoshi" 45 Male)
  ${show p}
|] `shouldBe` "\nPerson \"katsutoshi\" 45 Male\n"

        it "should process $case syntax 1" $
            let x = Female in
                [heredoc|
$case x
  $of Male
    Otoko
  $of Female
    Onna
  $of _
    P~~~~~~~
|] `shouldBe` "\nOnna\n"

        it "should process $case syntax 2" $
            let mp = Just (Person "Katsutoshi" 45 Male) in
                [heredoc|
$maybe Person name' age' sex' <- mp
  $case sex'
    $of Male
      ${name'}(${show age'}) - Otoko
    $of Female
      ${name'}(${show age'}) - Onna
    $of _
      ${name'}(${show age'}) - ?
|] `shouldBe` "\nKatsutoshi(45) - Otoko\n"

        it "should process $let = syntax 1" $
            let p = (Person "katsutoshi" 45 Male, Person "keiko" 44 Female) in
                [heredoc|
$let (Person n1 a1 g1, Person n2 a2 g2) = p
  ${n1}(${show a1}) ${show g1}
  ${n2}(${show a2}) ${show g2}
|] `shouldBe` "\nkatsutoshi(45) Male\nkeiko(44) Female\n"

        it "should process $let = syntax 2" $
            let p = Person "katsutoshi" 45 Male
                p' = Person "keiko" 44 Female in
                    [heredoc|
$let (Person n1 a1 g1, Person n2 a2 g2) = (p, p')
  ${n1}(${show a1}) ${show g1}
  ${n2}(${show a2}) ${show g2}
|] `shouldBe` "\nkatsutoshi(45) Male\nkeiko(44) Female\n"

        it "should process $let = syntax 3" $
            [heredoc|
$let x:y = 1:[]
  ${show x} OK
|] `shouldBe` "\n1 OK\n"

        it "should process $let = syntax 4" $
            [heredoc|
$let x:y:z = 1:2:3:4:[5,6,7]
  ${show z} OK
|] `shouldBe` "\n[3,4,5,6,7] OK\n"

        it "should process $let = syntax 5" $
            [heredoc|
$let x:y:z = [1,2,3]:[4]:[5,6,7]:[]
  ${show x} OK
  ${show y} OK
  ${show z} OK
|] `shouldBe` "\n[1,2,3] OK\n[4] OK\n[[5,6,7]] OK\n"

        it "should process $let = syntax 6" $
            [heredoc|
$let x:_:z = (1:2:[]):(3:4:[]):(5:[6,7]):[]
  ${show x} OK
  ${show z} OK
|] `shouldBe` "\n[1,2] OK\n[[5,6,7]] OK\n"

        it "should process $let = syntax 7" $
            [heredoc|
$let xss@(x@(_,_):xs) = [(1,2),(3,4),(5,6)]
  ${show $ fst x} and ${show xs} in ${show xss}
|] `shouldBe` "\n1 and [(3,4),(5,6)] in [(1,2),(3,4),(5,6)]\n"

        it "should process $let = syntax 8" $
            [heredoc|
$let (name1, name2) = ("Keiko", "Nao")
  Hello, ${Text.unpack name1} & ${Text.unpack name2}
|] `shouldBe` "\nHello, Keiko & Nao\n"

        it "should process $forall syntax" $
            let ps = [Person' "katsutoshi" 45 Male, Person' "keiko" 44 Female] in
                [heredoc|
$forall (i, p) <- zip [1,2] ps
  ${show i}
    Name : ${name p}
    Age  : ${show $ age p}
    Sex  : ${show $ sex p}
|] `shouldBe` "\n1\n  Name : katsutoshi\n  Age  : 45\n  Sex  : Male\n2\n  Name : keiko\n  Age  : 44\n  Sex  : Female\n"
