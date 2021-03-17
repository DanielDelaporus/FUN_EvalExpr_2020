module ASpec where

import Test.Hspec
import Test.QuickCheck
import Lib

spec :: Spec
spec = do
    describe "EvalEXpr tests" $ do
        describe "Error tests" $ do
            it "Division by Zero" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("4/0")))) `shouldBe` (Nothing :: Maybe Double)
            it "Empty parenthesis 01" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("(5+4")))) `shouldBe` (Nothing :: Maybe Double)
            it "Empty parenthesis 02" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5+4)")))) `shouldBe` (Nothing :: Maybe Double)
            it "Empty String" $ do
                evalExpr 600 (addParr ((filter (/=' ') (" ")))) `shouldBe` (Nothing :: Maybe Double)
            it "Illegal character 1" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5+a4")))) `shouldBe` (Nothing :: Maybe Double)
            it "Illegal character 2" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("a5+4")))) `shouldBe` (Nothing :: Maybe Double)
            it "Illegal character 3" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5+4a")))) `shouldBe` (Nothing :: Maybe Double)
            it "Missing number" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5+")))) `shouldBe` (Nothing :: Maybe Double)
            it "Missing parenthesis" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5+(2*-1*(-3*-(-1+4)+9*-2")))) `shouldBe` (Nothing :: Maybe Double)
            it "No number" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("a+t")))) `shouldBe` (Nothing :: Maybe Double)
            it "Reversed parenthesis" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5+)2*-1*)-3*-)-1+4((+9*-2(")))) `shouldBe` (Nothing :: Maybe Double)
            it "Two enchained operators" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5*/4")))) `shouldBe` (Nothing :: Maybe Double)
            it "Two numbers with no operators" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5 4")))) `shouldBe` (Nothing :: Maybe Double)

        describe "One number" $ do
            it "Less 1" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("-1")))) `shouldBe` (Just (-1) :: Maybe Double)
            it "1" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("1")))) `shouldBe` (Just 1 :: Maybe Double)
            it "0" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("0")))) `shouldBe` (Just 0 :: Maybe Double)

        describe "First tests" $ do
            it "Simple Add" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("(1+1)")))) `shouldBe` (Just 2 :: Maybe Double)
            it "Simple less" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("(1-1)")))) `shouldBe` (Just 0 :: Maybe Double)
            it "Simple mult" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("(2*2)")))) `shouldBe` (Just 4 :: Maybe Double)
            it "Simple div" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("(2/2)")))) `shouldBe` (Just 1 :: Maybe Double)
            it "Simple power" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("(2^2)")))) `shouldBe` (Just 4 :: Maybe Double)
            it "Simple Add2" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("(2+2)")))) `shouldBe` (Just 4 :: Maybe Double)
            it "Simple div" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("(2+2)")))) `shouldBe` (Just 4 :: Maybe Double)
            it "Simple div" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("(2+2)")))) `shouldBe` (Just 4 :: Maybe Double)

        describe "Middle tests" $ do
            it "5_less_3" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5-3")))) `shouldBe` (Just 2 :: Maybe Double)
            it "4_plus_4" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("4+4")))) `shouldBe` (Just 8 :: Maybe Double)
            it "5_less_3" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5-3")))) `shouldBe` (Just 2 :: Maybe Double)
            it "minus_5_plus_3" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("-5+3")))) `shouldBe` (Just (-2) :: Maybe Double)
            it "minus_500_plus_3000" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("-500+3000")))) `shouldBe` (Just 2500 :: Maybe Double)
            it "less_less_5" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("-(-5)")))) `shouldBe` (Just 5 :: Maybe Double)
            it "2_div_4" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("2/4")))) `shouldBe` (Just (0.5) :: Maybe Double)
            it "minus_2_div_4" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("-2/4")))) `shouldBe` (Just (-0.5) :: Maybe Double)
            it "2_div_minus_4" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("2/-4")))) `shouldBe` (Just (-0.5) :: Maybe Double)
            it "minus_2_div_minus_4" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("-2/-4")))) `shouldBe` (Just (0.5) :: Maybe Double)

        describe "Expert tests" $ do
            it "multiple_op_one" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5+(2*-1*(-3*-(-1+4))+9*-2)")))) `shouldBe` (Just (-31) :: Maybe Double)
            it "multiple_op_two" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5+(2*-1*(-3*-(-1+4))+9*-2)-2+2+2")))) `shouldBe` (Just (-29) :: Maybe Double)
            it "multiple_op_three" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5+(2*-1*(-3*-(-1+4)))+9*-2+2^2+2/2-1")))) `shouldBe` (Just (-27) :: Maybe Double)
            it "multiple_op_four" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("5+(2*-1*(-3*-(-1+4)))+9*-2+2^2+2/2-1+2+2-(2*2)")))) `shouldBe` (Just (-27) :: Maybe Double)
            it "multiple_op_five" $ do
                evalExpr 600 (addParr ((filter (/=' ') ("(-(5+(2*-1*(-3*-(-1+4)))+9*-2+2^2+2/2-1+2+2-2*2))/2")))) `shouldBe` (Just (13.5) :: Maybe Double)