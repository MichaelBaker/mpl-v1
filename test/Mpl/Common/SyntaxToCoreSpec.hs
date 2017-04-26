module Mpl.Common.SyntaxToCoreSpec where

import TestUtils

spec = do
  it "fails" $ do
    1 `shouldBe` 2

--  it "converts integers" $ do
--    "1" `transformsTo` (int 1)
--
--   it "converts symbols" $ do
--     "a" `transformsTo` (symbol "a")
-- 
--   it "converts functions of one parameter" $ do
--     "#(a = a)" `transformsTo`
--       (function (binder "a") (symbol "a"))
-- 
--   it "curries functions of multiple parameters" $ do
--     "#(a b c = a)" `transformsTo`
--       (function (binder "a")
--         (function (binder "b")
--           (function (binder "c") (symbol "a"))))
-- 
--   it "converts application of one argument" $ do
--     "f 1" `transformsTo` (application (symbol "f") (int 1))
-- 
--   it "curries application of multiple arguments" $ do
--     "f 1 2 3" `transformsTo`
--       (application
--         (application
--           (application
--             (symbol "f")
--             (int 1))
--           (int 2))
--         (int 3))
