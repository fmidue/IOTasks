module Interaction where

import Types

(<&) :: Interaction -> Interaction -> Interaction
OnInput b1 f1 a1 <& OnInput b2 f2 a2
  = OnInput (b1 <&& b2) (f1 <&& f2) (a1 <&& a2)
OnOutput f1 <& OnOutput f2
  = OnOutput $ f1 <&& f2
_ <& _ = error "TODO: fix <&"

(<&&) :: Maybe a -> Maybe a -> Maybe a
x <&& Nothing = x
_ <&& y = y

prompt :: String -> Interaction
prompt xs = OnInput (Just (Must,MatchExactly xs)) Nothing Nothing

mustReactWith :: (Value -> Matcher) -> Interaction
mustReactWith f = OnInput Nothing (Just (Must,f)) Nothing

mayReactWith :: (Value -> Matcher) -> Interaction
mayReactWith f = OnInput Nothing (Just (May,f)) Nothing

doNothing :: Interaction
doNothing = OnInput Nothing Nothing Nothing

displayValue :: Interaction
displayValue = OnOutput $ Just (Must, matchValue)

displayWithPrefix :: String -> Interaction
displayWithPrefix xs = OnOutput $ Just (Must, Template $ Fixed xs $ Parameter Nil)
