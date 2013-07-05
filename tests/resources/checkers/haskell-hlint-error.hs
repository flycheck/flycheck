module Foo
where
warnMe :: [String] -> [[String]]
warnMe xs = map lines xs
