-- A type error caught in strict mode
--
-- Checkers: luau-analyze
--!strict

local x: number = "not a number"
print(x)
