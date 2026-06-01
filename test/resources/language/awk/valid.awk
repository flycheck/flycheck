BEGIN { print "hello" }
{ print toupper($0) }
END { print "world" }
