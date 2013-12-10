Given /^(\w+) = (\w+)$/ do |var, value|
  instance_variable_set("@#{var}", value)
end
