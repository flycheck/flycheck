class Person
  def initialize(name)
    name = name # oops, not setting @name
  end

  def greet
    return "Hello, #{@name}"
  end
end

user = Person.new('Alice')
user.greet(:foo)
