class Person
  def initialize(name)
    # oops, not setting @name
  end
end

user = Person.new('Alice')
user2 = user
