# ruby_test.rb
module Greeter
  def self.say_hello
    puts 'Hello from the Greeter module!'
  end
end

class HelloWorld
  def say_hello
    puts 'Hello, World!'
  end
end

# A class instance variable is not shared by the class's descendants.
class Human
  @bar = 0

  def self.bar
    @bar
  end

  def self.bar=(value)
    @bar = value
  end
end

class Doctor < Human
  def brachial_plexus(
      roots,
      trunks, # edge case comment
      divisions: true,
      cords: [],
      branches: Time.now
    )
    # Method implementation...
  end
end
