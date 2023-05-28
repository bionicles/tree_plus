-- LuaTest.lua
HelloWorld = {}
function HelloWorld.new(self, name)
    newObj = { name = name }
    self.__index = self
    return setmetatable(newObj, self)
end

function HelloWorld.greet(self)
    print("Hello, " .. self.name)
end

function say_hello(name)
    local person = HelloWorld.new(HelloWorld, name)
    person:greet()
end
