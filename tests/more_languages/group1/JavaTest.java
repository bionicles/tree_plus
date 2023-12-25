// Improved JavaTest.java

import com.google.common.base.Optional;
import com.google.common.base.Preconditions;
import com.google.inject.Inject;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.java.Log;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.*;

// Base class for all living beings
abstract class LivingBeing {
    abstract void breathe();
}

// Interface for entities that can communicate
interface Communicator {
    String communicate();
}

// Using Lombok for reduced boilerplate
@Log
@Getter
@Setter
// Person now inherits from LivingBeing and implements Communicator
class Person extends LivingBeing implements Communicator {

    private String name;
    private int age;

    Person(String name, int age) {
        Preconditions.checkNotNull(name, "Name cannot be null");
        Preconditions.checkArgument(age > 0, "Age must be positive");
        this.name = name;
        this.age = age;
    }

    @Override
    void breathe() {
        log.info(this.name + " is breathing.");
    }

    @Override
    public String communicate() {
        return "Hello, I can communicate!";
    }

    void greet() {
        log.info("Hello, my name is " + this.name + " and I am " + this.age + " years old.");
    }

    String personalizedGreeting(String greeting, Optional<Boolean> includeAge) {
        String message = greeting + ", " + this.name;
        if (includeAge.or(true)) {
            message += ". I am " + this.age + " years old.";
        }
        return message;
    }
}

// Spring Boot Application
@Singleton
@RestController
@SpringBootApplication
public class Example {

    private final Person person;

    @Inject
    public Example(Person person) {
        this.person = person;
    }

    @RequestMapping("/greet")
    String home(@RequestParam(value = "name", defaultValue = "World") String name,
                @RequestParam(value = "age", defaultValue = "30") int age) {
        Person person = new Person(name, age);
        person.breathe();
        return person.personalizedGreeting("Hello", Optional.of(true));
    }

    public static void main(String[] args) {
        SpringApplication.run(Example.class, args);
    }
}