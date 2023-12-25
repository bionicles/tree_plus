# simplified.capnp
using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("simplified");

struct Employee {
  id @0 :Int32;
  name @1 :Text;
  role @2 :Text;
  skills @3 :List(Skill);

  struct Skill {
    name @0 :Text;
    level @1 :Level;

    enum Level {
      beginner @0;
      intermediate @1;
      expert @2;
    }
  }

  status :union {
    active @4 :Void;
    onLeave @5 :Void;
    retired @6 :Void;
  }
}

struct Company {
  employees @0 :List(Employee);
}