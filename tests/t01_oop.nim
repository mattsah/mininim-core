import
    mininim,
    std/unittest

type
    Foo = ref object of Class
    Bar = ref object of Foo

begin(Foo):
    proc get*(): int {. static .} =
        result = 1

    method run*(): int {. base .} =
        result = self.get()

begin(Bar):
    proc get*(): int {. static .} =
        result = 2

    method run*(): int =
        result = self.get() + proto.get()

    method superRun*(): int {. base .} =
        result = this.run() + super.run()

test "Self Functionality":
    var
        foo = Foo()

    check foo.run() == 1

test "Proto Functionality":
    var
        bar = Bar()

    check bar.run() == 3


test "Super Functionality":
    var
        bar = Bar()

    check bar.superRun() == 4