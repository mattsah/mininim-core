import
    mininim,
    std/tables

type
    Shared* = ref object of Facet
    Delegate* = ref object of Facet
    DelegateHook*[T] = proc(app: var App): T {. cdecl .}

var
    store = newTable[TypeID, pointer]()

begin Delegate:
    proc build(self: typedesc[Delegate], app: var App): Delegate =
        discard

shape Delegate: @[
    #[
        The call property of the `Hook` should effectively act as a template, parsed as NimNode
        then modified such that any appearances of the Hook's target (Delegate in this case)
        are replaced by the target when it is copied to teh `Delegate.hook` Facet for some other
        class.
    ]#
    Hook(
        call: proc(app: var App): Delegate =
            result = Delegate.build(app)
    )
]

begin App:
    proc get*[T](app: var App, target: typedesc[T]): T =
        let delegate = app.config.findOne(Delegate, (scope: target.TypeID))
        let shared   = app.config.findOne(Shared, (scope: target.TypeID))

        if shared != nil and store.hasKey(target.TypeID):
            result = cast[T](store[target.TypeID])

        else:
            if delegate != nil:
                result = cast[DelegateHook[T]](delegate.hook)(app)
            else:
                result = T.new()

            if shared != nil:
                store[target.TypeID] = cast[pointer](result)
