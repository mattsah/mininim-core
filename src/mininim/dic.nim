import
    mininim,
    std/tables

type
    Shared* = ref object of Facet
    Delegate* = ref object of Facet
    DelegateHook*[T] = proc(app: App): T {. nimcall .}

begin Delegate:
    proc build(app: App): Delegate {. static .}=
        discard

shape Delegate: @[
    #[
        The call property of the `Hook` should effectively act as a template, parsed as NimNode
        then modified such that any appearances of the Hook's `swap` target (Delegate in this case)
        are replaced by the shaped class.
    ]#
    Hook(
        call: proc(app: App): Delegate =
            result = Delegate.build(app)
    )
]

begin App:
    proc get*[T](target: typedesc[T]): T =
        let delegate = this.config.findOne(Delegate, (scope: target.TypeID))
        let shared   = this.config.findOne(Shared, (scope: target.TypeID))

        if shared != nil and this.store.hasKey(target.TypeID):
            result = cast[T](this.store[target.TypeID])

        else:
            if delegate != nil:
                result = cast[DelegateHook[T]](delegate.hook)(this)
            else:
                result = T.init()

            if shared != nil:
                this.store[target.TypeID] = cast[pointer](result)