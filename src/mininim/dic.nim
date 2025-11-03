import
    mininim,
    std/tables

type
    Delegate* = ref object of Facet
    DelegateHook*[T] = proc(): T {. closure .}

    Shared* = ref object of Facet

begin App:
    proc init*

    proc get*[T](target: typedesc[T]): T =
        let
            delegate = this.config.findOne(Delegate, (scope: target.TypeID))
            shared = this.config.findOne(Shared, (scope: target.TypeID))

        withLock this.store.lock:
            if shared != nil and this.store.instances.hasKey(target.TypeID):
                    result = cast[T](this.store.instances[target.TypeID])

            else:
                if delegate != nil:
                    result = DelegateHook[T].value(delegate.call)()
                else:
                    result = T.init()

                if shared != nil:
                    this.store.instances[target.TypeID] = cast[RootRef](result)

                when defined(debug):
                    echo fmt "created[{align($T.typeID, 3, '0')}]: new instance of '{$T}'"

begin Delegate:
    proc build(app: App): self {. static .}=
        result = self.init()

shape Delegate: @[
    #[
        The call property of the `Hook` should effectively act as a template, parsed as NimNode
        then modified such that any appearances of the Hook's `swap` target (Delegate in this case)
        are replaced by the shaped class.
    ]#
    Hook(
        call: proc(): self {. closure .}=
            result = self.build(this.app)
    )
]

shape Shared: @[
    Hook(
        init: true,
        call: proc(): void {. closure .} =
            discard this.app.get(self)
    )
]
