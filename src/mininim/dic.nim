import
    mininim,
    std/tables

type
    Delegate* = ref object of Facet
    DelegateHook*[T] = proc(app: App): T {. nimcall, gcsafe .}

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
                when defined(debug):
                    echo fmt "Instantiating new instance of {$T}"

                if delegate != nil:
                    result = delegate[DelegateHook[T]](this)
                else:
                    result = T.init()

                if shared != nil:
                    this.store.instances[target.TypeID] = cast[RootRef](result)

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
        call: proc(app: App): self =
            result = self.build(app)
    )
]

shape Shared: @[
    Hook(
        init: true,
        call: proc(app: App): void =
            discard app.get(self)
    )
]
