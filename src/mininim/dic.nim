import
    mininim,
    std/tables

type
    Shared* = ref object of Facet

    Builder* = ref object of Facet

    Delegate* = ref object of Facet
    DelegateHook*[T] = proc(): T {. closure .}

begin Shared:
    discard

shape Shared: @[
    Hook(
        init: true,
        call: proc(): void {. closure .} =
            discard this.app.get(shape)
    )
]

#[
    Can be extended to provide basic buld functionality
]#
begin Builder:
    proc build(app: App): self {. static .}=
        result = self.init()

shape Builder: @[

]

begin Delegate:
    discard

shape Delegate: @[
    Hook(
        call: proc(): shape {. closure .}=
            result = shape.build(this.app)
    )
]

begin App:
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

    proc build*(): self {. static .} =
        return self.init(config)

    proc build(app: App = nil): self {. static .} =
        return self.init(config)

shape App: @[
    Delegate()
]