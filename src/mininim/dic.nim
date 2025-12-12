import
    mininim

type
    Builder* = ref object of Facet

    Delegate* = ref object of Facet
    DelegateHook*[shape] = proc(): shape

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
        call: DelegateHook as (
            block:
                result = shape.build(this.app)
        )
    )
]

begin App:
    proc get*[T](target: typedesc[T]): T =
        let
            delegate = this.config.findOne(Delegate, (scope: target.TypeID))
            shared = this.config.findOne(Shared, (scope: target.TypeID))

        withLock this.storage.lock:
            if shared != nil and this.storage.instances.hasKey(target.TypeID):
                    result = cast[T](this.storage.instances[target.TypeID])

            else:
                if delegate != nil:
                    result = delegate[DelegateHook[T]]()
                else:
                    result = T.init()

                if result is Class:
                    result.app = this

                if shared != nil:
                    this.storage.instances[target.TypeID] = cast[RootRef](result)

                when defined(debug):
                    echo fmt "created[{align($T.typeID, 3, '0')}]: new instance of '{$T}'"

    proc build(app: App = nil): self {. static .} =
        result = self.init()

shape App: @[
    Delegate()
]
