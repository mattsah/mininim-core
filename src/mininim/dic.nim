import
    mininim

class Share of Facet

type DelegateHook*[T] = proc(app: var App): T {. cdecl .}

class Delegate of Facet:
    method build(app: var App): Delegate {. static .}

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

proc get*[T](app: var App, target: typedesc[T]): T =
    let delegate = app.config.findOne(Delegate, (scope: target.TypeID))

    if delegate != nil:
        result = cast[DelegateHook[T]](delegate.hook)(app)
    else:
        result = T.new()
