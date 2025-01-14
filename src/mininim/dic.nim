import
    mininim

#
# This seems inherently unsafe, but although we ultimately cast the hook function as a DelegateHook
# we initially simply take it in as a generic pointer.  There seems to be no way to make a generic
# procedure the type of an object property.
#
type
    DelegateHook[T] = proc(app: var App): T {.cdecl.}

class Share of Facet

class Delegate of Facet:
    var hook*: pointer # part of unsafety mentioned above

proc get*[T](app: var App, target: typedesc[T]): T =
    result = nil

    for node in app.config:
        if node.target == target.TypeID:
            if node.facet.matches(Delegate):
                #
                # Here we cast the generic function pointer to the right type -- hopefully this
                # just crashes if the structure of the provided hook is wrong?
                #
                result = cast[DelegateHook[T]](node.facet.Delegate.hook)(app)

    if result == nil:
        result = T.new()
