XPTemplate priority=lang-2

let s:f = g:XPTfuncs()

fun! s:f.getCurrentProvideString()
    let match = matchlist(getline(0, line('.')), 'goog.provide(''\(.*\)'');')
    if len(match) > 0
        return match[1]
    else
        return "namespace.Class"
    endif
endfunction

XPT arrclear "goog.array.clear
goog.array.clear(`array^)

XPT arrclone "goog.array.clone
goog.array.clone(`array^)

XPT arrcontains "goog.array.contains
goog.array.contains(`array^, `element^)

XPT arrequals "goog.array.equals
goog.array.equals(`array1^, `array2^`, `equalsFn..{{^, function (a, b) { `body^ }`}}^)

XPT arrevery "goog.array.every
goog.array.every(`array^, `Include:efun^`, `this^)

XPT arrfilter "goog.array.filter
goog.array.filter(`array^, `Include:efun^`, `this^)

XPT arrfind "goog.array.find
goog.array.find(`array^, `Include:efun^`, `this^)

XPT arrforeach "goog.array.forEach
goog.array.forEach(`array^, `Include:efun^`, `this^)

XPT arrindexof "goog.array.indexof
goog.array.indexOf(`array^, `element^)

XPT arrmap "goog.array.map
goog.array.map(`array^, `Include:efun^`, `this^)

XPT arrpeek "goog.array.peek
goog.array.peek(`array^)

XPT arrreduce "goog.array.reduce
goog.array.reduce(`array^, `function (previousValue, currentValue){ return `body^; }, `0^`, `this^)

XPT arrrepeat
goog.array.repeat(`value^, `1^)

XPT arrsome "goog.array.some
goog.array.some(`array^, `Include:efun^`, `this^)

XPT arrsort "goog.array.sort
goog.array.sort(`array^, `function (lhs, rhs) { return `body^; })

XPT arrto "goog.array.toArray
goog.array.toArray(`object^)

XPT assert "goog.asserts.assert
goog.asserts.assert(`condition^`, '`errorMsg`'^)

XPT bind "goog.bind
goog.bind(`this.fn{{^this.`fn^`}}^, `this^`, `parameters^)

XPT bling synonym=4 "goog.dom.$
goog.dom.getElement('`id^')

XPT blingbling synonym=44 "goog.dom.$$
goog.dom.getElementsByTagNameAndClass('`*^', `undefined^`, `context^)

XPT box "goog.math.box
goog.math.Box(`1^, `1^, `1^, `1^)

XPT class "namespace.Class = function\()
XSET namespace.Class|pre=getCurrentProvideString()
/**
 * @constructor
 */
`namespace.Class^ = function() {
  `cursor^
};

XPT click "goog.events.listen\(..EventType.CLICK..)
goog.events.listen(`element^, goog.events.EventType.CLICK, `function...{{^function() { `body^ }`}}^);

XPT clsadd "goog.dom.classes.add
goog.dom.classes.add(`element^, '`class^')

XPT clsget "goog.dom.classes.get
goog.dom.classes.get(`element^)

XPT clshas "goog.dom.classes.has
goog.dom.classes.has(`element^, '`class^')

XPT clsremove "goog.dom.classes.remove
goog.dom.classes.remove(`element^, '`class^')

XPT clsset "goog.dom.classes.set
goog.dom.classes.set(`element^, '`class^')

XPT clstoggle "goog.dom.classes.toggle
goog.dom.classes.toggle(`element^, '`class^')

XPT coord "goog.match.coord
goog.math.Coordinate(`x^, `y^)

XPT def synonym=isdef "goog.isDef
goog.isDef(`var^)

XPT dom "goog.dom.createDom
XSET attributes|post=EchoIfNoChange(', undefined')
goog.dom.createDom('`div^'`, { `attributes` }^`, `children^)

XPT exportproperty "goog.exportProperty
goog.exportProperty(`object^, '`publicName^', `object^.`publicName^);

XPT exportsymbol "goog.exportSymbol
goo.exportSymbol('`symbol^', `symbol^);

XPT hide "goog.style.hide
goog.style.showElement(`element^, false);

XPT isarray "goog.isArray
goog.isArray(`var^)

XPT isarraylike "goog.isArrayLike
goog.isArrayLike(`var^)

XPT isboolean "goog.isBoolean
goog.isBoolean(`var^)

XPT isdatelike "goog.isDateLike
goog.isDateLike(`var^)

XPT isdefandnotnull "goog.isDefAndNotNull
goog.isDefAndNotNull(`var^)

XPT isfunction "goog.isFunction
goog.isFunction(`var^)

XPT isnull "goog.isNull
goog.isNull(`var^)

XPT isnumber "goog.isNumber
goog.isNumber(`var^)

XPT isobject "goog.isObject
goog.isObject(`var^)

XPT isstring "goog.isString
goog.isString(`var^)

XPT listen "goog.events.listen
XSET capturephase|post=EchoIfNoChange('false')
XSET CLICK|post=UpperCase(V())
XSET thisobject|post=EchoIfNoChange(', this')
goog.events.listen(`eventtarget^, `goog.events.EventType.^`CLICK^, `function..{{^function (e) { `body^ }`}}^`, more..{{^, `capturephase^`, `thisobject^`}}^)

XPT msg "goog.getMsg
goog.getMsg(`str^)

XPT now "goog.now
goog.now()

XPT once "goog.Timer.callOnce
XSET interval|post=EchoIfNoChange('undefined')
goog.Timer.callOnce(`function..{{^function () { `body^ }`}}^`, `interval^`, `handler^)

XPT partial synonym=curry "goog.partial
goog.partial(`function^, `parameters^)

XPT proto "getCurrentProvideString().prototype
`getCurrentProvideString()^.prototype.

XPT protof "getCurrentProvideString().prototype.function
`getCurrentProvideString()^.prototype.`function^ = function(`parameters^) {{
    `cursor^
};

XPT prov "goog.provide
goog.provide('`name^');

XPT rect "goog.math.Rect
goog.math.Rect(`x^, `y^, `w^, `h^)

XPT req "goog.require
goog.require('`name^);

XPT show "goog.style.showElement
goog.style.showElement(`element^, true);

XPT size "goog.math.Size
goog.math.Size(`width^, `height^)

XPT static "getCurrentProvideString().
`getCurrentProvideString()^.

XPT staticf "getCurrentProvideString().function
`getCurrentProvideString()^.`function^ = function(`parameters^) {{
    `cursor^
};

XPT subclass "getCurrentProvideString() extends...
XSET namespace.Class|pre=getCurrentProvideString()
XSET superclass|post=EchoIfNoChange('goog.Disposable')
XSET ComeFirst=namespace.Class
/**
 * @constructor
 * @extends {`superclass^}
 */
`namespace.Class^ = function() {
  `superclass^.call(this);`
  `cursor^
};
goog.inherits(`namespace.Class^, `superclass^);

XPT super "getCurrentProvideString().superClass_.
`getCurrentProvideString()^.superClass_.

XPT tagname "goog.dom.TagName
XSET DIV|post=UpperCase(V())
goog.dom.TagName.`DIV^

XPT uid "goog.getUid
goog.getUid(`object^)

XPT up "goog.dom.getAncestorByTagNameAndClass
XSET DIV|post=UpperCase(V())
goog.dom.getAncestorByTagNameAndClass(`element^, goog.dom.TagName.`DIV^`, '`class`'^)

XPT efun "function\(element) \{ }
function (element) { return `body^; }


