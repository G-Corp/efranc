

# Module efranc #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#detect-1">detect/1</a></td><td>Equivalent to <a href="#detect-2"><tt>detect(Value, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#detect-2">detect/2</a></td><td> 
Detect the language of text.</td></tr><tr><td valign="top"><a href="#detect_all-1">detect_all/1</a></td><td>Equivalent to <a href="#detect_all-2"><tt>detect_all(Value, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#detect_all-2">detect_all/2</a></td><td> 
Detect the language of text.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="detect-1"></a>

### detect/1 ###

`detect(Value) -> any()`

Equivalent to [`detect(Value, #{})`](#detect-2).

<a name="detect-2"></a>

### detect/2 ###

`detect(Value, Options) -> any()`


Detect the language of text. Return the IANA code.

Options:

* ```
min_length: integer()
```
 : minimum length to accept (default: 10)

* ```
withlist: [string()]
```
 : allow languages (default: all)

* ```
blacklist: [string()]
```
 : disallow languages (default: none)


<a name="detect_all-1"></a>

### detect_all/1 ###

`detect_all(Value) -> any()`

Equivalent to [`detect_all(Value, #{})`](#detect_all-2).

<a name="detect_all-2"></a>

### detect_all/2 ###

`detect_all(Value, Options) -> any()`


Detect the language of text. Return a list of IANA codes with weight.

Options:

* ```
min_length: integer()
```
 : minimum length to accept (default: 10)

* ```
withlist: [string()]
```
 : allow languages (default: all)

* ```
blacklist: [string()]
```
 : disallow languages (default: none)


