

# Module efranc #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-options">options()</a> ###


<pre><code>
options() = #{min_length =&gt; integer(), whitelist =&gt; [<a href="efranc_lang.md#type-code">efranc_lang:code()</a>], blacklist =&gt; [<a href="efranc_lang.md#type-code">efranc_lang:code()</a>], details =&gt; true | false}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#detect-1">detect/1</a></td><td>Equivalent to <a href="#detect-2"><tt>detect(Value, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#detect-2">detect/2</a></td><td> 
Detect the language of text.</td></tr><tr><td valign="top"><a href="#detect_all-1">detect_all/1</a></td><td>Equivalent to <a href="#detect_all-2"><tt>detect_all(Value, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#detect_all-2">detect_all/2</a></td><td> 
Detect the language of text.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="detect-1"></a>

### detect/1 ###

<pre><code>
detect(Value::string()) -&gt; <a href="efranc_lang.md#type-code">efranc_lang:code()</a>
</code></pre>
<br />

Equivalent to [`detect(Value, #{})`](#detect-2).

<a name="detect-2"></a>

### detect/2 ###

<pre><code>
detect(Value::string(), Options::<a href="#type-options">options()</a>) -&gt; <a href="efranc_lang.md#type-code">efranc_lang:code()</a> | <a href="efranc_lang.md#type-iso6393">efranc_lang:iso6393()</a>
</code></pre>
<br />


Detect the language of text. Return the ISO-639-3 code.

Options:

* `min_length: integer()` : minimum length to accept (default: 10)

* `withlist: [string()]` : allow languages (default: all)

* `blacklist: [string()]` : disallow languages (default: none)

* `details: true | false` : return ISO-639-3 details (default: false)


<a name="detect_all-1"></a>

### detect_all/1 ###

<pre><code>
detect_all(Value::string()) -&gt; [{<a href="efranc_lang.md#type-code">efranc_lang:code()</a>, float()}]
</code></pre>
<br />

Equivalent to [`detect_all(Value, #{})`](#detect_all-2).

<a name="detect_all-2"></a>

### detect_all/2 ###

<pre><code>
detect_all(Value::string(), Options::<a href="#type-options">options()</a>) -&gt; [{<a href="efranc_lang.md#type-code">efranc_lang:code()</a> | <a href="efranc_lang.md#type-iso6393">efranc_lang:iso6393()</a>, float}]
</code></pre>
<br />


Detect the language of text. Return a list of ISO-639-3 codes with weight.

Options:

* `min_length: integer()` : minimum length to accept (default: 10)

* `withlist: [string()]` : allow languages (default: all)

* `blacklist: [string()]` : disallow languages (default: none)


