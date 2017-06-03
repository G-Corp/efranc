

# Module efranc_lang #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-code">code()</a> ###


<pre><code>
code() = string()
</code></pre>




### <a name="type-iso6393">iso6393()</a> ###


<pre><code>
iso6393() = #{code =&gt; string(), part_2b =&gt; string(), part_2t =&gt; string(), part_1 =&gt; string(), scope =&gt; individual | macrolanguage | special, type =&gt; ancient | constructed | extinct | historical | living | special, ref =&gt; string(), comment =&gt; string()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#info-1">info/1</a></td><td>
Return details for an ISO-369-3 code.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="info-1"></a>

### info/1 ###

<pre><code>
info(Code::<a href="#type-code">code()</a>) -&gt; <a href="#type-iso6393">iso6393()</a>
</code></pre>
<br />

Return details for an ISO-369-3 code.

