

#Module seestar_cqltypes#
* [Data Types](#types)



<a name="types"></a>

##Data Types##




###<a name="type-decimal">decimal()</a>##



<pre>decimal() = {Unscaled::integer(), Scale::integer()}</pre>



###<a name="type-native">native()</a>##



<pre>native() = ascii | bigint | blob | boolean | counter | decimal | double | float | int | text | timestamp | uuid | varchar | varint | timeuuid | inet</pre>



###<a name="type-type">type()</a>##



<pre>type() = <a href="#type-native">native()</a> | {list | set, <a href="#type-native">native()</a>} | {map, <a href="#type-native">native()</a>, <a href="#type-native">native()</a>} | {custom, string()}</pre>



###<a name="type-value">value()</a>##



<pre>value() = null | integer() | binary() | boolean() | float() | <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="#type-decimal">decimal()</a> | list() | dict() | set()</pre>
