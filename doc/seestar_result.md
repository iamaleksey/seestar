

#Module seestar_result#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

##Data Types##




###<a name="type-change">change()</a>##



<pre>change() = created | updated | dropped</pre>



###<a name="type-prepared_result">prepared_result()</a>##



__abstract datatype__: `prepared_result()`



###<a name="type-result">result()</a>##



<pre>result() = void | <a href="#type-rows_result">rows_result()</a> | <a href="#type-set_keyspace_result">set_keyspace_result()</a> | <a href="#type-prepared_result">prepared_result()</a> | <a href="#type-schema_change_result">schema_change_result()</a></pre>



###<a name="type-rows_result">rows_result()</a>##



__abstract datatype__: `rows_result()`



###<a name="type-schema_change_result">schema_change_result()</a>##



__abstract datatype__: `schema_change_result()`



###<a name="type-set_keyspace_result">set_keyspace_result()</a>##



__abstract datatype__: `set_keyspace_result()`



###<a name="type-type">type()</a>##



<pre>type() = void | rows | set_keyspace | prepared | schema_change</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#change-1">change/1</a></td><td></td></tr><tr><td valign="top"><a href="#keyspace-1">keyspace/1</a></td><td></td></tr><tr><td valign="top"><a href="#names-1">names/1</a></td><td></td></tr><tr><td valign="top"><a href="#query_id-1">query_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#rows-1">rows/1</a></td><td></td></tr><tr><td valign="top"><a href="#table-1">table/1</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr><tr><td valign="top"><a href="#type-2">type/2</a></td><td></td></tr><tr><td valign="top"><a href="#types-1">types/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="change-1"></a>

###change/1##


<pre>change(Result::<a href="#type-schema_change_result">schema_change_result()</a>) -> <a href="#type-change">change()</a></pre>
<br></br>


<a name="keyspace-1"></a>

###keyspace/1##


<pre>keyspace(Result::<a href="#type-set_keyspace_result">set_keyspace_result()</a> | <a href="#type-schema_change_result">schema_change_result()</a>) -> binary()</pre>
<br></br>


<a name="names-1"></a>

###names/1##


<pre>names(Result::<a href="#type-rows_result">rows_result()</a> | <a href="#type-prepared_result">prepared_result()</a>) -> [binary()]</pre>
<br></br>


<a name="query_id-1"></a>

###query_id/1##


<pre>query_id(Result::<a href="#type-prepared_result">prepared_result()</a>) -> binary()</pre>
<br></br>


<a name="rows-1"></a>

###rows/1##


<pre>rows(Rows::<a href="#type-rows_result">rows_result()</a>) -> [[<a href="seestar_cqltypes.md#type-value">seestar_cqltypes:value()</a>]]</pre>
<br></br>


<a name="table-1"></a>

###table/1##


<pre>table(Result::<a href="#type-schema_change_result">schema_change_result()</a>) -> binary() | undefined</pre>
<br></br>


<a name="type-1"></a>

###type/1##


<pre>type(Result::<a href="#type-result">result()</a>) -> <a href="#type-type">type()</a></pre>
<br></br>


<a name="type-2"></a>

###type/2##


<pre>type(Result::<a href="#type-rows_result">rows_result()</a> | <a href="#type-prepared_result">prepared_result()</a>, Name::binary()) -> <a href="seestar_cqltypes.md#type-type">seestar_cqltypes:type()</a></pre>
<br></br>


<a name="types-1"></a>

###types/1##


<pre>types(Result::<a href="#type-rows_result">rows_result()</a> | <a href="#type-prepared_result">prepared_result()</a>) -> [<a href="seestar_cqltypes.md#type-type">seestar_cqltypes:type()</a>]</pre>
<br></br>


