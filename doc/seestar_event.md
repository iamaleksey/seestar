

#Module seestar_event#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

##Data Types##




###<a name="type-event">event()</a>##



__abstract datatype__: `event()`



###<a name="type-schema_change">schema_change()</a>##



<pre>schema_change() = created | unpdated | dropped</pre>



###<a name="type-schema_change_event">schema_change_event()</a>##



__abstract datatype__: `schema_change_event()`



###<a name="type-status_change">status_change()</a>##



<pre>status_change() = up | down</pre>



###<a name="type-status_change_event">status_change_event()</a>##



__abstract datatype__: `status_change_event()`



###<a name="type-topology_change">topology_change()</a>##



<pre>topology_change() = new_node | removed_node</pre>



###<a name="type-topology_change_event">topology_change_event()</a>##



__abstract datatype__: `topology_change_event()`



###<a name="type-type">type()</a>##



<pre>type() = topology_change | status_change | schema_change</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#change-1">change/1</a></td><td></td></tr><tr><td valign="top"><a href="#ip-1">ip/1</a></td><td></td></tr><tr><td valign="top"><a href="#keyspace-1">keyspace/1</a></td><td></td></tr><tr><td valign="top"><a href="#port-1">port/1</a></td><td></td></tr><tr><td valign="top"><a href="#table-1">table/1</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="change-1"></a>

###change/1##


<pre>change(Event::<a href="#type-event">event()</a>) -> <a href="#type-topology_change">topology_change()</a> | <a href="#type-status_change">status_change()</a> | <a href="#type-schema_change">schema_change()</a></pre>
<br></br>


<a name="ip-1"></a>

###ip/1##


<pre>ip(Event::<a href="#type-topology_change_event">topology_change_event()</a> | <a href="#type-status_change_event">status_change_event()</a>) -> <a href="inet.md#type-ip_address">inet:ip_address()</a></pre>
<br></br>


<a name="keyspace-1"></a>

###keyspace/1##


<pre>keyspace(Event::<a href="#type-schema_change_event">schema_change_event()</a>) -> binary()</pre>
<br></br>


<a name="port-1"></a>

###port/1##


<pre>port(Event::<a href="#type-topology_change_event">topology_change_event()</a> | <a href="#type-status_change_event">status_change_event()</a>) -> <a href="inet.md#type-port_number">inet:port_number()</a></pre>
<br></br>


<a name="table-1"></a>

###table/1##


<pre>table(Event::<a href="#type-schema_change_event">schema_change_event()</a>) -> binary() | undefined</pre>
<br></br>


<a name="type-1"></a>

###type/1##


<pre>type(Event::<a href="#type-event">event()</a>) -> <a href="#type-type">type()</a></pre>
<br></br>


