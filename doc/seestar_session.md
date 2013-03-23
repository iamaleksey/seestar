

#Module seestar_session#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


__Behaviours:__ [`gen_server`](gen_server.md).
<a name="types"></a>

##Data Types##




###<a name="type-query">'query'()</a>##



<pre>'query'() = binary() | string()</pre>



###<a name="type-client_option">client_option()</a>##



<pre>client_option() = {keyspace, string() | binary()} | {credentials, <a href="#type-credentials">credentials()</a>} | {events, <a href="#type-events">events()</a>}</pre>



###<a name="type-connect_option">connect_option()</a>##



<pre>connect_option() = <a href="gen_tcp.md#type-connect_option">gen_tcp:connect_option()</a> | {connect_timeout, timeout()}</pre>



###<a name="type-credentials">credentials()</a>##



<pre>credentials() = [{string() | binary(), string() | binary()}]</pre>



###<a name="type-events">events()</a>##



<pre>events() = [topology_change | status_change | schema_change]</pre>



###<a name="type-query_id">query_id()</a>##



<pre>query_id() = binary()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#execute-4">execute/4</a></td><td>Equivalent to <a href="#execute-5"><tt>execute(Client, QueryID, Types, Values, one)</tt></a>.</td></tr><tr><td valign="top"><a href="#execute-5">execute/5</a></td><td>Synchronously execute a prepared query using the specified consistency level.</td></tr><tr><td valign="top"><a href="#execute_async-5">execute_async/5</a></td><td></td></tr><tr><td valign="top"><a href="#perform-2">perform/2</a></td><td>Equivalent to <a href="#perform-3"><tt>perform(Client, Query, one)</tt></a>.</td></tr><tr><td valign="top"><a href="#perform-3">perform/3</a></td><td>Synchoronously perform a CQL query using the specified consistency level.</td></tr><tr><td valign="top"><a href="#perform_async-3">perform_async/3</a></td><td>Asynchronously perform a CQL query using the specified consistency level.</td></tr><tr><td valign="top"><a href="#prepare-2">prepare/2</a></td><td>Prepare a query for later execution.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>Equivalent to <a href="#start_link-3"><tt>start_link(Host, Post, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td>Equivalent to <a href="#start_link-4"><tt>start_link(Host, Post, ClientOptions, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stop the client.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="execute-4"></a>

###execute/4##


<pre>execute(Client::pid(), QueryID::<a href="#type-query_id">query_id()</a>, Types::[<a href="seestar_cqltypes.md#type-type">seestar_cqltypes:type()</a>], Values::[<a href="seestar_cqltypes.md#type-value">seestar_cqltypes:value()</a>]) -> {ok, Result::<a href="seestar_result.md#type-result">seestar_result:result()</a>} | {error, Error::<a href="seestar_error.md#type-error">seestar_error:error()</a>}</pre>
<br></br>


Equivalent to [`execute(Client, QueryID, Types, Values, one)`](#execute-5).<a name="execute-5"></a>

###execute/5##


<pre>execute(Client::pid(), QueryID::<a href="#type-query_id">query_id()</a>, Types::[<a href="seestar_cqltypes.md#type-type">seestar_cqltypes:type()</a>], Values::[<a href="seestar_cqltypes.md#type-value">seestar_cqltypes:value()</a>], Consistency::<a href="seestar.md#type-consistency">seestar:consistency()</a>) -> {ok, Result::<a href="seestar_result.md#type-result">seestar_result:result()</a>} | {error, Error::<a href="seestar_error.md#type-error">seestar_error:error()</a>}</pre>
<br></br>


Synchronously execute a prepared query using the specified consistency level.
Use [`seestar_result`](seestar_result.md) module functions to work with the result.

__See also:__ [perform/3](#perform-3), [prepare/2](#prepare-2).<a name="execute_async-5"></a>

###execute_async/5##


`execute_async(Client, QueryID, Types, Values, Consistency) -> any()`

<a name="perform-2"></a>

###perform/2##


<pre>perform(Client::pid(), Query::<a href="#type-query">'query'()</a>) -> {ok, Result::<a href="seestar_result.md#type-result">seestar_result:result()</a>} | {error, Error::<a href="seestar_error.md#type-error">seestar_error:error()</a>}</pre>
<br></br>


Equivalent to [`perform(Client, Query, one)`](#perform-3).<a name="perform-3"></a>

###perform/3##


<pre>perform(Client::pid(), Query::<a href="#type-query">'query'()</a>, Consistency::<a href="seestar.md#type-consistency">seestar:consistency()</a>) -> {ok, Result::<a href="seestar_result.md#type-result">seestar_result:result()</a>} | {error, Error::<a href="seestar_error.md#type-error">seestar_error:error()</a>}</pre>
<br></br>


Synchoronously perform a CQL query using the specified consistency level.
Returns a result of an appropriate type (void, rows, set_keyspace, schema_change).
Use [`seestar_result`](seestar_result.md) module functions to work with the result.<a name="perform_async-3"></a>

###perform_async/3##


<pre>perform_async(Client::pid(), Query::<a href="#type-query">'query'()</a>, Consistency::<a href="seestar.md#type-consistency">seestar:consistency()</a>) -> ok</pre>
<br></br>


Asynchronously perform a CQL query using the specified consistency level.<a name="prepare-2"></a>

###prepare/2##


<pre>prepare(Client::pid(), Query::<a href="#type-query">'query'()</a>) -> {ok, Result::<a href="seestar_result.md#type-prepared_result">seestar_result:prepared_result()</a>} | {error, Error::<a href="seestar_error.md#type-error">seestar_error:error()</a>}</pre>
<br></br>


Prepare a query for later execution. The response will contain the prepared
query id and column metadata for all the variables (if any).

__See also:__ [execute/3](#execute-3), [execute/4](#execute-4).<a name="start_link-2"></a>

###start_link/2##


<pre>start_link(Host::<a href="inet.md#type-hostname">inet:hostname()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -> any()</pre>
<br></br>


Equivalent to [`start_link(Host, Post, [])`](#start_link-3).<a name="start_link-3"></a>

###start_link/3##


<pre>start_link(Host::<a href="inet.md#type-hostname">inet:hostname()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>, ClientOptions::[<a href="#type-client_option">client_option()</a>]) -> any()</pre>
<br></br>


Equivalent to [`start_link(Host, Post, ClientOptions, [])`](#start_link-4).<a name="start_link-4"></a>

###start_link/4##


<pre>start_link(Host::<a href="inet.md#type-hostname">inet:hostname()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>, ClientOptions::[<a href="#type-client_option">client_option()</a>], ConnectOptions::[<a href="#type-connect_option">connect_option()</a>]) -> any()</pre>
<br></br>


<a name="stop-1"></a>

###stop/1##


<pre>stop(Client::pid()) -&gt; ok</pre>
<br></br>


Stop the client.
Closes the socket and terminates the process normally.