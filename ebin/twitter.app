{application, twitter, [
	{description, 'Twitter Stream Receiving & Outputing Service'},
	{vsn, "1.0"},
	{modules, [twitter, stream_server, raw_processor]},
	{applications, [kernel, stdlib]},
	{mod, {twitter, []}}
]}.