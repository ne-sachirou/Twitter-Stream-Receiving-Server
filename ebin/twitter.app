{application, twitter, [
	{description, 'Twitter Stream Receiving & Outputing Service'},
	{vsn, "1.0"},
	{modules,
		[
			twitter, stream_server, couchdb_processor, raw_processor, unilib,
			erlang_couchdb, couchdb, mochijson2
		]
	},
	{applications, [kernel, stdlib]},
	{mod, {twitter, []}}
]}.