{
    application,
    esort_proxy,
    [
        {description, "esort Proxy Client"},
        {vsn, "1.0.0"},
        {modules, [
            esort_proxy_app,
            esort_proxy_sup,
            esort_proxy_connection,
	    esort_proxy_dipatch,
	    esort_proxy_connection_sup,
	    esort_proxy_dipatch_sup
        ]},
        {registered, [esort_proxy_sup]},
        {applications, [kernel, stdlib]},
        {mod, {esort_proxy_app, []}}
    ]
}.
