{
    application,
    esort_server,
    [
        {description, "esort Proxy server"},
        {vsn, "1.0.0"},
        {modules, [
            esort_server_app,
            esort_server_sup,
            esort_server_child
        ]},
        {registered, [esort_server_sup]},
        {applications, [kernel, stdlib]},
        {mod, {esort_server_app, []}}
    ]
}.
