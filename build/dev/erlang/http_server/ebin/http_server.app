{application, http_server, [
    {vsn, "1.0.0"},
    {applications, [elli,
                    gleam_elli,
                    gleam_http,
                    gleam_json,
                    gleam_stdlib,
                    gleeunit]},
    {description, ""},
    {modules, [http_server,
               http_server_test,
               person]},
    {registered, []}
]}.
