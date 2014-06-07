{application, test_app,
   [{description, "Test timed_supervisor"},
    {vsn, "1.0"},
    {modules, [test_app, test_srv]},
    {registered, []},
    {applications, [kernel, stdlib]},
    {env, []},
    {mod, {test_app, []}}
   ]
}.

