{application, my_db,
[{description, "DB Service"},
{vsn, "1.0"},
{modules, [my_db_gen, my_db_sup, my_db_app]},
{registered, [my_db_gen, my_db_sup]},
{applications, [kernel, stdlib]},
{env, [{data, [{some_key, some_val}]}]},
{mod, {my_db_app,[]}}]}.