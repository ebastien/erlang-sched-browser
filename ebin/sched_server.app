{application, sched_server, [
  {description, "Airlines Schedules Server"},
  {vsn, "1"},
  {modules, [
    sched_browser, sched_geography, sched_leex, sched_network_db,
    sched_oag, sched_parser, sched_pathfinder,
    sched_schedules_db, sched_yecc, sched_test, sched_nif,
    sched_server, sched_server_app, sched_server_sup,
    sched_solver
  ]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {sched_server_app, []}},
  {env, [{cache, true}]}
]}.
