-define(APP, seer).
-define(ENV(Key, Default), application:get_env(?APP, Key, Default)).
-define(ENV_PREFIX, prefix).
-define(ENV_HOST, host).
-define(ENV_MODE, mode).
-define(ENV_INTERVAL, interval).
-define(ENV_CARBON_HOST, carbon_host).
-define(ENV_CARBON_PORT, carbon_port).
-define(DEFAULT_PREFIX, <<"seer">>).
-define(DEFAULT_HOST, <<"localhost">>).
-define(DEFAULT_MODE, carbon).
-define(DEFAULT_INTERVAL, 10000).
-define(DEFAULT_CARBON_HOST, localhost).
-define(DEFAULT_CARBON_PORT, 2003).
