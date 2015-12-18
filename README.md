# talks_tweeter #

## Configuring ##

Create the `config/sys.config` file with the following content:

```erlang
[{etweet, [
           {screen_name, "TO_FILL"},
           {consumer_key, "TO_FILL"},
           {consumer_secret, "TO_FILL"},
           {access_token, "TO_FILL"},
           {access_token_secret, "TO_FILL"}
          ]
 }].
```

## Building and Running ##

```bash
make compile
make shell
```

> The config/sys.config file has to be filled with appropriate values.

## API ##

### Architecture ###

![architecture](https://docs.google.com/drawings/d/1Tg9J9MGxVXwA0_3NdQ4FWo9yQ3XYL78aljEgNRK3Yu4/pub?w=960&h=720)

### Talks Importer ###

```erlang
tt_importer:start_link() -> Result.

tt_importer:import_file(Filename) -> ok.
tt_importer:import_file("priv/talks") -> ok.

```

The `tt_importer` should be implemented as a `gen_server` plugged into the application supervision tree. The `tt_importer:import_file/1` function takes a file name as an argument and import its content into the database using its API (`tt_store:add/4`).

The file with the talks should have the following format:
```erlang
[
  {"School of Erlang", {{2015,12,15}, {10,00,00}}, {{2015,12,15}, {11,00,00}}, "ESL Office"},
  {"School of Elixir", {{2015,12,15}, {11,00,00}}, {{2015,12,15}, {12,00,00}}, "ESL Office"}
]
```

### Talks Database ###

```erlang
tt_store:start_link() -> Result.

tt_store:add(Title, StartTime, EndTime, Where) -> ok.
tt_store:add("School of Erlang", {{2015,12,17}, {17,00,00}}, {{2015,12,17}, {19,00,00}}) -> ok.

tt_store:find_by_time(StartTime, EndTime) -> 
tt_store:find_by_time({{2015,12,17}, {17,00,00}}, {{2015,12,17}, {20,00,00}}) -> [{TalkTitle, StartTime, EndTime, Where}]

tt_store:list() -> [{TalkTitle, StartTime, EndTime, Where}]
```

The `tt_store` module should be implemented as a `gen_server` plugged into the application supervision tree. The `tt_store:add/3` function is for adding new talks to the database. `tt_store:find_by_time/2` is for getting a list of talks that are planned to take place in a given time range. The `tt_store:list/0` should return the list of all the talks in the store.

### Scheduling Publishing Talks ###


```erlang
tt_scheduler:start_link() -> Result.

tt_scheduler:schedule(StartTime, EndTime, PublishInterval).
tt_scheduler:schedule({{2015, 12, 17}, {15, 00, 00}}, {{2015, 12, 17}, {20, 00, 00}}, {01,00,00}) -> Ref.

tt_scheduler:cancel_schedule(Ref) -> ok.
```

The `tt_scheduler` should be a `gen_server` plugged into the application supervision tree. It is intended for setting up scheduling of publishing information on talks. For example calling `tt_scheduler:schedule({{2015, 12, 17}, {15, 00, 00}}, {{2015, 12, 17}, {20, 00, 00}}, {00,20,00}).` should start a process that will check every hour what are the planned talks for the next hour and pass the result to the `tt_publisher`.

### Publisher ###

```erlang
tt_publisher:start_link() -> Result.

tt_publisher:publish([{Title, StartTime, EndTime, Where}], TimeInterval) -> ok
tt_publisher:publish([
                      {"School of Erlang", {{2015,12,17}, {17,00,00}}, {{2015,12,17}, {17,30,00}}, "ESL Office"}
                      {"School of Elixir", {{2015,12,17}, {18,00,00}}, {{2015,12,17}, {18,30,00}}, "ESL Office"},
                      {01,00,00}
                     ) -> ok.
```

`tt_publisher:publish/1` should publish on Twitter the talks. So the example invocation above should result in something like following in the Twitter account:

> The plan for the following hour is:
> * 17:00 - 17:30 "School of Erlang", ESL Office
> * 17:30 - 18:00 "School of Elixir", ESL Office

`tt_publisher` should run as a `gen_server` and be plugged into the application supervision tree.

## References ##

* [rebar3](https://www.rebar3.org/) for building
* [Erlang Applications](http://www.erlang.org/doc/design_principles/applications.html)
* [Erlang Supervisors](http://www.erlang.org/doc/design_principles/sup_princ.html)
* [Erlang Generic Servers](http://www.erlang.org/doc/design_principles/gen_server_concepts.html)
