# talks_tweeter #

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [talks_tweeter](#talkstweeter)
    - [Configuring](#configuring)
    - [Building and Running](#building-and-running)
    - [API](#api)
        - [Architecture](#architecture)
        - [Talks Importer](#talks-importer)
        - [Talks Database](#talks-database)
        - [Scheduling Publishing Talks](#scheduling-publishing-talks)
        - [Publisher](#publisher)
    - [References](#references)

<!-- markdown-toc end -->


## Configuring ##

Create the `config/sys.config` file with the following content:

```erlang
[
  %% talks_tweeter options are optional, you have only to provide 'etweet'
  %% options
  {talks_tweeter, [
    {action_interval, {0,1,0}}, %% check every 1 minute for talk
    {time_window,     {0,5,0}}, %% time period which will be used for
                                %% querying for talks. 'talks_tweeter' will
                                %% check for talks within
                                %% (now + before_talk, now + before_talk + time_window) minutes
    {schedule_start_time, {{2016,2,18}, {6,0,0}}}, %% start time from which
                                                   %% all talks has begin
    {schedule_end_time, {{2016,2,19}, {18,0,0}}}, %% end time for all talks
    {before_talk, {0,15,0}} %% tweet 15 minutes before talk
  ]},
  {etweet, [
           {screen_name, "TO_FILL"},
           {consumer_key, "TO_FILL"},
           {consumer_secret, "TO_FILL"},
           {access_token, "TO_FILL"},
           {access_token_secret, "TO_FILL"}
          ]
  }].
```

## Building and Running ##

> You need to have Erlang 18.3

```bash
make compile
make test
make shell
```

> The config/sys.config file has to be filled with appropriate values.

## Running as a release ##

> Ensure you have valid config/sys.config file

```bash
make rel
cd _build/default/rel/talks_tweeter
. /bin/talks_tweeter start #run application
. /bin/talks_tweeter attach #attach to console
```
In console run:
```erlang
1> talks_tweeter_app:run().
```
and exit by pressing `Ctrl + D`.

## Architecture ##

![architecture](https://docs.google.com/drawings/d/1Tg9J9MGxVXwA0_3NdQ4FWo9yQ3XYL78aljEgNRK3Yu4/pub?w=960&h=720)

## API ##

### Talks Importer ###

```erlang
tt_importer:start_link() -> Result.

tt_importer:import_file(Filename) -> ok.
tt_importer:import_file("priv/talks") -> ok.

tt_importer:import_csv_file(Filename, StartDate) -> ok | {error ,file_not_exists}.
tt_importer:import_csv_file("talks.csv", {2016,02,04}).

```

The `tt_importer` is responsible for importing talks from file. The `tt_importer:import_file/1` function takes a file name as an argument and imports its content into the database using its API (`tt_store:add/4`).

The file with the talks should have the following format:
```erlang
[
  {"School of Erlang", {{2015,12,15}, {10,00,00}}, {{2015,12,15}, {11,00,00}}, "ESL Office"},
  {"School of Elixir", {{2015,12,15}, {11,00,00}}, {{2015,12,15}, {12,00,00}}, "ESL Office"}
]
```

The `tt_importer:import_csv_file/2` functions takes two arguments: a CSV file and a date that talks stored in the file start on. An example file content is:

```csv
"Keynote: Haskell",room 3,2,185
I love teacher?,room 1,1,330
"Bad news: teacher?",room 3,2,240
"Joe, do you know Cooking",room 3,1,360
```

The format is: 
`Title, Location, Day No, Start Time in Minutes from 7:00`

There're 3 locations which map as follows:
```
room 1 -> Aula średnia A
room 2 -> Aula średnia B
room 3 -> Aula mała
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

The `tt_store` module is responsible for storing talks in ETS tables. The `tt_store:add/3` function is for adding new talks to the database. `tt_store:find_by_time/2` is for getting a list of talks that are planned to **start** in a given time range. The `tt_store:list/0` returns the list of all the talks in the store.

### Scheduling Publishing Talks ###


```erlang
tt_scheduler:start_link() -> Result.

tt_scheduler:schedule(StartTime, EndTime, PublishInterval).
tt_scheduler:schedule({{2015, 12, 17}, {15, 00, 00}}, {{2015, 12, 17}, {20, 00, 00}}, {01,00,00}) -> Ref.

tt_scheduler:cancel_schedule(Ref) -> ok.
```

The `tt_scheduler` is intended for setting up scheduling of publishing information on talks. For example calling `tt_scheduler:schedule({{2015, 12, 17}, {15, 00, 00}}, {{2015, 12, 17}, {20, 00, 00}}, {00,20,00}).` starts a process that will check every hour what are the planned talks for the next hour and pass the result to the `tt_publisher`.

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

`tt_publisher:publish/1` publishes the talks on Twitter. The example invocation above result in something like following in the Twitter account:

> The plan for the following hour is:
> * 17:00 - 17:30 "School of Erlang", ESL Office
> * 17:30 - 18:00 "School of Elixir", ESL Office

## References ##

* [rebar3](https://www.rebar3.org/) for building
* [Erlang Applications](http://www.erlang.org/doc/design_principles/applications.html)
* [Erlang Supervisors](http://www.erlang.org/doc/design_principles/sup_princ.html)
* [Erlang Generic Servers](http://www.erlang.org/doc/design_principles/gen_server_concepts.html)
