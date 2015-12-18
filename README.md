# jt
Intended to be a Haskell cmd line tool for querying info on a hadoop jobs


jt is configured by having a `.hadoop_cluster.conf` file in your tree, tracing back from the CWD, or from your $HOME.
In here you should on each line give a short name to each of your hadoop clusters, then a link to your RM and history end points. e.g.:
tstA http://tstA.example.com:50030 http://tstA.example.com:8080


Commands::

show [--history] [--resource-manager]
List out what is the default hadoop cluster and other clusters in the system. Optionally just print the URL's for history or RM

jobs [-u|--user USER] [-c|--cluster CLUSTER] [-l|--limit LIMIT]
                   [-o|--history] [-a|--resource-manager] [-t|--tabs]

List up to `LIMIT` jobs for optionally `USER` or all users from cluster `CLUSTER` or your default cluster. Its also possible to restrict it to the history or RM using the `-o` and `-a` flags respectively. Pretty printing of output is enabled by default. To use simple tables for consumption in other tooling pass in `-t`.

Example:
```bash
Name                                         User   State    JobId                   StartedTime
MyFirstJob.1450391964.(2015-07-18).(3/3)     myUser FINISHED job_1450230361xxx_xxx 1450401485960
```

