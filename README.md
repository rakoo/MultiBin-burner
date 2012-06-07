Multibin-burner is a couchdb tool for MultibBin that watches for
documents expire dates, and burns them (ie deletes them) when time has
come.

Very dirty code at the moment, more a proof-of-concept than anything
else.

# Why ?
CouchDB is just a database; it is not a web server. Thus, it cannot
process docs on a GET (and even if it could, it shouldn't). This
(future) daemon is here to do the backend work.

# Why not a frontend server, like Node.js, or whatever ?
Because it brings more dependencies for a little task. If someone runs
MultiBin, she has CouchDB; If she has CouchDB, she has an Erlang VM; If
she has an Erlang VM, she can run this burner. Plus, more code means
more chances to crash.

If more tasks are needed, an erlang web server might come in handy,
   though.

Note that the CouchDB API can be rewritten in any language, so the
erlang part might disappear someday, while there will always be
a JavaScript engine; a js script might have been a better idea, after
all.

# Dependencies

* [couchbeam](https://github.com/benoitc/couchbeam)

# Auth
Basic auth only at the moment. It will read a `credentials` file that
looks like this :

```erlang
{username, "myusername"}.
{password, "mypassword"}.
```

# Usage
It is still a bunch of lines of code at the moment. If you want to see
how it behaves :

```erlang
$ erl -pa /path/to/couchbeam/ebins
erl> c(burner).
erl> Pid = burner:start()
<0.127.0>
erl> burner:send_in_20_secs_doc(Pid).
Received Date to burn : <<"2012-06-07T23:55:01.378Z">>
{<0.113.0>,{start,{{2012,6,7},{23,55,1}}}}
erl> burner:send_in_10_secs_doc(Pid).       % send an earlier date
New Date : <<"2012-06-07T23:54:54.506Z">>
{<0.113.0>,{start,{{2012,6,7},{23,54,54}}}}
erl> burner:send_in_20_secs_doc(Pid).       % send a later date
Not a new date
{<0.113.0>,{start,{{2012,6,7},{23,55,5}}}}
Burning {{2012,6,7},{23,54,54}}
Burnt {{2012,6,7},{23,54,54}}, waiting 3s
erl> burner:send_in_20_secs_doc(Pid).       % send a new doc during processing ...
{<0.113.0>,{start,{{2012,6,7},{23,55,16}}}}
Waited 3s
Received Date to burn : <<"2012-06-07T23:55:16.876Z">>    % ... that is automatically processed
erl>
Burning {{2012,6,7},{23,55,12}}
Burnt {{2012,6,7},{23,55,12}}, waiting 3s
Waited 3s
```


# How it works
  MultiBin-burner is a simple state machine. There are 3 states : 
* `nothing_to_do`
* `waiting`
* `burning`

During the `nothing_to_do`, there is ... nothing to do. It just waits
for a new doc to process. When a new one is sent to the Pid, it switches
to the `waiting` state.

Upon entering this state, the machine will calculate the waiting time,
     and start a `receive` block with this time as a timeout.
In this state, a new doc can arrive; if it is to be burnt before the
previous one, we replace the previous one by this one (by starting a new
    loop with the new doc).

When the timeout is triggered, the machine switches to the `burning`
state. This state is just a sequential bunch of procedures to burn the
document(s) that have the specified expire date. When it's done, the
machine goes back to `nothing_to_do` state. 

For the purpose of testing, I introduced a 3 secs of `sleep` to
represent a long transaction the couchdb. If a doc is sent during this
fake processing, it will be stacked in the mailbox, and processed when
entering the `nothing_to_do` state. We do not want the burning process
to be interrupted.

# In the future

* Make it a daemon that could be spawned along with couchdb and watched
  by it.
* Watch for the `_changes` feed and recall the view each time a new doc
  comes in (but do not rely only on the volatile `_changes` feed; logic
      should always be made according to the view).
* `burnafterreading` logic ...
