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

# Dependencies

* [couchbeam](https://github.com/benoitc/couchbeam)

# Auth
Basic auth only at the moment. It will read a `credentials` file that
looks like this :

```erlang
{username, "myusername"}.
{password, "mypassword"}.
```

# How it works
It uses a special view from MultiBin (`expire_date`) that returns one
row for each doc indexed by the expire date. This view is queried with
the date at the moment of the call in descending order, which means that
the retrieved doc are the docs which expire date are in the past. They
are then all deleted.

# In the future

* Make it a daemon that could be spawned along with couchdb and watched
  by it.
* Watch for the `_changes` feed and recall the view each time a new doc
  comes in (but do not rely only on the volatile `_changes` feed; logic
      should always be made according to the view).
* `burnafterreading` logic ...
