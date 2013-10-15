# Chapter 6 - Implementing a caching system

##What can this application do?

1. Start/Stop 
2. Add key/value pair to cache
3. Retrieve a value given a key
4. Update value for a given key
5. Delete key/value pair

##Core ideas

* Separate process to store each value being inserted
* Key is therefore mapped to the process id (`pid`)
* This allows each value to have it's own lifecycle
* Each of this processes would be based on `GenServer.Behaviour`
* This allows the data to be stored in `defrecord State`

##Design

* `SimpleCache`: Main entry point/API/Application Behaviour
* `SimpleCache.Supervisor`: Supervisor
* `SimpleCache.Store`: Storage for key/value pairs
* `SimpleCache.Element`: Value storage processes