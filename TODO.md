Current reading: https://craftinginterpreters.com/jumping-back-and-forth.html

Bonus TODOs:

* Implement long globals similar to OP_LONG_CONSTANT
* Implement long locals
* Can I optimize the lookup key for globals? (instead of a string hash)
* Can I optimize the storage of global constants? Currently a new constants entry is created every time the global is referenced.
* Add constants. New keyword 'const'
* Optimize local resolution. Currently it does a linear scan through the Locals array.