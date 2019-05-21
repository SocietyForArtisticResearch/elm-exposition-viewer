# What's this ?

This viewer takes a JSON created by <https://github.com/SocietyForArtisticResearch/parse-exposition> and can display it in various ways.
It is built using Elm language.

To run, use elm reactor and open index.html
The input is a json (at this moment test-exposition2.json)

It will currently try to render the weaves as a grid based on position. 
This fails only because of problem nr 2.

# TODO

* cleanup of next links
* filter style elements from HTML tools (there are <tables> with fixed width, this messes up the Bootstrap.Grid.. 
