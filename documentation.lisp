(in-package #:org.shirakumo.cari3s)

;; generators
(docs:define-docs
  (type battery
    "A generator for battery status information.

You may supply the name of the battery of interest using the
:BATTERY initarg. It defaults to the first battery device.

This value-generator supplies the following six values:
  0. charge-percentage
  1. charging-p
  2. discharging-p
  3. hours-remaining
  4. minutes-remaining
  5. seconds-remaining

See VALUE-GENERATOR")

  (type clock
    "A generator for a current local time clock.

This value-generator supplies the following nine values:
  0. year
  1. month
  2. day-of-month
  3. day-of-week
  4. hour
  5. minute
  6. second
  7. short-day-name
  8. short-month-name

See VALUE-GENERATOR")

  (type cpu-usage
    "A generator for the current CPU usage.

This value-generator supplies the following value:
  0. usage-percentage

See VALUE-GENERATOR")

  (type disk-usage
    "A generator for the current partition capacity usage.

You may specify the path to the partition of interest using
the :DEVICE initarg.

This value-generator supplies the following value:
  0. usage-percentage

See VALUE-GENERATOR")

  (type io-usage
    "A generator for the current disk i/o usage.

You may specify whether you're interested in :read, :write, or
:read-write using the :DIRECTION initarg.

This value-generator supplies the following value:
  0. megabytes-per-second

See VALUE-GENERATOR")

  (type mem-usage
    "A generator for the current memory usage.

You may specify whether you're interested in swap and cache
by using the :SWAP and :CACHE initargs respectively.
When :CACHE is T, memory used by the cache is counted as
occupied. When :SWAP is T, swap memory and usage are added
to the total.

This value-generator supplies the following three values:
  0. usage-percentage
  1. total-megabytes
  2. free-megabytes

See VALUE-GENERATOR")

  (type network-ip
    "A generator for the current network IP address.

You may specify which device you're interested in with the
:DEVICE initarg. It defaults to whichever device is listed
first that does not bind to localhost.

This value-generator supplies the following two values:
  0. ip-address
  1. device-name

See VALUE-GENERATOR")

  (type uptime
    "A generator for the current system uptime.

This value-generator supplies the following four values:
  0. uptime-days (NIL if zero)
  1. uptime-hours
  2. uptime-minutes
  3. uptime-seconds")

  (type weather
    "A generator for current weather data.

You must specify your darksky.net api-key via the :API-KEY initarg,
and your global coordinates in the (LAT . LNG) format via the
:LOCATION initarg.

This requires an active internet connection to function.

This value-generator supplies the following seven values:
  0. summary
  1. temperature [C]
  2. temperature-apparent-p
  3. apparent-temperature [C]
  4. humidity [%]
  5. wind [km/h]
  6. pressure [hPa]"))

;; event.lisp
(docs:define-docs
  (type event
    "Superclass for all events that might happen in the system.

See PROCESS-EVENT")

  (type generate
    "An event to signal that blocks should be generated immediately.

This event does not reach generators.

See event")

  (type echo
    "An event that is sent back over the connection as-is.

This event does not reach generators.

See event")
  
  (type click
    "Class representing a click event being fired on a block.

This event comes from i3 itself if the connection was opened with
a header that has SEND-CLICK-EVENTS-P set to T.

See NAME
See INSTANCE
See BUTTON
See LOCATION
See RELATIVE-LOCATION
See BLOCK-SIZE")
  
  (function button
    "Accessor to the X11 button ID that was used to perform the click.

By default this should be:
  1 -- left
  2 -- middle
  3 -- right

See CLICK")
  
  (function location
    "Accessor to the screen position of the click.

The location is a cons of X and Y coordinates, relative to the top
left corner of the X11 root window in pixels.

See CLICK")
  
  (function relative-location
    "Accessor to the relative position of the click.

The location is a cons of X and Y coordinates, relative to the top
left corner of the block itself in pixels.

See CLICK")
  
  (function block-size
    "Accessor to the block's screen dimensions.

The dimensions is a cons of WIDTH and HEIGHT sizes in pixels.

See CLICK")

  (function process-event
    "Causes the given event to be processed by the specified receiver.

If the receiver does not understand the event, an error is
signalled. In the case of a status-bar, unknown events are passed
on to its generators. In the case of a generator, unknown events
are ignored.

You should add methods to this as necessary in order to interface
your object with the general event system.

See EVENT")

  (function parse-event-or-lose
    "Attempts to parse an event from the given source, signalling an error on failure.

This will return an EVENT instance.

See EVENT")

  (function object-initargs
    "Returns a list of initargs to reconstruct the given object with make-instance.

By default this simply  gathers a list of all slots that are
bound and that have at least one initarg. This is for the purpose
of reconstructing the object after serialisation.

You may add methods in case you want more precise control over
the reconstruction of the object.")

  (function serialize-object
    "Serialises the object to the given stream in the event protocol format.

The event protocol format is an UTF-8 based format:
  OBJECT    ::= \"NIL\" | \"T\" | INSTANCE
  INSTANCE  ::= CLASS-NAME INITARG*
  INITARG   ::= keyword VALUE
  VALUE     ::= symbol | string | real | list

Programs that intend on parsing events or return values
from Cari3s need to be able to parse this format and
serialise their respective format to it as well.

Restrictions on the encoding of values:
- Strings may not contain the Linefeed character.
- Symbols must be either unqualified (without the package
  prefix), or keywords.
- Symbol names may not contain the Linefeed character.
- Symbol names must be written in all lowercase with the
  exception of NIL and T which are written all uppercase.
- Only proper lists are allowed.")

  (type event-server
    "Represents an event exchange server.

This server runs a TCP listener on a given port on the
localhost. The port defaults to 2424. Once started, the
server accepts an arbitrary amount of connections, over which
events can be issued. An event should follow the format as
specified in SERIALIZE-OBJECT, and should be followed by a
single Linefeed. For each event issued, the server will
respond with one or more objects. The connection is never
closed by the server except for when the underlying stream is
closed, or a TCP error occurs and the connection is doomed
anyway. The client is allowed to terminate the connection at
any time.

This class on its own does nothing. In order to start the
server listener, START must be called. STOP can be called
to end the listener and disconnect all clients. Once started,
PROCESS-CONNECTIONS must be called regularly in order to accept
new connections, handle incoming events, and to clean up old
connections that have disconnected.

See PORT
See LISTENER
See CONNECTIONS
See START
See STOP
See PROCESS-CONNECTIONS")

  (function port
    "The port on which the server listens for connections.

Defaults to 2424.

See EVENT-SERVER")

  (function listener
    "The server socket to listen for connections on.

This is NIL if the server is not running.

This is not thread-safe to modify.

See EVENT-SERVER")

  (function connections
    "The list of connected client sockets.

This is not thread-safe to modify.

See EVENT-SERVER")

  (function start
    "Start the event-server.

If it is already started, an error is signalled.

See EVENT-SERVER")

  (function stop
    "Stops the event-server.

If it is already stopped, nothing is done.

See EVENT-SERVER")

  (function process-connections
    "Processes the event-server connections.

This proceeds by listening for new connections and accepting
them if there are any. It then listens on each socket in
turn, parsing and processing pending events.
If a stream or socket error occurs, the connection is removed.
If another error occurs during parsing or processing, an error
is sent back to the client.

See PARSE-EVENT-OR-LOSE
See PROCESS-EVENT
See SERIALIZE-OBJECT
See EVENT-SERVER"))

;; generators.lisp
(docs:define-docs
  (type generator
    "This is the superclass for all block generators.

A generator is responsible for creating one or more blocks
that are emitted into the status bar.

A concrete class of a generator must supply a primary method
on GENERATE.

See GENERATE
See INTERVAL
See PROCESS-EVENT")

  (function generate
    "Generates a list of blocks to use in the status bar.

See BLOCK
See GENERATOR")

  (type single-generator
    "Superclass for all block generators that only generate a single block.

As a convenience, this generator also directly inherits from
PANGO-BLOCK in order to let the user and subclasses customise
the block's look and feel conveniently.

The primary method of a single-generator's GENERATE does not
need to return anything useful, as the generator itself is
always automatically returned from GENERATE in a list.

If no :name is provided on a single-generator instance, the name is
automatically set to the class-name of the instance's class.

See PANGO-BLOCK
See GENERATOR
See GENERATE")

  (type value-generator
    "Superclass for all block generators that generate their text content based on some computed value.

The TEXT and SHORT-TEXT of a value-generator should be format
strings that may consume however many values the respective
generator promises to supply.

Note that the pango markup for both text and short-text is
applied AFTER the format string has been formatted in order to
avoid encoding issues with entities in format directives.

A concrete class of a value-generator must supply a primary
method on COMPUTE-VALUE.

See COMPUTE-VALUE
See SINGLE-GENERATOR")

  (function compute-value
    "Computes the values used for the format string in a value-generator's text.

This function is called whenever GENERATE is called on the
value-generator.

See VALUE-GENERATOR"))

;; protocol.lisp
(docs:define-docs
  (function to-table
    "Turn the object into a table of basic types.

This is intended for json serialisation. Allowed types are:
- hash-table
- string
- list
- vector
- number
- T
- NIL")

  (function from-table
    "Generate an object from a table of basic types.

This is intended for json deserialisation. Allowed types are:
- hash-table
- string
- list
- vector
- number
- T
- NIL")
  
  (type header
    "Class representing the i3bar protocol header.

Instances of this class can be serialised using Jonathan.

See VERSION
See STOP-SIGNAL
See CONTINUE-SIGNAL
See SEND-CLICK-EVENTS-P")

  (function version
    "The version integer of the protocol that is used.

Defaults to 1.

See HEADER")

  (function stop-signal
    "The unix signal used to pause bar updates.

Defaults to 19 (SIGSTOP)

See HEADER")

  (function continue-signal
    "The unix signal used to continue bar updates.

Defaults to 18 (SIGCONT)

See HEADER")

  (function send-click-events-p
    "Whether i3 should send click updates to the process.

Defaults to NIL

See HEADER")

  (type block
    "Class representing a block in the i3 status bar.

A block is a textual container with some styling options.
See your i3 documentation for more information on blocks and their
behaviour.

Instances of this class can be serialised using Jonathan.

See TEXT
See SHORT-TEXT
See FOREGROUND
See BACKGROUND
See BORDER
See MIN-WIDTH
See ALIGN
See NAME
See INSTANCE
See URGENT-P
See SEPARATOR
See TEXT-FORMAT")

  (function text
    "Accessor to the full textual content of the block.

This field is required to be set.

See BLOCK")

  (function short-text
    "Accessor to the short, alternate text content of the block.

From i3docs:
Where appropriate, the short-text should also be provided. It
will be used in case the status line needs to be shortened
because it uses more space than your screen provides. For
example, when displaying an IPv6 address, the prefix is
usually (!) more relevant than the suffix, because the latter
stays constant when using autoconf, while the prefix changes.
When displaying the date, the time is more important than the
date (it is more likely that you know which day it is than
what time it is).

See BLOCK")

  (function foreground
    "Accessor to the foreground text colour of the block.

A colour can either be an encoded RGB integer, or a list of R, G, B,
and optional A elements with each element being an integer between
0 and 255.

See BLOCK")

  (function background
    "Accessor to the background colour of the block.

A colour can either be an encoded RGB integer, or a list of R, G, B,
and optional A elements with each element being an integer between
0 and 255.

See BLOCK")

  (function border
    "Accessor to the border colour of the block.

A colour can either be an encoded RGB integer, or a list of R, G, B,
and optional A elements with each element being an integer between
0 and 255.

See BLOCK")

  (function min-width
    "Accessor to the minimum pixel width of the block.

From i3docs:
The minimum width (in pixels) of the block. If the content of the
text key take less space than the specified min-width, the block
will be padded to the left and/or the right side, according to the
align key. This is useful when you want to prevent the whole
status line to shift when value take more or less space between each
iteration. The value can also be a string. In this case, the width
of the text given by min-width determines the minimum width of the
block. This is useful when you want to set a sensible minimum width
regardless of which font you are using, and at what particular size.

See BLOCK")

  (function align
    "Accessor to the text alignment property of the block.

Can be one of: :center :right :left
Defaults to :left

Note that alignment is only in effect if the min-width has not been
reached.

See BLOCK")

  (function name
    "Accessor to the name of the block.

From i3docs:
Every block should have a unique name (string) entry so that it can
be easily identified in scripts which process the output. i3bar
completely ignores the name and instance fields. Make sure to also
specify an instance (string) entry where appropriate. For example,
the user can have multiple disk space blocks for multiple mount
points.

See INSTANCE
See BLOCK
See CLICK")

  (function instance
    "Accessor to the instance name of the block.

See NAME
See BLOCK
See CLICK")

  (function urgent-p
    "Accessor to whether the block is considered urgent or not.

From i3docs:
A boolean which specifies whether the current value is urgent.
Examples are battery charge values below 1 percent or no more
available disk space (for non-root users). The presentation of
urgency is up to i3bar.

See BLOCK")

  (function separator
    "Accessor to the block's separator behaviour.

If set should be either T, or a number of pixels for the width of
the separator. If T, the default i3 behaviour is to use 9 pixels
of width for the separator.")

  (function text-format
    "Returns the text format used by the block's text content.

Defaults to :none

See BLOCK")
  
  (type pango-block
    "Represents a block whose text should be marked up with pango.

Note that neither TEXT nor SHORT-TEXT should contain raw pango
markup tags themselves, but rather you should use the MARKUP and
SHORT-MARKUP slots to set the markup regions to be used.

TEXT and SHORT-TEXT will automatically render the textual contents
with the appropriate markup regions.

Each entry in the MARKUP and SHORT-MARKUP lists should have the
following structure:

  (START END . MARKUP/MARKUP-OPTIONS)

Please see the pango-markup library for more information:

  https://shinmera.github.io/pango-markup

See MARKUP
See SHORT-MARKUP
See PANGO-MARKUP:MARKUP-REGIONS")

  (function markup
    "Accessor to the list of markup regions for the pango block's text.

See PANGO-BLOCK")

  (function short-markup
    "Accessor to the list of markup regions for the pango block's short-text.

See PANGO-BLOCK"))

;; status-bar.lisp
(docs:define-docs
  (type status-bar
    "Class to represent an i3 status bar.

An instance of this class is capable of handling the i3 protocol
and invoking a number of block generators at the appropriate times.
It also handles remote client connections through the event-server.

See INTERVAL
See NEXT-TIME
See GENERATORS
See OUTPUT
See INPUT
See CLICK-PAUSE
See PRODUCE-OUTPUT
See PROCESS
See RUN-BAR
See EVENT-SERVER")

  (function interval
    "Accessor to the interval in seconds in which output is generated

The default interval is one second for generators, and 0.1 seconds for
status-bars.

GENERATE is only called on generators if their own interval has
expired. They will not be called more frequently than the status-bar's
own interval however.

See NEXT-TIME
See STATUS-BAR
See GENERATOR")

  (function next-time
    "Accessor to the next time a tick event should be fired.

This is automatically updated when a tick event has fired or when a
click event has fired and click-pause is set.

The value is in internal-time-units.

See INTERVAL
See CLICK-PAUSE
See STATUS-BAR")

  (function generators
    "Accessor to the list of generators that the status-bar invokes to generate its output.

See GENERATOR
See STATUS-BAR")

  (function output
    "Accessor to the output stream to which the status-bar writes its results.

This defaults to *STANDARD-OUTPUT*

See PRODUCE-OUTPUT
See STATUS-BAR")

  (function input
    "Accessor to the input stream from which the status-bar reads click events.

This defaults to *STANDARD-INPUT*

See PROCESS
See STATUS-BAR")

  (function click-pause
    "Accessor to the number of seconds that the next tick event is delayed to when a click event has come in.

See PROCESS
See NEXT-TIME
See STATUS-BAR")

  (function produce-output
    "Outputs the given payload as JSON to the status-bar's output stream.

See OUTPUT
See STATUS-BAR")

  (function process
    "Processes the status-bar, causing it to potentially read inputs and generate outputs.

First the input is checked for a new click event. If one is there,
it is parsed from the stream and output is produced according to
GENERATE on the generators and the click event. If click-pause is
set, the next-time is delayed by click-pause number of seconds from
the current time.

Next the next-time is checked against the current internal real
time. If the time has passed, output is produced according to
GENERATE on the generators and a new tick event. The next-time is
then delayed by interval number of seconds from the current time.

See CLICK
See TICK
See NEXT-TIME
See CLICK-PAUSE
See INTERVAL
See GENERATOR
See GENERATORS
See STATUS-BAR")

  (function run-bar
    "Starts the i3bar protocol and continuously processes the status bar.

If click-events-p is T, i3 will send click events to the status bar.
Pause is the minimal number of seconds between calls to PROCESS.

See PROCESS
See STATUS-BAR"))

;; toolkit.lisp
(docs:define-docs)

;; toplevel.lisp
(docs:define-docs
  (function load-from-file
    "Loads a status bar definition from the given file.

The file should have the following format:

  DEFINITION ::= (INITARG|GENERATOR)*
  INITARG    ::= keyword value
  GENERATOR  ::= (NAME INITARG*)
  NAME       --- A symbol naming the class of the generator.

So for instance a simple file for a status bar with a two-second
interval, a clock generator, and a cpu-usage generator would look as
follows:

  :interval 2
  (clock :text \"~4@*~2d:~2,'0d:~2,'0d\")
  (cpu-usage)

See GENERATOR
See STATUS-BAR")

  (function run-bar-from-file
    "Runs a status bar initialised from a file.

See LOAD-FROM-FILE
See RUN-BAR")

  (function toplevel
    "Lisp binary toplevel entry function.

This simply calls RUN-BAR-FROM-FILE, passing along the command line
options to the binary."))
