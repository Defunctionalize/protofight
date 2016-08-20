Hey, you wonderful Arcadian folks!

This is my project at the point when I began scale testing it with multiple entities and realized I needed
to find a native java solution :(

To run my project, place the contents of the repo in your assets folder AND

1. create an empty master game object and add an awake hook that points to #'core/in-the-beginning

2. create the following input axes in your project:
submit
cancel
fire
horizontal
vertical
dash
log-debug

That should be all you need.  To observe the memory problem, enter play mode.  once play mode begins:

submit: initializes the game state with a player entity, and creates the player game object that
        is constantly updated to be in the position specified by the game state

cancel: disposes the game state and all the game objects on screen.  this prevents you from
        having to reload play mode to escape a broken state

horizontal/vertical/dash: causes the player sprite in the game state to change position.
                          no effect on memory

fire:  creates bullet entities in the game state only (does not create a corresponding game-object,
       i never got around to that).  continue pressing this button to see memory tank and garbage
       collection start getting well out of control

log-debug: consoles the 3 different "views" of the current state.
           instant(aneous) view: the values of all attributes of all entities at the current time
           input view: the list of all the events (or system inputs) that are required to build
                       the instant state from scratch
           accretive view: a list of all of the games "facts", that is, temporal values that show
                           when something changed in the state.  I use this for traversing time