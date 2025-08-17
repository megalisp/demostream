# DEMOSTREAM

> Visuallly "Interesting Enough" Stuff Drawn With The Great [Skething Project](https://github.com/soegaard/sketching).

Intending to use it for background imagery for my streams.


## REPL Commands

| Command                        | Description                                      |
|------------------------------- |--------------------------------------------------|
| `(demo!list)`                  | Show all available demos                         |
| `(demo!set 'name)`             | Switch to a specific demo by symbol              |
| `(demo!set 'auto)` or `(demo!set #f)` | Start auto shuffle mode and turn hold off   |
| `(demo!shuffle ...)`           | Manage the shuffle list (see code for options)   |
| `(demo!skip)`                  | Go forward in shuffle                            |
| `(demo!next)`                  | Alias for `demo!skip`                            |
| `(demo!prev)`                  | Go backward in shuffle                           |
| `(demo!hold)`                  | Toggle hold mode                                 |
| `(demo!hold #:on #t)`          | Set hold ON                                      |
| `(demo!hold #:on #f)`          | Set hold OFF                                     |
| `(demo!script (list ...))`     | Run a scripted sequence of demo actions          |

See `main.rkt` for more advanced usage and scripting examples.

### Things I Want
##### (To Add.)

> A 'props' (properties) system 
>> That I can live edit demos; given things I make available.


> Interaction / Integration to Observ.
>> ...


