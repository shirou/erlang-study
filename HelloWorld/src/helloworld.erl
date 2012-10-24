-module(helloworld).
-export([main/1, loop/0]). 
-include_lib("eunit/include/eunit.hrl").

main( _ ) -> 
    Pid = spawn(helloworld, loop, []),
    Pid ! hello_world.

loop() -> 
    receive 
        helloworld -> 
		?debugVal("debug"),
             io:format("Hello, World!~n"), 
             loop()
    end.
