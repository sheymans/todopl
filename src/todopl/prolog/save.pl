:- ['scheduling_server'].

%% ONLY USE THIS TO SAVE AN EXE (On mac os X works without that stupid
%sleep_indefinitely)

% sleep till prolog gets killed. We need this to ensure the stuff does not just
% gets killed all.
%
sleep_indefinitely :-
    !, % cut to make sure memory consumption stays stable
    sleep(30000), %% to be decided how long this sleep should be
    sleep_indefinitely.

main :-
        pce_main_loop(main).

main(_Argv) :-
        todopl(5000), 
        sleep_indefinitely.

save(Exe) :-
    pce_autoload_all,
    pce_autoload_all,
    qsave_program(Exe, [stand_alone(true), goal(main), emulator(swi('bin/xpce-stub.exe'))]).
