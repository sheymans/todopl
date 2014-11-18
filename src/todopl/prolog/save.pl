%    Copyright (C) 2012-2014 Stijn Heymans
%
%    This program is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.



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
