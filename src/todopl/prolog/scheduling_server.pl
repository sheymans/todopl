
:- module( scheduling_server, [
			   todopl/1
			   ] ).

/** <module> Todopl: Prolog Back-End Server

This module provides the main entry point for starting the Prolog server via the command
  ==
  :- todopl(5000).
  ==

The server will listen on port 5000 for JSON HTTP requests.

@author Stijn Heymans
@license This is proprietary software.
@copyright Heymans Technologies, 2012

*/

:- set_prolog_flag(readline,false).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
%:- use_module(library(http/html_write)).
%:- use_module(library(http/http_parameters)).
%:- use_module(library(http/html_head)).
:- use_module(library(http/http_json)).
%:- use_module(library(http/json_convert)).
%:- use_module(library(http/http_client)). % for http_get
%:- use_module(library(http/json)).


% Main module for creating the schedule:
:- use_module(schedule).
% Utility functions:
:- use_module(utils).

:- http_handler(root(prolog_schedule), prolog_schedule, []).



%% todopl(+Port) is det.
%
% True when Prolog Server is successfully started.
%
% @param Port The Port number on which the server is running; the Web App assumes this is started with 5000.
todopl(Port) :-
	http_server(http_dispatch, [port(Port)]).



%% json_events(+Request) is det.
%
% The main handler for receiving JSON POSTS. The Web App sends a JSON POST to this handler.
% The handler converts the JSON to Prolog terms, creates a schedule for those Prolog terms (events) and converts the resulting tasks back to valid JSON.
%
% @param Request The incoming Request; handled by http_read_json/2.
prolog_schedule(Request) :-
    http_read_json(Request, json([json=json([tasks=JSONInEvents,preferences=json(JSON_Preferences), meetings=JSONInMeetings])]),[]), % the extra null parameter ensure that null objects get written as earlier tasks with 0.
    convert_jsontasks_prolog(JSONInEvents, Events),
    convert_jsonprefs_prolog(JSON_Preferences,Preferences),
    convert_jsonmeetings_prolog(JSONInMeetings,Meetings),
    %add_hours_domain(Prefs,Preferences),
    append(Meetings, Events, CombinedEvents),
    catch(  call_with_time_limit(15, create_schedule(CombinedEvents,Preferences,Tasks)),
            Exception,
            process_exception(Exception,Tasks)),
    convert_prolog_scheduledtasks(Tasks,JSONOut),
    reply_json(json([json=JSONOut])).


process_exception(time_limit_exceeded,time_limit_exceeded) :- !.
process_exception(no_schedule_found,no_schedule_found) :- !.
process_exception(_,unknown_exception).


convert_jsontasks_prolog([],[]).
convert_jsontasks_prolog( [json(J) | JSONArray] , [ event(Id,Due,Offset,'ToDo',EMFC,UWM,'',After) | Result] ) :-
	member(id=Id,J),
	member(duedate=Due,J),
	member(offset=TempOffset,J),
    Offset is TempOffset * 60 * (-1), % Prolog's time handling deals with an offset in seconds and positive when west
	member(emfc=EMFC,J),
	member(uwm=UWM,J),
    (member(after=[After,_],J) ; member(after=After,J)),
	convert_jsontasks_prolog(JSONArray,Result).

% Note that EMFC is here just the To date, we differentiate because of the
% 'Meeting' type.
convert_jsonmeetings_prolog([],[]).
convert_jsonmeetings_prolog( [json(J) | JSONArray] , [ event(Id,From,Offset,'Meeting',To,0, '', '') | Result] ) :-
	member(id=Id,J),
	member(from=From,J),
	member(to=To,J),
	member(offset=TempOffset,J),
    Offset is TempOffset * -60, % Prolog's time handling deals with an offset in seconds
	convert_jsonmeetings_prolog(JSONArray,Result).



convert_jsonprefs_prolog([],[]).

convert_jsonprefs_prolog([mon= @(true) |Rest],[no_work_day(1) | R]) :-
    !,
    convert_jsonprefs_prolog(Rest,R).

convert_jsonprefs_prolog([tue= @(true) |Rest],[no_work_day(2) | R]) :-
    !,
    convert_jsonprefs_prolog(Rest,R).

convert_jsonprefs_prolog([wed= @(true) |Rest],[no_work_day(3) | R]) :-
    !,
    convert_jsonprefs_prolog(Rest,R).

convert_jsonprefs_prolog([thu= @(true) |Rest],[no_work_day(4) | R]) :-
    !,
    convert_jsonprefs_prolog(Rest,R).

convert_jsonprefs_prolog([fri= @(true) |Rest],[no_work_day(5) | R]) :-
    !,
    convert_jsonprefs_prolog(Rest,R).

convert_jsonprefs_prolog([sat= @(true) |Rest],[no_work_day(6) | R]) :-
    !,
    convert_jsonprefs_prolog(Rest,R).

convert_jsonprefs_prolog([sun= @(true) |Rest],[no_work_day(7) | R]) :-
    !,
    convert_jsonprefs_prolog(Rest,R).

convert_jsonprefs_prolog([endday=E |Rest],[end_time_work(E2,E3) | R]) :-
    !,
    convert_enteredhour_to_number(E,E2,E3),
    convert_jsonprefs_prolog(Rest,R).

convert_jsonprefs_prolog([startday=S |Rest],[start_time_work(S2,S3) | R]) :-
    !,
    convert_enteredhour_to_number(S,S2,S3),
    convert_jsonprefs_prolog(Rest,R).

convert_jsonprefs_prolog([_|Rest],R) :-
    !,
    convert_jsonprefs_prolog(Rest,R).

add_hours_domain(Preferences,[hours_domain(H) | Preferences]) :-
    member(start_time_work(S,_Q1),Preferences),
    member(end_time_work(E,_Q2),Preferences),
    H is E - S - 1.

% 10:15 is Hours=10 and Quarter=1, 10:45 is Quarter=3
convert_enteredhour_to_number(TimeString, Hours, Quarter) :-
    atom_chars(TimeString,[X1,X2,':',X3,X4]),
    number_chars(Hours,[X1,X2]),
    number_chars(Minutes,[X3,X4]),
    convert_minutes_to_quarter(Minutes,Quarter).

convert_minutes_to_quarter(0,0) :- !.
convert_minutes_to_quarter(15,1) :- !.
convert_minutes_to_quarter(30,2) :- !.
convert_minutes_to_quarter(45,3) :- !.



%% convert_prolog_scheduledtasks(+Tasks,-JSONTasks) is det.
%
% Convert a list of tasks of the form =|task(StartDate,EndDate,Id)|= back to JSON format for processing by the Web App, where
% * =|StartDate|= is the start date (and hour, minutes) of the task
% * =|EndDate|= is the end date (and hour, minutes) of the task
% * =|Id|= is the task Id.
%
convert_prolog_scheduledtasks([],[]) :- !.
convert_prolog_scheduledtasks([ task(S,E,Type,O,I) | Rest], [ json([from=S,to=E,type=Type,item_id=I,offset=O2]) | Result]) :-
    !,
    O2 is O / (-60), % get the offset back from seconds to minutes
	convert_prolog_scheduledtasks(Rest,Result).

% convert exceptions if above does not contain tasks
convert_prolog_scheduledtasks(Exception,Exception).
