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


:- module( schedule, [
			     create_schedule/3
			    ] ).

/** <module> Creating Schedules

This module provides the main entry point (create_schedule/2) for creating a list of Tasks out of a list of Events, or, in other words, for creating a schedule.
  
@author Stijn Heymans
**/

:- use_module(library(clpfd)).
:- use_module(utils).

start_time_work(Preferences, Hour,Quarter) :-
    member(start_time_work(Hour,Quarter),Preferences).

end_time_work(Preferences, Hour, Quarter) :-
    member(end_time_work(Hour,Quarter), Preferences).

quarters_domain(3).


%% no_work_day(+Integer) is det.
%
% The days on which no work can be scheduled (1 is Monday, 6 is Saturday, 7 is Sunday)
no_work_day(Preferences,N) :-
    member(no_work_day(N),Preferences).

%% create_schedule(+Events,-Results) is semidet.
%
% Create a schedule out of events.
%
% @param Events The events to be scheduled (note that dates in events are JSON dates).
% @param Results A list of =|task(Start,End,Id)|= items that represent a schedule.
create_schedule([],_,[]) :- !.
create_schedule( Events, Preferences, Results ) :-
	% create same list but not with JSON time but Prolog Dates
	create_prolog_dates( Events, PrologEvents),
    rearrange_events_beforeafter(PrologEvents,[],PrologEventsSorted),
    break_up_events(PrologEventsSorted,SubEvents),
    move_meetings_for_priority_scheduling(SubEvents,SubEventsReady),
    schedule_subevents(SubEventsReady,Preferences,[],Res), % sorting according to after/before
    combine_tasks(Res,Tasks),
    create_json_dates(Tasks,Results),!.

% Final catch_all for create_schedule (so actually, when it did not succeed to
% schedule).
create_schedule(_,_,_) :-
    throw(no_schedule_found).

%%%
%%% Convert JSON dates in events to Prolog dates in events (in LOCAL time
%according to offset)
%%%
create_prolog_dates([],[]).
create_prolog_dates( [ event(Id,Due,Offset,'ToDo',EMFC,UWM, Before, After) | Events] , [ event(Id,Due2,Offset,'ToDo',EMFC,UWM, Before, After) |PrologEvents] ) :-
	convert_json_to_date(Due,Offset,Due2),
	create_prolog_dates(Events,PrologEvents).

create_prolog_dates( [ event(Id,From, Offset,'Meeting',To,0, _, _) | Events] , [ event(Id,From2,Offset,'Meeting',To2,0, '', '') |PrologEvents] ) :-
	convert_json_to_date(From,Offset,From2),
	convert_json_to_date(To,Offset,To2),
	create_prolog_dates(Events,PrologEvents).

%%%
%%% Convert Prolog Dates in Tasks to JSON Dates in Tasks.
%%%
create_json_dates([],[]).
create_json_dates( [task(Start,End,Type,Offset,TaskId,_,_) | Tasks] , [task(Start2,End2,Type,Offset,TaskId) |PrologTasks] ) :-
	convert_date_to_json(Start,Start2),
	convert_date_to_json(End,End2),
	create_json_dates(Tasks,PrologTasks).

%%%
%%% Breaking up events into subevents
%%%
% event(Id,Due,Offset,'ToDo',EMFC,UWM, Before, After)

% when EMFC is 0, you cannot break up the event.
break_up_event(event(_,_,_,'ToDo',0,_,_,_), []).

% when EMFC is smaller than UWM (undisturbed working minutes), there is nothing to break up anymore.
break_up_event(event(_,_,_,'ToDo',EMFC,UWM,_,_), []) :-
	EMFC < UWM,!. 

break_up_event(event(Id,Due,Offset,'ToDo',EMFC,UWM, Before, After) , [subevent(Id,Due,Offset,'ToDo',EMFC,Before,After)]) :-
	EMFC >= UWM,
	NewEMFC is EMFC - UWM,
	NewEMFC < UWM,!. % in next step there would not be enough space to satisfy the UWH.

break_up_event(event(Id,Due,Offset,'ToDo',EMFC,UWM, Before, After) , [subevent(Id,Due,Offset,'ToDo',UWM,Before,After) | Results]) :-
	EMFC >= UWM,
	NewEMFC is EMFC - UWM,!,
	break_up_event(event(Id,Due,Offset,'ToDo', NewEMFC, UWM, Before, After),Results).


% Meeings do not get broken up.
break_up_event(event(Id,Due,Offset,'Meeting',EMFC,_,Before,After), [subevent(Id,Due,Offset,'Meeting',EMFC,Before,After)]).

% Break up multiple events
break_up_events( [], []).
break_up_events( [E | Events], Results ) :-
	break_up_event(E,R),
	break_up_events(Events,Res),
	append(R,Res,Results).



%%%
%%% Moving meetings to front for priority scheduling
%%%


move_meetings_for_priority_scheduling(Events,NewEvents) :-
	move_meetings_for_priority_scheduling1(Events,[],Meetings,[],ToDos),
    reverse(ToDos,RToDos), % we already ordered the ToDos (in mergesort; did we or was this coincidence?), but breaking them up like below will reverse the order.
	append(Meetings,RToDos,NewEvents).

move_meetings_for_priority_scheduling1([],AkkuMeetings,AkkuMeetings,AkkuToDos,AkkuToDos).
move_meetings_for_priority_scheduling1([subevent(Id,Due,Offset,'Meeting',TimeNeeded,Before,After) | Rest ],AkkuMeetings,Meetings,AkkuToDos, ToDos) :-
	move_meetings_for_priority_scheduling1(Rest,[subevent(Id,Due,Offset,'Meeting',TimeNeeded,Before,After) | AkkuMeetings],Meetings,AkkuToDos,ToDos).

move_meetings_for_priority_scheduling1([ subevent(Id,Due,Offset,'ToDo',TimeNeeded,Before,After)| Rest ],AkkuMeetings,Meetings,AkkuToDos, ToDos) :-
	move_meetings_for_priority_scheduling1(Rest,AkkuMeetings,Meetings,[ subevent(Id,Due,Offset,'ToDo',TimeNeeded,Before,After) | AkkuToDos],ToDos).


%%%
%%% Scheduling subevents into tasks.
%%% subevent(Id,Due,Offset,'ToDo',EMFC,Before,After)
%%%


schedule_subevents([],_,R,R).

schedule_subevents([subevent(Id,From,Offset,'Meeting',To,_,_) | Rest ], Preferences, AccumulatedTasks, Result ) :-
	schedule_subevents(Rest, Preferences, [task(From,To,'Meeting',Offset,Id,'','') | AccumulatedTasks], Result).

% Now schedule Todo subevents (backtrack when constraints are not satisfied).

schedule_subevents([subevent(Id,Due,Offset,'ToDo',TimeNeeded,Before, After) | Rest], Preferences, AccumulatedTasks, Result) :-

    % Basic domains for the Task: TaskDay, TaskStartHour, TaskEndHour,
    % TaskStartQuarter, TaskEndQuarter
    start_time_work(Preferences, PrefStartHour, PrefStartQuarter),
    end_time_work(Preferences, PrefEndHour, PrefEndQuarter),

    TaskStartHour in PrefStartHour..PrefEndHour,
    TaskEndHour in PrefStartHour..PrefEndHour,

    % How the quarters range (from 0..3)
	quarters_domain(QD),
	TaskStartQuarter in 0..QD,
	TaskEndQuarter in 0..QD,


    % get the valid boundaries for the domain for the Duedate of the task:
	domain_maxes_for_date(Due,Offset, EndDayDomain,EndHourDomain,EndQuarterDomain),
    
    % always start scheduling from Now
    get_user_time(Offset,Now),
	domain_maxes_for_date(Now,Offset, StartDayDomain,StartHourDomain,StartQuarterDomain),

	domain_time_needed(TimeNeeded,HoursTimeNeeded,QuartersTimeNeeded),

	% determine the allowed days:
	TaskDay in StartDayDomain .. EndDayDomain, % every task is on only one day


    /*
* (4) timeneeded constraints
*
* */
	
    % Constraints:
    %
	
    % (1) Obey the preferences

    (TaskStartHour #= PrefStartHour #==> TaskStartQuarter #>= PrefStartQuarter),
    (TaskEndHour #= PrefEndHour #==> TaskEndQuarter #=< PrefEndQuarter),

    % (2) Obey the due date
	(TaskDay #= EndDayDomain #==> TaskEndHour #=< EndHourDomain), 
	(TaskDay #= EndDayDomain #/\ TaskEndHour #= EndHourDomain #==> TaskEndQuarter #=< EndQuarterDomain), 

    % (3) Obey the start date (now)
	(TaskDay #= StartDayDomain #==> TaskStartHour #>= StartHourDomain),
	(TaskDay #= StartDayDomain #/\ TaskStartHour #= StartHourDomain #==> TaskStartQuarter #>= StartQuarterDomain), 

    % (4) Obey the interval time for a task
	(TaskStartQuarter + QuartersTimeNeeded #> QD #==>	TaskEndQuarter #= (TaskStartQuarter + QuartersTimeNeeded) rem 4),
	(TaskStartQuarter + QuartersTimeNeeded #> QD #==>	TaskEndHour #= TaskStartHour + HoursTimeNeeded + 1),
	% otherwise:
	(TaskStartQuarter + QuartersTimeNeeded #=< QD #==> TaskEndQuarter #= TaskStartQuarter + QuartersTimeNeeded),
	(TaskStartQuarter + QuartersTimeNeeded #=< QD #==> TaskEndHour #= TaskStartHour + HoursTimeNeeded),
	

    % End of constraints

	label([TaskDay,TaskStartHour,TaskEndHour,TaskStartQuarter,TaskEndQuarter]),

	true_date_for_domain_instantiations(TaskDay,TaskStartHour,TaskStartQuarter,Offset,Start),
	true_date_for_domain_instantiations(TaskDay,TaskEndHour,TaskEndQuarter,Offset,End),

	allowed_working_day(Preferences,Start),

	\+ overlapping_tasks(task(Start,End,'ToDo',Offset,Id,Before,After),AccumulatedTasks),

   	\+ before_after_violated(task(Start,End,'ToDo',Offset,Id,Before,After),AccumulatedTasks),


	schedule_subevents(Rest, Preferences, [task(Start,End,'ToDo',Offset,Id, Before, After) | AccumulatedTasks],Result).




before_after_violated(task(_,_,_,_,_,_,_), []) :- !,fail.

before_after_violated(task(Start,End,Type,Offset,Id,Before,After), [task(S2,E2,Type2,O2,Id2,B2,A2) | _]) :-
    Before = Id2, % the current task needs to come before Id2 task
    \+ earlier_task(task(Start,End,Type,Offset,Id,Before,After), task(S2,E2,Type2,O2,Id2,B2,A2)),!.

before_after_violated(task(Start,End,Type,Offset,Id,Before,After), [task(S2,E2,Type2,O2,Id2,B2,A2) | _]) :-
    After = Id2, % the current task needs to come after Id2 task
    \+ earlier_task(task(S2,E2,Type2,O2,Id2,B2,A2), task(Start,End,Type,Offset,Id,Before,After)),!.

before_after_violated(task(Start,End,Type,Offset,Id,Before,After), [task(S2,E2,Type2,O2,Id2,B2,A2) | _]) :-
    Id = A2,
    earlier_task(task(S2,E2,Type2,O2,Id2,B2,A2), task(Start,End,Type,Offset,Id,Before,After)).


before_after_violated(T, [_ | Rest]) :-
    before_after_violated(T,Rest).


domain_maxes_for_date(Due,Offset,Days,Hours,Quarter) :-
	\+ earlier_than_now(Offset,Due),!,
	create_days_domain(Due,Offset,Days),
	Due = date(_,_,_,Hours,Mi,_,_,_,_),
	get_right_quarter(Mi,Quarter).

domain_maxes_for_date(_,Offset,Days,Hours,Quarter) :-
	get_user_time(Offset,Now),
	create_days_domain(Now,Offset,Days),
	Now = date(_,_,_,Hours,Mi,_,_,_,_),
	get_right_quarter(Mi,Quarter).

domain_time_needed(TimeNeeded,Hours,Quarters) :-
	% Timeneeded is minutes, we are going to round that to hours, minutes
    TimeNeeded2 is TimeNeeded / 60,
	Hours is floor(TimeNeeded2),
	AfterComma is TimeNeeded2 - floor(TimeNeeded2),
	Quarters is floor(AfterComma/0.25).
	

get_right_quarter(Mins,0) :-
	Mins < 15,!.

get_right_quarter(Mins,1) :-
	Mins < 30,!.

get_right_quarter(Mins,2) :-
	Mins < 45,!.

get_right_quarter(_,3).

% Calculate what Date it is InDays AtHour, and AtQuarter
true_date_for_domain_instantiations(InDays, AtHour, AtQuarter, Offset, Date) :-
	get_user_time(Offset, date(Y, M, D, _, _, _, X, TimeZone, Z)),
	NewDays is D+InDays,
	NewMinutes is AtQuarter*15,
	date_time_stamp(date(Y,M,NewDays,AtHour,NewMinutes,0,X,TimeZone,Z),Stamp),
	stamp_date_time(Stamp,Date,Offset).

% check whether the day is an allowed working day.
allowed_working_day(Preferences,date(Year,Month,Day, _, _, _, _, _, _)) :-
	day_of_the_week(date(Year,Month,Day),DayOfWeek),
	\+ no_work_day(Preferences,DayOfWeek),
	\+ no_work_day(Preferences,DayOfWeek).
	

overlapping_two_tasks(Task1,Task2) :-
	Task1 = task(Start1,End1,_,_,_,_,_),
	Task2 = task(Start2,End2,_,_,_,_,_),
	earlier_date(Start2,End1), earlier_date(Start1,End2),!.



overlapping_tasks(_,[]) :- fail.

overlapping_tasks(Task,[T|_]) :-
	overlapping_two_tasks(Task,T),!.

overlapping_tasks(Task,[_|R]) :-
	overlapping_tasks(Task,R).
	


%%%
%%% Combine adjacent tasks into bigger tasks.
%%%

% Let's combine the tasks with the same ID if they are on adjacent slots.
combine_tasks(Tasks,CombinedTasks) :-
	mergesort(Tasks,STasks),!,
	combine_tasks_for_sorted(STasks,CombinedTasks).

combine_tasks_for_sorted([],[]).
combine_tasks_for_sorted([task(S,E,Type,Offset,Id,Before,After)], [task(S,E,Type,Offset,Id,Before,After)]).

combine_tasks_for_sorted([task(S,E,Type,Offset,Id,Before,After),task(E,E2,Type,Offset,Id,Before,After) | R], Results) :-
	!,combine_tasks_for_sorted([task(S,E2,Type,Offset,Id,Before,After) | R], Results).

combine_tasks_for_sorted([task(S,E,Type,Offset,Id,Before,After) | R], [task(S,E,Type,Offset,Id,Before,After) | Results]) :-
	combine_tasks_for_sorted(R,Results).




% Sorting according to After/Before information:
mergesort_afterbefore([], []) :- !.
mergesort_afterbefore([A], [A]):- !.
mergesort_afterbefore([A, B | Rest], S) :-
  divide([A, B | Rest], L1, L2),
  mergesort_afterbefore(L1, S1),
  mergesort_afterbefore(L2, S2),
  my_merge_afterbefore(S1, S2, S).

my_merge_afterbefore(A, [], A) :- !.
my_merge_afterbefore([], B, B) :- !.
my_merge_afterbefore([A | Ra], [B | Rb], [A | M]) :-
  should_be_before(A,B),!,
  my_merge_afterbefore(Ra, [B | Rb], M).
my_merge_afterbefore([A | Ra], [B | Rb], [B | M]) :-
	\+ should_be_before(A,B),!,
  my_merge_afterbefore([A | Ra], Rb, M).

% event(Id,Due,Offset,'ToDo',EMFC,UWM, Before, After)
should_be_before(event(_,_,_,_,_,_,Before,_),event(Id2,_,_,_,_,_,_,_)) :-
    Before = Id2,!.

should_be_before(event(Id,_,_,_,_,_,_,_),event(_,_,_,_,_,_,_,A2)) :-
    Id = A2,!.



% rearrange event list to obey before/after information:

rearrange_events_beforeafter([],Result,R) :-
    reverse(Result,R).

rearrange_events_beforeafter(Events,Akku,Result) :-
    member(Event,Events),
    \+ should_be_before_any(Event,Akku),
    delete(Events,Event,Removed),
    rearrange_events_beforeafter(Removed,[Event|Akku],Result).

should_be_before_any(Event,Events) :-
    member(Event2,Events),
    should_be_before(Event,Event2).




% Sorting the Tasks with a Merge Sort:

mergesort([], []) :- !.
mergesort([A], [A]):- !.
mergesort([A, B | Rest], S) :-
  divide([A, B | Rest], L1, L2),
  mergesort(L1, S1),
  mergesort(L2, S2),
  my_merge(S1, S2, S).

divide([], [], []).
divide([A], [A], []).
divide([A, B | R], [A | Ra], [B | Rb]) :-  !,divide(R, Ra, Rb).

my_merge(A, [], A) :- !.
my_merge([], B, B) :- !.
my_merge([A | Ra], [B | Rb], [A | M]) :-
  earlier_task(A,B),!,
  my_merge(Ra, [B | Rb], M).
my_merge([A | Ra], [B | Rb], [B | M]) :-
	\+ earlier_task(A,B),!,
  my_merge([A | Ra], Rb, M).

earlier_task(task(Start1,_,_,_,_,_,_),task(Start2,_,_,_,_,_,_)) :-
	earlier_date(Start1,Start2).

