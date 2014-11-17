:- module( utils, [
		   convert_json_to_date/3,
		   convert_date_to_json/2,
		   increase_by/3,
		   earlier_date/2,
		   earlier_or_equal_date/2,
		   get_user_time/2,
		   earlier_than_now/2,
		   create_days_domain/3,
		   todopl_log/1] ).

:- use_module(library(clpfd)).


/** <module> Utilities

  This module provides some utility functions, independent of specific project data structures or methods. 
  
@author Stijn Heymans
@license This is proprietary software.
@copyright Heymans Technologies, 2012
*/

%% convert_json_to_date(+JSONTime,-DateTime) is det.
% Convert the GMT JSONTime to a local Prolog date object.
%
convert_json_to_date(JSONTime,Offset,DateTime) :-
    !,
    atom_codes(JSONTime,String), % we want to get rid of .000 in JSON times
    get_rid_of_decimal_in_time(String,NewString),
    atom_codes(NewJSON,NewString),
	parse_time(NewJSON,_Format,Stamp),stamp_date_time(Stamp,DateTime,Offset).


get_rid_of_decimal_in_time([],[]).
get_rid_of_decimal_in_time([46, 48, 48, 48, 90] , [90]) :- !.
get_rid_of_decimal_in_time([A|Rest],[A|Result]) :-
    get_rid_of_decimal_in_time(Rest,Result).



%% convert_date_to_json(+DateTime,-JSONTime) is det.
% 
% Convert a local time Prolog date object to JSON UTC time.
%
% @param DateTime A local Prolog date object, e.g., date(2012, 1, 4, 20, 58, 21.0, 28800, 'PST', false)
% @param JSONTime A JSON Time formatted string, e.g., '2012-01-05T04:58:21Z'.
convert_date_to_json(DateTime,JSONTime) :-
	% first create a time stamp to be able to transfer to UTC:
	date_time_stamp(DateTime, TimeStamp),
	% now convert the timestamp to UTC:
	stamp_date_time(TimeStamp,DateTimeUTC,'UTC'),
	format_time(atom(JSONTime), '%FT%TZ', DateTimeUTC, posix). 

%% earlier_date(+Date1,+Date2) is semidet.
%
% Determines whether Date1 is strictly earlier than Date2.
%
% @param Date1 A Prolog date.
% @param Date2 A Prolog date.
earlier_date( Date1, Date2 ) :-
	date_time_stamp(Date1,T1),
	date_time_stamp(Date2,T2),
	T1 < T2.

%% earlier_or_equal_date(+Date1,+Date2) is semidet.
%
% Determines whether Date1 is earlier or equal than Date2.
%
% @param Date1 A Prolog date.
% @param Date2 A Prolog date.
earlier_or_equal_date( Date1, Date2 ) :-
	date_time_stamp(Date1,T1),
	date_time_stamp(Date2,T2),
	T1 =< T2.

%% earlier_than_now(+Date) is semidet.
%
% Determine whether a Date is earlier than the current time.
%
% @param Date A Prolog date.
earlier_than_now(Offset,Date) :-
	get_user_time(Offset,Now),
	earlier_date(Date,Now).

%% get_local_time(-Date) is det.
%
% Get the current local time.
%
% @param Date Unbound variable that will contain the current time in Prolog date format.
get_user_time(Offset,Date) :-
	get_time(Timestamp),
	stamp_date_time(Timestamp,Date,Offset).

%% create_days_domain(+Due,-MaxDays) is det.
%
% Will calculate in how many days the Due date is. For example, MaxDays equal to 0 is today, to 1 is tomorrow.
%
% @param Due The Prolog Date for which you want to you now in how many days it is.
% @param MaxDays Indicates in how many days the Due date will be.
create_days_domain( Due, Offset, MaxDays ) :-
	MaxDays in 0..366,
	label([MaxDays]),
	get_user_time(Offset,Now),
	MaxHours is MaxDays*24,
	increase_by(Now,MaxHours,date(Y,M,D,_,_,_,_,_,_)),
	Due = date(Y,M,D,_,_,_,_,_,_).

%% increase_by(+Date,+I,-R) is det.
%
% Increase Date by I, resulting in R. I is a real number indicating
% hours. E.g., 2.25 is 2 hours and 15 minutes. R is a Prolog date
% structure.
%
increase_by(date(Y,M,D,H,Mi,S,Offset,TimeZone,Z),I,R) :-
	% transform I to minutes:
	Imins is I*60,
	NewMi is Mi + Imins,
	date_time_stamp(date(Y,M,D,H,NewMi,S,Offset,TimeZone,Z),Stamp),
	stamp_date_time(Stamp,R,Offset).

todopl_log(S) :-
	open('logfile',append,Stream),
	portray_clause(Stream,S),
	close(Stream).
