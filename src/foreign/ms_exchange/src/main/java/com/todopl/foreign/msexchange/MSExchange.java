package com.todopl.foreign.msexchange;


import java.util.Date;
import java.util.TimeZone;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.commons.lang3.tuple.Pair;

import microsoft.exchange.webservices.data.Appointment;
import microsoft.exchange.webservices.data.AppointmentSchema;
import microsoft.exchange.webservices.data.CalendarFolder;
import microsoft.exchange.webservices.data.CalendarView;
import microsoft.exchange.webservices.data.EWSHttpException;
import microsoft.exchange.webservices.data.ExchangeCredentials;
import microsoft.exchange.webservices.data.ExchangeService;
import microsoft.exchange.webservices.data.ExchangeVersion;
import microsoft.exchange.webservices.data.FindItemsResults;
import microsoft.exchange.webservices.data.PropertySet;
import microsoft.exchange.webservices.data.WebCredentials;
import microsoft.exchange.webservices.data.WellKnownFolderName;


public class MSExchange {
	
	
	public static ExchangeService getVersionedService(ExchangeVersion version, String username, String passwd, String email) throws Exception {

		ExchangeService service = new ExchangeService(version, TimeZone.getTimeZone("UTC"));
		ExchangeCredentials creds = new WebCredentials(username, passwd);
		service.setCredentials(creds);
		service.autodiscoverUrl(email);
		
		return service;
	}

	public static Pair<String, FindItemsResults<Appointment>> getEventsForService(ExchangeService service, Date from, Date to, String username, String passwd, String email) throws Exception {
		
		CalendarFolder calendar = CalendarFolder.bind(service, WellKnownFolderName.Calendar, new PropertySet());
		CalendarView cView = new CalendarView(from, to);
	
		cView.setPropertySet(new PropertySet(AppointmentSchema.Subject, AppointmentSchema.Start, AppointmentSchema.End, AppointmentSchema.Location));
		
		FindItemsResults<Appointment> appointments = calendar.findAppointments(cView);
		
		Pair<String, FindItemsResults<Appointment>> result = Pair.of(service.getRequestedServerVersion().toString(), appointments);
				
		return result;
	}
	
	public static Pair<String, FindItemsResults<Appointment>> getEvents(String version, Date from, Date to, String username, String passwd, String email) throws Exception {

		// always call this from clojure in a try block
		
		ExchangeService service = null;
		Pair<String, FindItemsResults<Appointment>> result;

		if (version != null) {
			try {
				ExchangeVersion exversion = ExchangeVersion.valueOf(version);
				service = getVersionedService(exversion, username, passwd, email);
				result = getEventsForService(service, from, to, username, passwd, email);
				return result;
			}
			catch (Exception e) {
				System.out.println("Saved version " + version + " is not succeeding. Going to try other versions before giving up.");
			}
		}
		
		ExchangeVersion[] versions = ExchangeVersion.values();
		ArrayUtils.reverse(versions);
			
		for (ExchangeVersion v : versions) {
			try {
				System.out.println("Exchange service is trying " + v.toString());
				service = getVersionedService(v, username, passwd, email);
				result = getEventsForService(service, from, to, username, passwd, email);
				System.out.println("Exchange service SUCCES for" + v.toString());
				return result;
			}
			catch (Exception e) {
				System.out.println(v.toString() + " failed; Trying next version");
			}
		}
		// Nothing worked; damnit
		throw new EWSHttpException("EWS failed (with all server versions) to satisfy your request.");
	}
	
	
	public static void main(String[] args) {
        
		try {
			Date from = new Date();
			Date to = DateUtils.addDays(from, 5);
			
			System.out.println("Appointments from today " + from.toString());
			
			Pair<String, FindItemsResults<Appointment>> result = getEvents(null, from, to, "USER", "PASS", "EMAIL");
			
			System.out.println("This is your version: " + result.getLeft());
			
			System.out.println("These are your appointments:");
			System.out.println();
			
			for  (Appointment a : result.getRight().getItems()) {
			
				System.out.println("Subject==== " + a.getSubject());
				System.out.println("From======= " + a.getStart());
				System.out.println("To========= " + a.getEnd());
				System.out.println("Id========= " + a.getId());
				System.out.println();	
			}
			
			System.out.println("Now with the version filled in:");
			Pair<String, FindItemsResults<Appointment>> result2 = getEvents(result.getLeft(), from, to, "USER", "PASSWD", "EMAIL");
			System.out.println("This is your version: " + result2.getLeft().toString());
			
			System.out.println("These are your appointments:");
			System.out.println();
			
			for  (Appointment a : result2.getRight().getItems()) {
				System.out.println("Subject==== " + a.getSubject());
				System.out.println("From======= " + a.getStart());
				System.out.println("To========= " + a.getEnd());
				System.out.println();	
			}
			
		}
        catch (Exception e) {
        	e.printStackTrace();
        }
    }
	
}
