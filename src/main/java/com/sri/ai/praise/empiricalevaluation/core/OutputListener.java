package com.sri.ai.praise.empiricalevaluation.core;

public interface OutputListener {
	void notification(String notification);
	void notificationException(Exception ex);
	void csvResultOutput(String csvLine);
}