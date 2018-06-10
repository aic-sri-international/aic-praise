package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm;

import java.util.List;

import com.sri.ai.expresso.api.Parser;

class HOGMParserErrorListener implements Parser.ErrorListener {
	
	HOGMQueryError.Scope context;
	List<HOGMQueryError> errors;
	
	HOGMParserErrorListener(HOGMQueryError.Scope context, List<HOGMQueryError> errors) {
		this.context = context;
		this.errors  = errors;
	}

	@Override
	public void parseError(Object offendingSymbol, int line, int charPositionInLine, String message, Exception exception) {
		HOGMLinePortion portion = new HOGMLinePortion(exception); 
		String messageWithLineInformation = addLineInformation(line, charPositionInLine, message);
		HOGMQueryError error = new HOGMQueryError(context, messageWithLineInformation, line, portion);
		errors.add(error);
	}

	private String addLineInformation(int line, int charPositionInLine, String message) {
		return "Error at line " + line + " column " + charPositionInLine + " - " + message;
	}
}