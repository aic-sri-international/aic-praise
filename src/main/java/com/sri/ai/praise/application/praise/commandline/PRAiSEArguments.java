package com.sri.ai.praise.application.praise.commandline;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.lang.ModelLanguage;

class PRAiSEArguments implements AutoCloseable {
	List<File>    inputFiles      = new ArrayList<>(); // non option arguments (at least 1 required)
	ModelLanguage inputLanguage   = null;              // --language (optional - 0 or 1)
	List<String>  globalQueries   = new ArrayList<>(); // --query    (optional - 0 or more)
	PrintStream   out             = System.out;        // --output   (optional - 0 or 1)
	
	@Override
	public void close() throws IOException {
		out.flush();
		// Only close if not System.out
		if (out != System.out) {				
			out.close();
		}
	}
}