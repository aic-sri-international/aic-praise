/*
 * RuleTokenMaker.java - Token maker for the PRAISE rule language.
 * 
 * To generate this file, run:
 *     jflex /src/main/resources/com/sri/ai/praise/rules/rsyntaxtextarea/RuleTokenMaker.flex
 * 
 * Then delete the generated versions of yyreset(Reader) and zzRefill() so
 * that we use the versions defined in this file.  This will eliminate 
 * memory allocations.
 */
package com.sri.ai.praise.rules.rsyntaxtextarea;

import java.io.*;
import javax.swing.text.Segment;

import org.fife.ui.rsyntaxtextarea.AbstractJFlexTokenMaker;
import org.fife.ui.rsyntaxtextarea.DefaultToken;
import org.fife.ui.rsyntaxtextarea.Token;
import org.fife.ui.rsyntaxtextarea.TokenMaker;


/**
 * Scanner for the PRAISE rule language.
 */
%%

%public
%class RuleTokenMaker
%extends AbstractJFlexTokenMaker
%implements TokenMaker
%unicode
%type org.fife.ui.rsyntaxtextarea.Token


%{


	/**
	 * Constructor.  This must be here because JFlex does not generate a
	 * no-parameter constructor.
	 */
	public RuleTokenMaker() {
		super();
	}


	/**
	 * Adds the token specified to the current linked list of tokens.
	 *
	 * @param tokenType The token's type.
	 * @see #addToken(int, int, int)
	 */
	private void addHyperlinkToken(int start, int end, int tokenType) {
		int so = start + offsetShift;
		addToken(zzBuffer, start,end, tokenType, so, true);
	}


	/**
	 * Adds the token specified to the current linked list of tokens.
	 *
	 * @param tokenType The token's type.
	 */
	private void addToken(int tokenType) {
		addToken(zzStartRead, zzMarkedPos-1, tokenType);
	}


	/**
	 * Adds the token specified to the current linked list of tokens.
	 *
	 * @param tokenType The token's type.
	 */
	private void addToken(int start, int end, int tokenType) {
		int so = start + offsetShift;
		addToken(zzBuffer, start,end, tokenType, so);
	}


	/**
	 * Adds the token specified to the current linked list of tokens.
	 *
	 * @param array The character array.
	 * @param start The starting offset in the array.
	 * @param end The ending offset in the array.
	 * @param tokenType The token's type.
	 * @param startOffset The offset in the document at which this token
	 *                    occurs.
	 */
	public void addToken(char[] array, int start, int end, int tokenType, int startOffset) {
		super.addToken(array, start,end, tokenType, startOffset);
		zzStartRead = zzMarkedPos;
	}


	/**
	 * Returns the text to place at the beginning and end of a
	 * line to "comment" it in a this programming language.
	 *
	 * @return The start and end strings to add to a line to "comment"
	 *         it out.
	 */
	public String[] getLineCommentStartAndEnd() {
		return new String[] { "#", null };
	}


	/**
	 * Returns the first token in the linked list of tokens generated
	 * from <code>text</code>.  This method must be implemented by
	 * subclasses so they can correctly implement syntax highlighting.
	 *
	 * @param text The text from which to get tokens.
	 * @param initialTokenType The token type we should start with.
	 * @param startOffset The offset into the document at which
	 *        <code>text</code> starts.
	 * @return The first <code>Token</code> in a linked list representing
	 *         the syntax highlighted text.
	 */
	public Token getTokenList(Segment text, int initialTokenType, int startOffset) {


		resetTokenList();
		this.offsetShift = -text.offset + startOffset;

		// Start off in the proper state.
		int state = Token.NULL;
		switch (initialTokenType) {
			case Token.COMMENT_MULTILINE:
				state = MLC;
				start = text.offset;
				break;
			case Token.LITERAL_STRING_DOUBLE_QUOTE:
				state = LONG_STRING_2;
				break;
			case Token.LITERAL_CHAR:
				state = LONG_STRING_1;
				break;
			default:
				state = Token.NULL;
		}

		s = text;
		try {
			yyreset(zzReader);
			yybegin(state);
			return yylex();
		}
		catch (IOException ioe) {
			ioe.printStackTrace();
			return new DefaultToken();
		}

	}


	/* Keep this version of the method. */
	/**
	 * Resets the scanner to read from a new input stream.
	 * Does not close the old reader.
	 *
	 * All internal variables are reset, the old input stream 
	 * <b>cannot</b> be reused (internal buffer is discarded and lost).
	 * Lexical state is set to <tt>YY_INITIAL</tt>.
	 *
	 * @param reader   the new input stream 
	 */
	public final void yyreset(java.io.Reader reader) throws java.io.IOException {
		// 's' has been updated.
		zzBuffer = s.array;
		/*
		 * We replaced the line below with the two below it because zzRefill
		 * no longer "refills" the buffer (since the way we do it, it's always
		 * "full" the first time through, since it points to the segment's
		 * array).  So, we assign zzEndRead here.
		 */
		//zzStartRead = zzEndRead = s.offset;
		zzStartRead = s.offset;
		zzEndRead = zzStartRead + s.count - 1;
		zzCurrentPos = zzMarkedPos = s.offset;
		zzLexicalState = YYINITIAL;
		zzReader = reader;
//		zzAtBOL  = true;
		zzAtEOF  = false;
	}


	/* Keep this version of the method. */
	/**
	 * Refills the input buffer.
	 *
	 * @return      <code>true</code> if EOF was reached, otherwise
	 *              <code>false</code>.
	 * @exception   IOException  if any I/O-Error occurs.
	 */
	private boolean zzRefill() throws java.io.IOException {
		return zzCurrentPos>=s.offset+s.count;
	}


%}

identifier			= (({letter}|"_")({letter}|{digit}|"_")*)
letter				= ({lowercase}|{uppercase})
lowercase			= ([a-z])
uppercase			= ([A-Z])
digit				= ([0-9])
stringliteral		= ({shortstring})
shortstring1item	= ({shortstring1char}|{escapeseq}|[\\])
shortstring2item	= ({shortstring2char}|{escapeseq}|[\\])
shortstring1char	= ([^\\\n\'])
shortstring2char	= ([^\\\n\"])
escapeseq			= ([\\].)
integer				= ({decimalinteger}|{octinteger}|{hexinteger})
decimalinteger		= ({nonzerodigit}{digit}*|"0")
octinteger			= ("0"{octdigit}+)
hexinteger			= ("0"[xX]{hexdigit}+)
nonzerodigit		= ([1-9])
octdigit			= ([0-7])
hexdigit			= ({digit}|[a-f]|[A-F])
floatnumber			= ({pointfloat}|{exponentfloat})
pointfloat			= ({intpart}?{fraction}|{intpart}".")
exponentfloat		= (({intpart}|{pointfloat}){exponent})
intpart				= ({digit}+)
fraction			= ("."{digit}+)
exponent			= ([eE][\+\-]?{digit}+)
imagnumber			= (({floatnumber}|{intpart})[jJ])

ErrorNumberFormat	= ({digit}{NonSeparator}+)
NonSeparator		= ([^\t\f\r\n\ \(\)\{\}\[\]\;\,\.\=\>\<\!\~\?\:\+\-\*\/\&\|\^\%\"\']|"#")

LongStringDelimiter1	= "'"
LongStringDelimiter2	= "\""

LineTerminator		= (\n)
WhiteSpace			= ([ \t\f])

MLCBegin			= "/*"
MLCEnd				= "*/"
LineCommentBegin	= "//"

URLGenDelim				= ([:\/\?#\[\]@])
URLSubDelim				= ([\!\$&'\(\)\*\+,;=])
URLUnreserved			= ({letter}|{digit}|[_\-\.\~])
URLCharacter			= ({URLGenDelim}|{URLSubDelim}|{URLUnreserved}|[%])
URLCharacters			= ({URLCharacter}*)
URLEndCharacter			= ([\/\$]|{letter}|{digit})
URL						= (((https?|f(tp|ile))"://"|"www.")({URLCharacters}{URLEndCharacter})?)

%state LONG_STRING_1
%state LONG_STRING_2
%state MLC
%state EOL_COMMENT


%%

/* Keywords */
<YYINITIAL> "all"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "and"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "as"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "be"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "else"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "exists"				{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "for"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "if"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "may"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "not"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "or"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "random"				{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "same"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "sort"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "then"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "there"					{ addToken(Token.RESERVED_WORD); }
<YYINITIAL> "P"						{ addToken(Token.RESERVED_WORD); }


<YYINITIAL> {

	{LineTerminator}				{ addNullToken(); return firstToken; }

	{identifier}					{ addToken(Token.IDENTIFIER); }

	{WhiteSpace}+					{ addToken(Token.WHITESPACE); }

	/* String/Character Literals. */
	{LongStringDelimiter1}			{ yybegin(LONG_STRING_1); addToken(Token.LITERAL_CHAR); }
	{LongStringDelimiter2}			{ yybegin(LONG_STRING_2); addToken(Token.LITERAL_STRING_DOUBLE_QUOTE); }

	/* Comment Literals. */
	"/**/"							{ addToken(Token.COMMENT_MULTILINE); }
	{MLCBegin}						{ start = zzMarkedPos-2; yybegin(MLC); }
	{LineCommentBegin}				{ start = zzMarkedPos-2; yybegin(EOL_COMMENT); }

	/* Separators. */
	"("								{ addToken(Token.SEPARATOR); }
	")"								{ addToken(Token.SEPARATOR); }
	","								{ addToken(Token.SEPARATOR); }
	"x"								{ addToken(Token.SEPARATOR); }
	"|"								{ addToken(Token.SEPARATOR); }

	/* Operators. */
	"!="							{ addToken(Token.OPERATOR); }
	"="								{ addToken(Token.OPERATOR); }
	"+"								{ addToken(Token.OPERATOR); }
	"-"								{ addToken(Token.OPERATOR); }
	"minus"							{ addToken(Token.OPERATOR); }
	"*"								{ addToken(Token.OPERATOR); }
	"/"								{ addToken(Token.OPERATOR); }
	"%"								{ addToken(Token.OPERATOR); }
	"^"								{ addToken(Token.OPERATOR); }
	"->"							{ addToken(Token.OPERATOR); }
	"=>"							{ addToken(Token.OPERATOR); }
	"<=>"							{ addToken(Token.OPERATOR); }
	":-"							{ addToken(Token.OPERATOR); }
	":"								{ addToken(Token.OPERATOR); }
	";"								{ addToken(Token.OPERATOR); }

	/* Numbers */
	{integer}						{ addToken(Token.LITERAL_NUMBER_DECIMAL_INT); }
	{floatnumber}|{imagnumber}		{ addToken(Token.LITERAL_NUMBER_FLOAT); }
	{ErrorNumberFormat}				{ addToken(Token.ERROR_NUMBER_FORMAT); }

	/* Other punctuation, we'll highlight it as "identifiers." */
	"."								{ addToken(Token.IDENTIFIER); }
	";"								{ addToken(Token.IDENTIFIER); }

	/* Ended with a line not in a string or comment. */
	<<EOF>>						{ addNullToken(); return firstToken; }

	/* Catch any other (unhandled) characters and flag them as bad. */
	.							{ addToken(Token.ERROR_IDENTIFIER); }

}

<LONG_STRING_1> {
	{shortstring1item}+			{ addToken(Token.LITERAL_CHAR); }
	"'"							{ yybegin(YYINITIAL); addToken(Token.LITERAL_CHAR); }
	<<EOF>>						{
									if (firstToken==null) {
										addToken(Token.LITERAL_CHAR); 
									}
									return firstToken;
								}
}

<LONG_STRING_2> {
	{shortstring2item}+			{ addToken(Token.LITERAL_STRING_DOUBLE_QUOTE); }
	\"							{ yybegin(YYINITIAL); addToken(Token.LITERAL_STRING_DOUBLE_QUOTE); }
	<<EOF>>						{
									if (firstToken==null) {
										addToken(Token.LITERAL_STRING_DOUBLE_QUOTE); 
									}
									return firstToken;
								}
}

<MLC> {

	[^hwf\n\*]+				{}
	{URL}					{ int temp=zzStartRead; addToken(start, zzStartRead-1, Token.COMMENT_MULTILINE); addHyperlinkToken(temp,zzMarkedPos-1, Token.COMMENT_MULTILINE); start = zzMarkedPos; }
	[hwf]					{}

	\n						{ addToken(start, zzStartRead-1, Token.COMMENT_MULTILINE); return firstToken; }
	{MLCEnd}				{ yybegin(YYINITIAL); addToken(start, zzStartRead+1, Token.COMMENT_MULTILINE); }
	\*						{}
	<<EOF>>					{ addToken(start, zzStartRead-1, Token.COMMENT_MULTILINE); return firstToken; }

}


<EOL_COMMENT> {
	[^hwf\n]+				{}
	{URL}					{ int temp=zzStartRead; addToken(start,zzStartRead-1, Token.COMMENT_EOL); addHyperlinkToken(temp,zzMarkedPos-1, Token.COMMENT_EOL); start = zzMarkedPos; }
	[hwf]					{}
	\n						{ addToken(start,zzStartRead-1, Token.COMMENT_EOL); addNullToken(); return firstToken; }
	<<EOF>>					{ addToken(start,zzStartRead-1, Token.COMMENT_EOL); addNullToken(); return firstToken; }

}
