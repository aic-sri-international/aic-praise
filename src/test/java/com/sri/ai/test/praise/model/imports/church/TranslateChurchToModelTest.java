package com.sri.ai.test.praise.model.imports.church;

import java.util.Arrays;
import java.util.List;
import java.util.StringJoiner;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.imports.church.TranslateChurchToModel;
import com.sri.ai.test.praise.AbstractLPITest;
import com.sri.ai.util.base.Triple;

public class TranslateChurchToModelTest extends AbstractLPITest {
	
	private TranslateChurchToModel translator;
	
	@Override
	@Before
	public void setUp() {
		super.setUp();
		
		translator = new TranslateChurchToModel();
	}
	
	@Test
	public void testExample1() {
		Triple<String, Model, List<Expression>> translation = translator.translate("Example 1", ""
				+ "(define sunny #t)\n"
				);
	
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(define sunny #t)",				
				"",
				"--->",
				"",
				"sort Values;",
				"",
				"random sunny: Boolean;",
				"",
				"if sunny then 1 else 0;"
		);
	}
	
	
	@Test
	public void testExample2() {
		Triple<String, Model, List<Expression>> translation = translator.translate("Example 2", ""
				+ "(define sunny (flip 0.3))\n"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(define sunny (flip 0.3))",				
				"",
				"--->",
				"",
				"sort Values;",
				"",
				"random sunny: Boolean;",
				"",
				"if sunny then 0.3 else 0.7;"
		);
	}

	@Test
	public void testExample3() {					
		Triple<String, Model, List<Expression>> translation = translator.translate("Example 3", ""
				+ "(define goOut (mem (lambda (day) (if (eq? day 'friday) (flip 0.8) (flip 0.3)))))\n"
				+ "(goOut 'friday)\n"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(define goOut (mem (lambda (day) (if (eq? day \\'friday) (flip 0.8) (flip 0.3)))))",
				"(goOut \\'friday)",				
				"",
				"--->",
				"",
				"sort Values : Unknown, friday;",
				"",
				"random goOut: Values -> Boolean;",
				"",
				"if Day = friday then if goOut(friday) then 0.8 else 0.2 else if goOut(Day) then 0.3 else 0.7;"
		);
	}
	
	@Test
	public void testExample4() {					
		Triple<String, Model, List<Expression>> translation = translator.translate("Example 4", ""
				+ "(define epidemic (mem (lambda () (flip 0.01))))\n"
				+ "(define sick (mem (lambda (person) (if epidemic (flip 0.6) (flip 0.1)))))\n"
				+ "(sick 'john)\n"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(define epidemic (mem (lambda () (flip 0.01))))",
				"(define sick (mem (lambda (person) (if epidemic (flip 0.6) (flip 0.1)))))",
				"(sick \\'john)",				
				"",
				"--->",
				"",
				"sort Values;",
				"",
				"random epidemic: Boolean;",
				"random sick: Values -> Boolean;",
				"",
				"if epidemic then 0.01 else 0.99;",
				"if epidemic then if sick(Person) then 0.6 else 0.4 else if sick(Person) then 0.1 else 0.9;"
		);
	}
	
	@Test
	public void testLogicalNotOnVariable() {
		Triple<String, Model, List<Expression>> translation = translator.translate("Logical Not on Variable", ""
				+ "(define goOut (mem (lambda (day) (if (not (eqv? day 'friday)) (flip 0.8) (flip 0.3)))))\n"
				+ "(goOut 'friday)\n"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(define goOut (mem (lambda (day) (if (not (eqv? day \\'friday)) (flip 0.8) (flip 0.3)))))",
				"(goOut \\'friday)",			
				"",
				"--->",
				"",
				"sort Values : Unknown, friday;",
				"",
				"random goOut: Values -> Boolean;",
				"",
				"if Day = friday then if goOut(friday) then 0.3 else 0.7 else if goOut(Day) then 0.8 else 0.2;"
		);
	}
	
	@Test
	public void testLogicalOrOnVariable() {
		Triple<String, Model, List<Expression>> translation = translator.translate("Logical Or on Variable", ""
				+ "(define goOut (mem (lambda (day) (if (or (equal? day 'friday) (equal? day 'saturday)) (flip 0.8) (flip 0.3)))))\n"
				+ "(goOut 'friday)\n"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(define goOut (mem (lambda (day) (if (or (equal? day \\'friday) (equal? day \\'saturday)) (flip 0.8) (flip 0.3)))))",
				"(goOut \\'friday)",			
				"",
				"--->",
				"",
				"sort Values : Unknown, friday, saturday;",
				"",
				"random goOut: Values -> Boolean;",
				"",
				"if Day = friday then if goOut(friday) then 0.8 else 0.2 else if Day = saturday then if goOut(saturday) then 0.8 else 0.2 else if goOut(Day) then 0.3 else 0.7;"
		);
	}
	
	@Test
	public void testLogicalAndOnVariables() {
		Triple<String, Model, List<Expression>> translation = translator.translate("Logical Or on Variable", ""
				+ "(define goOut (mem (lambda (day1 day2) (if (and (eq? day1 'friday) (eq? day2 'saturday)) (flip 0.8) (flip 0.3)))))\n"
				+ "(goOut 'friday 'saturday)\n"
				);
		
		print(translation);
		assertDescriptionEquals(translation.second.getDescription(),
				"(define goOut (mem (lambda (day1 day2) (if (and (eq? day1 \\'friday) (eq? day2 \\'saturday)) (flip 0.8) (flip 0.3)))))",
				"(goOut \\'friday \\'saturday)",			
				"",
				"--->",
				"",
				"sort Values : Unknown, friday, saturday;",
				"",
				"random goOut: Values x Values -> Boolean;",
				"",
				"if Day1 = friday then if Day2 = saturday then if goOut(friday, saturday) then 0.8 else 0.2 else if goOut(friday, Day2) then 0.3 else 0.7 else if goOut(Day1, Day2) then 0.3 else 0.7;"
		);
	}
	
	@Test
	public void testReferToOtherVariableInDefinition() {					
		Triple<String, Model, List<Expression>> translation = translator.translate("Refer to other variable in definition", ""
				+ "(define sunny (mem (lambda (day) (flip 0.3))))\n"
				+ "(define goOut (mem (lambda (day) (if (sunny day) (flip 0.8) (flip 0.3)))))\n"
				+ "(goOut 'friday)\n"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(define sunny (mem (lambda (day) (flip 0.3))))",
				"(define goOut (mem (lambda (day) (if (sunny day) (flip 0.8) (flip 0.3)))))",
				"(goOut \\'friday)",				
				"",
				"--->",
				"",
				"sort Values;",
				"",
				"random sunny: Values -> Boolean;",
				"random goOut: Values -> Boolean;",
				"",
				"if sunny(Day) then 0.3 else 0.7;",				
				"if sunny(Day) then if goOut(Day) then 0.8 else 0.2 else if goOut(Day) then 0.3 else 0.7;"
		);
	}
	
	@Test
	public void testReferringLogicalNot() {					
		Triple<String, Model, List<Expression>> translation = translator.translate("Referring Logical Not", ""
				+ "(define sunny (mem (lambda (day) (flip 0.3))))\n"
				+ "(define goOut (mem (lambda (day) (if (not (sunny day)) (flip 0.8) (flip 0.3)))))\n"
				+ "(goOut 'friday)\n"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(define sunny (mem (lambda (day) (flip 0.3))))",
				"(define goOut (mem (lambda (day) (if (not (sunny day)) (flip 0.8) (flip 0.3)))))",
				"(goOut \\'friday)",				
				"",
				"--->",
				"",
				"sort Values;",
				"",
				"random sunny: Values -> Boolean;",
				"random goOut: Values -> Boolean;",
				"",
				"if sunny(Day) then 0.3 else 0.7;",				
				"if sunny(Day) then if goOut(Day) then 0.3 else 0.7 else if goOut(Day) then 0.8 else 0.2;"
		);
	}
	
	@Test
	public void testReferringLogicalAnd() {					
		Triple<String, Model, List<Expression>> translation = translator.translate("Referring Logical And", ""
				+ "(define sunny (mem (lambda (day) (flip 0.3))))\n"
				+ "(define goOut (mem (lambda (day) (if (and (sunny day) (eq? day 'friday)) (flip 0.8) (flip 0.3)))))\n"
				+ "(goOut 'friday)\n"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(define sunny (mem (lambda (day) (flip 0.3))))",
				"(define goOut (mem (lambda (day) (if (and (sunny day) (eq? day \\'friday)) (flip 0.8) (flip 0.3)))))",
				"(goOut \\'friday)",				
				"",
				"--->",
				"",
				"sort Values : Unknown, friday;",
				"",
				"random sunny: Values -> Boolean;",
				"random goOut: Values -> Boolean;",
				"",
				"if sunny(Day) then 0.3 else 0.7;",				
				"if Day = friday then if sunny(friday) then if goOut(friday) then 0.8 else 0.2 else if goOut(friday) then 0.3 else 0.7 else if goOut(Day) then 0.3 else 0.7;"
		);
	}
	
	@Test
	public void testReferringLogicalOr() {					
		Triple<String, Model, List<Expression>> translation = translator.translate("Referring Logical Or", ""
				+ "(define sunny (mem (lambda (day) (flip 0.3))))\n"
				+ "(define goOut (mem (lambda (day) (if (or (sunny day) (eq? day 'friday)) (flip 0.8) (flip 0.3)))))\n"
				+ "(goOut 'friday)\n"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(define sunny (mem (lambda (day) (flip 0.3))))",
				"(define goOut (mem (lambda (day) (if (or (sunny day) (eq? day \\'friday)) (flip 0.8) (flip 0.3)))))",
				"(goOut \\'friday)",				
				"",
				"--->",
				"",
				"sort Values : Unknown, friday;",
				"",
				"random sunny: Values -> Boolean;",
				"random goOut: Values -> Boolean;",
				"",
				"if sunny(Day) then 0.3 else 0.7;",				
				"if Day = friday then if goOut(friday) then 0.8 else 0.2 else if sunny(Day) then if goOut(Day) then 0.8 else 0.2 else if goOut(Day) then 0.3 else 0.7;"
		);
	}
	
	//
	// PRIVATE
	//		
	private void print(Triple<String, Model, List<Expression>> translation) {
		Model model = translation.second;
		
		StringJoiner sj = new StringJoiner("\n", "\n", "\n")
			.add("-- HOGM:")
			.add(translation.first)
			.add("-- MODEL NAME:")
			.add(model.getName().toString())
			.add("-- DESCRIPTION (CHURCH to HOGM RULES):")
			.add(model.getDescription().toString())
			.add("-- HOGM RULES to LOW LEVEL MODEL:");
		model.getSortDeclarations().stream()
			.forEach(sort -> sj.add(sort.getSortDeclaration().toString()));
		model.getRandomVariableDeclarations().stream()
			.forEach(rv -> sj.add(rv.getRandomVariableDeclaration().toString()));
		model.getParfactorsDeclaration().getParfactors().stream()
			.forEach(parfactor -> sj.add(parfactor.toString()));		
		sj.add("-- WITH QUERIES:");
		translation.third.stream()
			.forEach(query -> sj.add(query.toString()));	
		
		System.out.print(sj.toString());
				
		GrinderUtil.doTreeUtilWaitUntilClosed();
	}
	
	private void assertDescriptionEquals(Expression description, String... descLines) {
		StringJoiner sj = new StringJoiner("\n", "'\n", "\n'");
		Arrays.stream(descLines).forEach(line -> sj.add(line));
		Assert.assertEquals(sj.toString(), description.toString());
	}
}
