package com.sri.ai.test.praise.model.imports.church;

import java.util.Arrays;
import java.util.List;
import java.util.StringJoiner;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
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
				+ "(query \n"
				+ "  (define sunny #t)\n"
				+ "  sunny\n"
				+ ")"
				);
	
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define sunny #t)",	
				"  sunny",
				")",
				"--->",
				"",
				"random sunny: Boolean;",
				"",
				"if sunny then 1 else 0;"
		);
	}
	
	
	@Test
	public void testExample2() {
		Triple<String, Model, List<Expression>> translation = translator.translate("Example 2", ""
				+ "(query \n"
				+ "  (define sunny (flip 0.3))\n"
				+ "  sunny\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define sunny (flip 0.3))",	
				"  sunny",
				")",
				"--->",
				"",
				"random sunny: Boolean;",
				"",
				"if sunny then 0.3 else 0.7;"
		);
	}

	@Test
	public void testExample3() {					
		Triple<String, Model, List<Expression>> translation = translator.translate("Example 3", ""
				+ "(query \n"
				+ "  (define goOut (mem (lambda (day) (if (eq? day 'friday) (flip 0.8) (flip 0.3)))))\n"
				+ "  (goOut 'friday)\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define goOut (mem (lambda (day) (if (eq? day \\'friday) (flip 0.8) (flip 0.3)))))",
				"  (goOut \\'friday)",				
				")",
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
				+ "(query \n"
				+ "  (define epidemic (flip 0.01))\n"
				+ "  (define sick (mem (lambda (person) (if epidemic (flip 0.6) (flip 0.1)))))\n"
				+ "  (define testsPositive (mem (lambda (person) (if (sick person) (flip 0.6) (flip 0.2)))))\n"
				+ "  epidemic\n"
				+ "  (forall (person) (if (eq? person 'john) (not (testsPositive 'john)) (testsPositive person)))\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define epidemic (flip 0.01))",
				"  (define sick (mem (lambda (person) (if epidemic (flip 0.6) (flip 0.1)))))",
				"  (define testsPositive (mem (lambda (person) (if (sick person) (flip 0.6) (flip 0.2)))))",
				"  epidemic",
				"  (forall (person) (if (eq? person \\'john) (not (testsPositive \\'john)) (testsPositive person)))",
				")",
				"--->",
				"",
				"sort Values;",
				"",
				"random epidemic: Boolean;",
				"random sick: Values -> Boolean;",
				"random testsPositive: Values -> Boolean;",
                "",
				"if epidemic then 0.01 else 0.99;",
				"if epidemic then if sick(Person) then 0.6 else 0.4 else if sick(Person) then 0.1 else 0.9;",
				"if sick(Person) then if testsPositive(Person) then 0.6 else 0.4 else if testsPositive(Person) then 0.2 else 0.8;",
				"if Person = john then not testsPositive(john) else testsPositive(Person);"
		);
	}
	
	@Test
	public void testExample5() {					
		Triple<String, Model, List<Expression>> translation = translator.translate("Example 5", ""
				+ "(query \n"
				+ "  (define A (flip))\n"
				+ "  (define B (flip))\n"
				+ "  (define C (flip))\n"
				+ "  A\n"
				+ "  (or (and A B) (and A C) (and B C))\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define A (flip))",
				"  (define B (flip))",
				"  (define C (flip))",
				"  A",
				"  (or (and A B) (and A C) (and B C))",
				")",
				"--->",
				"",
				"random a: Boolean;",
				"random b: Boolean;",
				"random c: Boolean;",
				"",
				"a and b or a and c or b and c;"
		);
	}
	
	@Test
	public void testExample6() {					
		Triple<String, Model, List<Expression>> translation = translator.translate("Example 6", ""
				+ "(query \n"
				+ "  (define lung-cancer (flip 0.01))\n"
				+ "  (define TB (flip 0.005))\n"
				+ "  (define stomach-flu (flip 0.1))\n"
				+ "  (define cold (flip 0.2))\n"
				+ "  (define other (flip 0.1))\n"
				+ "  (define cough \n"
				+ "    (or (and cold (flip 0.5))\n"
				+ "        (and lung-cancer (flip 0.3))\n"
				+ "        (and TB (flip 0.7))\n"
				+ "        (and other (flip 0.01))))\n"
				+ "  (define fever \n"
				+ "    (or (and cold (flip 0.3))\n"
				+ "        (and stomach-flu (flip 0.5))\n"
				+ "        (and TB (flip 0.1))\n"
				+ "        (and other (flip 0.01))))\n"
				+ "  (define chest-pain \n"
				+ "    (or (and lung-cancer (flip 0.5))\n"
				+ "        (and TB (flip 0.5))\n"
				+ "        (and other (flip 0.01))))\n"
				+ "  (define shortness-of-breath \n"
				+ "    (or (and lung-cancer (flip 0.5))\n"
				+ "        (and TB (flip 0.2))\n"
				+ "        (and other (flip 0.01))))\n"
				+ "  cough\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define lung-cancer (flip 0.01))",
                "  (define TB (flip 0.005))",
                "  (define stomach-flu (flip 0.1))",
                "  (define cold (flip 0.2))",
                "  (define other (flip 0.1))",
                "  (define cough ",
                "    (or (and cold (flip 0.5))",
                "        (and lung-cancer (flip 0.3))",
                "        (and TB (flip 0.7))",
                "        (and other (flip 0.01))))",
                "  (define fever ",
                "    (or (and cold (flip 0.3))",
                "        (and stomach-flu (flip 0.5))",
                "        (and TB (flip 0.1))",
                "        (and other (flip 0.01))))",
                "  (define chest-pain ",
                "    (or (and lung-cancer (flip 0.5))",
                "        (and TB (flip 0.5))",
                "        (and other (flip 0.01))))",
                "  (define shortness-of-breath ",
                "    (or (and lung-cancer (flip 0.5))",
                "        (and TB (flip 0.2))",
                "        (and other (flip 0.01))))",
                "  cough",
				")",
				"--->",
				"",
				"random lung_cancer: Boolean;",
				"random tB: Boolean;",
				"random stomach_flu: Boolean;",
				"random cold: Boolean;",
				"random other: Boolean;",
				"random cough: Boolean;",
				"random fever: Boolean;",
				"random chest_pain: Boolean;",
				"random shortness_of_breath: Boolean;",
                "",
				"if lung_cancer then 0.01 else 0.99;",
				"if tB then 0.005 else 0.995;",
				"if stomach_flu then 0.1 else 0.9;",
				"if cold then 0.2 else 0.8;",
				"if other then 0.1 else 0.9;",
				"if other then if tB then if lung_cancer then if cold then if cough then 0.89605 else 0.10395 else if cough then 0.7921 else 0.2079 else if cold then if cough then 0.8515 else 0.1485 else if cough then 0.703 else 0.297 else if lung_cancer then if cold then if cough then 0.6535 else 0.3465 else if cough then 0.307 else 0.693 else if cold then if cough then 0.505 else 0.495 else if cough then 0.01 else 0.99 else if tB then if lung_cancer then if cold then if cough then 0.895 else 0.105 else if cough then 0.79 else 0.21 else if cold then if cough then 0.85 else 0.15 else if cough then 0.7 else 0.3 else if lung_cancer then if cold then if cough then 0.65 else 0.35 else if cough then 0.3 else 0.7 else if cold then 0.5 else if cough then 0 else 1;",
				"if other then if tB then if stomach_flu then if cold then if fever then 0.68815 else 0.31185 else if fever then 0.5545 else 0.4455 else if cold then if fever then 0.3763 else 0.6237 else if fever then 0.109 else 0.891 else if stomach_flu then if cold then if fever then 0.6535 else 0.3465 else if fever then 0.505 else 0.495 else if cold then if fever then 0.307 else 0.693 else if fever then 0.01 else 0.99 else if tB then if stomach_flu then if cold then if fever then 0.685 else 0.315 else if fever then 0.55 else 0.45 else if cold then if fever then 0.37 else 0.63 else if fever then 0.1 else 0.9 else if stomach_flu then if cold then if fever then 0.65 else 0.35 else 0.5 else if cold then if fever then 0.3 else 0.7 else if fever then 0 else 1;",
				"if other then if tB then if lung_cancer then if chest_pain then 0.7525 else 0.2475 else if chest_pain then 0.505 else 0.495 else if lung_cancer then if chest_pain then 0.505 else 0.495 else if chest_pain then 0.01 else 0.99 else if tB then if lung_cancer then if chest_pain then 0.75 else 0.25 else 0.5 else if lung_cancer then 0.5 else if chest_pain then 0 else 1;",
				"if other then if tB then if lung_cancer then if shortness_of_breath then 0.604 else 0.396 else if shortness_of_breath then 0.208 else 0.792 else if lung_cancer then if shortness_of_breath then 0.505 else 0.495 else if shortness_of_breath then 0.01 else 0.99 else if tB then if lung_cancer then if shortness_of_breath then 0.6 else 0.4 else if shortness_of_breath then 0.2 else 0.8 else if lung_cancer then 0.5 else if shortness_of_breath then 0 else 1;"
		);
	}
	
	@Test
	public void testExample7() {					
		Triple<String, Model, List<Expression>> translation = translator.translate("Example 7", ""
				+ "(query \n"
				+ "  (define breast-cancer (flip 0.01))\n"
				+ "  (define positive-mammogram (if breast-cancer (flip 0.8) (flip 0.096)))\n"
				+ "  breast-cancer\n"
				+ "  positive-mammogram\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define breast-cancer (flip 0.01))",
				"  (define positive-mammogram (if breast-cancer (flip 0.8) (flip 0.096)))",
				"  breast-cancer",
				"  positive-mammogram",
				")",
				"--->",
				"",
				"random breast_cancer: Boolean;",
				"random positive_mammogram: Boolean;",
				"",
				"if breast_cancer then 0.01 else 0.99;",
				"if breast_cancer then if positive_mammogram then 0.8 else 0.2 else if positive_mammogram then 0.096 else 0.904;",
				"positive_mammogram;"
		);
	}
	
	@Test
	public void testLogicalNotOnVariable() {
		Triple<String, Model, List<Expression>> translation = translator.translate("Logical Not on Variable", ""
				+ "(query \n"
				+ "  (define goOut (mem (lambda (day) (if (not (eqv? day 'friday)) (flip 0.8) (flip 0.3)))))\n"
				+ "  (goOut 'friday)\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define goOut (mem (lambda (day) (if (not (eqv? day \\'friday)) (flip 0.8) (flip 0.3)))))",
				"  (goOut \\'friday)",			
				")",
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
				+ "(query \n"
				+ "  (define goOut (mem (lambda (day) (if (or (equal? day 'friday) (equal? day 'saturday)) (flip 0.8) (flip 0.3)))))\n"
				+ "  (goOut 'friday)\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define goOut (mem (lambda (day) (if (or (equal? day \\'friday) (equal? day \\'saturday)) (flip 0.8) (flip 0.3)))))",
				"  (goOut \\'friday)",			
				")",
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
				+ "(query \n"
				+ "  (define goOut (mem (lambda (day1 day2) (if (and (eq? day1 'friday) (eq? day2 'saturday)) (flip 0.8) (flip 0.3)))))\n"
				+ "  (goOut 'friday 'saturday)\n"
				+ ")"
				);
		
		print(translation);
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define goOut (mem (lambda (day1 day2) (if (and (eq? day1 \\'friday) (eq? day2 \\'saturday)) (flip 0.8) (flip 0.3)))))",
				"  (goOut \\'friday \\'saturday)",			
				")",
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
				+ "(query \n"
				+ "  (define sunny (mem (lambda (day) (flip 0.3))))\n"
				+ "  (define goOut (mem (lambda (day) (if (sunny day) (flip 0.8) (flip 0.3)))))\n"
				+ "  (goOut 'friday)\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define sunny (mem (lambda (day) (flip 0.3))))",
				"  (define goOut (mem (lambda (day) (if (sunny day) (flip 0.8) (flip 0.3)))))",
				"  (goOut \\'friday)",				
				")",
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
				+ "(query \n"
				+ "  (define sunny (mem (lambda (day) (flip 0.3))))\n"
				+ "  (define goOut (mem (lambda (day) (if (not (sunny day)) (flip 0.8) (flip 0.3)))))\n"
				+ "  (goOut 'friday)\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define sunny (mem (lambda (day) (flip 0.3))))",
				"  (define goOut (mem (lambda (day) (if (not (sunny day)) (flip 0.8) (flip 0.3)))))",
				"  (goOut \\'friday)",	
				")",
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
				+ "(query \n"
				+ "  (define sunny (mem (lambda (day) (flip 0.3))))\n"
				+ "  (define goOut (mem (lambda (day) (if (and (sunny day) (eq? day 'friday)) (flip 0.8) (flip 0.3)))))\n"
				+ "  (goOut 'friday)\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define sunny (mem (lambda (day) (flip 0.3))))",
				"  (define goOut (mem (lambda (day) (if (and (sunny day) (eq? day \\'friday)) (flip 0.8) (flip 0.3)))))",
				"  (goOut \\'friday)",				
				")",
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
				+ "(query \n"
				+ "  (define sunny (mem (lambda (day) (flip 0.3))))\n"
				+ "  (define goOut (mem (lambda (day) (if (or (sunny day) (eq? day 'friday)) (flip 0.8) (flip 0.3)))))\n"
				+ "  (goOut 'friday)\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define sunny (mem (lambda (day) (flip 0.3))))",
				"  (define goOut (mem (lambda (day) (if (or (sunny day) (eq? day \\'friday)) (flip 0.8) (flip 0.3)))))",
				"  (goOut \\'friday)",				
				")",
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
	
	@Test
	public void testConditionalEvidence() {
		Triple<String, Model, List<Expression>> translation = translator.translate("Conditional Evidence", ""
				+ "(query \n"
				+ "  (define a (flip))\n"
				+ "  (define b (flip))\n"
				+ "  (define c (flip))\n"
				+ "  a\n"
				+ "  (or (and a b) (and a c) (and b c))\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define a (flip))",
				"  (define b (flip))",
				"  (define c (flip))",
				"  a",
				"  (or (and a b) (and a c) (and b c))",
				")",
				"--->",
				"",
				"random a: Boolean;",
				"random b: Boolean;",
				"random c: Boolean;",
				"",
				"a and b or a and c or b and c;"
		);
	}
	
	@Test
	public void testChurchQuoteSupport() {
		Triple<String, Model, List<Expression>> translation = translator.translate("Example 1 using upper case", ""
				+ "(query \n"
				+ "  (define sunny (mem (lambda (day) (flip 0.3))))\n"
				+ "  (sunny 'friday)\n"
				+ ")"
				);
	
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define sunny (mem (lambda (day) (flip 0.3))))",	
				"  (sunny \\'friday)",
				")",
				"--->",
				"",
				"sort Values;",
				"",
				"random sunny: Values -> Boolean;",
				"",
				"if sunny(Day) then 0.3 else 0.7;"
		);
		
		translation = translator.translate("Example 1 using upper case", ""
				+ "(query \n"
				+ "  (define sunny (mem (lambda (day) (flip 0.3))))\n"
				+ "  (sunny (quote friday))\n"
				+ ")"
				);
	
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define sunny (mem (lambda (day) (flip 0.3))))",	
				"  (sunny (quote friday))",
				")",
				"--->",
				"",
				"sort Values;",
				"",
				"random sunny: Values -> Boolean;",
				"",
				"if sunny(Day) then 0.3 else 0.7;"
		);
	}
	
	@Test
	public void testUpperToLowerCase() {
		Triple<String, Model, List<Expression>> translation = translator.translate("Example 1 using upper case", ""
				+ "(query \n"
				+ "  (define Sunny #t)\n"
				+ "  Sunny\n"
				+ ")"
				);
	
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define Sunny #t)",
				"  Sunny",
				")",
				"--->",
				"",
				"random sunny: Boolean;",
				"",
				"if sunny then 1 else 0;"
		);
		
		translation = translator.translate("Conditional Evidence using upper case", ""
				+ "(query \n"
				+ "  (define A (flip))\n"
				+ "  (define B (flip))\n"
				+ "  (define C (flip))\n"
				+ "  A\n"
				+ "  (or (and A B) (and A C) (and B C))\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define A (flip))",
				"  (define B (flip))",
				"  (define C (flip))",
				"  A",
				"  (or (and A B) (and A C) (and B C))",
				")",
				"--->",
				"",
				"random a: Boolean;",
				"random b: Boolean;",
				"random c: Boolean;",
				"",
				"a and b or a and c or b and c;"
		);
		
		translation = translator.translate("Refer to other variable in definition using upper case", ""
				+ "(query \n"
				+ "  (define Sunny (mem (lambda (dayOfWeek) (flip 0.3))))\n"
				+ "  (define GoOut (mem (lambda (day) (if (Sunny day) (flip 0.8) (flip 0.3)))))\n"
				+ "  (GoOut 'friday)\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define Sunny (mem (lambda (dayOfWeek) (flip 0.3))))",
				"  (define GoOut (mem (lambda (day) (if (Sunny day) (flip 0.8) (flip 0.3)))))",
				"  (GoOut \\'friday)",				
				")",
				"--->",
				"",
				"sort Values;",
				"",
				"random sunny: Values -> Boolean;",
				"random goOut: Values -> Boolean;",
				"",
				"if sunny(DayOfWeek) then 0.3 else 0.7;",				
				"if sunny(Day) then if goOut(Day) then 0.8 else 0.2 else if goOut(Day) then 0.3 else 0.7;"
		);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testIllegalArgumentToFlip() {
		// NOTE: Currently require literal number arguments in the interval [0, 1] to flip.
		translator.translate("Illegal argument to flip", ""
				+ "(query \n"
				+ "  (define (coin-flip weight) (if (flip weight) #t #f))\n"
				+ "  (coin-flip 0.5)"
				+ ")"
				);
	}
	
	@Test
	public void testHyphenReplacementWithUnderscore() {
		Triple<String, Model, List<Expression>> translation = translator.translate("Hyphen random variable", ""
				+ "(query \n"
				+ "  (define stomach-flu (flip 0.1))\n"
				+ "  stomach-flu\n"
				+ ")"
				);
	
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define stomach-flu (flip 0.1))",	
				"  stomach-flu",
				")",
				"--->",
				"",
				"random stomach_flu: Boolean;",
				"",
				"if stomach_flu then 0.1 else 0.9;"
		);
		
		translation = translator.translate("Hyphen logical variable", ""
				+ "(query \n"
				+ "  (define a-sunny-day (mem (lambda (a-day) (flip 0.3))))\n"
				+ "  (a-sunny-day 'friday)\n"
				+ ")"
				);
	
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define a-sunny-day (mem (lambda (a-day) (flip 0.3))))",	
				"  (a-sunny-day \\'friday)",
				")",
				"--->",
				"",
				"sort Values;",
				"",
				"random a_sunny_day: Values -> Boolean;",
				"",
				"if a_sunny_day(A_day) then 0.3 else 0.7;"
		);
	}
	
	@Test
	public void testExplicitlyConditionedEvidence() {					
		Triple<String, Model, List<Expression>> translation = translator.translate("Explicitly Conditioned Evidence", ""
				+ "(query \n"
				+ "  (define breast-cancer (flip 0.01))\n"
				+ "  (define positive-mammogram (if breast-cancer (flip 0.8) (flip 0.096)))\n"
				+ "  breast-cancer\n"
				+ "  (condition positive-mammogram)\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define breast-cancer (flip 0.01))",
				"  (define positive-mammogram (if breast-cancer (flip 0.8) (flip 0.096)))",
				"  breast-cancer",
				"  (condition positive-mammogram)",
				")",
				"--->",
				"",
				"random breast_cancer: Boolean;",
				"random positive_mammogram: Boolean;",
				"",
				"if breast_cancer then 0.01 else 0.99;",
				"if breast_cancer then if positive_mammogram then 0.8 else 0.2 else if positive_mammogram then 0.096 else 0.904;",
				"positive_mammogram;"
		);
	}
	
	@Test
	public void testSpecialUniversallyQuantifiedEvidence() {					
		Triple<String, Model, List<Expression>> translation = translator.translate("Special Universally Quantified Evidence", ""
				+ "(query \n"
				+ "  (define epidemic (flip 0.01))\n"
				+ "  (define sick (mem (lambda (person) (if epidemic (flip 0.6) (flip 0.1)))))\n"
				+ "  (sick 'john)\n"
				+ "  (forall (x) (if (eq? x 'john) (sick 'john) (not (sick x))))\n"
				+ ")"
				);
		
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define epidemic (flip 0.01))",
				"  (define sick (mem (lambda (person) (if epidemic (flip 0.6) (flip 0.1)))))",
				"  (sick \\'john)",				
				"  (forall (x) (if (eq? x \\'john) (sick \\'john) (not (sick x))))",
				")",
				"--->",
				"",
				"sort Values;",
				"",
				"random epidemic: Boolean;",
				"random sick: Values -> Boolean;",
				"",
				"if epidemic then 0.01 else 0.99;",
				"if epidemic then if sick(Person) then 0.6 else 0.4 else if sick(Person) then 0.1 else 0.9;",
				"if X = john then sick(john) else not sick(X);"
		);
	}
	
	@Ignore("TODO - Need to figure out how to map Boolean constants to Values sort")
	@Test
	public void testLogicalVariableCondition() {
		Triple<String, Model, List<Expression>> translation = translator.translate("Logical Variable Condition", ""
				+ "(query \n"
				+ "  (define observer (mem (lambda (h) (if (eq? h #f) (flip 0.8) (flip 0.3)))))\n"
				+ "  (observer #f)\n"
				+ ")"
				);
	
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define observer (mem (lambda (h) (if (eq? h #f) (flip 0.8) (flip 0.3)))))",	
				"  (observer #f)",
				")",
				"--->",
				"",
				"sort Values;",
				"",
				"random observer: Values -> Boolean;",
				"",
				"if H = false then if observer(false) then 0.8 else 0.2 else if observer(H) then 0.3 else 0.7;"
		);
		
		translation = translator.translate("Logical Variable Condition", ""
				+ "(query \n"
				+ "  (define observer (mem (lambda (h) (if (eq? h #t) (flip 0.8) (flip 0.3)))))\n"
				+ "  (observer #t)\n"
				+ ")"
				);
	
		print(translation);
		
		assertDescriptionEquals(translation.second.getDescription(),
				"(query ",
				"  (define observer (mem (lambda (h) (if (eq? h #t) (flip 0.8) (flip 0.3)))))",	
				"  (observer #t)",
				")",
				"--->",
				"",
				"sort Values;",
				"",
				"random observer: Values -> Boolean;",
				"",
				"if H = true then if observer(true) then 0.8 else 0.2 else if observer(H) then 0.3 else 0.7;"
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
