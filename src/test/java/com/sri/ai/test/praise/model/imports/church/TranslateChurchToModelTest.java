package com.sri.ai.test.praise.model.imports.church;

import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.praise.model.imports.church.TranslateChurchToModel;
import com.sri.ai.test.praise.AbstractLPITest;
import com.sri.ai.util.base.Triple;

public class TranslateChurchToModelTest extends AbstractLPITest {
	
	private TranslateChurchToModel translator;
	
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
// TODO - validate translation	
		print(translation);
	}
	
	
	@Test
	public void testExample2() {
		Triple<String, Model, List<Expression>> translation = translator.translate("Example 2", ""
				+ "(define sunny (flip 0.3))\n"
				);
// TODO - validate translation		
		print(translation);
	}

	@Test
	public void testExample3() {					
		Triple<String, Model, List<Expression>> translation = translator.translate("Example 3", ""
				+ "(define goOut (mem (lambda (day) (if (= day friday) (flip 0.8) (flip 0.3)))))\n"
				+ "(goOut friday)\n"
				+ "(goOut monday)\n"
				);
// TODO - validate translation		
		print(translation);
	}
	
	//
	// PRIVATE
	//
	private void print(Triple<String, Model, List<Expression>> translation) {		
		Model model = translation.second;
		System.out.println("-- HOGM:");
		System.out.println(translation.first);
		System.out.println("-- MODEL NAME:");
		System.out.println(model.getName());
		System.out.println("-- DESCRIPTION (CHURCH to HOGM RULES):");
		System.out.println(model.getDescription());
		System.out.println("-- HOGM RULES to LOW LEVEL MODEL:");
		for (SortDeclaration sort : model.getSortDeclarations()) {
			System.out.println(sort.getSortDeclaration());
		}
		for (RandomVariableDeclaration rv : model.getRandomVariableDeclarations()) {
			System.out.println(rv.getRandomVariableDeclaration());
		}
		for (Expression parfactor : model.getParfactorsDeclaration().getParfactors()) {
			System.out.println(parfactor);
		}
		System.out.println("-- WITH QUERIES:");
		for (Expression query : translation.third) {
			System.out.println(query);
		}
		System.out.println("\n");
				
		GrinderUtil.doTreeUtilWaitUntilClosed();
	}
	
}
