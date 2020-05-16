package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor.arrayTableFactor;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.round;
import static org.junit.Assert.assertEquals;

import java.util.List;
import java.util.function.Consumer;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.AnytimeExactBP;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.AnytimeExactBPSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.api.ExactBPNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBPSolver;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.util.computation.anytime.api.Approximation;


public class AnytimeExactBPWithSimplificationTest {
	
	@BeforeEach
	public void before() {
		ArrayTableFactor.decimalPlaces = 2;
		ArrayTableFactor.maximumNumberOfEntriesToShow = 20;
	}

	@Test
	public void testNoSimplifications() {
		
		FactorNetwork factorNetwork;
		Variable query;
		
		TableVariable a = new TableVariable("a", 2);
		TableVariable b = new TableVariable("b", 2);
		TableVariable c = new TableVariable("c", 2);
		TableVariable d = new TableVariable("d", 2);
		
		factorNetwork = new DefaultFactorNetwork(
				arrayTableFactor(
						list(d), 
						(vd) -> 
						vd == 0? 0.5: 0.5),
				arrayTableFactor(
						list(a, d), 
						(va, vd) -> 
						vd == 0? 
								va == 1? 0.8 : 0.2 :  
								va == 1? 0.9 : 0.1),
				arrayTableFactor(
						list(c, d), 
						(vc, vd) -> 
						vd == 0? 
								vc == 1? 0.7 : 0.3 : 
								vc == 1? 0.6 : 0.4),
				arrayTableFactor(
						list(a, b, c), 
						(va, vb, vc) -> vb == va? 1.0 : 0.0)
				);
		
		query = b;
		
		var expectedHistory = list(
				"", 
				"Message   : b   <----   phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]", 
				"Simplified: {(on a, c) phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]}", 
				"to        : {(on a, c) phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]}", 
				"", 
				"Message   :    <----   b", 
				"Simplified: {(on a, c) phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]}", 
				"to        : {(on a, c) phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]}", 
				"", 
				"Message   : a   <----   phi[a, d]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a", 
				"Simplified: {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a", 
				"Updated   : {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a", 
				"Simplified: {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c", 
				"Updated   : Simplex(c)", 
				"to        : Simplex(c)", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c", 
				"Simplified: Simplex(c)", 
				"to        : Simplex(c)", 
				"", 
				"Message   : b   <----   phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]", 
				"Simplified: {(on c, d) phi[d, b, c]: [0.2, 0.2, 0.8, 0.8, 0.1, 0.1, 0.9, 0.9]}", 
				"to        : {(on c, d) phi[d, b, c]: [0.2, 0.2, 0.8, 0.8, 0.1, 0.1, 0.9, 0.9]}", 
				"", 
				"Message   :    <----   b", 
				"Simplified: {(on c, d) phi[d, b, c]: [0.2, 0.2, 0.8, 0.8, 0.1, 0.1, 0.9, 0.9]}", 
				"to        : {(on c, d) phi[d, b, c]: [0.2, 0.2, 0.8, 0.8, 0.1, 0.1, 0.9, 0.9]}", 
				"", 
				"Message   : c   <----   phi[c, d]: [0.3, 0.4, 0.7, 0.6]", 
				"Simplified: {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"to        : {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c", 
				"Simplified: {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"to        : {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a", 
				"Updated   : {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : Simplex(d)*{phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a", 
				"Simplified: Simplex(d)*{phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : Simplex(d)*{phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c", 
				"Updated   : {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"to        : {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c", 
				"Simplified: {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"to        : {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"", 
				"Message   : b   <----   phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]", 
				"Simplified: {(on d) phi[d, b]: [0.2, 0.8, 0.1, 0.9]}", 
				"to        : {(on d) phi[d, b]: [0.2, 0.8, 0.1, 0.9]}", 
				"", 
				"Message   :    <----   b", 
				"Simplified: {(on d) phi[d, b]: [0.2, 0.8, 0.1, 0.9]}", 
				"to        : {(on d) phi[d, b]: [0.2, 0.8, 0.1, 0.9]}", 
				"", 
				"Message   : d   <----   phi[d]: [0.5, 0.5]", 
				"Simplified: {phi[d]: [0.5, 0.5]}", 
				"to        : {phi[d]: [0.5, 0.5]}", 
				"", 
				"Message   : phi[a, d]: [0.2, 0.1, 0.8, 0.9]   <----   d", 
				"Simplified: {phi[d]: [0.5, 0.5]}", 
				"to        : {phi[d]: [0.5, 0.5]}", 
				"", 
				"Message   : a   <----   phi[a, d]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a", 
				"Simplified: {phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a", 
				"Updated   : {phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a", 
				"Simplified: {phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c", 
				"Updated   : {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"to        : {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c", 
				"Simplified: {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"to        : {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"", 
				"Message   : b   <----   phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]", 
				"Simplified: {phi[b]: [0.3, 1.7]}", 
				"to        : {phi[b]: [0.3, 1.7]}", 
				"", 
				"Message   :    <----   b", 
				"Simplified: {phi[b]: [0.3, 1.7]}", 
				"to        : {phi[b]: [0.3, 1.7]}"
				);
		
		runTest(query, factorNetwork, expectedHistory);
	}

	@Test
	public void test() {
		
		FactorNetwork factorNetwork;
		Variable query;
		
		TableVariable q = new TableVariable("q", 2);
		TableVariable a = new TableVariable("a", 2);
		TableVariable b = new TableVariable("b", 2);
		TableVariable c = new TableVariable("c", 2);
		
		factorNetwork = new DefaultFactorNetwork(
				arrayTableFactor(
						list(q,a), 
						(vq, va) -> 
						va == 0? 
								vq == 0? 0.8 : 0.2 :  
								vq == 0? 0.9 : 0.1)
				,
				arrayTableFactor(
						list(q,b), 
						(vq, vb) -> 
						vb == 0? 
								vq == 0? 0.4 : 0.6 :  
								vq == 0? 0.3 : 0.7)
				,
				arrayTableFactor(
						list(b,c), 
						(vb, vc) -> 
						vc == 0? 
								vb == 0? 0.9 : 0.1 :  
								vb == 0? 0.8 : 0.2)
				);
		
		query = q;
		
		var expectedHistory = list(
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Simplified: {(on a) phi[q, a]: [0.8, 0.9, 0.2, 0.1]}", 
				"to        : {(on a) phi[q, a]: [0.8, 0.9, 0.2, 0.1]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Simplified: {(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"to        : {(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {(on a) phi[q, a]: [0.8, 0.9, 0.2, 0.1]}*{(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"to        : {(on a) phi[q, a]: [0.8, 0.9, 0.2, 0.1]}*{(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"", 
				"Message   : phi[q, a]: [0.8, 0.9, 0.2, 0.1]   <----   a", 
				"Simplified: {1}", 
				"to        : {1}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Simplified: {phi[q]: [1.7, 0.3]}", 
				"to        : {phi[q]: [1.7, 0.3]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Updated   : {phi[q]: [1.7, 0.3]}", 
				"to        : {phi[q]: [1.7, 0.3]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Simplified: {phi[q]: [1.7, 0.3]}", 
				"to        : {phi[q]: [1.7, 0.3]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Updated   : {(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"to        : {(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Simplified: {(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"to        : {(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {phi[q]: [1.7, 0.3]}*{(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"to        : {phi[q]: [1.7, 0.3]}*{(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"", 
				"Message   : b   <----   phi[b, c]: [0.9, 0.8, 0.1, 0.2]", 
				"Simplified: {(on c) phi[b, c]: [0.9, 0.8, 0.1, 0.2]}", 
				"to        : {(on c) phi[b, c]: [0.9, 0.8, 0.1, 0.2]}", 
				"", 
				"Message   : phi[q, b]: [0.4, 0.3, 0.6, 0.7]   <----   b", 
				"Simplified: {(on c) phi[b, c]: [0.9, 0.8, 0.1, 0.2]}", 
				"to        : {(on c) phi[b, c]: [0.9, 0.8, 0.1, 0.2]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Simplified: {(on c) phi[c, q]: [0.39, 0.61, 0.38, 0.62]}", 
				"to        : {(on c) phi[c, q]: [0.39, 0.61, 0.38, 0.62]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Updated   : {phi[q]: [1.7, 0.3]}", 
				"to        : {phi[q]: [1.7, 0.3]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Simplified: {phi[q]: [1.7, 0.3]}", 
				"to        : {phi[q]: [1.7, 0.3]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Updated   : {(on c) phi[c, q]: [0.39, 0.61, 0.38, 0.62]}", 
				"to        : {(on c) phi[c, q]: [0.39, 0.61, 0.38, 0.62]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Simplified: {(on c) phi[c, q]: [0.39, 0.61, 0.38, 0.62]}", 
				"to        : {(on c) phi[c, q]: [0.39, 0.61, 0.38, 0.62]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {phi[q]: [1.7, 0.3]}*{(on c) phi[c, q]: [0.39, 0.61, 0.38, 0.62]}", 
				"to        : {phi[q]: [1.7, 0.3]}*{(on c) phi[c, q]: [0.39, 0.61, 0.38, 0.62]}", 
				"", 
				"Message   : phi[b, c]: [0.9, 0.8, 0.1, 0.2]   <----   c", 
				"Simplified: {1}", 
				"to        : {1}", 
				"", 
				"Message   : b   <----   phi[b, c]: [0.9, 0.8, 0.1, 0.2]", 
				"Simplified: {phi[b]: [1.7, 0.3]}", 
				"to        : {phi[b]: [1.7, 0.3]}", 
				"", 
				"Message   : phi[q, b]: [0.4, 0.3, 0.6, 0.7]   <----   b", 
				"Simplified: {phi[b]: [1.7, 0.3]}", 
				"to        : {phi[b]: [1.7, 0.3]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Simplified: {phi[q]: [0.77, 1.23]}", 
				"to        : {phi[q]: [0.77, 1.23]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Updated   : {phi[q]: [1.7, 0.3]}", 
				"to        : {phi[q]: [1.7, 0.3]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Simplified: {phi[q]: [1.7, 0.3]}", 
				"to        : {phi[q]: [1.7, 0.3]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Updated   : {phi[q]: [0.77, 1.23]}", 
				"to        : {phi[q]: [0.77, 1.23]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Simplified: {phi[q]: [0.77, 1.23]}", 
				"to        : {phi[q]: [0.77, 1.23]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {phi[q]: [1.31, 0.37]}", 
				"to        : {phi[q]: [1.31, 0.37]}"
				);
		
		runTest(query, factorNetwork, expectedHistory);
	}

	@Test
	public void testCycle() {
		
		FactorNetwork factorNetwork;
		Variable query;
		
		TableVariable q = new TableVariable("q", 2);
		TableVariable a = new TableVariable("a", 2);
		TableVariable b = new TableVariable("b", 2);
		TableVariable c = new TableVariable("c", 2);
		
		factorNetwork = new DefaultFactorNetwork(
				arrayTableFactor(
						list(q,a), 
						(vq, va) -> 
						vq == 0? 
								va == 1? 0.8 : 0.2 :  
								va == 1? 0.9 : 0.1),
				arrayTableFactor(
						list(a, b), 
						(va, vb) -> 
						vb == 0? 
								va == 1? 0.8 : 0.2 :  
								va == 1? 0.9 : 0.1),
				arrayTableFactor(
						list(b, c), 
						(vb, vc) -> 
						vc == 0? 
								vb == 1? 0.8 : 0.2 :  
								vb == 1? 0.9 : 0.1),
				arrayTableFactor(
						list(c, a), 
						(vc, va) -> 
						va == 0? 
								vc == 1? 0.8 : 0.2 :  
								vc == 1? 0.9 : 0.1)
				);
		
		query = q;
		
		var expectedHistory = list(
				"", 
				"Message   : q   <----   phi[q, a]: [0.2, 0.8, 0.1, 0.9]", 
				"Simplified: {(on a) phi[q, a]: [0.2, 0.8, 0.1, 0.9]}", 
				"to        : {(on a) phi[q, a]: [0.2, 0.8, 0.1, 0.9]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {(on a) phi[q, a]: [0.2, 0.8, 0.1, 0.9]}", 
				"to        : {(on a) phi[q, a]: [0.2, 0.8, 0.1, 0.9]}", 
				"", 
				"Message   : a   <----   phi[a, b]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {(on b) phi[a, b]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on b) phi[a, b]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[q, a]: [0.2, 0.8, 0.1, 0.9]   <----   a", 
				"Simplified: {(on b) phi[a, b]: [0.2, 0.1, 0.8, 0.9]}*{(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on b) phi[a, b]: [0.2, 0.1, 0.8, 0.9]}*{(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.2, 0.8, 0.1, 0.9]", 
				"Simplified: {(on b, c) phi[b, c, q]: [0.07, 0.08, 0.61, 0.66, 0.08, 0.08, 0.66, 0.74]}", 
				"to        : {(on b, c) phi[b, c, q]: [0.07, 0.08, 0.61, 0.66, 0.08, 0.08, 0.66, 0.74]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {(on b, c) phi[b, c, q]: [0.07, 0.08, 0.61, 0.66, 0.08, 0.08, 0.66, 0.74]}", 
				"to        : {(on b, c) phi[b, c, q]: [0.07, 0.08, 0.61, 0.66, 0.08, 0.08, 0.66, 0.74]}", 
				"", 
				"Message   : b   <----   phi[b, c]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {phi[b, c]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {phi[b, c]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b]: [0.2, 0.1, 0.8, 0.9]   <----   b", 
				"Simplified: {phi[b, c]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {phi[b, c]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : a   <----   phi[a, b]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {phi[c, a]: [0.12, 0.88, 0.11, 0.89]}", 
				"to        : {phi[c, a]: [0.12, 0.88, 0.11, 0.89]}", 
				"", 
				"Message   : a   <----   phi[a, b]: [0.2, 0.1, 0.8, 0.9]", 
				"Updated   : {phi[c, a]: [0.12, 0.88, 0.11, 0.89]}", 
				"to        : {phi[c, a]: [0.12, 0.88, 0.11, 0.89]}", 
				"", 
				"Message   : a   <----   phi[a, b]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {phi[c, a]: [0.12, 0.88, 0.11, 0.89]}", 
				"to        : {phi[c, a]: [0.12, 0.88, 0.11, 0.89]}", 
				"", 
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]", 
				"Updated   : {(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : Simplex(c)*{phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: Simplex(c)*{phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : Simplex(c)*{phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[q, a]: [0.2, 0.8, 0.1, 0.9]   <----   a", 
				"Simplified: {(on c) phi[c, a]: [0.02, 0.09, 0.09, 0.8]}", 
				"to        : {(on c) phi[c, a]: [0.02, 0.09, 0.09, 0.8]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.2, 0.8, 0.1, 0.9]", 
				"Simplified: {(on c) phi[c, q]: [0.08, 0.08, 0.66, 0.73]}", 
				"to        : {(on c) phi[c, q]: [0.08, 0.08, 0.66, 0.73]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {(on c) phi[c, q]: [0.08, 0.08, 0.66, 0.73]}", 
				"to        : {(on c) phi[c, q]: [0.08, 0.08, 0.66, 0.73]}", 
				"", 
				"Message   : phi[c, a]: [0.2, 0.1, 0.8, 0.9]   <----   c", 
				"Simplified: {1}", 
				"to        : {1}", 
				"", 
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : a   <----   phi[a, b]: [0.2, 0.1, 0.8, 0.9]", 
				"Updated   : {phi[c, a]: [0.12, 0.88, 0.11, 0.89]}", 
				"to        : {phi[c, a]: [0.12, 0.88, 0.11, 0.89]}", 
				"", 
				"Message   : a   <----   phi[a, b]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {phi[c, a]: [0.12, 0.88, 0.11, 0.89]}", 
				"to        : {phi[c, a]: [0.12, 0.88, 0.11, 0.89]}", 
				"", 
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]", 
				"Updated   : {phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[q, a]: [0.2, 0.8, 0.1, 0.9]   <----   a", 
				"Simplified: {phi[a]: [0.11, 0.89]}", 
				"to        : {phi[a]: [0.11, 0.89]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.2, 0.8, 0.1, 0.9]", 
				"Simplified: {phi[q]: [0.73, 0.81]}", 
				"to        : {phi[q]: [0.73, 0.81]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {phi[q]: [0.73, 0.81]}", 
				"to        : {phi[q]: [0.73, 0.81]}"
				);
		
		runTest(query, factorNetwork, expectedHistory);
	}

	private void runTest(Variable query, FactorNetwork factorNetwork, List<String> expectedHistory) {
		println("Exact: ", new ExactBPSolver().apply(query, factorNetwork).normalize());
		
		Trace simplificationTrace = new Trace();
		
		var it = new AnytimeExactBPSolverWithTracing(simplificationTrace).apply(query, factorNetwork);
		
		while (it.hasNext()) {
			var polytope = (Polytope) it.next();
			var atomicPolytope = polytope.getEquivalentAtomicPolytope();
			var normalized = atomicPolytope.normalize(list(query));
			println(normalized + ", " + round(normalized.length(), 3));
		}
		
		println("Simplification trace:");
		println(join("\n", simplificationTrace.history));
		
		assertEquals(expectedHistory, simplificationTrace.history);
	}

	private static class AnytimeExactBPWithTracing<RootType, SubRootType> 
	extends AnytimeExactBP<RootType, SubRootType> {
	
		private Trace trace;
	
		public AnytimeExactBPWithTracing(ExactBPNode<RootType, SubRootType> base, Trace trace) {
			super(base);
			this.trace = trace;
		}
		
		@Override
		protected
		<RootType2, SubRootType2>
		AnytimeExactBPWithTracing<RootType2,SubRootType2> newInstance(ExactBPNode<RootType2,SubRootType2> base) {
			return new AnytimeExactBPWithTracing<RootType2,SubRootType2>(base, trace);
		}
		
		@Override
		protected Approximation<Factor> simplify(Approximation<Factor> approximation) {
			var result = super.simplify(approximation);
			var root = getBase().getRoot();
			var parentifAny = getBase().getParent() != null? getBase().getParent() : "";
			trace.accept("");
			trace.accept("Message   : " + parentifAny + "   <----   " + root);
			trace.accept("Simplified: " + approximation);
			trace.accept("to        : " + result);
			return result;
		}
	
		@Override
		protected 
		Approximation<Factor> 
		computeUpdatedByItselfApproximationGivenThatExternalContextHasChanged(Approximation<Factor> currentApproximation) {
			var result = super.computeUpdatedByItselfApproximationGivenThatExternalContextHasChanged(currentApproximation);
			var root = getBase().getRoot();
			var parentifAny = getBase().getParent() != null? getBase().getParent() : "";
			trace.accept("");
			trace.accept("Message   : " + parentifAny + "   <----   " + root);
			trace.accept("Updated   : " + currentApproximation);
			trace.accept("to        : " + result);
			return result;
		}
	}

	private static class AnytimeExactBPSolverWithTracing
	extends AnytimeExactBPSolver {
		
		Trace trace;
	
		public AnytimeExactBPSolverWithTracing(Trace trace) {
			this.trace = trace;
		}
	
		@Override
		protected AnytimeExactBP<Variable, Factor> makeAnytimeExactBP(ExactBP exactBP) {
			return new AnytimeExactBPWithTracing<>(exactBP, trace);
		}
		
	}

	private static class Trace implements Consumer<String> {
		public List<String> history;
		
		public Trace() {
			this.history = list();
		}
		
		@Override
		public void accept(String item) {
			history.add(item);
		}
	}	
}
