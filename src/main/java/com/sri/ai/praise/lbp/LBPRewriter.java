/*
 * Copyright (c) 2013, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-praise nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.praise.lbp;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * <b>Rewriters for Lifted First-order Probabilistic Belief Propagation</b><br>
 * Preliminaries<br>
 * Syntax: the set of expressions used is recursively defined as follows:<br>
 * <ul>
 * <li>constants and variables (referred generically to as symbols) are
 * expressions.</li>
 * <li>if f is an expression and t1,...,tn are expressions, the function
 * application f(t1,...,tn) is an expression. Note that constants and symbols
 * can be seen as function applications in which n is 0.</li>
 * <li>if t1,...,tn are expressions, the extensional set definition {t1,...,tn}
 * is an expression.</li>
 * <li>if Alpha is an expression, C is an expression of a boolean type, and Ind
 * is a tuple of function applications, the intensional (multi) set definition {
 * Alpha | C }_Ind is an expression. Multisets are denoted with double curly
 * brackets ({{ Alpha | C }}_I ).</li>
 * <li>if t is a function application of the type v(t1,...,tn), where v is a
 * random variable symbol (a subset of all symbols defined so), the random
 * variable definition [ t ] is an expression.</li>
 * <li>if t is an expression, the factor definition [ t ] is an expression.</li>
 * </ul>
 * Semantics: we extend a base interpretation function I for constants to other
 * types of expressions in the following manner:<br>
 * <ul>
 * <li>if f is an expression and t1,...,tn are expressions, the function
 * application f(t1,...,tn) is interpreted as I(f)(I(t1),...,I(tn)).</li>
 * <li>if t1,...,tn are expressions, the extensional set definition {t1,...,tn}
 * is interpreted as the set {I(t1),...,I(tn)}.</li>
 * <li>if Alpha is an expression, C is an expression of a boolean type, and Ind
 * is a tuple of function applications, the intensional (multi) set definition {
 * Alpha | C }_Ind is interpreted as the (multi) set containing elements
 * Alpha[Ind/v], for each v an assignment to Ind such that I(C[Ind/v]) is true.</li>
 * <li>if t is a function application of the type v(t1,...,tn), where v is a
 * random variable symbol (a subset of all symbols defined so), the random
 * variable definition [ t ] is interpreted as a random variable X_{v,
 * I(t1),...,I(tn)}.</li>
 * <li>if t is an expression, the factor definition [ t ] is interpreted as the
 * factor (V, lambda t1,...,tn t), where t1,...,tn are the random variable value
 * expressions occurring in t and V is the set of random variables [ t_i ] for
 * each t_i.</li>
 * </ul>
 * <p>
 * Summations (sum_i ...), products (prod_i ...), universal and existential
 * quantifications are simply addition, multiplication, conjunction and
 * disjunction operations on intensionally defined multisets). |C|_I, where C is
 * a boolean formula, is short for | {{ true | C }}_I |, and it means the number
 * of solutions (satisfying assignments on I) for C.
 * </p>
 * <p>
 * Expressions and meta-expressions. This document describes a series of
 * expression rewriters, that is, functions taking expressions as input and
 * outputing expressions. Therefore, they are meta-functions, and, in order to
 * describe them, we use meta-variables and other meta-functions that represent
 * and operate on expressions. In future latex documents, these two levels will
 * be more clearly indicated by using different fonts for different levels. At
 * this point, the reader is supposed to distinguish between them, by using the
 * context and knowledge of the subject. For example, when we write prod_F
 * m_V<-F, F is a regular variable, not meta-variable, because it is the index
 * of the product. However, when I define a rewriter on m_V<-F, V and F are
 * meta-variables, for they may be standing for expressions representing a
 * variable and factor respectively (for example, m_[p(X)]<-[ if p(X) then 1
 * else 0 ]).
 * </p>
 * <p>
 * "Conditionals". When we say that an expression is a "conditional something,"
 * we mean that it is either a "something", or an if then else expression with
 * branches being another “conditional something.” Two examples of conditional
 * numbers are 10, and if X=a then 10 else if X=b then 1 else 5. Two examples of
 * conditional sets are { f(X) | X !=a }_X and if Y=c then {} else { g(X) | X!=d
 * }_X.
 * </p>
 * <p>
 * Similarly, when an expression is said to be a union of sets, it is either an
 * application of the union operator to a sequence of sets, or a single set.
 * </p>
 * <p>
 * A set is said to be normalized if it is intensionally defined, or a multiset,
 * or, if it is an extensionally defined uniset {a_1,...,a_n}, then a_i and a_j
 * are known to be distinct for every i, j in {1,...,n}.
 * </p>
 * <p>
 * "Externalizes conditionals". A rewriter is said to "externalize conditionals"
 * if it deals with conditional argument expressions by simply being applied to
 * its then and else branches. For example, a rewriter R+ for a + operation
 * externalizes conditionals: this means R+((if X then 1 else 3) + 4) is the
 * same as if X then R+(1 + 4) else R+(3 + 4), which is if X then 5 else 7.
 * Indicating that a rewriter externalizes conditionals is simply a shortcut. We
 * could instead write down all the cases in which arguments are conditionals,
 * indicating how they are to be rewritten, but this would be tedious,
 * redundant, obfuscate the presentation, and be less effective. A rewriter
 * might externalize conditionals only on certain parts of its input. These
 * cases will be explicitly indicated. When rewriters are invoked under a
 * particular set of conditions, those conditions must be included as part of
 * their contextual constraints (along with whatever contextual constraint
 * already existed). The most typical example of this is during the
 * externalization of if-then-elses. For example, if a rewriter R is being
 * applied to an expression if Condition then A else B, with contextual
 * constraint C, then the if-then-else externalization will invoke R on A with
 * contextual constraint Condition and C, and will invoke R on B with contextual
 * constraint not Condition and C.
 * </p>
 * <p>
 * Contextual constraints. Meta-functions (rewriters included) take expressions
 * as parameters, often with free variables. It may be that the user wants to
 * provide a constraint on the values of these free variables, which we can call
 * a contextual constraint, or simply “context” (i.e. a boolean formula). For
 * this reason, meta-functions should also take a contextual constraint as a
 * parameter. If during processing a meta-function F determines further
 * constraints on the free variables before invoking another meta-function G,
 * the contextual constraint in the call to G should be a conjunction of the
 * original context and the new conditions. Conditions present in the arguments
 * are assumed to have already been simplified by the contextual constraint, so
 * there is no need to do that simplification inside a function. For the sake of
 * keeping things readable, we do not explicitly include the contextual
 * constraint in the calls to meta-functions. In this case, it is assumed that
 * the the current context is passed along to the meta-function. It is only when
 * new constraints are defined that we explicitly indicate the context being
 * passed in a meta-function call. Analogously, if a meta-function does not make
 * use of the contextual constraint in any way other than passing it unchanged
 * to other meta-functions, we do not explicitly include it in its parameter
 * list.
 * </p>
 * 
 * @author oreilly
 * 
 */
@Beta
public interface LBPRewriter extends Rewriter {	
	
	/**
	 * The name space that the LBP rewriters belong to.
	 */
	String LBP_NAMESPACE = "lbp.";
	
	/**
	 * <b>R_basic(E)</b>, A rewriter based on the exhaustive application of
	 * rewriters operating on "basic" operations. It aims at simplifying an
	 * expression as much as possible by performing the operations on constants
	 * that can be replaced by the result.
	 */
	String R_basic = LBP_NAMESPACE+"R_basic";
	
	/**
	 * <pre>
	 * R_belief(belief(V))
	 * V is a random variable
	 * Returns the belief of V according to the current (implicitly defined) model.
	 * 
	 * beingComputed    <- empty set
	 * belief_expansion <- R_prod_factor(prod_F in R_neigh_v(Neigh(V)) m_V<-F, beingComputed)
	 * belief_expansion <- R_complete_normalize(belief_expansion)
	 * if belief_expansion does not contain 'previous message' expressions
	 *         return R_normalize_message(V, belief_expansion)
	 *         
	 * // At this point, belief_expansion is a basic expression containing 'previous message' expressions
	 * 
	 * msg_sets <- R_extract_previous_msg_sets(belief_expansion)
	 * // the above is a union of sets of pairs (N1, N2), with each pair
	 * // representing an occurrence of "previous message" on that pair occurring in belief_expansion.
	 * // We call these pairs "message pairs".
	 * // The reason we use pairs of nodes instead of "message to N1 from N2" is that this will make
	 * // these sets be sets of, ultimately, numbers. This is not what we really want because we want a record 
	 * // of which messages on which nodes to compute, not their values. This becomes essential when we 
	 * // compute differences of these sets in order to decide which messages from a certain previous 
	 * // message iteration need to be computed, given that we have already computed a subset of them. 
	 * // That is to say, these sets are a record of the computations that need to be  
	 * // done, as opposed to their results.
	 * 
	 * msg_expansions <- get_msg_expansions(msg_sets)
	 * // the above is a union of intensional sets containing tuples of the form
	 * // (Destination, Origin, Expansion), where Expansion is a basic expression
	 * // possibly containing "previous message" expressions, representing their value in terms of messages
	 * // from the previous loopy BP iteration.
	 * 
	 * msg_values <- copy of msg_expansions, with the values replaced by uniform messages
	 * // msg_values is a union of sets of tuples (N1, N2, value) where (N1,N2) represents a message and value
	 * // is a basic expression representing its value at the current loopy BP iteration (this expression is 
	 * // therefore free of previous message expressions).
	 * 
	 * // We now use the expansions to get the values of message pairs in successive loopy BP iterations
	 * // until either convergence or maximum number of iterations.
	 * while belief_value <- use_values_for_previous_msgs(belief_expansion, msg_values) not final
	 *     msg_values <- iterate_values_using_expansions(msg_values, msg_expansions)
	 *         
	 * return R_normalize_message(V, belief_value)
	 * </pre>
	 */
	String R_belief = LBP_NAMESPACE+"R_belief";
	
	String R_bound_belief = LBP_NAMESPACE+"R_bound_belief";
	String R_bound_m_to_f_from_v = LBP_NAMESPACE+"R_bound_m_to_f_from_v";
	String R_bound_m_to_v_from_f = LBP_NAMESPACE+"R_bound_m_to_v_from_f";
	String R_bound_prod_factor = LBP_NAMESPACE+"R_bound_prod_factor";
	String R_bound_prod_m_and_prod_factor = LBP_NAMESPACE+"R_bound_prod_m_and_prod_factor";
	String R_bound_sum = LBP_NAMESPACE+"R_bound_sum";
	
	/**
	 * A rewriter to be used to check if a branch is reachable when calling
	 * branch and merge logic.
	 */
	String R_check_branch_reachable = LBP_NAMESPACE+"R_complete_normalize";
	
	/**
	 * <pre>
	 * R_DifferenceOfExtensionalAndExtensionalSet({a_1,...,a_n}, {b_1,...,b_m}, i, j)
	 * {a_1,...,a_n} and {b_1,...,b_m} are normalized sets.
	 * For k < i, a_k is assumed not to unify with any b_1,...,b_m
	 * a_i is assumed not to unify with any b_1,...,b_j-1
	 * Returns a conditional extensional set equivalent to the difference of the two given sets
	 * Cases:
	 * if i > n
	 *     return {a_1,...,a_n}
	 * if j > m
	 *     return R_DifferenceOfExtensionalAndExtensionalSet({a_1,...,a_i,...,a_n}, {b_1,...,b_m}, i+1, 1)
	 * 
	 * condition <- {@link #R_formula_simplification}(a_i = b_j)
	 * if true = condition
	 *     return R_DifferenceOfExtensionalAndExtensionalSet(
	 *              {a_1,...,a_i-1,a_i+1,...,an}, {b_1,...,b_j-1,b_j+1,b_m}, i, 1)
	 * if false = condition
	 *     return R_DifferenceOfExtensionalAndExtensionalSet(
	 *              {a_1,...,a_i,...,a_n}, {b_1,...,b_m}, i, j+1)
	 * return {@link #R_basic}(if condition
	 *                then R_DifferenceOfExtensionalAndExtensionalSet(
	 *                       {a_1,...,a_i-1,a_i+1,...,a_n}, {b_1,...,b_m}, i, 1)
	 *                else R_DifferenceOfExtensionalAndExtensionalSet(
	 *                       {a_1,...,a_i,...,a_n}, {b_1,...,b_m}, i, j+1))
	 * </pre> 
	 */
	String R_DifferenceOfExtensionalAndExtensionalSet = LBP_NAMESPACE+"R_DifferenceOfExtensionalAndExtensionalSet";
	
	/**
	 * <pre>
	 * R_DifferenceOfExtensionalAndIntensionalSet({a_1,...,a_n}, { Alpha | C }_I, i)
	 * For k < i, C and a_k = Alpha is always false
	 * Returns a conditional set equivalent to the difference of the two sets
	 * Cases:
	 * if i > n
	 *     return {a_1,...,a_n}
	 * { Alpha' | C' }_I' <- standardize { Alpha | C }_I apart from {a_1,...,a_n}
	 * condition <- {@link #R_formula_simplification}(there exists I' : C' and a_i = Alpha')
	 * return {@link #R_normalize}( 
	 *          if condition
	 *          then R_DifferenceOfExtensionalAndIntensionalSet({a_1,...,a_i-1,a_i+1,...,a_n}, { Alpha' | C' }_I', i)
	 *          else R_DifferenceOfExtensionalAndIntensionalSet({a_1,...,a_n}, { Alpha' | C' }_I', i+1))
	 * </pre>
	 */
	String R_DifferenceOfExtensionalAndIntensionalSet = LBP_NAMESPACE+"R_DifferenceOfExtensionalAndIntensionalSet";
	
	/**
	 * <pre>
	 * R_extract_previous_msg_sets(E)
	 * E is a basic expression
	 * Returns a union of sets, each set containing the instances of one message pair 
	 * (the pair (destination, origin) corresponding to a message) occurring in E 
	 * as a "previous message" expression, including the constraints on their free 
	 * variables.
	 * 
	 * if E is previous message to N1 from N2
	 *     return { (N1, N2) | C } where C is the current contextual constraint
	 * else
	 *     let E1, ..., En be the sub-expressions of E
	 *     let Ci be the conditions E imposes on the free variables of Ei
	 *     // if E is a basic expression, Ci will be its condition and negation if E is an
	 *     // if-then-else, and true otherwise
	 *     return R_extract_previous_msg_sets(E1) under cont. constraint extended by C1
	 *                union ... union
	 *            R_extract_previous_msg_sets(En) under cont. constraint extended by Cn
	 * </pre>
	 * 
	 * For example, if E is:
	 * 
	 * <pre>
	 * if X != a then previous message to [p(X)] from [ Alpha ]
	 *           else previous message to [p(a)] from [ Beta ]
	 * </pre>
	 * 
	 * The union of sets will be:
	 * 
	 * <pre>
	 * { (on X) ([p(X)], [Alpha]) | X != a } union
	 * { ([p(a)], [Beta]) | X = a }
	 * </pre>
	 */
	String R_extract_previous_msg_sets = LBP_NAMESPACE+"R_extract_previous_msg_sets";
	
	/**
	 * <pre>
	 * R_formula_simplification(E)
	 * This is the top rewriter for equality boolean formulas simplification, described elsewhere.
	 * It includes the fact that random variable expressions are bijective:
	 * replace all subexpressions of the form [ v(t1, ..., tn) ] = [ v'(r1, ..., rm) ]
	 *         by R_formula_simplification(v = v' and n = m and t1 = r1 and ... tn = rm)
	 * </pre>
	 */
	String R_formula_simplification = LBP_NAMESPACE+"R_formula_simplification";
	
	/**
	 * <pre>
	 * R_in(Alpha, Set)
	 * Returns a condition under which Alpha belongs to Set (a conditional union of sets).
	 * Externalizes conditionals.
	 * Cases:
	 * if Set is Set_1 union ... union Set_n
	 *     return {@link #R_formula_simplification}(Disjunction_i R_in(Alpha, Set_i))
	 *     // Disjunction here is a meta, preferably short-circuited, operation.
	 * if Set is { a1,..., an } or {{ a1,..., an }}
	 *     return {@link #R_formula_simplification}(Disjunction_i Alpha = ai)
	 *     // Disjunction here is a meta, preferably short-circuited, operation.
	 * if Set is { Beta | C }_I or {{ Beta | C }}_I (1)
	 *     { Beta' | C' }_I' <- standardize { Beta | C }_I apart from Alpha
	 *     return {@link #R_basic}(there exists I' : C' and Alpha = Beta')
	 *     
	 * 
     * Implementation Note:
     * (1) CheapDisequalityModule.isACheapDisequality() can be used to cheaply check whether 
     *     Alpha != Beta, in which case 'false' should be returned.
	 * </pre>
	 */
	String R_in = LBP_NAMESPACE+"R_in";
	
	/**
	 * <pre>
	 * R_intensional_simplification( { Alpha | C }_I ) // holds for multisets in the same manner
	 * Returns an equivalent conditional set with possibly simpler descriptions.
	 * if C is false
	 *     return empty_set
	 * if I is empty
	 *     return if C then { Alpha } else empty_set
	 * if C is (C' and i = Beta) for i an index in I // We are assuming that Beta is in the range of i.
	 *     return R_intensional_simplifications({ Alpha[i/Beta] | C'[i/Beta] }_{I \ {i}})
	 * </pre>
	 */
	String R_intensional_simplification = LBP_NAMESPACE+"R_intensional_simplification";
	
	/**
	 * <pre>
	 * R_intersection(Set1 intersection Set2)
	 * Set1 and Set2 are sets
	 * Returns the intersection set of Set1 and Set2, guaranteed to be expression "{}" if it is empty.
	 * Cases:
	 * Set1 is { (on I1) Alpha1 | C1 } and Set2 is { (on I2) Alpha2 | C2 } (or multiset version) (1)
	 *     standardize Set1 apart from (I2, Alpha2, C2)
	 *     C <- R_complete_normalize(Alpha1 = Alpha2 and C1 and C2)
	 *     if C is "false"
	 *         return {}
	 *     I <- concatenation of I1 and I2
	 *     return { (on I) Alpha1 | C } (or multiset version)
	 * Set1 is {...} and Set2 is {...}
	 *     return R_set_diff(Set1 \ R_set_diff(Set1 \ Set2))
	 * Else
	 *     "Not currently supported"
	 *     
	 * Implementation Note:
     * (1) CheapDisequalityModule.isACheapDisequality() can be used to cheaply check whether 
     *     Alpha1 != Alpha2, in which case {} should be returned. 
	 * </pre>
	 */
	String R_intersection = LBP_NAMESPACE+"R_intersection";
	
	/**
	 * <pre>
	 * R_m_to_f_from_v(m_F<-V, beingComputed)
	 * V is a random variable, F a factor, beingComputed is a conditional union of sets of pairs of nodes indicating 
	 * which messages depend on this computation.
	 * Returns a conditional message on V equivalent to
	 * if (F,V) in beingComputed
	 *         then pm_F<-V
	 *         else prod_{F' in Neigh(V) \ {F}} m_V<-F'
	 * where pm_F<-V stands for the value of m_F<-V in the previous iteration of loopy BP.
	 * 
	 * if asynchronous schedule
	 *     In <- {@link #R_in}((V,F) in beingComputed)
	 * else // synchronous
	 *     In <- beingComputed is NOT empty
	 *
	 * if asynchronous schedule
	 *     In <- R_in((V,F) in beingComputed)
	 * else // synchronous
	 *     In <- beingComputed is NOT empty
	 * 
	 * under contextual constraint extended by 'not In'
	 *     neighbors_minus_F <- {@link #R_set_diff}({@link #R_neigh_v}(Neigh(V))\{F})
	 *     M <- {@link #R_prod_factor}(prod_F' in neighbors_minus_F m_V<-F', {@link #R_basic}(beingComputed union {{(F,V)}})))
	 *     M <- {@link #R_normalize_random_variable_condition}(V, M)
	 * 
	 * return {@link #R_basic}(if In then pm_F<-V else M)
	 * </pre>
	 */
	String R_m_to_f_from_v = LBP_NAMESPACE+"R_m_to_f_from_v";
	
	/**
	 * <pre>
	 * R_m_to_v_from_f(m_V<-F, C, I, beingComputed)
	 * m_V<-F is a message from a factor to a random variable.
	 * C is optional (defaults to true), which if provided represents the constraint on the the intensional 
	 * set from which the representative factor F was selected.
	 * I is optional (defaults to (on )), which if provided represents the scoping expression on the 
	 * intensional set from which the representative factor F was selected.
	 * beingComputed is a conditional union of sets of pairs of nodes (random variables and factors),
	 * representing the messages that depend on the calculation of this message, which allows us to detect cycles.
	 * Returns a conditional message on V representing the message that would be arriving at V from F in loopy BP.
	 * Therefore, part of the conditional message may be the function pm_V<-F, which stands for the value of the 
	 * message in a previous iteration of loopy BP.
	 * More specifically, the conditional message returned is equivalent to
	 * if (V,F) in beingComputed
	 *         then pm_V_F
	 *         else sum_{V' in Neigh(F) \ {V} } value(F) prod_{V' in Neigh(F) \ {V} } m_F<-V'
	 * where pm_F<-V stands for the value of m_F<-V in the previous iteration of loopy BP.
	 * 
	 * if asynchronous individual schedule
	 *     In <- {@link #R_in}((V,F) in beingComputed)
	 *     beingComputed <- {@link #R_basic}(beingComputed union {{(V,F)}} )
	 * else if asynchronous group schedule
	 *     In <- {@link #R_in}((V,F) in beingComputed)
	 *     In <- {@link #R_complete_normalize}(In)
	 *     beingComputed <- beingComputed union {{(V,F) | C}}_I
	 * else // synchronous
	 *     In <- beingComputed is NOT empty
	 *     beingComputed <- {@link #R_basic}(beingComputed union {{(V,F)}} ) 
	 *     
	 * under contextual constraint incremented by 'not In'
	 *     N  <- {@link #R_set_diff}({@link #neighborsFactor(Expression, RewritingProcess) R_neigh_f}(Neigh(F)) \ V)
	 *     M  <- {@link #R_sum}(sum_N value(F) prod_{V' in N} m_F<-V', V, beingComputed)
	 *     M  <- {@link #R_normalize_random_variable_condition}(V, M)
	 * return {@link #R_basic}(if In then pm_V<-F else M)
	 * // We here use a special notation sum_N, where N is a set of random variables.
	 * // This means the sum is indexed by the random variables values.
	 * // For example, sum_{  { [p(X)], [q(Y)] }  } Alpha is the same as sum_{p(X), q(Y)} Alpha
	 * </pre>
	 */
	String R_m_to_v_from_f = LBP_NAMESPACE+"R_m_to_v_from_f";
	
	/**
	 * <pre>
	 * R_neigh_f(Neigh(F))
	 * Returns a normalized extensional uniset of random variables connected to a given factor F in the form [ Ef ].
	 * 
	 * return {@link #R_basic}({ v1, ..., vn })
	 *         where v1, ..., vn are the subexpressions of Ef that are random variable value expressions.
	 * </pre>
	 */
	String R_neigh_f = LBP_NAMESPACE+"R_neigh_f";
	
	/**
	 * <pre>
	 * R_neigh_v_parf( [ Ev ], PF)
	 * [ Ev ] a random variable, PF a parfactor (that is, a set of factors)
	 * // we could have used V and F, but writing it so makes the expressions 'inside' promptly available to us.
	 * Returns the set of neighbors of [ Ev ] in the given set of factors
	 * if PF is { [Ef_1], ..., [Ef_n]  } where [Ef_i] are factor expressions
	 *     Let Fi be the expression
	 *     if {@link ALBPRewriterUtil#randomVariableIsReferencedByExpression rv_is_referenced_by}([Ev], Ef_i)
	 *         then { [ Ef_i ] }
	 *         else {}
	 *     return {@link #R_basic}(F1 union ... union F_n)
	 * if PF is an intensionally defined set {{ [ Ef ] | C }}_I) (where [ Ef ] is a factor expression)
	 *     {{ [ Ef' ] | C' }}_I' <- standardize {{ [ Ef ] | C }}_I apart from [ Ev ]
	 *     Extend contextual symbols with I'
	 *     return {@link #R_intensional_simplification}(
	 *                     {{ [ Ef' ] | {@link #R_formula_simplification}(C' and {@link ALBPRewriterUtil#randomVariableIsReferencedByExpression rv_is_referenced_by}([Ev], Ef')) }}_I')
	 * </pre>
	 */
	String R_neigh_v_parf = LBP_NAMESPACE+"R_neigh_v_parf";
	
	/**
	 * <pre>
	 * R_neigh_v(Neigh(V))
	 *     return {@link #R_union}({@link #R_neigh_v_parf}(V,PF_1)
	 *                    union ... union
	 *                    {@link #R_neigh_v_parf}(V,PF_n))
	 * 
	 *     where (PF_i)_i are the parfactors in the model
	 *     (extensionally or intensionally defined sets of factors)
	 *     Returns a conditional union of singleton or intensionally defined multisets of factors
	 *     describing the neighbors of V.
	 * </pre>
	 */
	String R_neigh_v = LBP_NAMESPACE+"R_neigh_v";
	
	/**
	 * <pre>
	 * R_normalize_message([ v ], E)
	 * [ v ] is a boolean random variable
	 * E is a conditional arithmetic expression
	 * E has logical variables conditions
	 * on top of, and separated from, random variable conditions.
	 * Random variables are always instances of the
	 * random variable on which to normalize
	 * (that is, other random variables are not supposed to be present).
	 * Returns an expression representing a conditional normalized message over the random variable value v.
	 * Externalizes conditionals
	 * if E is ‘if v then Alpha else Beta’
	 *     if Alpha = Beta = 0
	 *         error: 'Cannot normalize message with partition 0'
	 *     if Alpha = 0
	 *         return if v then 0 else 1
	 *     if Beta = 0
	 *         return if v then 1 else 0
	 * 	   Z = R_basic(Alpha + Beta)
	 *     return if v then R_basic(Alpha/Z) else R_basic(Beta/Z)
	 * otherwise // not a conditional on v
	 * 	   return 0.5
	 * </pre>
	 */
	String R_normalize_message = LBP_NAMESPACE+"R_normalize_message";
	
	/**
	 * <pre>
	 * R_normalize_random_variable_condition([ v ], E)
	 * Transforms a message into an equivalent expression of the form
	 * if v then <basic expression> else <basic expression>.
	 * This avoids messages represented as more complex (and sometimes redundant)
 	 * forms such as if nice(bob) then if not nice(bob)...
 	 * This is a poor man's solution to the problem. Ideally, this kind of
 	 * thing should be dealt with in the same manner constraint simplifications are,
 	 * but we don't have that yet.
	 * [ v ] is a boolean random variable
	 * E is a conditional arithmetic expression
	 * E has logical variables conditions
	 * on top of, and separated from, random variable conditions.
	 * Random variables are always instances of the
	 * random variable on which to normalize
	 * (that is, other random variables are not supposed to be present).
	 * Returns a conditional expression, the leaves of which are of the form
	 * "if v then Alpha else Beta"
	 * Note: not to be confused with R_normalize_message, which normalizes a message's values to the [0, 1] interval.
	 * Externalizes conditionals
	 * Et <- {@link #R_normalize}(E[v/true]);
	 * Ef <- {@link #R_normalize}(E[v/false]);
	 * return if v then Et else Ef
	 * </pre>
	 */
	String R_normalize_random_variable_condition = LBP_NAMESPACE+"R_normalize_random_variable_condition";
	
	/**
	 * <pre>
	 * R_prod_factor(prod_F in S m_V<-F, beingComputed)
	 * S is a conditional union of empty, singleton and multisets of factors,
	 * beingComputed is a conditional union of sets of pairs of nodes, representing messages being computed and
	 * depending on this computation.
	 * Returns an equivalent conditional message on V
	 * Externalizes conditionals
	 * Cases for input:
	 * prod_F in {} m_V<-F
	 *        return 1
	 * prod_F in {F1} m_V<-F
	 *        return {@link #R_m_to_v_from_f}(m_V<-F1, beingComputed)
	 * prod_F in {{ F1 | C }}_I m_V<-F 
	 *        message <- {@link #R_m_to_v_from_f}(m_V<-F1, C, I, beingComputed) 
	 *                under cont. constraint extended by C and contextual symbols extended by I
	 *        return {@link #R_basic}(prod_{{ (on I) message | C }})
	 * prod_F in {{F1,...,Fn}} m_V<-F
	 *        return R_prod_factor(prod_F in {F1} union {{F2,...,Fn}} m_V<-F, beingComputed)
	 * prod_F in Set union Union m_V<-F
	 *        return
	 *        {@link #R_prod_m_and_prod_factor}(
	 *                  {@link #R_prod_factor}(prod_F in Set m_V<-F, beingComputed)
	 *                  * prod_F in Union m_V<-F, beingComputed)
	 *        // R_prod_factor returns a message m on V, and R_prod_m_and_prod_factor multiplies it
	 *        // by the remaining prod_F in Union m_V<-F, computing the latter only if m is not deterministic
	 *        // (since, if it is, there is no need to compute it).
	 * </pre>
	 */
	String R_prod_factor = LBP_NAMESPACE+"R_prod_factor";
	
	/**
	 * <pre>
	 * R_prod_m_and_prod_factor(m * prod_F in S m_V<-F, beingComputed)
	 * m is a conditional message
	 * beingComputed is a conditional union of sets of pairs of nodes, representing messages being computed and
	 * depending on this computation.
	 * Externalizes conditionals
	 * Cases:
	 * m is deterministic
	 *     return m
	 * else
	 *     return {@link #R_normalize}(m * {@link #R_prod_factor}(prod_F in S m_V<-F, beingComputed))
	 * </pre>
	 */
	String R_prod_m_and_prod_factor = LBP_NAMESPACE+"R_prod_m_and_prod_factor";
	
	/**
	 * <pre>
	 * R_set_diff(S1 \ S2)
	 * S1 and S2 are conditional unions of normalized sets
	 * *Multi*set operations are complicated, so we only cover some specific cases for them:
	 *          S2 is extensional
	 *          and
	 *              S1 is an extensionally defined multiset
	 *              or
	 *              S1 is a union of multisets (possibly a single-argument union, that is a single multiset),
	 *              but each such multiset is guaranteed to have unique elements
	 *              (that is to say only the result of the union will possibly have repeated elements)
	 * Returns a conditional union of sets
	 * Externalizes conditionals
	 * Cases:
	 * if S2 is the empty set
	 *     return S1
	 * if S1 is S11 union S1rest, where S1i and S2 are unisets
	 *     return R_set_diff(S11 \ S2) union R_set_diff(S1rest \ S2)
	 * if S2 is S21 union S2rest, where S1 and S2i are sets
	 *     return R_set_diff(R_set_diff(S1 \ S21) \ S2rest)
	 * if S1 is S11 union S1rest, where each S1i is a multiset guaranteed to have unique elements,
	 *                                  or a singleton, and S2 is a singleton { b }
	 *     return
	 *     {@link #R_basic}(if {@link #R_in}(b in S11)
	 *              then R_set_diff(S11 \ S2) union S1rest
	 *              else S11 union R_set_diff(S1rest \ S2))
	 * if S1 is {{a1,...,an}} and S2 is { b }
	 *     return R_DifferenceOfExtensionalAndExtensionalSet({{a1,...,an}}, {b}, 1, 1) 
	 * if S1 is {{ Alpha | C }}_I and S2 is { b } (1)
	 *     {{ Alpha' | C' }}_I' <- standardize {{ Alpha | C}}_I apart from {b}
	 *     C'' <- {@link #R_formula_simplification}(C' and not Alpha' = b) with cont. variables extended by I'
	 *     return {@link #R_basic}({{ Alpha' | C'' }}_I')
	 * if S1 is a multiset and S2 is {b1,...,bm}
	 *     return R_set_diff(R_set_diff(S1, {b1}), {b2,...,bm})
	 * if S1 is a multiset and S2 is S21 union ... union S2m
	 *     return R_set_diff(R_set_diff(S1, S21), S22 union ... union S2m)
	 * if S1 is {a1,...,an} and S2 is {b1,...,bm}
	 *     return {@link #R_DifferenceOfExtensionalAndExtensionalSet}({a1,...,an}, {b1,...,bm}, 1, 1)
	 * if S1 is { Alpha | C }_I and S2 is {b1,...,bm}
	 *     { Alpha' | C' }_I' <- standardize { Alpha | C }_I apart from {b1,...,bm}
	 *     C'' <- {@link #R_formula_simplification}(C' and not (Disjunction_i Alpha' = b_i)) with cont. variables extended by I'
	 *     return {@link #R_basic}({ Alpha' | C'' }_I')
	 * if S1 is { Alpha | C }_I and S2 is { Alpha' | C' }_I' (2)
	 *     { Alpha' | C' }_I' <- standardize { Alpha' | C' }_I' apart from (Alpha, C)
	 *     C'' <- {@link #R_formula_simplification}(C and for all I' : C' => Alpha != Alpha') with cont. variables extended by I
	 *     return {@link #R_basic}({ Alpha | C'' }_I)
	 * if S1 is {a1,...,an} and S2 is { Alpha | C }_I
	 *     return {@link #R_DifferenceOfExtensionalAndIntensionalSet}({a1,...,an}, { Alpha | C }_I, 1)
	 * </pre>
	 * 
	 * Implementation Note:
     * (1) CheapDisequalityModule.isACheapDisequality() can be used to cheaply check whether Alpha != b, 
     *     in which case S1 should be returned.
     * (2) CheapDisequalityModule.isACheapDisequality() can be used to cheaply check whether Alpha != Alpha', 
     *     in which case S1 should be returned. 
	 * 
	 * Note: From Rodrigo as regards multisets - the cases of differences
	 * involving multisets that we actually need in ALBP are fairly restricted.
	 * We only need multiset difference when the multiset is of factors, and
	 * even so, the second set will be a singleton. The multiset (always the
	 * first operand of \ ) will either be an extensionally defined set of
	 * factors (e.g. {{ [ if p(X) then 1 else 0 ], [ if p(Y) then 1 else 0 ] }}
	 * ) or a union that may generate a multiset, but the operands of which are
	 * intensionally defined multisets guaranteed to be unisets. For example:<br>
	 * 
	 * <pre>
	 * {{ [ if p(X) then 1 else 0 ] | X != a }}_X union {{ [ if p(Y) then 1 else 0 ] | Y != b }}_Y
	 * </pre>
	 * 
	 * each of the multisets in the union will not have repeated factors because
	 * they use all random variables in the index (one in each multiset, in this
	 * case), but the resulting one will have repeated factors.
	 * 
	 * This will generally hold, but if the user is naughty and defines a
	 * parfactor like:<br>
	 * 
	 * <pre>
	 * {{ [ if p(X) then 1 else 0 ] | X != a and Y != c }}_X,Y
	 * </pre>
	 * 
	 * that is, a parfactor in which the factor template does not use all the
	 * indices, then there will be repeated factors. For now we can simply
	 * forbid users to write something like that (it's pretty nonsensical
	 * anyway), error-checking or not (that would be nice, of course). There are
	 * ways of dealing with it, but we can do it down the road.
	 */
	String R_set_diff = LBP_NAMESPACE+"R_set_diff";
	
	/**
	 * R_normalize(E)
	 */
	String R_normalize = LBP_NAMESPACE+"R_normalize";
	
	/**
	 * R_complete_normalize(E).
	 */
	String R_complete_normalize = LBP_NAMESPACE+"R_complete_normalize";

	/**
	 * R_simplify(E).<br>
	 * Interface for R_simplify(E) functionality used by R_normalize.
	 * It performs simplifications of expressions based on full or partial evaluation of known functions.
	 */
	String R_simplify = LBP_NAMESPACE+"R_simplify";

	/**
	 * R_complete_simplify(E).<br>
	 * Complete version of R_simplify(E), that is,
	 * guaranteeing that tautologies and contradictions are replaced by true and false respectively.
	 */
	String R_complete_simplify = LBP_NAMESPACE+"R_complete_simplify";

	/**
	 * <pre>	
	 * R_sum(sum_N E * prod_{V in N'} m_F<-V, T, beingComputed)
	 * N is a conditional extensionally defined set of random variables indexing the summation
	 * // (see note at end of {@link #R_m_to_v_from_f}(m_V<-F) for details)
	 * N' is a conditional extensionally defined set of random variables.
	 * For any assignment \theta to logical variables, N\theta contains N'\theta.
	 * E is a basic expression.
	 * T is the target random variable on which the message is being computed.
	 * beingComputed is a conditional union of sets of pairs of nodes, representing messages being computed and
	 * depending on this computation.
	 * Returns an equivalent conditional basic expression (an expression with numeric and boolean operators only) up to a constant.
	 * 
	 * Externalizes conditionals on N, N' and E
	 * 
	 * RV_in_E <- getRandomVariablesUsedIn(E)
	 *     
	 * N <- N intersection RV_in_E
	 * if N is the empty set
	 *     // then we know N' is also the empty set because N includes all elements in N'.
	 *     return E
	 *         
	 * N' <- N' intersection RV_in_E
	 * if N' is not the empty set
	 *     toBeSummedOut <- N'
	 * else
	 *     toBeSummedOut <- N
	 * pick V' in toBeSummedOut(V' has the form [v'])
	 *     
	 * relevantRangeSoFar = {v in range(v') : {@link #R_basic}(E[v'/v]) is not zero}
	 * if relevantRangeSoFar is {}
	 *     throw exception: model does not validate any values for V'
	 *         
	 * // We now compute M (the value of the message from V'), products (the new product of incoming messages excluding the one from V') and refine relevantRangeSoFar         
	 * if N' is not the empty set
	 *     products <- prod_{V in {@link #R_set_diff}(N'\{V'})} m_F<-V
	 *     if relevantRangeSoFar is singleton {v}
	 *         M <- 1 // message is irrelevant as value of v' will be v anyway
	 *                // this assumes the message would be consistent with E
	 *                // and put all weight on v as well,
	 *                // so it is a heuristic; for exactness, always execute else clause
	 *     else
	 *         M <- {@link #R_m_to_f_from_v}(m_F<-V', beingComputed)
	 *         if M contains previous message to F from V'
	 *              M <- (lambda v' : previous message to F from V')(v')
	 *         else
	 *              relevantRangeSoFar = {v in relevantRangeSoFar : R_basic(M[v'/v]) is not zero }
	 *              if relevantRangeSoFar is {}
	 *                   throw exception: model does not validate any values for V'
	 * else
	 *     products <- 1
	 *     M <- 1
	 *         
	 * return R_sum(sum_{{@link #R_set_diff}(N\{V'})}
	 *              {@link #R_basic}((E*M)[v'/v1] + ... + (E*M)[v'/vn])
	 *              * products,
	 *              T, beingComputed)
	 *        for relevantRangeSoFar in the form {v1,...,vn}
	 * </pre>
	 */
	String R_sum = LBP_NAMESPACE+"R_sum";
	
	/**
	 * <pre>
	 * R_union(U)
	 * U is a conditional flattened union of sets
	 * Externalizes conditionals
	 * Returns a conditional flattened union of sets not including empty sets.
	 * Cases for U:
	 * Set
	 *     return Set
	 * Set union Union'
	 *     if Set is empty set
	 *         return R_union(Union')
	 *     else return {@link #R_basic}(Set union R_union(Union'))
	 * </pre>
	 */
	String R_union = LBP_NAMESPACE+"R_union";
	
	/**
	 * <pre>
	 * R_union_of_intensional_sets_with_unifiable_heads({ (on I1) H1 | C1 }  union { (on I2) H2 | C2 } )
	 * U is a union of two intensional sets with unifiable heads
	 * Returns an intensional set equivalent to the union.
	 *     { (on I3) H3 | C3 } <- standardize { (on I2) H2 | C2 } apart from { (on I1) H1 | C1 }
	 *     return R_basic( { (on I1, I3) H1 | H1 = H3 and (C1 or C3) } )  
	 * </pre>
	 */
	String R_union_of_intensional_sets_with_unifiable_heads = LBP_NAMESPACE+"R_union_of_intensional_sets_with_unifiable_heads";
}
