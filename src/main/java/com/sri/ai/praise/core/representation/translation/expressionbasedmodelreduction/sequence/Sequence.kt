package com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.sequence

import com.sri.ai.expresso.api.Expression
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.api.ExpressionBasedModelDownReduction
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.api.ExpressionBasedModelReducer
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.api.ExpressionBasedModelReduction
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.api.ExpressionBasedModelUpReduction

class SequenceExpressionBasedModelReducer(val reducers: List<out ExpressionBasedModelReducer>)
    : ExpressionBasedModelReducer {

    constructor(vararg reducers: ExpressionBasedModelReducer): this(listOf(*reducers))

    override fun invoke(expressionBasedModel: ExpressionBasedModel): ExpressionBasedModelReduction {
        var currentExpressionBasedModel = expressionBasedModel
        val reductions = reducers.map {
            val currentReduction = it(currentExpressionBasedModel)
            currentExpressionBasedModel = currentReduction.down.translation
            currentReduction
        }
        return SequenceExpressionBasedModelReduction(expressionBasedModel, reductions)
    }

}

class SequenceExpressionBasedModelReduction(
        override val expressionBasedModel: ExpressionBasedModel,
        reductions: List<out ExpressionBasedModelReduction>)
    : ExpressionBasedModelReduction {

    override val down = SequenceExpressionBasedModelDownReduction(reductions.map { it.down })
    override val up = SequenceExpressionBasedModelUpReduction(reductions.map { it.up })
}

class SequenceExpressionBasedModelDownReduction(
        private val downReductions: List<out ExpressionBasedModelDownReduction>)
    : ExpressionBasedModelDownReduction {

    override val expressionBasedModel: ExpressionBasedModel = downReductions.first().expressionBasedModel
    override val translation: ExpressionBasedModel = downReductions.last().translation

    override fun translate(expression: Expression) =
            downReductions.fold(expression) {
                expression, elementDownReduction -> elementDownReduction.translate(expression)
            }
}

class SequenceExpressionBasedModelUpReduction(
        private val upReductions: List<out ExpressionBasedModelUpReduction>)
    : ExpressionBasedModelUpReduction {

    override val upperExpressionBasedModel = upReductions.first().upperExpressionBasedModel
    override val lowerExpressionBasedModel = upReductions.last().lowerExpressionBasedModel

    override fun translateBack(expression: Expression) =
            upReductions.foldRight(expression) {
                elementUpReduction, expression -> elementUpReduction.translateBack(expression)
            }
}