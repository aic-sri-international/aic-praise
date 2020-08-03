package com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.base

import com.sri.ai.expresso.api.Expression
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.api.ExpressionBasedModelDownReduction

abstract class AbstractExpressionBasedModelDownReduction(override val expressionBasedModel: ExpressionBasedModel)
    : ExpressionBasedModelDownReduction {

    override val translation: ExpressionBasedModel by lazy {

        val factors = expressionBasedModel.factors.map(::processAllSubExpressions)

        val mapFromRandomVariableNameToTypeName =
                processTypeNames(expressionBasedModel.mapFromRandomVariableNameToTypeName)

        val mapFromNonUniquelyNamedConstantNameToTypeName =
                processTypeNames(expressionBasedModel.mapFromNonUniquelyNamedConstantNameToTypeName)

        val mapFromUniquelyNamedConstantNameToTypeName =
                processTypeNames(expressionBasedModel.mapFromUniquelyNamedConstantNameToTypeName)

        val mapFromCategoricalTypeNameToSizeString = processMapFromCategoricalTypeNameToSizeString()

        DefaultExpressionBasedModel(
                factors,
                mapFromRandomVariableNameToTypeName,
                mapFromNonUniquelyNamedConstantNameToTypeName,
                mapFromUniquelyNamedConstantNameToTypeName,
                mapFromCategoricalTypeNameToSizeString,
                expressionBasedModel.additionalTypes,
                expressionBasedModel.isKnownToBeBayesianNetwork,
                expressionBasedModel.theory
        )
    }

    private fun processMapFromCategoricalTypeNameToSizeString() = mapOf<String, String>()
    private fun processTypeNames(mapToTypeNames: MutableMap<String, String>) =
            mapToTypeNames.mapValues(::processTypeName)

    private fun processTypeName(symbolAndTypeName: Map.Entry<String, String>): String {
        val typeName = symbolAndTypeName.value
        return processTypeName(typeName)
    }

    protected abstract fun processTypeName(typeName: String): String

    private fun processAllSubExpressions(expression: Expression): Expression {
        return expression.replaceAllOccurrences({processSubExpression(it!!)}, expressionBasedModel.context)
    }

    protected abstract fun processSubExpression(subExpression: Expression): Expression
}