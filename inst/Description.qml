import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	title :			qsTr("Distributions")
	name:			"jaspDistributions"
	icon:			"discoverdistributions-distributions.svg"
	description: 	qsTr("Discover distributions with JASP")
	version:		"0.2"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"www.jasp-stats.org"
	license:		"GPL (>= 2)"
	requiresData:	false

	GroupTitle
	{
		title: 	qsTr("Continuous")
		icon: 	"discoverdistributions-continuousDist.svg"
	}

	Analysis
	{
		title:	qsTr("Normal")
		qml:	"LDgaussianunivariate.qml"
		func:	"LDgaussianunivariate"
	}

	Analysis
	{
		title:	qsTr("Skew normal")
		qml:	"LDnormalSkew.qml"
		func:	"LDnormalSkew"
	}

	Analysis
	{
		title:	qsTr("Generalized normal (version 1)")
		qml:	"LDnormalGeneralized.qml"
		func:	"LDnormalGeneralized"
	}

	Analysis
	{
		title:	qsTr("Cauchy")
		qml:	"LDcauchy.qml"
		func:	"LDcauchy"
	}

	Analysis
	{
	  title:	qsTr("Student's t")
	  qml:		"LDt.qml"
	  func:		"LDt"
	}

	Analysis
	{
	  title:	qsTr("Skew t")
	  qml:		"LDtSkew.qml"
	  func:		"LDtSkew"
	}

	Analysis
	{
	  title:	qsTr("Skewed generalized t")
	  qml:		"LDtSkewedGeneralized.qml"
	  func:		"LDtSkewedGeneralized"
	}

	Analysis
	{
	  title:	qsTr("F-distribution")
	  qml:		"LDf.qml"
	  func:		"LDf"
	}

	Analysis
	{
		title:	qsTr("Chi-squared")
		qml:	"LDchisq.qml"
		func:	"LDchisq"
	}

	Analysis
	{
		title:	qsTr("Beta")
		qml:	"LDbeta.qml"
		func:	"LDbeta"
	}

	Analysis
	{
		title:	qsTr("Stretched beta")
		qml:	"LDbetaStretched.qml"
		func:	"LDbetaStretched"
	}

	Analysis
	{
		title:	qsTr("Gamma")
		qml:	"LDgamma.qml"
		func:	"LDgamma"
	}

	Analysis
	{
		title:	qsTr("Inverse gamma")
		qml:	"LDgammaInverse.qml"
		func:	"LDgammaInverse"
	}

	Analysis
	{
		title:	qsTr("Exponential")
		qml:	"LDexponential.qml"
		func:	"LDexponential"
	}

	Analysis
	{
		title:	qsTr("Laplace")
		qml:	"LDlaplace.qml"
		func:	"LDlaplace"
	}

	Analysis
	{
		title:	qsTr("Log-normal")
		qml:	"LDlognormal.qml"
		func:	"LDlognormal"
	}

	Analysis
	{
		title:	qsTr("Logistic")
		qml:	"LDlogistic.qml"
		func:	"LDlogistic"
	}

	Analysis
	{
		title:	qsTr("Pareto")
		qml:	"LDpareto.qml"
		func:	"LDpareto"
	}

	Analysis
	{
		title:	qsTr("Amoroso")
		qml:	"LDamoroso.qml"
		func:	"LDamoroso"
	}

	Analysis
	{
		title:	qsTr("Triangular")
		qml:	"LDtriangular.qml"
		func:	"LDtriangular"
	}

	Analysis
	{
		title:	qsTr("Wald (inverse Gaussian)")
		qml:	"LDwald.qml"
		func:	"LDwald"
	}

	Analysis
	{
		title:	qsTr("Weibull")
		qml:	"LDweibull.qml"
		func:	"LDweibull"
	}

	GroupTitle
	{
		title: 	qsTr("Discrete")
		icon: 	"discoverdistributions-discreteDist.svg"
	}

	Analysis
	{
		title:	qsTr("Bernoulli")
		qml:	"LDbernoulli.qml"
		func:	"LDbernoulli"
	}

	Analysis
	{
		title:	qsTr("Binomial")
		qml:	"LDbinomial.qml"
		func:	"LDbinomial"
	}

	Analysis
	{
		title:	qsTr("Beta-binomial")
		qml:	"LDbetaBinomial.qml"
		func:	"LDbetaBinomial"
	}

	Analysis
	{
		title:	qsTr("Poisson")
		qml:	"LDpoisson.qml"
		func:	"LDpoisson"
	}

	Analysis
	{
		title:	qsTr("Negative binomial")
		qml:	"LDnegbinomial.qml"
		func:	"LDnegbinomial"
	}

	Analysis
	{
		title:	qsTr("Zero-inflated Poisson")
		qml:	"LDpoissonZeroInflated.qml"
		func:	"LDpoissonZeroInflated"
	}

	Analysis
	{
		title:	qsTr("Zero-inflated negative binomial")
		qml:	"LDnegbinomialZeroInflated.qml"
		func:	"LDnegbinomialZeroInflated"
	}

	Analysis
	{
		title:	qsTr("Geometric")
		qml:	"LDgeometric.qml"
		func:	"LDgeometric"
	}

	Analysis
	{
		title:	qsTr("Hypergeometric")
		qml:	"LDhypergeometric.qml"
		func:	"LDhypergeometric"
	}
}
