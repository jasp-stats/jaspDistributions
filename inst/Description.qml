import QtQuick
import JASP.Module

Description
{
	title :			qsTr("Distributions")
	icon:			"discoverdistributions-distributions.svg"
	description: 	qsTr("Visualize distributions and fit them to data")
	requiresData:	false
	hasWrappers	: true

	// Setting directly the useSubMenus property would crash in JASP 0.96.0, since this setting does not exist in this version.
	// Instead set it via jasvaScript: this will just add a warning in the log file if the property does not exist.
	// Component.onCompleted: useSubMenus = true

	GroupTitle
	{
		title: 	qsTr("Continuous")
		icon: 	"discoverdistributions-continuousDist.svg"
	}

	Analysis
	{
		title:			qsTr("Single Distribution")
		qml:			"ContinuousDistribution.qml"
		func:			"ContinuousDistribution"
		preloadData:	true
	}

	Analysis
	{
		title:			qsTr("Compare Distributions")
		qml:			"CompareContinuousDistributions.qml"
		func:			"compareContinuousDistributions"
		requiresData:	true
		preloadData:	true
	}

	// LDgaussianunivariate is not in the menu (it is replaced by ContinuousDistribution), but it is kept because other
	// modules use it: jaspLearnStats uses its form (NormalDistribution in qmldir) and LDgaussianunivariateInternal.

	Analysis
	{
		title:	qsTr("Mixture of normal and normal")
		qml:	"LDmixtureNormalNormal.qml"
		func:	"LDmixtureNormalNormal"
	}

	Analysis
	{
		title:	qsTr("Mixture of normal and uniform")
		qml:	"LDmixtureNormalUniform.qml"
		func:	"LDmixtureNormalUniform"
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

	// GroupTitle
	// {
	// 	title: 	qsTr("Compare Distributions")
	// 	icon: 	"discoverdistributions-compareDistributions.svg"
	// }

}
