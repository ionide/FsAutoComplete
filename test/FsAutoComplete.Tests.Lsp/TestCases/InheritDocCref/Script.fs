module InheritDocTest

open InheritDocLib

// Line 4: consumer is InheritDocConsumer, which uses <inheritdoc cref="..."/> on its members
let consumer = InheritDocConsumer()
let inheritedResult = consumer.GetValue()
let inheritedNamed = consumer.GetNamed("world")

// Line 9: provider is DocumentedProvider, which has direct XML documentation
let provider = DocumentedProvider()
let directResult = provider.GetValue()
let directNamed = provider.GetNamed("world")
